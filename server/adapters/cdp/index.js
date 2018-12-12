const cdp = require("chrome-remote-interface");
const { resolveUrl } = require("../../helpers/workspace");
const { queued } = require("../../helpers/queue");
const { doOrRetry } = require("../../helpers/async");
const {
	send, error,
	convertRemoteObject,
	convertCompletionResult,
	convertConsoleEventType,
	convertCallFrames,
	resolveFileLocation,
	resolveScriptLocation,
	completionFunction,
	getScriptSourceMap
} = require("./helpers");

let state = {
	client: null,
	configuration: null,
	scripts: {},
	breakpoints: {},
	currentCallFrameId: null,
	isChrome: false
};

const enableListeners = () => {
	if (state.isChrome) {
		state.client.Log.entryAdded(log);
	}
	state.client.Runtime.exceptionThrown(logException);
	state.client.Runtime.consoleAPICalled(consoleToLog);
	state.client.Debugger.scriptParsed(scriptAdded);
	state.client.Debugger.paused(debuggerPaused);
	state.client.Debugger.resumed(debuggerResumed);
};

const enableDomains = () => {
	if (state.isChrome) {
		state.client.Log.enable();
		state.client.Network.enable();
	}
	state.client.Debugger.enable();
	state.client.Debugger.setPauseOnExceptions({
		state: "uncaught"
	});
	state.client.Runtime.enable();
	state.client.Runtime.runIfWaitingForDebugger();
};

// API

const connect = async (options = {}) => {
	if (state.client) {
		throw new Error("Already connected, please close the current connection first");
	}
	state.configuration = options;
	state.client = await doOrRetry(async () => await cdp(state.configuration));

	state.isChrome = !!state.client.Network;

	enableListeners();
	enableDomains();

	return state.client;
};

const disconnect = async () => {
	if (state.client) {
		await state.client.close();
		state.client = null;
	}
};

const evaluate = async ({expression, frameId}) => {
	ensureConnected();
	let method = state.currentCallFrameId
		? state.client.Debugger.evaluateOnCallFrame
		: state.client.Runtime.evaluate;
	let response = await method({
		generatePreview: true,
		includeCommandLineAPI: true,
		callFrameId: frameId || state.currentCallFrameId,
		expression
	});

	return convertRemoteObject(response.result);
};

const getProperties = async (id) => {
	ensureConnected();

	let response = await state.client.Runtime.getProperties({
		objectId: id,
		ownProperties: true,
		generatePreview: true
	});

	return response.result.map(({ name, value, get }) => ({
		name,
		value: convertRemoteObject(value || get)
	}));
};

const getCompletion = async ({expression, frameId}) => {
	ensureConnected();

	let method = state.currentCallFrameId
		? state.client.Debugger.evaluateOnCallFrame
		: state.client.Runtime.evaluate;

	let response = await method({
		expression,
		objectGroup: "completion",
		includeCommandLineAPI: true,
		callFrameId: frameId || state.currentCallFrameId
	});

	let { objectId, type } = response.result;

	if (objectId) {
		return getCompletionByReference(objectId);
	} else {
		return getCompletionByType(type);
	}
};

const addBreakpoint = breakpoint => {
	ensureConnected();

	if (hasBreakpointAt(breakpoint)) {
		throw new Error(`Breakpoint at location already set with id ${breakpoint.id}`);
	}

	state.breakpoints[breakpoint.id] = breakpoint;
	registerBreakpoint(breakpoint);
};

const removeBreakpoint = async ({ id }) => {
	let breakpoint = state.breakpoints[id];
	delete state.breakpoints[id];
	unregisterBreakpoint(breakpoint);
};

const activateBreakpoints = () => {
	state.client.Debugger.setBreakpointsActive({
		active: true
	});
};

const deactivateBreakpoints = () => {
	state.client.Debugger.setBreakpointsActive({
		active: false
	});
};

const getSource = async id => {
	let { scriptSource } = await state.client.Debugger.getScriptSource({scriptId: id});
	return scriptSource;
};

const resume = () => {
	state.client.Debugger.resume();
};

const stepInto = () => {
	state.client.Debugger.stepInto();
};

const stepOut = () => {
	state.client.Debugger.stepOut();
};

const stepOver = () => {
	state.client.Debugger.stepOver();
};

const continueToLocation = async fileLocation => {
	let location = await resolveFileLocation(
		fileLocation,
		state.configuration,
		state.scripts
	);

	if (location) {
		let script = Object.values(state.scripts).find(s => s.url === location.url);
		await state.client.Debugger.continueToLocation({
			location: {
				scriptId: script.id,
				lineNumber: --location.line,
				columnNumber: location.column
			}
		});
	} else {
		error(`Invalid location to jump to ${fileLocation.file}:${fileLocation.line}`);
	}
};

// Events

const scriptAdded = ({ scriptId, url, sourceMapURL }) => {
	try {
		let script = { id: scriptId, url, sourceMapURL };

		// Remove any previous version of the same script first
		for (let id of Object.keys(state.scripts)) {
			if (state.scripts[id].url === url) {
				delete state.scripts[id];
			}
		}

		state.scripts[scriptId] = script;

		// We've got new script, so let's try to resolve unresolved breakpoints
		if (script.url || script.sourceMapURL) {
			resolveAllBreakpoints();
		}
	} catch(e) {
		error(e.message);
	}
};

const debuggerPaused = async ({ callFrames, reason, data = {} }) => {
	try {
		if (state.isChrome) {
			state.client.Overlay.setPausedInDebuggerMessage({
				message: "Paused in Indium"
			});
		}

		state.currentCallFrameId = callFrames[0].callFrameId;

		notify({
			type: "paused",
			frames: await convertCallFrames(
				callFrames,
				state.configuration,
				state.scripts
			),
			reason: reason === "exception"
				? "Exception occured"
				: "Breakpoint hit",
			description: data.description
		});

	} catch(e) {
		error(e.message);
	}
};

const debuggerResumed = () => {
	try {
		state.currentCallFrameId = null;

		if (state.isChrome) {
			state.client.Overlay.setPausedInDebuggerMessage();
		}

		notify({ type: "resumed" });
	} catch (e) {
		error(e.message);
	}
};

// Async operations

const registerBreakpoint = async breakpoint => {
	try {
		let urlLocation = await resolveFileLocation(
			breakpoint,
			state.configuration,
			state.scripts
		);

		// The breakpoint doesn't resolve to any location.  The script might not
		// have been parsed yet, to we'll try again later.
		if (!urlLocation) {
			return;
		}

		let { url, line, column } = urlLocation;

		console.log(`Setting breakpoint to ${url}:${line}:${column}`);

		let result = await state.client.Debugger.setBreakpointByUrl({
			url,
			condition: breakpoint.condition,
			// lines are 1-based in NPM's source-map package and Emacs, but 0-based
			// in the CDP.
			lineNumber: --line,
			columnNumber: column
		});

		let { locations: [ location ], breakpointId } = result;

		console.dir(result);

		breakpoint.remoteId = breakpointId;

		if (location) {
			let { line, column } = await resolveScriptLocation(
				{
					scriptId: location.scriptId,
					line: ++location.lineNumber,
					column: location.columnNumber
				},
				state.configuration,
				state.scripts
			);
			breakpointResolved(breakpoint, line);
		}

	} catch(e) {
		error(e.message);
	}
};

/**
 * Resolve all breakpoints that are not yet resolved.
 */

const resolveAllBreakpoints = queued(async () => {
	let unresolved = Object.values(state.breakpoints).filter(
		brk => !brk.resolved
	);

	for (let brk of unresolved) {
		await reRegisterBreakpoint(brk);
	}
});

/**
 * Some breakpoint registrations might have failed in the past for two reasons:
 * - no parsed script could be found for the breakpoint
 * - the breakpoint resolution itself failed
 *
 * Try to register it again, removing it from the runtime first if it was
 * registered.
 */
const reRegisterBreakpoint = async (breakpoint) => {
	await unregisterBreakpoint(breakpoint);
	await registerBreakpoint(breakpoint);
};

const unregisterBreakpoint = async (breakpoint) => {
	let remoteId = breakpoint.remoteId;
	delete breakpoint.remoteId;

	if (remoteId) {
		try {
			await state.client.Debugger.removeBreakpoint({ breakpointId: remoteId });
		} catch(e) {
			error(e.message);
		}
	}
};

const breakpointResolved = (breakpoint, line) => {
	breakpoint.line = line;
	breakpoint.resolved = true;
	notify({
		type: "breakpointResolved",
		line,
		id: breakpoint.id
	});
};

const hasBreakpointAt = ({ id, file, line }) => {
	if (state.breakpoints[id]) {
		return true;
	}

	return Object.values(state.breakpoints).some(brk =>
		brk.file === file && brk.line === line
	);
};

const getSourcemapSources = async () => {
	let sources = [];
	for (let script of Object.values(state.scripts)) {
		let sourcemap = await getScriptSourceMap(
			script,
			state.configuration
		);

		sourcemap && sourcemap.eachMapping(mapping => {
			sources.push(mapping.source);
		});
	}

	return [ ... new Set(sources) ];
};

const getScriptSources = async () => {
	let sources = [];
	return Object.values(state.scripts)
		.filter(s => s.url)
		.map(s => resolveUrl(s.url, state.configuration));
};

// Helpers

const getCompletionByReference = async objectId => {
	let result = await state.client.Runtime.callFunctionOn({
		functionDeclaration: completionFunction,
		returnByValue: true,
		objectId
	});

	return convertCompletionResult(result);
};

const getCompletionByType = async type => {
	let expression = `(${completionFunction})("${type}")`;
	let result = await state.client.Runtime.evaluate({
		expression,
		returnByValue: true
	});

	return convertCompletionResult(result);
};

const logException = ({ exceptionDetails: { exception, url } }) => {
	send({
		type: "log",
		payload: { type: "error", result: convertRemoteObject(exception) , url }
	});
};

const log = ({ entry: { level, text, url, line }}) => {
	const filter = [ "verbose" ];
	if (!filter.includes(level)) {
		send({
			type: "log",
			payload: { type: level, result: { description: text }, url, line }
		});
	}
};

const consoleToLog = ({ type, args }) => {
	args.forEach(remoteObj => {
		send({
			type: "log",
			payload: {
				type: convertConsoleEventType(type),
				result: convertRemoteObject(remoteObj)
			}
		});
	});
};

const notify = (payload) => {
	send({
		type: "notification",
		payload
	});
};

const ensureConnected = () => {
	if (!state.client) {
		throw new Error("Not connected");
	}
};

module.exports = {
	connect,
	disconnect,
	evaluate,
	getCompletion,
	getProperties,
	activateBreakpoints,
	deactivateBreakpoints,
	addBreakpoint,
	removeBreakpoint,
	resume,
	stepOver,
	stepInto,
	stepOut,
	continueToLocation,
	getSource,
	getSourcemapSources,
	getScriptSources,
	_test: {state, scriptAdded}
};
