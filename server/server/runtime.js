const adapter = require("../adapters/cdp");

const runtime = (data = {}, { success, error, stop }) => {
	switch(data.action) {
		case "evaluate":
			return evaluate(data, { success, error });
		case "getCompletion":
			return getCompletion(data, { success, error });
		case "getProperties":
			return getProperties(data, { success, error });
		case "activateBreakpoints":
			return activateBreakpoints(data, { success, error });
		case "deactivateBreakpoints":
			return deactivateBreakpoints(data, { success, error });
		case "addBreakpoint":
			return addBreakpoint(data, { success, error });
		case "removeBreakpoint":
			return removeBreakpoint(data, { success, error });
		case "resume":
			return resume(data, { success, error });
		case "stepInto":
			return stepInto(data, { success, error });
		case "stepOut":
			return stepOut(data, { success, error });
		case "stepOver":
			return stepOver(data, { success, error });
		case "continueToLocation":
			return continueToLocation(data, { success, error });
		case "getSource":
			return getSource(data, { success, error });
		case "getSourcemapSources":
			return getSourcemapSources(data, { success, error });
		case "getScriptSources":
			return getScriptSources(data, { success, error });
		default:
			return error(`Unknown runtime action ${data.action}`);
	}
};

const evaluate = async ({ expression, frameId } = {}, { success, error }) => {
	if (!expression) {
		error("No expression to evaluate");
	}

	try {
		let result = await adapter.evaluate({expression, frameId});
		success(result);
	} catch(e) {
		error(e.message);
	}
};

const getCompletion = async ({ expression, frameId } = {}, { success, error }) => {
	try {
		success(await adapter.getCompletion({expression, frameId}));
	} catch(e) {
		error(e.message);
	}
};

const getProperties = async ({ id } = {}, { success, error }) => {
	try {
		success(await adapter.getProperties(id));
	} catch(e) {
		error(e.message);
	}
};

const activateBreakpoints = (_, { success, error }) => {
	try {
		adapter.activateBreakpoints();
		success();
	} catch(e) {
		error(e.message);
	}
};

const deactivateBreakpoints = (_, { success, error }) => {
	try {
		adapter.deactivateBreakpoints();
		success();
	} catch(e) {
		error(e.message);
	}
};

const addBreakpoint = (data, { success, error }) => {
	try {
		adapter.addBreakpoint(data);
		success();
	} catch(e) {
		error(e.message);
	}
};

const removeBreakpoint = async (data, { success, error }) => {
	try {
		await adapter.removeBreakpoint(data);
		success();
	} catch(e) {
		error(e.message);
	}
};

const resume = (_, { success, error }) => {
	try {
		adapter.resume();
		success();
	} catch(e) {
		error(e.message);
	}
};

const stepInto = (_, { success, error }) => {
	try {
		adapter.stepInto();
		success();
	} catch(e) {
		error(e.message);
	}
};

const stepOut = (_, { success, error }) => {
	try {
		adapter.stepOut();
		success();
	} catch(e) {
		error(e.message);
	}
};

const stepOver = (_, { success, error }) => {
	try {
		adapter.stepOver();
		success();
	} catch(e) {
		error(e.message);
	}
};

const continueToLocation = async ({ location }, { success, error }) => {
	try {
		await adapter.continueToLocation(location);
		success();
	} catch(e) {
		error(e.message);
	}
};

const getSource = async ({ id }, { success, error }) => {
	try {
		success(await adapter.getSource(id));
	} catch(e) {
		error(e.message);
	}
};

const getSourcemapSources = async ({ }, { success, error }) => {
	try {
		success(await adapter.getSourcemapSources());
	} catch(e) {
		error(e.message);
	}
};

const getScriptSources = async ({ }, { success, error }) => {
	try {
		success(await adapter.getScriptSources());
	} catch(e) {
		error(e.message);
	}
};

module.exports = runtime;
