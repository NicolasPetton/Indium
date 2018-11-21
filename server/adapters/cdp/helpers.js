const { resolveSourceMap } = require("../../helpers/sourcemap");
const { resolveUrl, isFile } = require("../../helpers/workspace");
const { URL } = require("url");

const send = data => {
	require("../../server").send(data);
};

const error = data => {
	require("../../server").error()(data);
};

const convertRemoteObject = ({ objectId, value, type, subtype, description, preview }) => {
	if (objectId) {
		return {
			id: objectId,
			type: subtype || type,
			description: convertDescription(description),
			preview: preview ? convertPreview(preview) : ""
		};
	}

	return { description: convertValue({ value, type, subtype }) };
};

const convertDescription = (description) => {
	return description.length > 200
		? `${description.substring(0, 200)}…`
		: description;
};

const convertValue = ({ value, type, subtype }) => {
	if (type === "undefined") {
		return "undefined";
	}
	if (type === "string") {
		return `"${value}"`;
	}
	if (value === null) {
		return "null";
	}
	if (type === "function") {
		return "function";
	}

	return `${value}`;
};

const convertPreview = preview => {
	if (preview.subtype === "array") {
		return convertArrayPreview(preview);
	}

	if (preview.subtype === "map") {
		return convertMapPreview(preview);
	}

	if (preview.subtype === "set") {
		return convertSetPreview(preview);
	}

	return convertObjectPreview(preview);
};

const convertArrayPreview = preview => {
	let properties = preview.properties
		.map(convertPreviewProperty)
		.join(", ");

	return `[ ${properties}${preview.overflow ? ", …" : ""} ]`;
};

const convertSetPreview = preview => {
	let properties = preview.entries
		.map(entry => convertPreviewProperty(entry.value))
		.join(", ");

	return `[ ${properties}${preview.overflow ? ", …" : ""} ]`;
};

const convertMapPreview = preview => {
	let properties = preview.entries
		.map(entry => `${convertPreviewProperty(entry.key)} ⇒ ${convertPreviewProperty(entry.value)}`)
		.join(", ");

	return `[ ${properties}${preview.overflow ? ", …" : ""} ]`;
};

const convertObjectPreview = preview => {
	let properties = preview.properties
		.map(prop => `${prop.name}: ${convertPreviewProperty(prop)}`)
		.join(", ");

	return `{ ${properties}${preview.overflow ? ", …" : ""} }`;
};

const convertPreviewProperty = property => {
	if (property.type === "string") {
		return `"${property.value || property.description}"`;
	}

	return property.value || property.description || property.type;
};

const convertCompletionResult = result => {
	return Object.keys(result.result.value);
};

const convertConsoleEventType = type => {
	switch(type) {
		case "error":
			return "error";
		case "warning":
			return "warning";
		default:
			return "log";
	}
};

const convertCallFrames = async (frames, conf, scripts) => {
	let result = [];
	for (let frame of frames) {
		result.push(await convertCallFrame(frame, conf, scripts));
	}

	return result;
};

const convertCallFrame = async (
	{ functionName, location, scopeChain, callFrameId },
	conf,
	scripts
) => {
	let fileLocation = await resolveScriptLocation({
		scriptId: location.scriptId,
		line: ++location.lineNumber,
		column: location.columnNumber
	}, conf, scripts);

	return {
		functionName,
		id: callFrameId,
		scriptId: location.scriptId,
		location: fileLocation,
		scopeChain: scopeChain
			.filter(s => s.type !== "global")
			.map(s => ({
				type: s.type,
				name: s.name,
				id: convertRemoteObject(s.object).id
			}))
	};
};

const resolveFileLocation = async (location, conf, scripts = {}) => {
	return await resolveFileLocationWithSourceMaps(location, conf, scripts)
		|| resolveFileLocationWithScriptUrls(location, conf, scripts);
};

const resolveFileLocationWithSourceMaps = async ({ file, line, column }, conf, scripts) => {
	for (let script of Object.values(scripts)) {
		let sourcemap = await getScriptSourceMap(script, conf);

		if (sourcemap) {
			let position = sourcemap.generatedPositionFor({
				source: file, line, column
			});

			if (position.line) {
				return {
					url: script.url,
					line: position.line,
					column: position.column
				};
			}
		}
	}

	return null;
};

const resolveFileLocationWithScriptUrls = ({ file, line }, conf, scripts) => {
	for (let script of Object.values(scripts)) {
		if (script.url) {
			let scriptFile = resolveUrl(script.url, conf);
			if (file === scriptFile) {
				return {
					url: script.url,
					line,
					column: 0
				};
			}
		}
	}

	return null;
};

const resolveScriptLocation = async (location, conf, scripts = {}) => {
	let script = Object.values(scripts).find(s => s.id === location.scriptId);
	if (!script) {
		return { ...location, url: "" };
	}

	return await resolveUrlLocation(
		{
			...location,
			url: script.url
		},
		conf,
		scripts
	);
};

const resolveUrlLocation = async (location, conf, scripts = {}) => {
	return await resolveUrlLocationWithSourceMaps(location, conf, scripts)
		|| await resolveUrlLocationWithConfiguration(location, conf, scripts);
};

const resolveUrlLocationWithSourceMaps = async (location, conf, scripts) => {
	let script = Object.values(scripts).find(s => s.url === location.url);

	if (script && await getScriptSourceMap(script, conf)) {
		let { source, line, column } = script.sourceMap.originalPositionFor(location);

		if (isFile(source) && line) {
			return { file: source, line, column };
		}
	}

	return null;
};

const resolveUrlLocationWithConfiguration = ({ url, line, column = 0 }, conf) => {
	return {
		line,
		column,
		file: resolveUrl(url, conf)
	};
};

const getScriptSourceMap = async (script, conf) => {
	if (script.sourceMap) return script.sourceMap;

	if (script.sourceMapURL) {
		try {
			script.sourceMap = await resolveSourceMap(script, conf);
		} catch(e) {
			send({
				type: "log",
				payload: {
					type: "log",
					result: {
					description: `Soucemap parsing failed for ${script.url}: ${e.message}`
					}
				}
			});
		} finally {
			// Delete the URL so we won't try to get the source map again.
			delete script.sourceMapURL;
		}

		return script.sourceMap;
	}
};

const completionFunction = "function getCompletions(type)\n{var object;if(type==='string')\nobject=new String('');else if(type==='number')\nobject=new Number(0);else if(type==='boolean')\nobject=new Boolean(false);else\nobject=this;var resultSet={};for(var o=object;o;o=o.__proto__){try{if(type==='array'&&o===object&&ArrayBuffer.isView(o)&&o.length>9999)\ncontinue;var names=Object.getOwnPropertyNames(o);for(var i=0;i<names.length;++i)\nresultSet[names[i]]=true;}catch(e){}}\nreturn resultSet;}";

module.exports = {
	send,
	error,
	convertRemoteObject,
	convertCompletionResult,
	convertConsoleEventType,
	convertCallFrames,
	completionFunction,
	resolveFileLocation,
	resolveUrlLocation,
	resolveScriptLocation,
	getScriptSourceMap
};
