const { SourceMapConsumer } = require("source-map");
const { readFileSync } = require("fs");
const path = require("path");
const { URL, resolve } = require("url");
const fetch = require("node-fetch");

const { expandRoot, resolveRoot } = require("./workspace");

const defaultSourceMapPathOverrides = {
	"webpack:///./~/": "${root}/node_modules/",
	"webpack:///src/": "${root}/",
	"webpack:///./":   "${root}/",
	"webpack:///":     "${root}/"
};

const resolveSourceMap = async ({ url = "", sourceMapURL } = {}, conf) => {
    let sourcemapPath = resolve(url, sourceMapURL);
    let contents = await getSourceMapContents(sourcemapPath);
    let json = JSON.parse(contents);
    json.sources = json.sources.map(s => {
		return path.resolve(sanitizePath(applySourceMapPathOverrides(s, conf)));
    });
	return await new SourceMapConsumer(json);
};

/**
 * `path` can either reference an URL, inline data (base64 or uri-encoded), or a
 * file path on disk.
 */
const getSourceMapContents = async path => {
	if (path.indexOf("data:application/json") >= 0) {
		return getInlineSourceMapContents(path);
	} else {
		return getUrlSourceMapContents(path);
	}
};

const getInlineSourceMapContents = uri => {
	const firstCommaPos = uri.indexOf(",");
	const header = uri.substr(0, firstCommaPos);
    const data = uri.substr(firstCommaPos + 1);

    if (header.indexOf(";base64") !== -1) {
        const buffer = new Buffer(data, "base64");
        return buffer.toString();
    } else {
        // URI encoded.
        return decodeURI(data);
    }
};

const getUrlSourceMapContents = async path => {
	let url = new URL(path);
	if (url.protocol === "file:") {
		return readFileSync(url).toString();
	} else {
		return await (await fetch(url)).text();
	}
};

const getSourceMapPathOverrides = ({ sourceMapPathOverrides } = {}) =>
	  sourceMapPathOverrides || defaultSourceMapPathOverrides;

/**
 * Return a path with "sourceMapPathOverrides" applied from `conf`.
 */
const applySourceMapPathOverrides = (path, conf) => {
	let overrides = getSourceMapPathOverrides(conf);
	let root = resolveRoot(conf);
	return Object.keys(overrides).reduce((acc, key) => {
		return acc.replace(new RegExp(key), expandRoot(overrides[key], root));
	}, path);
};

const sanitizePath = path => {
	// Remove query params
    if (path.indexOf("?") >= 0) {
        path = path.split("?")[0];
    }

	return path;
};

const absoluteSourceMapPath = ({ url, sourceMapUrl }) => {
	return resolve(url, sourceMapUrl);
};

module.exports = {
	resolveSourceMap,
	applySourceMapPathOverrides,
	absoluteSourceMapPath
};
