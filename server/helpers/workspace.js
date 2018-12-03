const { existsSync, readFileSync, readdirSync, statSync } = require("fs");
const { join, resolve, basename, dirname } = require("path");
const { URL, parse } = require("url");

const projectFilename = ".indium.json";

const lookupProjectFile = dir => {
	dir = resolve(dir);

	if (!isDirectory(dir)) {
		throw new Error(`Not a directory "${dir}"`);
	}

	let projectFile = getFiles(dir).find(isProjectFile);
	if (projectFile) {
		return projectFile;
	}

	let parent = getParentDirectory(dir);
	if (parent !== dir) {
		return lookupProjectFile(parent);
	}

	throw new Error(`No ${projectFilename} found`);
};

const isDirectory = path =>
	  existsSync(path) && statSync(path).isDirectory();

const isFile = path =>
	  existsSync(path) && statSync(path).isFile();

const isProjectFile = path =>
	  basename(path) === projectFilename;

const isAbsolute = path => {
	return path.startsWith("/") || /^[a-zA-Z]\:[\\\/]/.test(path);
};

const getFiles = dir =>
	  readdirSync(dir).map(name => join(dir, name)).filter(isFile);

const getParentDirectory = path =>
	  dirname(resolve(path));

const readConfigurations = dir => {
	let file = lookupProjectFile(dir);
	let configurations = JSON.parse(readFileSync(file)).configurations;
	configurations.forEach(conf => conf.projectFile = file);
	configurations.forEach(conf => conf.resolvedRoot = resolveRoot(conf));
	return configurations;
};

const getConfiguration = (dir, name) => {
	let configurations = readConfigurations(dir);
	return configurations.find(conf => conf.name === name);
};

// webRoot is an alias for root.
const getRoot = ({ root, webRoot = "" }) => root || webRoot;

const resolveRoot = conf => {
	let dir = dirname(conf.projectFile || "");

	if (getRoot(conf)) {
		dir = join(dir, getRoot(conf));
	}

	return resolve(dir);
};

const locateScript = (pathname, conf) => {
        let overrides = conf.scriptPathOverrides || {};

        for (let pattern of Object.keys(overrides)) {
                let regex = new RegExp(pattern);

                if (pathname.match(regex)) {
                        return pathname.replace(regex, overrides[pattern]);
                }
        }
        return pathname;
}

const resolveUrl = (url, conf) => {
	// In Node, script urls can be file paths.	The path doesn't
	// always exist either, so also check for a protocol when parsed
	// as a URL.
	if (isAbsolute(url) || !parse(url).protocol) {
		console.log(`Resolved to ${url}`);
		return url;
	}

	let root = resolveRoot(conf);
	let { protocol, pathname } = new URL(url);

	// Always treat URLs using the file: protocol to have absolute pathnames.
	if (protocol === "file:") {
		return pathname;
	}

	return resolve(`${root}/${locateScript(pathname, conf)}`);
};

const expandRoot = (path, root = "") => {
	return path.replace(/\${(root|webRoot)}/, root);
};

module.exports = {
	readConfigurations,
	lookupProjectFile,
	getConfiguration,
	resolveRoot,
	resolveUrl,
	expandRoot,
	isFile
};
