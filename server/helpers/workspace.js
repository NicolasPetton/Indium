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

const resolveUrl = (url, conf) => {
	// In Node, script urls can be file paths
	if (!parse(url).protocol) {
		return url;
	}
	let root = resolveRoot(conf);
	let { pathname } = new URL(url);
	return resolve(`${root}/${pathname}`);
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
