const adapter = require("../adapters/cdp");
const { getConfiguration } = require("../helpers/workspace");

const connection = (data = {}, { success, error, stop }) => {
	switch(data.action) {
		case "connect":
			return connect(data, { success, error });
		case "close":
			return stop();
		default:
			return error(`Unknown connection action ${data.action}`);
	}
};

const connect = (data = {}, { success, error }) => {
	let { directory, name } = data;
	let configuration;

	try {
		configuration = getConfiguration(data.directory, name);
	} catch(e) {
		return error(e);
	}

	if (!configuration) {
		return error(`No configuration named ${name}`);
	}

	connectWithConfiguration(configuration, { success, error });
};

const connectWithConfiguration = (data = {}, { success, error }) => {
	switch(data.type) {
		case "chrome":
			return connectToChrome(data, { success, error });
		case "node":
			return connectToNode(data, { success, error });
		default:
			error(`Unknown connection type ${data.type}`);
	}
};

const connectToChrome = async (options = {}, { success, error }) => {
	try {
		await adapter.connect(options, { success, error });
	} catch(e) {
		return error(e.message);
	}
	success("Connected to Chrome!");
};

const connectToNode = async (options = {}, { success, error }) => {
	try {
		await adapter.connect({
			...options,
			port: options.port || 9229 // The default port is 9229 in NodeJS.
		}, { success, error });
	} catch(e) {
		error(e.message);
	}
	success("Connected to Node!");
};

module.exports = connection;
