const fs = require("fs");
const { readConfigurations } = require("../helpers/workspace");

const configurations = (data = {}, { success, error }) => {
	switch(data.action) {
		case "list":
			return listConfigurations(data, { success, error });
		default:
			return error(`Unknown action ${data.action}`);
	}
};

const listConfigurations = (data, { success, error }) => {
	try {
		let configurations = readConfigurations(data.directory);
		success(configurations);
	} catch(e) {
		error(e.message);
	}
};

module.exports = configurations;
