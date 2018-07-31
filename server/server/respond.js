const version = require("./version");
const configurations = require("./configurations");
const connection = require("./connection");
const runtime = require("./runtime");

const respond = (data = {}, server) => {
	switch(data.type) {
		case "version":
			return version(data, server);
		case "configurations":
			return configurations(data.payload, server);
		case "connection":
			return connection(data.payload, server);
		case "runtime":
			return runtime(data.payload, server);
		default:
			return server.error(`Unknown data type ${data.type}`);
	}
};

module.exports = respond;
