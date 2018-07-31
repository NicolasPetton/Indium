const proxyquire = require("proxyquire");

const connection = jasmine.createSpy("connection");
const version = jasmine.createSpy("version");

const respond = proxyquire("../../server/respond", {
	"./version": version,
	"./connection": connection
});

describe("Responding", () => {
	it("can call the version handler", () => {
		let data = { type: "version" };
		let server = {};

		respond(data, server);

		expect(version).toHaveBeenCalledWith(data, server);
	});

	it("can call the connection handler with the payload", () => {
		let data = { type: "connection", payload: {action: "connect"} };
		let server = {};

		respond(data, server);

		expect(connection).toHaveBeenCalledWith(data.payload, server);
	});

	it("should default to an error", () => {
		let data = { type: "unknown" };
		let server = { error: jasmine.createSpy("error") };

		respond(data, server);

		expect(server.error).toHaveBeenCalled();
	});
});
