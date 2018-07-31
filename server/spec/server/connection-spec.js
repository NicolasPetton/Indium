const connection = require("../../server/connection");

describe("Connection", () => {
	it("can close the connection", () => {
		let data = { action: "close" };
		let server = { stop: jasmine.createSpy("stop") };

		connection(data, server);

		expect(server.stop).toHaveBeenCalled();
	});
});
