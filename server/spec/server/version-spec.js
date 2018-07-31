const version = require("../../server/version");
const package = require("../../package.json");
const semver = require("semver");

describe("Requesting version", () => {
	it("answers the version", () => {
		let success = jasmine.createSpy("send");
		let expected = {
			version: {
				major: semver.major(package.version),
				minor: semver.minor(package.version),
				patch: semver.patch(package.version)
			}
		};

		version({}, { success });

		expect(success).toHaveBeenCalledWith(expected);
	});
});
