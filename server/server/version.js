const package = require("../package.json");
const semver = require("semver");

const version = (_data, { success }) => {
	success({
		version: {
			major: semver.major(package.version),
			minor: semver.minor(package.version),
			patch: semver.patch(package.version)
		}
	});
};

module.exports = version;
