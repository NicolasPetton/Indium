const {
	_test: {state, scriptAdded}
} = require("../../../adapters/cdp/index");

// Regression test for GitHub issue #175
describe("Adding a new script with existing url", () => {
	it("should not result in an undefined script", () => {
		let id1 = "1";
		let id2 = "2";
		let url = "https://foo.bar";

		scriptAdded({scriptId: id1, url});
		scriptAdded({scriptId: id2, url});

		let scripts = state.scripts;

		expect(Object.keys(scripts)).toEqual([id2]);
	});
});
