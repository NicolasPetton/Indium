const {
	applySourceMapPathOverrides,
	absoluteSourceMapPath
} = require("../../helpers/sourcemap");

describe("Applying source map path overrides", () => {
	it("should leave the path intact when there is no override", () => {
		let conf = { sourceMapPathOverrides: {} };
		expect(applySourceMapPathOverrides("/foo/bar", conf)).toEqual("/foo/bar");
	});

	it("should leave the path intact when no override matches", () => {
		let conf = { sourceMapPathOverrides: {"/baz": "/quux"} };
		expect(applySourceMapPathOverrides("/foo/bar", conf)).toEqual("/foo/bar");
	});

	it("should apply matching overrides", () => {
		let conf = {
			sourceMapPathOverrides: {
				"foo://": "/foo",
				"bar://": "/bar"
			}
		};
		expect(applySourceMapPathOverrides("foo:///bar", conf)).toEqual("/foo/bar");
		expect(applySourceMapPathOverrides("bar:///foo", conf)).toEqual("/bar/foo");
	});

	it("should expand the root directory when apply matching overrides", () => {
		let conf = {
			projectFile: "/home/user/p/.indium.json",
			root: "src",
			sourceMapPathOverrides: {
				"foo://": "${root}/foo",
				"bar://": "${root}/bar"
			}
		};
		expect(applySourceMapPathOverrides("foo:///bar", conf))
			.toEqual("/home/user/p/src/foo/bar");
		expect(applySourceMapPathOverrides("bar:///foo", conf))
			.toEqual("/home/user/p/src/bar/foo");
	});

	it("should use the default overrides when no override is provided", () => {
		let conf = {
			projectFile: "/home/user/p/.indium.json",
			root: "src"
		};
		expect(applySourceMapPathOverrides("webpack:///./bar", conf))
			.toEqual("/home/user/p/src/bar");
		expect(applySourceMapPathOverrides("webpack:///src/bar", conf))
			.toEqual("/home/user/p/src/bar");
		expect(applySourceMapPathOverrides("webpack:///./~/foo", conf))
			.toEqual("/home/user/p/src/node_modules/foo");
		expect(applySourceMapPathOverrides("webpack:///foo", conf))
			.toEqual("/home/user/p/src/foo");
	});
});

describe("Absolute source map paths", () => {
	it("should leave the path intact when already absolute", () => {
		let sourceMapUrl = "http://bar.com/s.js.map";
		expect(absoluteSourceMapPath({url: "http://foo.com/", sourceMapUrl}))
			.toEqual(sourceMapUrl);
	});

	it("should leave the path intact when using the file:// protocol", () => {
		let sourceMapUrl = "file:///home/user/s.js.map";
		expect(absoluteSourceMapPath({url: "http://foo.com/", sourceMapUrl}))
			.toEqual(sourceMapUrl);
	});

	it("should return an absolute url resolved using `url`", () => {
		let sourceMapUrl = "bar/s.js.map";
		expect(absoluteSourceMapPath({url: "http://foo.com/", sourceMapUrl}))
			.toEqual("http://foo.com/bar/s.js.map");
	});

	it("should canonicalize urls", () => {
		let sourceMapUrl = "../baz/a/../s.js.map";
		expect(absoluteSourceMapPath({url: "http://foo.com/bar", sourceMapUrl}))
			.toEqual("http://foo.com/baz/s.js.map");
	});

	it("should handle trailing slashes", () => {
		let sourceMapUrl = "../baz/s.js.map";
		expect(absoluteSourceMapPath({url: "http://foo.com/bar/", sourceMapUrl}))
			.toEqual("http://foo.com/baz/s.js.map");
	});
});
