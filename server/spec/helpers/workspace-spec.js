const {
	lookupProjectFile,
	resolveRoot,
	resolveUrl,
	expandRoot
} = require("../../helpers/workspace");

const mock = require("mock-fs");

describe("Looking up the .indium.json files", () => {
	afterEach(() => {
		mock.restore();
	});

	it("should throw an error if the directory does not exist", () => {
		mock({
			"/home/user/projects/": {
				"foo.txt": "foo contents"
			}
		});

		expect(() => lookupProjectFile("/home/user/projects/bar")).toThrow();
	});

	it("should throw an error if not .indium.json file can be found in the hierarchy", () => {
		mock({
			"/home/user/projects/": {
				"foo": {
					"bar": {
						"index.js": "foo contents"
					}
				}
			}
		});

		expect(() => lookupProjectFile("/home/user/projects/foo/bar/")).toThrow();
	});

	it("should throw if the .indium.json file is a directory", () => {
		mock({
			"/home/user/projects/": {
				"foo": {
					"bar": {
						".indium.json": {},
						"index.js": "foo contents"
					}
				}
			}
		});

		expect(() => lookupProjectFile("/home/user/projects/foo/bar/")).toThrow();
	});

	it("should find the .indium.json file in the current directory", () => {
		mock({
			"/home/user/projects/": {
				"foo": {
					"bar": {
						".indium.json": "{}"
					}
				}
			}
		});

		expect(lookupProjectFile("/home/user/projects/foo/bar/"))
			.toEqual("/home/user/projects/foo/bar/.indium.json");
	});

	it("should lookup the .indium.json file in parent directories", () => {
		mock({
			"/home/user/projects/": {
				"foo": {
					".indium.json": "{}",
					"bar": {}
				}
			}
		});

		expect(lookupProjectFile("/home/user/projects/foo/bar/"))
			.toEqual("/home/user/projects/foo/.indium.json");
	});

	it("should lookup the closest .indium.json file in the hierarchy", () => {
		mock({
			"/home/user/projects/": {
				".indium.json": "{}",
				"foo": {
					".indium.json": "{}",
					"bar": {}
				}
			}
		});

		expect(lookupProjectFile("/home/user/projects/foo/bar/"))
			.toEqual("/home/user/projects/foo/.indium.json");
	});
});

describe("Root directory resolution", () => {
	it("should find the root directory from the project file", () => {
		let conf = {
			projectFile: "/home/user/projects/foo/.indium.js"
		};

		expect(resolveRoot(conf)).toEqual("/home/user/projects/foo");
	});

	it("should canonicalize the root directory", () => {
		let conf = {
			projectFile: "/home/user/projects/foo/..//foo///.indium.js"
		};

		expect(resolveRoot(conf)).toEqual("/home/user/projects/foo");
	});

	it("should use \"root\" when specified", () => {
		let conf = {
			projectFile: "/home/user/projects/foo/..//foo///.indium.js",
			root: "src"
		};

		expect(resolveRoot(conf)).toEqual("/home/user/projects/foo/src");
	});

	it("should resolve \"root\" based on the project directory", () => {
		let conf = {
			projectFile: "/home/user/projects/foo/..//foo///bar/.indium.js",
			root: "../src"
		};

		expect(resolveRoot(conf)).toEqual("/home/user/projects/foo/src");
	});

	it("should alias \"webRoot\" to \"root\"", () => {
		let conf = {
			projectFile: "/home/user/projects/foo/..//foo///bar/.indium.js",
			webRoot: "../src"
		};

		expect(resolveRoot(conf)).toEqual("/home/user/projects/foo/src");
	});
});

describe("Root directory expansion", () => {
	it("should not expand root when not provided", () => {
		expect(expandRoot("/foo/bar", "baz")).toEqual("/foo/bar");
	});

	it("should expand root when provided", () => {
		expect(expandRoot("${root}/foo/bar", "/baz")).toEqual("/baz/foo/bar");
	});

	it("should alias webRoot to root", () => {
		expect(expandRoot("${webRoot}/foo/bar", "/baz")).toEqual("/baz/foo/bar");
	});
});


describe("URL resolution", () => {
	it("resolves the url based on the projectFile", () => {
		let conf = {
			projectFile: "/home/user/projects/foo/.indium.json"
		};

		let url = "http://localhost:3000/js/app.js";

		expect(resolveUrl(url, conf)).toEqual("/home/user/projects/foo/js/app.js");
	});

	it("resolves the url using ${root} is provided", () => {
		let conf = {
			projectFile: "/home/user/projects/foo/.indium.json",
			root: "./src"
		};

		let url = "http://localhost:3000/js/app.js";

		expect(resolveUrl(url, conf)).toEqual("/home/user/projects/foo/src/js/app.js");
	});

	it("ignores query parameters when performing resolution", () => {
		let conf = {
			projectFile: "/home/user/projects/foo/.indium.json"
		};

		let url = "http://localhost:3000/js/app.js?id=abc";

		expect(resolveUrl(url, conf)).toEqual("/home/user/projects/foo/js/app.js");
	});

	// In Node, script urls can be plain file paths
	it("resolves file paths as urls", () => {
		let conf = {
			projectFile: "/home/user/projects/foo/.indium.json"
		};

		let url = "/foo/bar/baz";

		expect(resolveUrl(url, conf)).toEqual(url);
	});

	it("resolves file protocol uris", () => {
		let conf = {
			projectFile: "/home/user/projects/foo/.indium.json"
		};

		let url = "file:///home/user/projects/foo/bar.js";

		expect(resolveUrl(url, conf)).toEqual("/home/user/projects/foo/bar.js");
	});


	// Regression test for GH issue #161
	it("does not transform the path when the file exists on Windows", () => {
		let conf = {
			projectFile: "C:\\Users\\john\\projects\\foo\\.indium.json"
		};

		let url = "C:\\Users\\john\\projects\\foo\\bar.js";

		expect(resolveUrl(url, conf)).toEqual(url);
	});

        it("supports script path overrides", () => {
		let conf = {
			projectFile: "/home/user/projects/foo/.indium.json",
                        "scriptPathOverrides": {
                                "(/js/.*\\.js)/[0-9]+": "private$1"
                        }
		};

                let url = "http://localhost:3000/js/app.js/1234567890";

                expect(resolveUrl(url, conf)).toEqual("/home/user/projects/foo/private/js/app.js");
        });
});