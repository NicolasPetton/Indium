const {
	convertRemoteObject,
	resolveFileLocation,
	resolveUrlLocation
} = require("../../../adapters/cdp/helpers");

describe("Converting remote objects", () => {
	it("returns the description when not a reference", () => {
		expect(convertRemoteObject({
			value: 3,
			type: "number"
		})).toEqual({
			description: "3"
		});
		expect(convertRemoteObject({
			value: "Hello",
			type: "string"
		})).toEqual({
			description: "\"Hello\""
		});
		expect(convertRemoteObject({
			value: true,
			type: "boolean"
		})).toEqual({
			description: "true"
		});
		expect(convertRemoteObject({
			value: null,
			type: "object"
		})).toEqual({
			description: "null"
		});
		expect(convertRemoteObject({
			value: null,
			type: "object",
			type: "undefined"
		})).toEqual({
			description: "undefined"
		});
	});

	it("converts object previews", () => {
		let result = {
			objectId: "1",
			description: "Object",
			preview: {
				type: "object",
				subtype: "object",
				properties: [
					{ name: "a", type: "number", value: "3"},
					{ name: "b", type: "string", value: "hello"},
					{ name: "c", type: "boolean", value: "true"},
					{ name: "d", type: "object", subtype: "Location", value: "Location"}
				]
			}
		};

		let preview = convertRemoteObject(result).preview;

		expect(preview).toEqual("{ a: 3, b: \"hello\", c: true, d: Location }");
	});

	it("converts object previews with overflows", () => {
		let result = {
			objectId: "1",
			description: "Object",
			preview: {
				type: "object",
				subtype: "object",
				overflow: true,
				properties: [
					{ name: "a", type: "number", value: "3"},
					{ name: "b", type: "string", value: "hello"},
				]
			}
		};

		let preview = convertRemoteObject(result).preview;

		expect(preview).toEqual("{ a: 3, b: \"hello\", … }");
	});

	it("converts array previews", () => {
		let result = {
			objectId: "1",
			description: "Array",
			preview: {
				type: "object",
				subtype: "array",
				properties: [
					{ name: "0", type: "number", value: "3" },
					{ name: "1", type: "string", value: "hello" },
					{ name: "2", type: "boolean", value: "true" },
					{ name: "3", type: "object", type: "Location", value: "Location" }
				]
			}
		};

		let preview = convertRemoteObject(result).preview;

		expect(preview).toEqual("[ 3, \"hello\", true, Location ]");
	});

	it("converts array previews with overflows", () => {
		let result = {
			objectId: "1",
			description: "Array",
			preview: {
				type: "object",
				subtype: "array",
				overflow: true,
				properties: [
					{ name: "0", type: "number", value: "3" },
					{ name: "1", type: "string", value: "hello" },
				]
			}
		};

		let preview = convertRemoteObject(result).preview;

		expect(preview).toEqual("[ 3, \"hello\", … ]");
	});

	it("converts set previews", () => {
		let result = {
			objectId: "1",
			description: "Set",
			preview: {
				type: "object",
				subtype: "set",
				entries: [
					{value: {type: "number", description: "1", overflow: false, properties: []}},
					{value: {type: "string", description: "hello", overflow: false, properties: []}},
					{value: {type: "boolean", description: "true", overflow: false, properties: []}}
				]
			}
		};

		let preview = convertRemoteObject(result).preview;

		expect(preview).toEqual("[ 1, \"hello\", true ]");
	});

	it("converts map previews", () => {
		let result = {
			objectId: "1",
			description: "Map",
			preview: {
				type: "object",
				subtype: "map",
				entries: [
					{
						key: {type: "number", description: "1", overflow: false, properties: []},
						value: {type: "string", description: "one", overflow: false, properties: []}
					},
					{
						key: {type: "string", description: "hello", overflow: false, properties: []},
						value: {type: "boolean", description: "false", overflow: false, properties: []}
					}
				]
			}
		};

		let preview = convertRemoteObject(result).preview;

		expect(preview).toEqual("[ 1 ⇒ \"one\", \"hello\" ⇒ false ]");
	});
});

describe("Resolving file locations", () => {
	it("should not resolve a file location when no script matches", async () => {
		let conf = {
			projectFile: "/home/user/projects/p/.indium.json",
			root: "src"
		};

		let scripts = [
			{ url: "http://localhost:3000/js/foo.js" }
		];

		let location = {
			file: "/home/user/projects/p/src/js/bar.js",
			line: 23,
			column: 0
		};

		expect(await resolveFileLocation(location, conf, scripts)).toEqual(null);
	});

	it("should resolve a file location using the script urls", async () => {
		let conf = {
			projectFile: "/home/user/projects/p/.indium.json",
			root: "src"
		};

		let scripts = [
			{ url: "http://localhost:3000/js/foo.js" },
			{ url: "http://localhost:3000/js/bar.js" }
		];

		let location = {
			file: "/home/user/projects/p/src/js/bar.js",
			line: 23,
			column: 0
		};

		expect(await resolveFileLocation(location, conf, scripts)).toEqual({
			url: "http://localhost:3000/js/bar.js",
			line: 23,
			column: 0
		});
	});
});

describe("Resolving url locations", () => {
	it("should resolve an url location using the configuration", async () => {
		let conf = {
			projectFile: "/home/user/projects/p/.indium.json",
			root: "src"
		};

		let location = {
			url: "http://localhost:3000/js/foo.js",
			line: 23,
			column: 0
		};

		expect(await resolveUrlLocation(location, conf)).toEqual({
			file: "/home/user/projects/p/src/js/foo.js",
			line: 23,
			column: 0
		});
	});
});
