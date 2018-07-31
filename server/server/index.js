const { createServer } = require("net");
const respond = require("./respond");

let socket;

const start = port => {
	let server = createServer(sock => {
		socket = sock;

		sock.on("error", (e) => { throw e; });

		sock.on("data", receive);
	});

	server.listen(port, () => {
		console.log(`Indium server listening on ${port}`);
	});
};

const stop = () => {
	send({message: "closing"});
	process.exit();
};

let input = "";
const receive = (str) => {
	input += str;

	let linefeedPos = input.indexOf("\n");

	while (linefeedPos >= 0) {
		let data;
		try {
			data = JSON.parse(input.substring(0, linefeedPos));
			input = input.substring(++linefeedPos);
			linefeedPos = input.indexOf("\n");
		} catch(e) {
			error()(`Invalid JSON data received: "${input}"`);
			return;
		}

		if (!data.id) {
			error()("Missing id from JSON data");
			return;
		}

		respond(data, {
			success: success(data.id),
			error: error(data.id),
			stop
		});
	}


};

const send = (data) => {
	socket.write(`${JSON.stringify(data)}\n`);
};

const error = (id) => (error) => {
	send({id, type: "error", payload: { error }});
};

const success = (id) => (payload = {}) => {
	send({id, type: "success", payload});
};

module.exports = { start, stop, success, error, send };
