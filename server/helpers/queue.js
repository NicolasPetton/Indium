/**
 * Return a queued version of the function `f`.
 */
// TODO: Return a promise
const queued = f => (q => () => q(f))(queue());

/**
 * Async queue.
 *
 * Define a queue and register workers with:
 *
 *   let q = queue();
 *   q(() => {...});
 *   q(async () => {...});
 *   q(() => {...});
 *
 * Registered workers are automatically evaluated in sequence.
 */
const queue = () => {
	let workers = [];
	let running = false;

	const run = async () => {
		if (running) return;
		running = true;

		try {
			while (workers.length) {
				await workers.shift()();
			}
		}
		finally { running = false; }
	};

	// TODO: Return a promise, to make it async and enable promise error handling
	return f => {
		workers.push(f);
		setTimeout(run, 0);
	};
};

module.exports = { queued, queue };
