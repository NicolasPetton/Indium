const {default: PQueue} = require("p-queue");

/**
 * Return a queued version of the function `f`.
 */
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
	const pqueue = new PQueue({concurrency: 1});

	return f => pqueue.add(f);
};

module.exports = { queued, queue };
