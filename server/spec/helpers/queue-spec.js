const { queue, queued } = require("../../helpers/queue.js");
const { timeout } = require("../../helpers/async.js");

describe("Running queues", () => {
	it("should run all workers in sequence", (done) => {
		let q = queue();

		let expected = [ 1, 2, 3 ];

		let result = [];
		let workers = expected.map(n => () => result.push(n));

		let promises = workers.map(q);

                Promise.all(promises).then(() => {
			expect(result).toEqual(expected);
			done();
		});
	});

	it("should run async workers in sequence", (done) => {
		let q = queue();

		let expected = [ 1, 2, 3 ];

		let result = [];

		let promises = expected.map((n) => q(async () => {
                        // make the first ones slower than the later ones
			await timeout((3-n)*20);
			result.push(n);
		}));

		Promise.all(promises).then(() => {
			expect(result).toEqual(expected);
			done();
		});
	});

	it("should run again when adding new workers", (done) => {
		let q = queue();

		let result = [];

		let workers = [ 1, 2, 3 ].map(n => async () => {
			await timeout(20);
			result.push(n);
		});

		let promises = workers.map(q);

                Promise.all(promises).then(() => {
			expect(result).toEqual([ 1, 2, 3 ]);
		});

		let newWorkers = [ 4, 5, 6 ].map(n => async () => {
			await timeout(20);
			result.push(n);
		});

		promises = newWorkers.map(q);

                Promise.all(promises).then(() => {
			expect(result).toEqual([ 1, 2, 3, 4, 5, 6 ]);
			done();
		});
	});

	it("queues should not evaluate other queues workers", (done) => {
		let q1 = queue();
		let q2 = queue();

		let expected1 = [ 1, 2, 3 ];
		let expected2 = [ 4, 5, 6 ];

		let result1 = [];
		let result2 = [];

		let promises1 = expected1.map(n => q1(() => result1.push(n)));
		let promises2 = expected2.map(n => q2(() => result2.push(n)));

		Promise.all([...promises1, ...promises2]).then(() => {
			expect(result1).toEqual(expected1);
			expect(result2).toEqual(expected2);
			done();
		});
	});
});

describe("queued functions", () => {
	it("can queue functions", async (done) => {
		let count = 0;

		let increase = () => {
			return ++count;
		};

		let queuedIncrease = queued(increase);

		await queuedIncrease();
		await queuedIncrease();
		await queuedIncrease();

		expect(count).toBe(3);
		done();
	});
})
