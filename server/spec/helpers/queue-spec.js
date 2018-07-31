const { queue, queued } = require("../../helpers/queue.js");
const { timeout } = require("../../helpers/async.js");

describe("Running queues", () => {
	it("should run all workers in sequence", (done) => {
		let q = queue();

		let expected = [ 1, 2, 3 ];

		let result = [];
		let workers = expected.map(n => () => result.push(n));

		workers.forEach(q);

		setTimeout(() => {
			expect(result).toEqual(expected);
			done();
		}, 100);
	});

	it("should run async workers in sequence", (done) => {
		let q = queue();

		let expected = [ 1, 2, 3 ];

		let result = [];

		expected.forEach((n) => q(async () => {
			await timeout(20);
			result.push(n);
		}));

		setTimeout(() => {
			expect(result).toEqual(expected);
			done();
		}, 200);
	});

	it("should run again when adding new workers", (done) => {
		let q = queue();

		let result = [];

		let workers = [ 1, 2, 3 ].map(n => async () => {
			await timeout(20);
			result.push(n);
		});

		workers.forEach(q);

		setTimeout(() => {
			expect(result).toEqual([ 1, 2, 3 ]);
			done();
		}, 100);

		let newWorkers = [ 4, 5, 6 ].map(n => async () => {
			await timeout(20);
			result.push(n);
		});

		setTimeout(() => newWorkers.forEach(q), 300);

		setTimeout(() => {
			expect(result).toEqual([ 1, 2, 3, 4, 5, 6 ]);
			done();
		}, 400);
	});

	it("queues should not evaluate other queues workers", (done) => {
		let q1 = queue();
		let q2 = queue();

		let expected1 = [ 1, 2, 3 ];
		let expected2 = [ 4, 5, 6 ];

		let result1 = [];
		let result2 = [];

		expected1.forEach(n => q1(() => result1.push(n)));
		expected2.forEach(n => q2(() => result2.push(n)));

		setTimeout(() => {
			expect(result1).toEqual(expected1);
			expect(result2).toEqual(expected2);
			done();
		}, 100);
	});
});

describe("queued functions", () => {
	it("can queue functions", (done) => {
		let count = 0;

		let increase = () => {
			return ++count;
		};

		let queuedIncrease = queued(increase);

		queuedIncrease();
		queuedIncrease();
		queuedIncrease();

		setTimeout(() => {
			expect(count).toBe(3);
			done();
		}, 10);
	});
})
