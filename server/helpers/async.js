const timeout = (ms) => new Promise(resolve => setTimeout(resolve, ms));

const doOrRetry = async (f, retries = 10, delay = 500) => {
	try {
		return await f();
	} catch(e) {
		if (retries > 0) {
			await timeout(delay);
			return doOrRetry(f, --retries, delay);
		} else {
			throw(e);
		}
	}
};

module.exports = {
	timeout,
	doOrRetry
};
