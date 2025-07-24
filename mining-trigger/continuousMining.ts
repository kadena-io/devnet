import { getCutHeight } from './consensusApi';
import { makeBlocks } from './minerApi';
import { defaultChainMap } from './config';
import { retry } from './utils';

export async function continuousMining(): Promise<NodeJS.Timeout> {
	let lastHeight = await retry(() => getCutHeight(), { count: 5, delay: 2000 });
	const last10intervalHeights: number[] = [];
	await makeBlocks(defaultChainMap);
	let expectedCutHeight = lastHeight + 98;
	let retryCount = 0;
	let maxRetryCount = 20;

	const intervalRef = setInterval(async () => {
		const newCutHeight = await getCutHeight();
		last10intervalHeights.push(newCutHeight - lastHeight);
		if (last10intervalHeights.length > 10) last10intervalHeights.shift();
		const avgHeightsPerTrigger = Math.ceil(
			(Array.from(last10intervalHeights).sort((a, b) => b - a).slice(0, 5).reduce((a, b) => a + b, 0) / 5) * 100
		) / 100;
		maxRetryCount = Math.ceil(98 / avgHeightsPerTrigger);

		lastHeight = newCutHeight;
		if (lastHeight === newCutHeight && lastHeight < expectedCutHeight && maxRetryCount !== Infinity) {
			retryCount++;
			console.log(`    ${lastHeight} < ${expectedCutHeight} (try: ${retryCount}/${maxRetryCount}, avg: ${avgHeightsPerTrigger}, dynamic retry: ${Math.ceil((98 / avgHeightsPerTrigger) * 10) / 10})`);
			if (retryCount <= maxRetryCount) return;
		}
		if (retryCount > maxRetryCount) {
			console.log(`Retry count exceeded: ${retryCount} > ${maxRetryCount}`);
			console.log('Forcing block request...');
		}
		retryCount = 0;

		expectedCutHeight = lastHeight + 98;
		console.log(`Requesting new blocks.`);
		console.log(`Current: ${lastHeight}, waiting for cut height: ${expectedCutHeight}`);
		await makeBlocks(defaultChainMap).catch(error => {
			console.error('Error creating blocks:', error);
			throw error;
		});
	}, 1000);

	return intervalRef;
}

