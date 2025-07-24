import { config, defaultChainMap } from './config';

export async function makeBlocks(chainMap: Record<string, number> = defaultChainMap): Promise<Response> {
	const sameCount = Object.entries(chainMap).reduce((acc, [_, count]) => {
		if (acc === false) return false;
		if (acc === undefined) return count;
		if (acc !== count) return false;
		return acc;
	}, undefined as undefined | number | false);

	if (sameCount) {
		console.log(`[${new Date().toTimeString().split(' ')[0]!.trim()}] Requesting ${sameCount} block(s) on chains:`, Object.keys(chainMap));
	} else {
		console.log('Chains: ', Object.keys(chainMap));
		console.log('Counts: ', Object.values(chainMap));
	}

	const url = `http://${config.MINER_HOSTNAME}:${config.MINER_PORT}/make-blocks`;
	const response = await fetch(url, {
		method: 'POST',
		headers: { 'Content-Type': 'application/json' },
		body: JSON.stringify(chainMap),
	});

	if (!response.ok) {
		if (response.statusText.includes('Unable to connect')) {
			console.error('Unable to connect to the miner.');
			console.error('Check if the docker `networks` are configured correctly.');
			console.error('Check if the miner is set to `--worker=on-demand`.');
		}
		throw new Error(`Error making blocks: ${response.statusText}`);
	}

	return response;
}
