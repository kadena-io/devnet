import net from 'net';
import { makeBlocks } from './minerApi';
import { defaultChainMap } from './config';

export function startMiningTriggerListener(): Promise<net.Server> {
	return new Promise((resolve) => {
		console.log('Starting mining trigger listener...');
		console.log('Continuous mining for every 10 seconds is enabled.');
		let miningTriggered = false;

		const server = net.createServer((client) => {
			client.on('data', (data) => {
				const chainId = data.toString().split('\r\n')
					.find(line => line.startsWith('X-Original-URI: '))
					?.match(/\/chain\/(\d+)\/evm\/rpc/)![1] as string;
				const body = JSON.parse(data.toString().split('\r\n\r\n')[1] || '') as { jsonrpc: string; method: string; params: any[] };

				const methods = ['eth_sendRawTransaction', 'eth_sendTransaction'];
				if (methods.includes(body.method)) {
					console.log(`Waiting 3 seconds...`);
					miningTriggered = true;
					setTimeout(() => { miningTriggered = false; }, 10000);
					setTimeout(() => {
						console.log(`Triggering mining for chain ${chainId}`);
						makeBlocks({ [chainId]: 3 });
					}, 3000);
				} else {
					console.log(`   Received request for chain: ${chainId} (method: ${body.method}, params: ${JSON.stringify(body.params)})`);
				}
				client.end('HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\nMining triggered');
			});
		});

		makeBlocks(defaultChainMap).catch(error => console.error('Error creating blocks:', error));
		setInterval(() => {
			if (!miningTriggered) {
				console.log('No mining trigger received in the last 10 seconds, triggering mining...');
				makeBlocks(defaultChainMap).catch(error => console.error('Error creating blocks:', error));
			}
		}, 10000);

		server.listen(11848, '0.0.0.0', () => {
			console.log('Mining trigger listener started on port 11848');
			resolve(server);
		});
	});
}
