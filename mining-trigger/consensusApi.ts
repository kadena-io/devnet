import { config } from './config';

export async function getCutHeight(): Promise<number> {
	const response = await fetch(config.CONSENSUS_ENDPOINT + '/cut');
	if (!response.ok) {
		throw new Error(`Error fetching cut height: ${response.statusText}`);
	}
	const data = (await response.json()) as { instance: string; height: number };
	return data.height;
}
