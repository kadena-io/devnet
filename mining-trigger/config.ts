import * as process from 'process';

export const config = {
  MINING_CONFIRMATION_PERIOD: parseFloat(process.env.MINING_CONFIRMATION_PERIOD ?? '12'),
  MINER_HOSTNAME: process.env.MINER_HOSTNAME ?? 'localhost',
  MINER_PORT: parseInt(process.env.MINER_PORT ?? '1917', 10),
  TRIGGERED_MINING: process.env.TRIGGERED_MINING ? process.env.TRIGGERED_MINING.toLowerCase() === 'true' : true,
  CONTINUOUS_MINING: process.env.CONTINUOUS_MINING ? process.env.CONTINUOUS_MINING.toLowerCase() === 'true' : true,
  CONSENSUS_ENDPOINT: process.env.CONSENSUS_ENDPOINT ?? 'http://localhost:1848/chainweb/0.0/evm-development',
  CHAINS: process.env.CHAINS ? process.env.CHAINS.split(',').map(c => c.trim()) : Array.from({ length: 98 }, (_, i) => i.toString()),
};

export const defaultChainMap = config.CHAINS.reduce((acc, chain) => {
  acc[chain] = 1;
  return acc;
}, {} as Record<string, number>);
