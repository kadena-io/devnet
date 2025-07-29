import net from 'net';
import fetch from 'node-fetch';

// Configuration from environment variables
const config = {
  MINING_MODE: process.env.MINING_MODE || 'triggered', // 'triggered' or 'continuous'
  MINER_HOSTNAME: process.env.MINER_HOSTNAME || 'localhost',
  MINER_PORT: parseInt(process.env.MINER_PORT || '1917', 10),
  CONSENSUS_ENDPOINT:
    process.env.CONSENSUS_ENDPOINT ||
    'http://localhost:1848/chainweb/0.0/evm-development',
  CHAINS: process.env.CHAINS
    ? process.env.CHAINS.split(',').map(c => c.trim())
    : Array.from({ length: 98 }, (_, i) => i.toString()),
  TRIGGER_PORT: parseInt(process.env.TRIGGER_PORT || '11848', 10),
  TRIGGER_BLOCK_COUNT: parseInt(process.env.TRIGGER_BLOCK_COUNT || '1', 10),
  CONTINUOUS_INTERVAL: parseInt(process.env.CONTINUOUS_INTERVAL || '20000', 10),
};

/**
 * Creates blocks on multiple chains
 * @param chainMap - Map of chain IDs to number of blocks to create
 */
export async function makeBlocks(chainMap: Record<string, number>): Promise<Response> {
  const chainCount = Object.keys(chainMap).length;
  const blockCounts = Object.values(chainMap);
  
  if (blockCounts.every(count => count === blockCounts[0])) {
    console.log(`[${new Date().toLocaleTimeString()}] Mining ${blockCounts[0]} block(s) on ${chainCount} chains`);
  } else {
    console.log('Mining on chains:', Object.keys(chainMap));
    console.log('Block counts:', blockCounts);
  }

  const url = `http://${config.MINER_HOSTNAME}:${config.MINER_PORT}/make-blocks`;
  const response = await fetch(url, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(chainMap),
  });

  if (!response.ok) {
    if (response.statusText.includes('Unable to connect')) {
      console.error('Miner connection error:');
      console.error('- Check docker network configuration');
      console.error('- Ensure miner is using "--worker=on-demand"');
    }
    throw new Error(`Block creation failed: ${response.statusText}`);
  }

  return response;
}

// Get default chain map (1 block per configured chain)
function getDefaultChainMap(): Record<string, number> {
  return config.CHAINS.reduce((map, chain) => {
    map[chain] = 1;
    return map;
  }, {} as Record<string, number>);
}

// Get current cut height from consensus
async function getCutHeight(): Promise<number> {
  const response = await fetch(`${config.CONSENSUS_ENDPOINT}/cut`);
  if (!response.ok) throw new Error(`Cut height fetch failed: ${response.statusText}`);
  const { height } = (await response.json()) as { height: number };
  return height;
}

// Retry function with configurable attempts
async function retryOperation<T>(
  operation: () => Promise<T>,
  options = { attempts: 5, delay: 2000 }
): Promise<T> {
  for (let i = 1; i <= options.attempts; i++) {
    try {
      return await operation();
    } catch (error) {
      if (i === options.attempts) throw error;
      console.error(`Attempt ${i}/${options.attempts} failed. Retrying...`);
      await new Promise(resolve => setTimeout(resolve, options.delay));
    }
  }
  throw new Error('Max retry attempts reached');
}

// Triggered mining mode
async function startTriggeredMining() {
  console.log('🚀 Starting triggered mining mode');
  let miningTriggered = false;
  const defaultChainMap = getDefaultChainMap();

  // Initial mining
  await makeBlocks(defaultChainMap);

  // Periodic mining (fallback)
  const interval = setInterval(async () => {
    if (!miningTriggered) {
      console.log(`⏰ No triggers received in the last ${config.CONTINUOUS_INTERVAL / 1000} seconds`);
      await makeBlocks(defaultChainMap);
    }
    miningTriggered = false;
  }, config.CONTINUOUS_INTERVAL);

  // Start trigger server
  const server = net.createServer(client => {
    client.on('data', async data => {
      try {
        const rawData = data.toString();
        const chainId = rawData.match(/X-Original-URI: .*\/chain\/(\d+)\//)?.[1] as string;
        const body = JSON.parse(rawData.split('\r\n\r\n')[1] || '{}');
        
        if (['eth_sendRawTransaction', 'eth_sendTransaction'].includes(body.method)) {
          console.log(`⚡ Transaction detected on chain ${chainId}`);
          miningTriggered = true;
          
          setTimeout(async () => {
            console.log(`⛏️  Mining ${config.TRIGGER_BLOCK_COUNT} blocks on chain ${chainId}`);
            await makeBlocks({ [chainId]: config.TRIGGER_BLOCK_COUNT });
          }, 3000);
        }
      } catch (error) {
        console.error('Error processing trigger:', error);
      } finally {
        client.end('HTTP/1.1 200 OK\r\n\r\nTrigger acknowledged');
      }
    });
  });

  server.listen(config.TRIGGER_PORT, '0.0.0.0', () => {
    console.log(`🔌 Trigger server listening on port ${config.TRIGGER_PORT}`);
  });

  // Cleanup
  process.on('SIGTERM', () => {
    clearInterval(interval);
    server.close(() => {
      console.log('🛑 Trigger server stopped');
      process.exit(0);
    });
  });
}

// Continuous mining mode
async function startContinuousMining() {
  console.log('🔁 Starting continuous mining mode');
  const defaultChainMap = getDefaultChainMap();
  const chainCount = config.CHAINS.length;
  let lastHeight = await retryOperation(getCutHeight);
  let expectedHeight = lastHeight + chainCount;
  
  console.log(`📏 Current height: ${lastHeight}, Target: ${expectedHeight}`);

  // Mining interval
  const interval = setInterval(async () => {
    try {
      const currentHeight = await getCutHeight();
      
      if (currentHeight < expectedHeight) {
        console.log(`⏳ Height: ${currentHeight}/${expectedHeight} - mining...`);
        await makeBlocks(defaultChainMap);
      } else {
        expectedHeight = currentHeight + chainCount;
        console.log(`✅ Reached target. New target: ${expectedHeight}`);
      }
      
      lastHeight = currentHeight;
    } catch (error) {
      console.error('Continuous mining error:', error);
    }
  }, config.CONTINUOUS_INTERVAL);

  // Cleanup
  process.on('SIGTERM', () => {
    clearInterval(interval);
    console.log('🛑 Continuous mining stopped');
    process.exit(0);
  });
}

// Main function
async function main() {
  console.log('⛏️  Starting Pact miner controller');
  console.log('Configuration:', config);

  switch (config.MINING_MODE.toLowerCase()) {
    case 'triggered':
      await startTriggeredMining();
      break;
    case 'continuous':
      await startContinuousMining();
      break;
    default:
      throw new Error(`Invalid mining mode: ${config.MINING_MODE}`);
  }
}

// Start with error handling
let errorCount = 0;
const MAX_ERRORS = 5;

async function runWithRetry() {
  try {
    await main();
  } catch (error) {
    errorCount++;
    console.error(`Main error (${errorCount}/${MAX_ERRORS}):`, error);
    
    if (errorCount < MAX_ERRORS) {
      console.log(`Retrying in 2 seconds...`);
      setTimeout(runWithRetry, 2000);
    } else {
      console.error('❌ Max errors reached. Exiting.');
      process.exit(1);
    }
  }
}

runWithRetry();