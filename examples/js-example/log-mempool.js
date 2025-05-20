import WebSocket from 'ws';
import { promises as fs } from 'fs';

// Configuration
const OGMIOS_URL = "ws://localhost:1337";
const LOG_TO_FILE = true;
const LOG_DIR = "logs";
const LOG_FILE = `${LOG_DIR}/mempool-report-${new Date().toISOString().replace(/[:.]/g, '-')}.json`;
const DEBUG = true; // Set to true for verbose logging


// Create log directory if it doesn't exist
try {
  await fs.mkdir(LOG_DIR, { recursive: true });
  if (DEBUG) {
    console.log(`DEBUG: Log directory '${LOG_DIR}' ready`);
  }
} catch (error) {
  console.error(`Error creating log directory: ${error.message}`);
}

/**
 * Simple WebSocket-based Mempool inspector with direct approach
 */
class SimpleMempoolInspector {
  constructor(url) {
    this.url = url;
    this.client = null;
    this.transactions = [];
    this.mempoolSize = null;
    this.connected = false;
    this.requestCounter = 1;
  }

  log(message) {
    const timestamp = new Date().toISOString();
    console.log(`[${timestamp}] ${message}`);
  }

  debug(message) {
    if (DEBUG) {
      const timestamp = new Date().toISOString();
      console.log(`[${timestamp}] DEBUG: ${message}`);
    }
  }

  // Connect to WebSocket server
  connect() {
    return new Promise((resolve, reject) => {
      this.log(`Connecting to ${this.url}...`);
      
      this.client = new WebSocket(this.url);
      
      this.client.on('open', () => {
        this.log('Connection established');
        this.connected = true;
        resolve();
      });
      
      this.client.on('close', (code, reason) => {
        this.log(`Connection closed: ${code} ${reason}`);
        this.connected = false;
      });
      
      this.client.on('error', (error) => {
        this.log(`WebSocket error: ${error.message}`);
        reject(error);
      });
      
      // Set up the message handler
      this.client.on('message', (data) => {
        try {
          const json = JSON.parse(data.toString());
          this.debug(`RAW MESSAGE: ${JSON.stringify(json)}`);
          this.handleMessage(json);
        } catch (error) {
          this.log(`Error parsing message: ${error.message}`);
          this.log(`Raw data: ${data.toString().substring(0, 200)}...`);
        }
      });
      
      // Connection timeout
      setTimeout(() => {
        if (!this.connected) {
          this.log('Connection timeout');
          reject(new Error('Connection timeout'));
        }
      }, 5000);
    });
  }

  // Send a request to the server
  sendRequest(method, params = {}) {
    if (!this.connected || !this.client) {
      this.log('Cannot send request: not connected');
      return false;
    }
    
    const id = `req-${this.requestCounter++}`;
    const request = {
      jsonrpc: '2.0',
      method,
      params,
      id
    };
    
    try {
      this.log(`Sending ${method} request (${id})`);
      this.debug(`Request: ${JSON.stringify(request)}`);
      this.client.send(JSON.stringify(request));
      return id;
    } catch (error) {
      this.log(`Error sending request: ${error.message}`);
      return false;
    }
  }

  // Handle incoming messages
  handleMessage(message) {
    // Log response information
    if (message.id) {
      this.log(`Received response for request ID: ${message.id}`);
    } else if (message.method) {
      this.log(`Received method call: ${message.method}`);
    } else {
      this.log(`Received unidentified message`);
    }
    
    // Check for errors
    if (message.error) {
      this.log(`Error from server: ${JSON.stringify(message.error)}`);
      
      // If mempool not acquired, proceed with acquisition
      if (message.error.code === 4000) {
        this.log('Mempool not acquired, attempting to acquire...');
        setTimeout(() => this.sendRequest('acquireMempool'), 500);
      }
      return;
    }
    
    // Handle specific responses based on context clues in the response
    if (message.result) {
      // Handle mempool size response
      if (message.result.maxCapacity && message.result.currentSize) {
        this.handleMempoolSize(message.result);
        return;
      }
      
      // Handle transaction response
      if (message.result.transaction !== undefined) {
        this.handleTransaction(message.result);
        return;
      }
      
      // Handle release/acquire responses
      if (message.result.released === true) {
        this.log('Mempool successfully released');
        setTimeout(() => this.sendRequest('acquireMempool'), 500);
        return;
      }
      
      if (message.result.acquired === "mempool") {
        this.log(`Mempool successfully acquired at slot ${message.result.slot}`);
        setTimeout(() => this.sendRequest('sizeOfMempool'), 500);
        return;
      }
      
      // Handle unknown result
      this.log(`Unhandled result: ${JSON.stringify(message.result).substring(0, 100)}...`);
    }
  }
  
  // Handle mempool size information
  handleMempoolSize(result) {
    // Format the data to fit our expected structure
    this.mempoolSize = {
      bytes: result.currentSize.bytes,
      numberOfTransactions: result.transactions.count,
      capacity: result.maxCapacity.bytes
    };
    
    this.log('');
    this.log('=== MEMPOOL SIZE INFORMATION ===');
    this.log(`Size: ${this.mempoolSize.bytes} bytes`);
    this.log(`Number of transactions: ${this.mempoolSize.numberOfTransactions}`);
    this.log(`Capacity: ${this.mempoolSize.capacity} bytes`);
    this.log(`Utilization: ${(this.mempoolSize.bytes / this.mempoolSize.capacity * 100).toFixed(2)}%`);
    this.log('');
    
    if (this.mempoolSize.numberOfTransactions > 0) {
      this.log('Fetching transactions...');
      setTimeout(() => this.sendRequest('nextTransaction', { fields: "all" }), 500);
    } else {
      this.log('No transactions in mempool.');
      this.generateReport();
    }
  }
  
  // Handle transaction data
  handleTransaction(result) {
    this.debug(`Transaction result: ${JSON.stringify(result).substring(0, 200)}...`);
    
    if (result.transaction === null) {
      this.log(`No more transactions. Total found: ${this.transactions.length}`);
      this.generateReport();
      return;
    }
    
    const txData = result.transaction;
    this.transactions.push(txData);
    this.log(`[${this.transactions.length}] Transaction: ${txData.id || 'Unknown ID'}`);
    
    // Request next transaction
    setTimeout(() => this.sendRequest('nextTransaction', { fields: "all" }), 500);
  }
  
  // Generate the final report
  async generateReport() {
    this.log('');
    this.log('=== MEMPOOL INSPECTION REPORT ===');
    this.log(`Timestamp: ${new Date().toISOString()}`);
    this.log(`Total Transactions: ${this.transactions.length}`);
    
    if (this.mempoolSize) {
      this.log(`Mempool Size: ${this.mempoolSize.bytes} bytes`);
      this.log(`Mempool Capacity: ${this.mempoolSize.capacity} bytes`);
      this.log(`Utilization: ${(this.mempoolSize.bytes / this.mempoolSize.capacity * 100).toFixed(2)}%`);
    }
    
    // Create transaction summaries
    const txSummaries = this.transactions.map(tx => {
      try {
        const inputs = tx.inputs?.length || 0;
        const outputs = tx.outputs?.length || 0;
        const fee = tx.fee?.ada?.lovelace || 0;
        const validityInterval = tx.validityInterval || {};
        const size = tx.cbor ? tx.cbor.length / 2 : 'unknown';  // CBOR is hex encoded, so divide by 2 for bytes
        
        return {
          id: tx.id,
          size,
          inputs,
          outputs,
          fee,
          validityInterval,
          metadata: tx.auxiliaryData ? 'Present' : 'None'
        };
      } catch (error) {
        this.log(`Error processing transaction: ${error.message}`);
        return { id: tx.id || 'Unknown', error: error.message };
      }
    });
    
    this.log('\nTransaction Summaries:');
    txSummaries.forEach((tx, i) => {
      this.log(`\n[${i+1}] ID: ${tx.id}`);
      
      if (tx.error) {
        this.log(`  Error: ${tx.error}`);
        return;
      }
      
      this.log(`  Size: ${tx.size} bytes`);
      this.log(`  Inputs/Outputs: ${tx.inputs}/${tx.outputs}`);
      this.log(`  Fees: ${tx.fee} lovelace`);
      
      if (tx.validityInterval.invalidBefore) {
        this.log(`  Invalid Before: ${tx.validityInterval.invalidBefore}`);
      }
      if (tx.validityInterval.invalidHereafter) {
        this.log(`  Invalid Hereafter: ${tx.validityInterval.invalidHereafter}`);
      }
      
      this.log(`  Metadata: ${tx.metadata}`);
    });
    
    // Additional analysis of transaction inputs
    this.log('\nDetailed Transaction Analysis:');
    this.transactions.forEach((tx, i) => {
      this.log(`\n[${i+1}] Transaction: ${tx.id}`);
      this.log(`  Spending Strategy: ${tx.spends}`);
      
      // Input UTXOs
      this.log('  Input UTXOs:');
      if (tx.inputs && tx.inputs.length > 0) {
        tx.inputs.forEach((input, j) => {
          const txId = input.transaction?.id || 'unknown';
          const index = input.index !== undefined ? input.index : 'unknown';
          this.log(`    [${j+1}] ${txId}#${index}`);
        });
      } else {
        this.log('    No inputs found');
      }
      
      // Output addresses
      this.log('  Output Addresses:');
      if (tx.outputs && tx.outputs.length > 0) {
        tx.outputs.forEach((output, j) => {
          const address = output.address || 'unknown';
          const lovelace = output.value?.ada?.lovelace || 0;
          this.log(`    [${j+1}] ${address} (${lovelace} lovelace)`);
        });
      } else {
        this.log('    No outputs found');
      }
      
      // Additional attributes that might be useful for troubleshooting
      if (tx.validityInterval && Object.keys(tx.validityInterval).length > 0) {
        this.log('  Validity Interval:');
        Object.entries(tx.validityInterval).forEach(([key, value]) => {
          this.log(`    ${key}: ${value}`);
        });
      }
      
      // Check for scripts
      const hasScripts = tx.scripts && tx.scripts.length > 0;
      this.log(`  Contains Scripts: ${hasScripts ? 'Yes' : 'No'}`);
      
      // Check for collateral
      const hasCollateral = tx.collaterals && tx.collaterals.length > 0;
      this.log(`  Has Collateral: ${hasCollateral ? 'Yes' : 'No'}`);
    });
    
    // Log to file if enabled
    if (LOG_TO_FILE) {
      const report = {
        timestamp: new Date().toISOString(),
        mempoolSize: this.mempoolSize,
        transactionCount: this.transactions.length,
        transactions: this.transactions,
        transactionSummaries: txSummaries
      };
      
      try {
        await fs.writeFile(LOG_FILE, JSON.stringify(report, null, 2));
        this.log(`\nFull report written to ${LOG_FILE}`);
      } catch (error) {
        this.log(`Error writing report to file: ${error.message}`);
      }
    }
    
    // Close the connection
    if (this.client && this.client.readyState === WebSocket.OPEN) {
      this.log('Closing connection...');
      this.client.close();
    }
    
    this.log('\nMempool inspection complete.');
  }
  
  // Run the inspection
  async run() {
    try {
      await this.connect();
      
      // First try to release any existing mempool acquisition
      this.log('Releasing any existing mempool acquisition...');
      this.sendRequest('releaseMempool');
      
      // If nothing happens after 5 seconds, try to acquire directly
      setTimeout(() => {
        if (this.transactions.length === 0 && !this.mempoolSize) {
          this.log('No response received, trying to acquire mempool directly...');
          this.sendRequest('acquireMempool');
        }
      }, 5000);
      
    } catch (error) {
      this.log(`Failed to inspect mempool: ${error.message}`);
      process.exit(1);
    }
  }
}

// Run the inspector
console.log(`Starting mempool inspection at ${new Date().toISOString()}`);
console.log(`Debug mode: ${DEBUG ? 'ON' : 'OFF'}`);

const inspector = new SimpleMempoolInspector(OGMIOS_URL);
inspector.run();

// Handle process termination
process.on('SIGINT', () => {
  console.log('\nProcess interrupted, closing connection...');
  if (inspector.client && inspector.client.readyState === WebSocket.OPEN) {
    inspector.client.close();
  }
  process.exit(0);
});
