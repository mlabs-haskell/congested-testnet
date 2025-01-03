(async () => {
    const {workerData, parentPort} = await import("node:worker_threads");
    const path = await import("path");
    const {spammer} = await import(path.resolve(__dirname, "../output/Spammer/index.js"));
    
    
    var controlVars = {
        isAllowTransactions: false,
        parentPort : parentPort,
        spammerId : workerData.spammerId,
        awaitTxTime : 0.0,
        waitTx : workerData.waitTx 
      }
  

    parentPort.on("message", msg => { 
      if (msg == "pause") {
        console.log(`pause spammerID : ${workerData}`)
        controlVars.isAllowTransactions = false;
      };
      if (msg == "unpause") {
        console.log(`unpause spammerID : ${workerData}`)
        controlVars.isAllowTransactions = true;
      };
    });

   spammer(controlVars)();

   // prometheus exporter if waitTx
   if (workerData.waitTx ) { 
    const http = await import("http");
    const promExporter = http.createServer((req, res) => {
        if (req.url === '/metrics') {
            res.writeHead(200, { 'Content-Type': 'text/plain' });
            res.end(`# TYPE await_time_tx gauge\nawait_time_tx ${controlVars.awaitTxTime }\n`);
        } else {
            res.writeHead(404, { 'Content-Type': 'text/plain' });
            res.end('Not Found');
        }
    });

     const SPAMMER_METRIC_PORT = process.env.SPAMMER_METRIC_PORT  
     promExporter.listen(SPAMMER_METRIC_PORT, () => {
         console.log(`Prometheus metrics available at http://0.0.0.0:${SPAMMER_METRIC_PORT}/metrics`);
     });
    }

})()


