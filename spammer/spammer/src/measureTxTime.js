(async () => {
    const {workerData, parentPort} = await import("node:worker_threads");
    const path = await import("path");
    const http = await import("http");
    const {measureTxTime} = await import(path.resolve(__dirname, "../output/Spammer/index.js"));
    
    var controlVars = {
        idSpammer : workerData, 
        isAllowTransactions: false,
        parentPort : parentPort,
        awaitTxTime : 0.0
      }
  

    parentPort.on("message", msg => { 
      console.log(msg);
      if (msg == "pause") {
        controlVars.isAllowTransactions = false;
      };
      if (msg == "unpause") {
        controlVars.isAllowTransactions = true;
      };
    });

    measureTxTime(controlVars)();


    // prometheus exporter
   const promExporter = http.createServer((req, res) => {
       if (req.url === '/metrics') {
           res.writeHead(200, { 'Content-Type': 'text/plain' });
           res.end(`# TYPE await_time_tx gauge\nawait_time_tx ${controlVars.awaitTxTime }\n`);
       } else {
           res.writeHead(404, { 'Content-Type': 'text/plain' });
           res.end('Not Found');
       }
   });

    promExporter.listen(8001, () => {
        console.log(`Prometheus metrics available at http://0.0.0.0:${8001}/metrics`);
    });

})()

