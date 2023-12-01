const fetch = require('node-fetch');

exports._queryAvgMemPoolUsage = function (onError, onSuccess) {
    const prometheusURL = 'http://localhost:9090'; 
    const prometheusQuery = 'avg(avg_over_time(cardano_node_metrics_mempoolBytes_int[1d]))';
    query =`${prometheusURL}/api/v1/query?query=${prometheusQuery}` 
    fetch(query)
        .then(response => {
            if (!response.ok) {
                err = new Error(`Error querying Prometheus: ${response.statusText}`);
                onError(err);
                throw err;
            }
            response.json()
            .then(x => onSuccess(x.data.result[0].value[1]))

        });
}


