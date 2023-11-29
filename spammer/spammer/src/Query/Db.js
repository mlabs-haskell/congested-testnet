let pg = require('pg')


exports._executeQuery = function (queryString) {
  return function (onError, onSuccess) {
    const client = new pg.Client({
      host : 'localhost',
      port : 5432,
      database : 'spammer', 
      user : 'user'
    })
    return client.connect()
        .then(() => {
            console.log(queryString)
            return client.query(queryString);
        })
        .then(result => {
          if (Array.isArray(result)) {
            L = result.length
            onSuccess(result[L-1].rows)
            } else { 
            onSuccess(result.rows)
            }
        })
        .catch(err => {
            console.error('Error query:', err);
            onError(err);
            throw err;  
        })
        .finally(() => {
            return client.end();  
        });
}
};


