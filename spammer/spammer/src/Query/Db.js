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
            return client.query(queryString);
        })
        .then(result => {
            console.log(result.rows);
            onSuccess(result.rows)
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


