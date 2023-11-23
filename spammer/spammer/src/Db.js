let pg = require('pg')

const client = new pg.Client({
  host : 'localhost',
  port : 5432,
  database : 'spammer', 
  user : 'user'
})

exports._executeQuery = function (queryString) {
  return function (onError, onSuccess) {
    return client.connect()
        .then(() => {
            return client.query(queryString);
        })
        .then(result => {
            console.log(result.rows);
            // console.log(result.rows.length);
            // onSuccess([{"one" : 1}])
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

// exports._executeQuery = function (queryString) {
//   return function (onError, onSuccess) {
//       onSuccess([{"one":1}]);
//   };
// };

