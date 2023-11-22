let pg = require('pg')

const client = new pg.Client({
  host : 'localhost',
  port : 5334,
  database : 'spammer', 
  user : 'user'
})

exports.executeQuery = async queryString => {
  await client.connect(); 
  try {
    const result = await client.query(queryString);
    return result.rows; 
  } catch (err) {
    console.error('Error query:', err);
    throw err; 
  } finally {
    await client.end(); 
  }
}


