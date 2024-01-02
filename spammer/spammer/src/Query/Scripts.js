const fs = require('fs');
const mathjs = require('mathjs');

const fscripts = "/tmp/wallet/scripts"
const fcounts = "/tmp/wallet/counts"

function getValidatorsCounts(fscripts, fcounts) {
try {
  let contents = fs.readFileSync(fscripts, 'utf-8');
  validators = contents.split(/\r?\n/);
  validators = validators.slice(0,-1);
  contents = fs.readFileSync(fcounts, 'utf-8');
  let counts = contents.split(/\r?\n/);
  counts = counts.slice(0,-1).map(x => parseInt(x));

  return [validators, counts];

} catch (err) {
    console.log(err);
}
}

const data = getValidatorsCounts(fscripts, fcounts) 

exports._sampleValidator = () => mathjs.pickRandom(data[0],data[1]) 
exports._allValidators = () => data[0].slice(1)  



