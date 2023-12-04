// use aiken/hash.{Blake2b_224, Hash}
// use aiken/list
// use aiken/transaction.{ScriptContext}
// use aiken/transaction/credential.{VerificationKey}
 
 
validator {
  fn always_true(_datum : Data, _redeemer : Data, _context : Data ) {
  True
  }
}
