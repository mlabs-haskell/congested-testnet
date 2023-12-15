// use aiken/hash.{Blake2b_224, Hash}
// use aiken/list
// use aiken/transaction.{ScriptContext}
// use aiken/transaction/credential.{VerificationKey}
 
fn add_num(s,t) {
  if s > t {
  True
  } else {
   add_num(s+1,t)
  }
}

//test add_num_1(){
//  add_num(0,2000) > 0
//}
//
//test add_num_2(){
//  add_num(0,2000000) > 0
//}
 
validator {
  fn always_true(_datum : Data, _redeemer : Data, _context : Data ) {
//  let res = add_num(0,20)
  True && True
  }
//  fn mint(_redeemer : Data, _context : Data ) {
//  True 
//  }
}
