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
  let res = add_num(0,20)
  True && True && True && True && res  
  }
}
// 510100003222253330044a229309b2b2b9a1
// 581801000032222533300453330044a229445280a4c26cacae69
// 5820010000322225333004533300453330044a229445280a5114a029309b2b2b9a01
// 58270100003222253330045333004533300453330044a229445280a5114a029445280a4c26cacae681
