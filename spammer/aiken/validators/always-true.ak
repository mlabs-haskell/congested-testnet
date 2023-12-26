use aiken/list

fn add_num(s,t) {
  if s > t {
  True && True && True && True && True && True && True  
  } else {
   add_num(s+1,t)
  }
}


test add_num_1(){
  add_num(0,1) 
}

 
validator {
  fn always_true(_datum : Data, _redeemer : Data, _context : Data ) {
   add_num(0,1)
  }
}
