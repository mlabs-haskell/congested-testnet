{pkgs,...}:
let
 trues = n : pkgs.lib.strings.concatStrings (pkgs.lib.strings.intersperse " && " (pkgs.lib.replicate n "True")); 
 ones_add =  n : pkgs.lib.strings.concatStrings (pkgs.lib.strings.intersperse " + " (pkgs.lib.replicate n "1")); 
 ones_mul =  n : pkgs.lib.strings.concatStrings (pkgs.lib.strings.intersperse " * " (pkgs.lib.replicate n "1")); 
 data = n_bytes : pkgs.lib.strings.concatStrings (pkgs.lib.replicate n_bytes "ff"); 
 code = s : h : u : pkgs.writeText "code" 
    # let add = ${ones_add j} 
    # let mul = ${ones_mul k} 
    # let trues = ${trues l}  
    # (add > 0) && (mul > 0) && trues && ind && rec(0) && data.ind
    # unit(0) && foo.ind && (mem(${toString m}) >= 0) && ind
    # fn mem(n, el, lst){
    #  if n==0 { True } else { mem(n-1, el, lst // [el] ) }
    #  }
 ''
   // use aiken/builtin.{append_bytearray}
   //  
   //  type Foo {
   //    Foo {data : ByteArray , ind : Bool}
   //  }

   //  fn last(lst : List<a>) {
   //       when lst is {
   //        [] -> None 
   //        [x] -> Some(x) 
   //        [_, ..xs] -> last(xs)
   //    }
   //  }

   //  fn foldl(lst: List<a>, zero: b, with: fn(a, b) -> b) -> b {
   //    when lst is {
   //    [] -> zero
   //    [x, ..xs] -> foldl(xs, with(x, zero), with)
   //   }
   //  }

   //  
   //  fn f(x,y) {
   //   x && y
   //  }

   //  fn mem(n, el, lst : List<Bool>){
   //    if (n==0) {
   //    // last(lst) == Some(el) 
   //    foldl(lst, True, f) 
   //    }     
   //    else {
   //    let lst1 = lst // [el]
   //    mem(n-1, el, lst1) 
   //    }
   //  }
   //  
   //  
   //  fn unit(i) {
   //    if i > ${toString u} {True}
   //    else {unit(i+1)}
   //  }

   //  type Helper {
   //    Helper {data : ByteArray , ind : Bool}
   //  }
   //  
   //  fn use_mem_unit(n, old : Helper){
   //    if n==0 {
   //     old.ind
   //    }
   //    else {
   //      let data = append_bytearray(old.data, old.data) 
   //      let new = Helper {data : data , ind : old.ind }
   //      use_mem_unit(n-1, new)
   //    }
   //  }

   //  fn body(ind) {
   //    let foo = Foo { data : #"${data s}", ind : True} 
   //    let helper = Helper {data : #"${data h}", ind : True}
   //    foo.ind && ind && use_mem_unit(1, helper)  
   //  }

   //  test foo(){
   //   body(True) 
   //  } 

    validator {
      fn always_true(_datum : Data, _redeemer : Data, _context : Data ) {
      True && True
     }
    }
   '';
  
in
[
 # {
 #  count = 3359;
 #  code = code 35 2 40 410; 
 # }
 # {
 #  count = 150;
 #  code = code 3000 1010 2 2; 
 # }
 {
  count = 686;
  # code = code 470 1220 2 2; 
  code = code 1 2 1; 
 }
]
 # code = i: j: k: l: pkgs.writeText "code" 
 # ''
 #    type Data {
 #      Data {data : Int, ind : Bool}
 #    }
 #    
 #    fn rec(i) {
 #      if i > ${toString i} {True}
 #      else {rec(i+1)}
 #    }
 #
 #
 #
 #    fn body(ind) {
 #    let data = Data { data : 1000, ind : True} 
 #    let add = ${ones_add j} 
 #    let mul = ${ones_mul k} 
 #    let trues = ${trues l}  
 #    (add > 0) && (mul > 0) && trues && ind && rec(0) && data.ind
 #    }
 #
 #    test foo(){
 #     body(True) 
 #    } 
 #
 #    validator {
 #      fn always_true(_datum : Data, _redeemer : Data, _context : Data ) {
 #      body(True)
 #     }
 #    }
 #   '';
