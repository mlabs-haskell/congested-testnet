{ pkgs,  ... }:
let
  # trues = n: pkgs.lib.strings.concatStrings (pkgs.lib.strings.intersperse " && " (pkgs.lib.replicate n "True"));
  data = n_bytes: pkgs.lib.strings.concatStrings (pkgs.lib.replicate n_bytes "ff");
  code = s: n: ''
        DATA="${data s}"
        DATA="$VKEY$DATA"

        cat << EOF > validators/always-true.ak 
        
        use aiken/builtin.{append_bytearray, sha2_256}
        
        type Foo {
          Foo {data : ByteArray , ind : Bool}
        }
        
        fn use_mem_unit(n, x : ByteArray){
          if n==0 {
            x 
          }
          else {
            let x1 = append_bytearray(x, #"ff") 
            use_mem_unit(n-1, x1)
          }
        }

        fn body(ind) {
          let foo = Foo { data : #"$DATA", ind : True} 
          let mem = use_mem_unit(${toString n}, #"ff")
          foo.ind && ind && (sha2_256(foo.data) != sha2_256(mem)) 
        }

        test foo(){
         body(True) 
        } 

       validator {
         fn always_true(_datum : Data, _redeemer : Data, _context : Data ) {
         body(True)
        }
       }
    EOF
    '';

in
[
  {
   count = 66000;
   code = "";
  }
  {
   count = 3359;
   code = code 1500 130;
  }
  {
   count = 150;
   code = code 1700 2400; 
  }
  {
    count = 686;
    code = code 2700 500; 
  }
  {
    count = 5622;
    code = code 3000 2; 
  }
  {
    count = 2679;
    code = code 3900 1000; 
  }
  {
    count = 1200;
    code = code 4500 1000; 
  }
]
