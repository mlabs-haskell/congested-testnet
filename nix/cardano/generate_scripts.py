import os
import pathlib
import textwrap

def validators():
    code_parts = []
    code = f"""
    use aiken/builtin.{{append_bytearray, sha2_256}}
    type Foo {{
      Foo {{data : ByteArray , ind : Bool}}
    }}

    fn use_mem_unit(n, x : ByteArray){{
      if n==0 {{
        x 
      }}
      else {{
        let x1 = append_bytearray(x, #"ff") 
        use_mem_unit(n-1, x1)
      }}
    }}

    fn body(ind, data_hex, n_iterations) {{
      let foo = Foo {{ data : data_hex, ind : True}} 
      let mem = use_mem_unit(n_iterations, #"ff")
      foo.ind && ind && (sha2_256(foo.data) != sha2_256(mem)) 
    }}
    """
    code_parts.append(textwrap.dedent(code))

    n_bytes = list(range(1,100))
    n_iterations = list(range(1,100))
    for i, (n_byte, n_iter) in enumerate(zip(n_bytes, n_iterations)): 
        data = "".join(["ff" for _ in range(n_byte)]) 
        code = f"""
        validator {{
         fn always_true_{i}(_datum : Data, _redeemer : Data, _context : Data ) {{
         body(True,#"{data}",{n_iter})
        }}
        }}
        """
        code_parts.append(textwrap.dedent(code))
    return "".join(code_parts)







PARS = [
        {"count" : 686, "n_bytes" : 2700 , "n_iterations" : 500 }
]
       
if __name__ == '__main__':
    # root = pathlib.Path(os.environ["wallet_path"])
    os.system("aiken new spammer/scripts")
    os.chdir("scripts")
    with open(pathlib.Path("validators") /"always_true.ak", "w+") as f:
        f.write(validators())
    with open("aiken.toml", "w") as f:
        text = """
        name = "spammer/scripts"
        version = "0.0.0"
        """ 
        f.write(textwrap.dedent(text))
    os.system("aiken build")


    


