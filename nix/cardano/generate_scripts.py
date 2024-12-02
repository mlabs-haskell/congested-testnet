import os
import pathlib
import textwrap
import json

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

    # for distribution
    counts = [6000,3359,150,686,5622,2679,1200]
    s = 0 
    cumsum = []
    for c in counts:
        s += c
        cumsum.append(s)

    distrib = [] 
    for c in cumsum:
        distrib.append(c / cumsum[-1])

    n_bytes = [1500,1700,2700,3000,3900,5000]
    n_iters= [130,2400,500,2,1000,1000]

    n_validators = len(n_bytes)
    for i, (n_byte, n_iter) in enumerate(zip(n_bytes, n_iters)): 
        data = "".join(["ff" for _ in range(n_byte)]) 
        code = f"""
        validator {{
         fn always_true_{i}(_datum : Data, _redeemer : Data, _context : Data ) {{
             body(True,#"{data}",{n_iter})
        }}
        }}
        """
        code_parts.append(textwrap.dedent(code))
    return "".join(code_parts), n_validators, distrib


       
if __name__ == '__main__':
    os.system("aiken new spammer/scripts")
    os.chdir("scripts")
    with open(pathlib.Path("validators") /"always_true.ak", "w+") as f:
        val_str, n_validators, distrib = validators()
        f.write(val_str)
    with open("aiken.toml", "w") as f:
        text = """
        name = "spammer/scripts"
        version = "0.0.0"
        """ 
        f.write(textwrap.dedent(text))
    os.system("aiken build")
    scripts = []
    for j in range(n_validators):
        os.system(f"aiken blueprint convert -v always_true.always_true_{j} > temp.json")
        with open("temp.json", 'r') as f:
            dct = json.load(f)
            scripts.append(dct["cborHex"])

    json_str = json.dumps(scripts, indent=4)
    javascript_code = f"export const alwaysSucceeds = {json_str}" 
    javascript_code += f"\nexport const distribution = {distrib}" 
    print(javascript_code)





    

    


    


