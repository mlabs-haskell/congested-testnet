const main =  async () => {
    const {spammer} = await import("../spammer/output/SpammerUtils/index.js");
    // const {_new, write} = await import("../spammer/output/Effect.Ref/foreign.js");
    // s = _new("hello")();
    //
    // setInterval(() => {
    //  i+=1;
    //  write("hhhh" + i)(s)();
    // }, 0)
    //
    // checkMut(s)();
  console.log("hi");
  // console.log(spammer);
  spammer()
    
}


main()

