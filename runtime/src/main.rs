use std::error::Error;
use wasmtime::*;
use std::env;

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <wasm_file_path>", args[0]);
        std::process::exit(1);
    }
    let wasm_file_path = &args[1];

    let mut config = Config::new();
    config.wasm_gc(true);
    let engine = Engine::new(&config)?;
    let mut store = Store::new(&engine, ());

    let print_i32 = Func::wrap(&mut store, |i: i32| {
        println!("{}", i);
    });

    let read_i32 = Func::wrap(&mut store, || -> i32 {
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        let number: i32 = input.trim().parse().unwrap();
        number
    });

    let module = Module::from_file(&engine, wasm_file_path)?;

    let instance = Instance::new(&mut store, &module, &[print_i32.into(), read_i32.into()])?;

    let entrypoint = instance.get_func(&mut store, "entrypoint")
        .expect("`entrypoint` was not an exported function");
    let entrypoint = entrypoint.typed::<(), i32>(&store)?;
    let result = entrypoint.call(&mut store, ())?;
    println!("Exit Code: {:?}", result);
    Ok(())
}
