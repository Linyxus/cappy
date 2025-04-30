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
    config.wasm_reference_types(true);
    config.wasm_function_references(true);
    config.max_wasm_stack(2 * 1024 * 1024);
    let engine = Engine::new(&config)?;
    let mut store = Store::new(&engine, ());

    let println_i32 = Func::wrap(&mut store, |i: i32| {
        println!("{}", i);
    });

    let read_i32 = Func::wrap(&mut store, || -> i32 {
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        let number: i32 = input.trim().parse().unwrap();
        number
    });

    let println_char = Func::wrap(&mut store, |i: i32| {
        let ch = char::from_u32(i as u32).unwrap();
        print!("{}", ch);
    });

    let module = Module::from_file(&engine, wasm_file_path)?;

    let instance = Instance::new(&mut store, &module, &[println_i32.into(), read_i32.into(), println_char.into()])?;
    //let instance = Instance::new(&mut store, &module, &[])?;

    let entrypoint = instance.get_func(&mut store, "entrypoint")
        .expect("`entrypoint` was not an exported function");
    let entrypoint = entrypoint.typed::<(), i32>(&store)?;
    let result = entrypoint.call(&mut store, ())?;
    println!("Exit Code: {:?}", result);
    Ok(())
}
