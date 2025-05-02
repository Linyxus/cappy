use std::error::Error;
use wasmtime::*;
use std::env;
use std::time::Instant;

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

    let mut linker = Linker::new(&engine);

    let now = Instant::now();
    linker.func_wrap("host", "get_timestamp", move || -> i32 {
        now.elapsed().as_millis() as i32
    })?;

    linker.func_wrap("host", "println_i32", |i: i32| {
        println!("{}", i);
    })?;

    linker.func_wrap("host", "read_i32", || -> i32 {
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        let number: i32 = input.trim().parse().unwrap();
        number
    })?;

    linker.func_wrap("host", "println_char", |i: i32| {
        let ch = char::from_u32(i as u32).unwrap();
        print!("{}", ch);
    })?;

    let module = Module::from_file(&engine, wasm_file_path)?;

    let instance = 
      linker.instantiate(&mut store, &module)?;

    let entrypoint = instance.get_func(&mut store, "entrypoint")
        .expect("`entrypoint` was not an exported function");
    let entrypoint = entrypoint.typed::<(), i32>(&store)?;
    let result = entrypoint.call(&mut store, ())?;
    println!("Exit Code: {:?}", result);
    Ok(())
}
