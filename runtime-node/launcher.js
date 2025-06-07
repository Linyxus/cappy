import fs from 'node:fs/promises';
import assert from 'assert';

const wasmPath = process.argv[2];
assert(wasmPath != undefined)

const importObject = {
  host: {
    println_i32: (x) => console.log('println_i32: ', x),
    read_i32: () => 42,
    println_char: (ch) => {
      const char = String.fromCharCode(ch);
      process.stdout.write(char);
    },
    get_timestamp: () => {
      return Number(process.hrtime.bigint()) / 1_000_000;
    },
  }
}

const wasmBuffer = await fs.readFile(wasmPath);
const wasmModule = await WebAssembly.instantiate(wasmBuffer, importObject);

const { entrypoint } = wasmModule.instance.exports;
entrypoint();
