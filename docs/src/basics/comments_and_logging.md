# Comments and Logging

## Comments

Comments in Sway start with two slashes and continue until the end of the line. For comments that extend beyond a single line, you'll need to include `//` on each line.

```sway
// hello world
```

```sway
// let's make a couple of lines
// commented.
```

You can also place comments at the ends of lines containing code.

```sway
fn main() {
    let baz = 8; // Eight is a lucky number
}
```

You can also do block comments

```sway
fn main() {
    /*
    You can write on multiple lines
    like this if you want
    */
    let baz = 8;
}
```

## Logging

The `logging` library provides a generic `log` function that can be imported using `use std::logging::log` and used to log variables of any type. Each call to `log` appends a `receipt` to the list of receipts. There are two types of receipts that a `log` can generate: `Log` and `LogData`.

> **Note**: The receipts shown below are being re-worked to become more readable in [#1717](https://github.com/FuelLabs/sway/pull/1717).

### `Log` Receipt

The `Log` receipt is generated for _non-reference_ types, namely `bool`, `u8`, `u16`, `u32`, and `u64`. For example, logging an integer variable `x` that holds the value `42` using `log(x)` may generate the following receipt:

```console
Log {
    id: 0x0000000000000000000000000000000000000000000000000000000000000000,
    ra: 42,
    rb: 0,
    rc: 0,
    rd: 0,
    pc: 10404,
    is: 10352,
}
```

Note that `ra` will include the value being logged. The additional registers `rb` to `rd` will be zero when using `log(x)`.

### `LogData` Receipt

`LogData` is generated for _reference_ types which include all types except for the _non_reference_ types mentioned above. For example, logging a `b256` variable `b` that holds the value `0x1111111111111111111111111111111111111111111111111111111111111111` using `log(b)` may generate the following receipt:

```console
LogData {
    id: 0x0000000000000000000000000000000000000000000000000000000000000000,
    ra: 0,
    rb: 0,
    ptr: 10468,
    len: 32,
    digest: 0x02d449a31fbb267c8f352e9968a79e3e5fc95c1bbeaa502fd6454ebde5a4bedc,
    data: [
        17,
        17,
        17,
        17,
        17,
        17,
        17,
        17,
        17,
        17,
        17,
        17,
        17,
        17,
        17,
        17,
        17,
        17,
        17,
        17,
        17,
        17,
        17,
        17,
        17,
        17,
        17,
        17,
        17,
        17,
        17,
        17,
    ],
    pc: 10444,
    is: 10352,
}
```

Note that `data` in the receipt above is a essentially a list of bytes where each entry is the decimal representation of a given byte in the data being logged (e.g. `17 = 0x11`). 