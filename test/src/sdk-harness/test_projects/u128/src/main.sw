contract;

use std::u128::*;

abi U128Contract {
    fn multiply_u64(a: u64, b: u64) -> (u64, u64);
}

impl U128Contract for Contract {
    fn multiply_u64(a: u64, b: u64) -> (u64, u64) {
        let result_u128: U128 = mul64(a, b);
        (result_u128.upper, result_u128.lower)
    }
}
