library i64;

use core::num::*;
use ::assert::assert;

/// The 64-bit signed integer type.
/// Represented as an underlying u64 value.
pub struct I64 {
    underlying: u64,
}

pub trait From {
    /// Function for creating I64 from u64
    fn from(value: u64) -> Self;
}

impl From for I64 {
    fn from(value: u64) -> I64 {
        I64 {
            underlying: value,
        }
    }
}

impl core::ops::Eq for I64 {
    pub fn eq(self, other: I64) -> bool {
        self.underlying == other.underlying
    }
}

impl core::ops::Ord for I64 {
    pub fn gt(self, other: Self) -> bool {
        self.underlying > other.underlying
    }

    pub fn lt(self, other: Self) -> bool {
        self.underlying < other.underlying
    }
}

impl I64 {
    pub fn indent() -> u64 {
        9223372036854775808u64
    }
}

impl I64 {
    /// Initializes a new, zeroed I64.
    pub fn new() -> I64 {
        I64 {
            underlying: ~I64::indent(),
        }
    }

    /// The smallest value that can be represented by this integer type.
    pub fn min() -> I64 {
        I64 {
            underlying: ~u64::min(),
        }
    }

    /// The largest value that can be represented by this type,
    pub fn max() -> I64 {
        I64 {
            underlying: ~u64::max(),
        }
    }

    /// The size of this type in bits.
    pub fn bits() -> u32 {
        64
    }

    pub fn neg_from(value: u64) -> I64 {
        I64 {
            underlying: ~I64::indent() - value,
        }
    }

    fn from_uint(value: u64) -> I64 {
        let underlying: u64 = value + ~I64::indent(); // as the minimal value of I64 is -~I64::indent() (1 << 63) we should add ~I64::indent() (1 << 63) 
        I64 {
            underlying
        }
    }
}

impl core::ops::Add for I64 {
    /// Add a I64 to a I64. Panics on overflow.
    pub fn add(self, other: Self) -> Self {
        ~I64::from(self.underlying - ~I64::indent() + other.underlying) // subtract 1 << 63 to avoid double move
    }
}

impl core::ops::Subtract for I64 {
    /// Subtract a I64 from a I64. Panics of overflow.
    pub fn subtract(self, other: Self) -> Self {
        let mut res = ~I64::new();
        if self > other {
            res = ~I64::from(self.underlying - other.underlying + ~I64::indent()); // add 1 << 63 to avoid loosing the move
        } else {
            res = ~I64::from(~I64::indent() - (other.underlying - self.underlying)); // subtract from 1 << 63 as we are getting a negative value
        }
        res
    }
}

impl core::ops::Multiply for I64 {
    /// Multiply a I64 with a I64. Panics of overflow.
    pub fn multiply(self, other: Self) -> Self {
        let mut res = ~I64::new();
        if self.underlying >= ~I64::indent() && other.underlying >= ~I64::indent() {
            res = ~I64::from((self.underlying - ~I64::indent()) * (other.underlying -~I64::indent()) + ~I64::indent());
        } else if self.underlying < ~I64::indent() && other.underlying < ~I64::indent() {
            res = ~I64::from((~I64::indent() - self.underlying) * (~I64::indent() - other.underlying) + ~I64::indent());
        } else if self.underlying >= ~I64::indent() && other.underlying < ~I64::indent() {
            res = ~I64::from(~I64::indent() - (self.underlying - ~I64::indent()) * (~I64::indent() - other.underlying));
        } else if self.underlying < ~I64::indent() && other.underlying >= ~I64::indent() {
            res = ~I64::from(~I64::indent() - (other.underlying - ~I64::indent()) * (~I64::indent() - self.underlying));
        }
        res
    }
}

impl core::ops::Divide for I64 {
    /// Divide a I64 by a I64. Panics if divisor is zero.
    pub fn divide(self, divisor: Self) -> Self {
        assert(divisor != ~I64::new());
        let mut res = ~I64::new();
        if self.underlying >= ~I64::indent() && divisor.underlying > ~I64::indent() {
            res = ~I64::from((self.underlying - ~I64::indent()) / (divisor.underlying -~I64::indent()) + ~I64::indent());
        } else if self.underlying < ~I64::indent() && divisor.underlying < ~I64::indent() {
            res = ~I64::from((~I64::indent() - self.underlying) / (~I64::indent() - divisor.underlying) + ~I64::indent());
        } else if self.underlying >= ~I64::indent() && divisor.underlying < ~I64::indent() {
            res = ~I64::from(~I64::indent() - (self.underlying - ~I64::indent()) / (~I64::indent() - divisor.underlying));
        } else if self.underlying < ~I64::indent() && divisor.underlying > ~I64::indent() {
            res = ~I64::from(~I64::indent() - (~I64::indent() - self.underlying) / (divisor.underlying - ~I64::indent()));
        }
        res
    }
}