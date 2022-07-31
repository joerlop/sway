library i32;

use core::num::*;
use ::assert::assert;

/// The 32-bit signed integer type.
/// Represented as an underlying u32 value.
pub struct I32 {
    underlying: u32,
}

pub trait From {
    /// Function for creating I32 from u32
    fn from(value: u32) -> Self;
}

impl From for I32 {
    fn from(value: u32) -> I32 {
        I32 {
            underlying: value,
        }
    }
}

impl core::ops::Eq for I32 {
    pub fn eq(self, other: I32) -> bool {
        self.underlying == other.underlying
    }
}

impl core::ops::Ord for I32 {
    pub fn gt(self, other: Self) -> bool {
        self.underlying > other.underlying
    }

    pub fn lt(self, other: Self) -> bool {
        self.underlying < other.underlying
    }
}

impl I32 {
    pub fn indent() -> u32 {
        2147483648u32
    }
}

impl I32 {
    /// Initializes a new, zeroed I32.
    pub fn new() -> I32 {
        I32 {
            underlying: ~I32::indent(),
        }
    }

    /// The smallest value that can be represented by this integer type.
    pub fn min() -> I32 {
        I32 {
            underlying: ~u32::min(),
        }
    }

    /// The largest value that can be represented by this type,
    pub fn max() -> I32 {
        I32 {
            underlying: ~u32::max(),
        }
    }

    /// The size of this type in bits.
    pub fn bits() -> u32 {
        32
    }

    pub fn neg_from(value: u32) -> I32 {
        I32 {
            underlying: ~I32::indent() - value,
        }
    }

    fn from_uint(value: u32) -> I32 {
        let underlying: u32 = value + ~I32::indent(); // as the minimal value of I32 is 2147483648 (1 << 31) we should add ~I32::indent() (1 << 31) 
        I32 {
            underlying
        }
    }
}

impl core::ops::Add for I32 {
    /// Add a I32 to a I32. Panics on overflow.
    pub fn add(self, other: Self) -> Self {
        ~I32::from(self.underlying - ~I32::indent() + other.underlying) // subtract 1 << 31 to avoid double move
    }
}

impl core::ops::Subtract for I32 {
    /// Subtract a I32 from a I32. Panics of overflow.
    pub fn subtract(self, other: Self) -> Self {
        let mut res = ~I32::new();
        if self > other {
            res = ~I32::from(self.underlying - other.underlying + ~I32::indent()); // add 1 << 31 to avoid loosing the move
        } else {
            res = ~I32::from(~I32::indent() - (other.underlying - self.underlying)); // subtract from 1 << 31 as we are getting a negative value
        }
        res
    }
}

impl core::ops::Multiply for I32 {
    /// Multiply a I32 with a I32. Panics of overflow.
    pub fn multiply(self, other: Self) -> Self {
        let mut res = ~I32::new();
        if self.underlying >= ~I32::indent() && other.underlying >= ~I32::indent() {
            res = ~I32::from((self.underlying - ~I32::indent()) * (other.underlying -~I32::indent()) + ~I32::indent());
        } else if self.underlying < ~I32::indent() && other.underlying < ~I32::indent() {
            res = ~I32::from((~I32::indent() - self.underlying) * (~I32::indent() - other.underlying) + ~I32::indent());
        } else if self.underlying >= ~I32::indent() && other.underlying < ~I32::indent() {
            res = ~I32::from(~I32::indent() - (self.underlying - ~I32::indent()) * (~I32::indent() - other.underlying));
        } else if self.underlying < ~I32::indent() && other.underlying >= ~I32::indent() {
            res = ~I32::from(~I32::indent() - (other.underlying - ~I32::indent()) * (~I32::indent() - self.underlying));
        }
        res
    }
}

impl core::ops::Divide for I32 {
    /// Divide a I32 by a I32. Panics if divisor is zero.
    pub fn divide(self, divisor: Self) -> Self {
        assert(divisor != ~I32::new());
        let mut res = ~I32::new();
        if self.underlying >= ~I32::indent() && divisor.underlying > ~I32::indent() {
            res = ~I32::from((self.underlying - ~I32::indent()) / (divisor.underlying -~I32::indent()) + ~I32::indent());
        } else if self.underlying < ~I32::indent() && divisor.underlying < ~I32::indent() {
            res = ~I32::from((~I32::indent() - self.underlying) / (~I32::indent() - divisor.underlying) + ~I32::indent());
        } else if self.underlying >= ~I32::indent() && divisor.underlying < ~I32::indent() {
            res = ~I32::from(~I32::indent() - (self.underlying - ~I32::indent()) / (~I32::indent() - divisor.underlying));
        } else if self.underlying < ~I32::indent() && divisor.underlying > ~I32::indent() {
            res = ~I32::from(~I32::indent() - (~I32::indent() - self.underlying) / (divisor.underlying - ~I32::indent()));
        }
        res
    }
}
