#![feature(generic_const_exprs)]

use std::{
    fmt::{Debug, Display},
    ops::{Add, Div, Mul, Sub},
};

pub trait UnitExps {
    const KG: i8;
    const M: i8;
    const S: i8;
}

/// Represents in the type system a unit.
/// * KG = Gram exponent
/// * M = Meter exponent
/// * S = Second exponent
///
/// ## Example
/// `Unit<1, 1, -2>` represents the unit `kg m / s^2`.
#[derive(Clone, Copy)]
pub struct Unit<const KG: i8, const M: i8, const S: i8>;

impl<const KG: i8, const M: i8, const S: i8> Display for Unit<KG, M, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(if KG < 0 { "1/" } else { "" })?;

        match M.abs() {
            0 => (),
            1 => write!(f, "kg")?,
            n => write!(f, "kg^{n}")?,
        }

        f.write_str(if M < 0 { "/" } else { " " })?;

        match M.abs() {
            0 => (),
            1 => write!(f, "m")?,
            n => write!(f, "m^{n}")?,
        }

        f.write_str(if S < 0 { "/" } else { " " })?;

        match S.abs() {
            0 => (),
            1 => write!(f, "s")?,
            n => write!(f, "s^{n}")?,
        }

        Ok(())
    }
}

impl<T: Debug, const KG: i8, const M: i8, const S: i8> Debug for Quantity<T, Unit<KG, M, S>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} {}", self.value, self.unit)
    }
}

impl<const KG1: i8, const M1: i8, const S1: i8> UnitExps for Unit<KG1, M1, S1> {
    const KG: i8 = KG1;
    const M: i8 = M1;
    const S: i8 = S1;
}

#[derive(Clone, Copy)]
pub struct Quantity<Value, U: UnitExps> {
    pub value: Value,
    pub unit: U,
}

impl<T, const KG: i8, const M: i8, const S: i8> Quantity<T, Unit<KG, M, S>> {
    pub fn new(value: T) -> Self {
        Self { value, unit: Unit }
    }
}

impl<T1, T2, T3, U1, U2> Mul<Quantity<T2, U2>> for Quantity<T1, U1>
where
    T1: Mul<T2, Output = T3>,
    U1: UnitExps,
    U2: UnitExps,
    Unit<{ U1::KG + U2::KG }, { U1::M + U2::M }, { U1::S + U2::S }>: Sized,
{
    type Output = Quantity<T3, Unit<{ U1::KG + U2::KG }, { U1::M + U2::M }, { U1::S + U2::S }>>;
    fn mul(self, rhs: Quantity<T2, U2>) -> Self::Output {
        Quantity {
            value: self.value * rhs.value,
            unit: Unit,
        }
    }
}

impl<T1, T2, T3, U1, U2> Div<Quantity<T2, U2>> for Quantity<T1, U1>
where
    T1: Div<T2, Output = T3>,
    U1: UnitExps,
    U2: UnitExps,
    Unit<{ U1::KG - U2::KG }, { U1::M - U2::M }, { U1::S - U2::S }>: Sized,
{
    type Output = Quantity<T3, Unit<{ U1::KG - U2::KG }, { U1::M - U2::M }, { U1::S - U2::S }>>;
    fn div(self, rhs: Quantity<T2, U2>) -> Self::Output {
        Quantity {
            value: self.value / rhs.value,
            unit: Unit,
        }
    }
}

#[test]
fn test_mul_div() {
    let mass = Quantity::<_, Unit<1, 0, 0>>::new(0.5);
    let length = Quantity::<_, Unit<0, 1, 0>>::new(1.0);
    let time = Quantity::<_, Unit<0, 0, 1>>::new(2.0);

    let force = mass * length / time / time;

    println!("{force:?}");
}
