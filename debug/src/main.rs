// for play ground.

use derive_debug::CustomDebug;

#[derive(CustomDebug)]
pub struct Field<T> {
    value: T,
    #[debug = "0b{:08b}"]
    bitmask: u8,
}

#[derive(CustomDebug)]
pub struct FieldStr {
    value: &'static str,
    #[debug = "0b{:08b}"]
    bitmask: u8,
}

#[derive(CustomDebug)]
pub struct FieldX<T, U>
where
    T: 'static + Send + Sync,
    U: 'static + Send + Sync,
{
    #[debug = "TTT{:?}TTT"]
    t: T,
    #[debug = "UUU{:?}UUU"]
    u: U,
}

fn main() {
    let f1 = FieldStr {
        value: "F",
        bitmask: 0b00011100,
    };

    let debug = format!("{:?}", f1);
    let expected = r#"FieldStr { value: "F", bitmask: 0b00011100 }"#;

    assert_eq!(debug, expected);

    let f2 = Field {
        value: "F",
        bitmask: 0b00011100,
    };

    let debug = format!("{:?}", f2);
    let expected = r#"Field { value: "F", bitmask: 0b00011100 }"#;

    assert_eq!(debug, expected);

    let f = FieldX { t: f1, u: f2 };

    let debug = format!("{:?}", f);
    let expected = r#"FieldX { t: TTTFieldStr { value: "F", bitmask: 0b00011100 }TTT, u: UUUField { value: "F", bitmask: 0b00011100 }UUU }"#;

    assert_eq!(debug, expected);

    println!("{}", debug);
}
