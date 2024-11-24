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
    t: std::marker::PhantomData<T>,
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

    struct Hoge;

    let f = FieldX {
        t: std::marker::PhantomData::<Hoge>,
        u: f2,
    };

    let debug = format!("{:?}", f);
    let expected = r#"FieldX { t: TTTPhantomData<derive_debug::main::Hoge>TTT, u: UUUField { value: "F", bitmask: 0b00011100 }UUU }"#;

    assert_eq!(debug, expected);

    println!("{}", debug);
}
