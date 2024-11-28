// for playground

use seq::seq;

seq!(N in 1..4 {
    fn f~N () -> u64 {
        N * 2
    }
});

seq!(N in 1..4 {
    fn f~N~_ () -> u64 {
        N * 3
    }
});

seq!(N in 0..16 {
    #[derive(Copy, Clone, PartialEq, Debug)]
    enum Interrupt {
        #(
            Irq~N,
        )*
    }
});

// This f0 is written separately to detect whether your macro correctly starts
// with the first iteration at N=1 as specified in the invocation. If the macro
// incorrectly started at N=0 like in the previous tests cases, the first
// generated function would conflict with this one and the program would not
// compile.
fn f0() -> u64 {
    100
}

// try nest

seq!(N in 0..5 {
    #(
        fn nest~N~outer() {
            seq!(M in 0..10 {
                println!("{}", N + M);
            });
            seq!(L in 0..5 {
                let tmp~N = vec![#(L,)*].len() + N;
                println!("{}", tmp~N);
            });
        }
    )*
});

fn main() {
    let sum = f0() + f1() + f2() + f3();
    let _ = f1_() + f2_() + f3_();

    assert_eq!(sum, 100 + 2 + 4 + 6);

    let interrupt = Interrupt::Irq8;

    assert_eq!(interrupt as u8, 8);
    assert_eq!(interrupt, Interrupt::Irq8);

    nest0outer();
    nest1outer();
    nest2outer();
    nest3outer();
    nest4outer();
}
