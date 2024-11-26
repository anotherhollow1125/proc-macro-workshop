use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let input = TokenStream2::from(input);

    dbg!(&input);

    TokenStream::new()
}
