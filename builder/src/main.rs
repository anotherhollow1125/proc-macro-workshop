// for playground

use derive_builder::Builder;

#[derive(Builder)]
pub struct Command {
    pub executable: String,
    pub args: Vec<String>,
    pub env: Vec<String>,
    pub current_dir: String,
}

fn main() {
    let builder = Command::builder();

    let _ = builder;
}
