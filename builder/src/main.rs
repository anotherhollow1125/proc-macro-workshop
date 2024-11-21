// for playground

use derive_builder::{Builder, Nop};

#[derive(Builder, Nop)]
pub struct Command {
    executable: String,
    #[nop]
    #[builder(each = "arg")]
    args: Vec<String>,
    #[builder(each = "env")]
    #[nop]
    env: Vec<String>,
    current_dir: Option<String>,
}

fn main() {
    let command = Command::builder()
        .executable("cargo".to_owned())
        .arg("build".to_owned())
        .arg("--release".to_owned())
        .build()
        .unwrap();

    assert_eq!(command.executable, "cargo");
    assert_eq!(command.args, vec!["build", "--release"]);
}
