use crate::commands::arguments::OptLevel;
use crate::commands::main_command::MainArguments;
use crate::commands::Arguments;
use std::path::PathBuf;
use std::process::Command;

const TEST_SOURCE: &str = "def Nat = (A : Type) -> (A -> A) -> A -> A;
def pow = (n : Nat) => (m : Nat) => (
    _ => n(_)(m(_))
) as Nat;
def three = (A => f => a => f(f(f(a)))) as Nat;
def nat_main = pow(three)(three);
";

#[test]
pub fn test_main() {
    let output = PathBuf::from("./twenty_seven");
    MainArguments {
        source: TEST_SOURCE.to_string(),
        args: Arguments {
            path: Default::default(),
            output: output.clone(),
            opt_level: OptLevel::Three,
            verbose: false,
            emit_un_opt_llvm: None,
            emit_llvm: None,
        },
    }
    .run()
    .unwrap();

    let program_output = Command::new(output).output().expect("program not found");
    assert_eq!(program_output.status.code(), Some(27), "program failed");
}
