use crate::main_helper;

const TEST_SOURCE: &str = "let Nat = (A : Type) -> (A -> A) -> A -> A;
let pow = (n : Nat) => (m : Nat) => (
    _ => n(_)(m(_))
) as Nat;
let three = (A => f => a => f(f(f(a)))) as Nat;
let twenty_seven = pow(three)(three);
";

#[test]
pub fn test_main() {
    main_helper(TEST_SOURCE);
}
