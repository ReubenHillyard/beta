use inkwell::values::PointerValue;

pub mod build;
pub mod compile;
mod constants;
mod functions;
mod initialize;
pub mod lambda;
#[cfg(test)]
pub mod tests;
pub mod types;
pub mod values;

#[derive(Copy, Clone)]
pub enum Location<'ctx> {
    Alloca,
    Arg,
    Within(PointerValue<'ctx>),
}
