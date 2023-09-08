use inkwell::values::FunctionValue;

pub struct Lambda<'ctx> {
    pub(crate) raw_function: FunctionValue<'ctx>,
    pub(crate) ret_size_function: FunctionValue<'ctx>,
    pub(crate) captures_clone_function: FunctionValue<'ctx>,
    pub(crate) captures_destroy_function: FunctionValue<'ctx>,
    pub(crate) captures: Vec<usize>,
}

impl<'ctx> Lambda<'ctx> {
    pub(crate) fn new(
        function: FunctionValue<'ctx>,
        size_function: FunctionValue<'ctx>,
        captures_clone_function: FunctionValue<'ctx>,
        captures_destroy_function: FunctionValue<'ctx>,
        captures: &[bool],
    ) -> Lambda<'ctx> {
        Lambda {
            raw_function: function,
            ret_size_function: size_function,
            captures_clone_function,
            captures_destroy_function,
            captures: captures
                .iter()
                .enumerate()
                .filter(|p| *p.1)
                .map(|p| p.0)
                .collect(),
        }
    }
    fn pos_in_captures(&self, index: usize) -> Option<usize> {
        self.captures.iter().position(|e| *e == index)
    }
}
