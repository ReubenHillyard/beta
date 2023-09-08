//! Types and functions for dealing with lambda captures.

use crate::ir_gen::build::CompilerWithBuilder;
use crate::ir_gen::values::destructors::Destructors;
use crate::ir_gen::values::low::LoadStore;
use crate::ir_gen::values::tags::args::MemcpyArgs;
use crate::ir_gen::values::tags::flow::{Flow, Partial, Read, Write};
use crate::ir_gen::values::tags::{tag, Tag};
use crate::ir_gen::values::PtrValue;
use crate::typing::ast::EVariable;
use crate::typing::environments::DefsWithCtx;
use crate::typing::expression::CoreExpression;
use crate::typing::value::{Level, Type, VVariable, Value};
use std::iter::Copied;
use std::slice;

/// The record of what locals a lambda captures.
#[derive(Default)]
pub struct Captures {
    ctx_len: usize,
    captures: Vec<Level>,
}

/// The location of a local variable.
enum VarLocation {
    /// The argument to the current function.
    Arg,
    /// The nth capture of the current function.
    Capture(u64),
}

impl Captures {
    pub(crate) fn from_ret_val(
        defs_ctx: &DefsWithCtx,
        _ret_val: &Value,
        _ret_type: &Type,
    ) -> Captures {
        let ctx_len = defs_ctx.ctx.len();
        /*let ret_expr = read_back_with_ctx_len(defs_ctx.defs, ctx_len + 1, ret_val);
        let ret_type_expr = read_back_with_ctx_len(defs_ctx.defs, ctx_len + 1, ret_type.wrapped());
        let mut vars = vec![false; defs_ctx.ctx.len()];
        get_captures(&ret_expr, &mut vars, 1);
        get_captures(&ret_type_expr, &mut vars, 1);

        let captures = vars
            .into_iter()
            .enumerate() // pair with index
            .filter_map(|(index, captured)| {
                if captured {
                    let level = (ctx_len - 1) - index;
                    Some(Level::create_level(level))
                } else {
                    None
                }
            }) // get captured levels
            .rev() // reverse order so lower levels appear earlier
            .collect(); // collect into Vec*/
        let captures = (0..ctx_len).map(Level::create_level).collect();
        // capture everything, for now
        // improve capture algorithm later
        Captures { ctx_len, captures }
    }

    /// Determines the location of the local variable with a given level.
    ///
    /// Requires `level` is a valid level for a variable in the environment from which
    /// `var_location` is called.
    fn var_location(&self, level: Level) -> VarLocation {
        if level.get_inner() == self.ctx_len {
            VarLocation::Arg
        } else {
            VarLocation::Capture(self.captures.iter().position(|e| *e == level).unwrap() as u64)
        }
    }

    /// Produces an iterator over the levels of the captured variables.
    pub fn iter(&self) -> Copied<slice::Iter<'_, Level>> {
        self.captures.iter().copied()
    }

    /// Determines the number of captured variables.
    pub fn number(&self) -> usize {
        self.captures.len()
    }
}

impl<'b> IntoIterator for &'b Captures {
    type Item = Level;
    type IntoIter = Copied<slice::Iter<'b, Level>>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

fn _get_captures(expr: &CoreExpression, vars: &mut [bool], extended: usize) {
    use CoreExpression::*;
    match expr {
        Variable(EVariable::Local(index)) => {
            let index = index._get_inner();
            if index >= extended {
                let index = index - extended;
                assert!(index < vars.len());
                vars[index] = true;
            }
        }
        Lambda { ret_val } => {
            _get_captures(ret_val, vars, extended + 1);
        }
        Application { func, arg } => {
            _get_captures(func, vars, extended);
            _get_captures(arg, vars, extended);
        }
        _ => {}
    }
}

pub trait CaptureOffsetIndexable: Flow {
    type Offset: Flow;
}

impl CaptureOffsetIndexable for Read {
    type Offset = Read;
}

impl CaptureOffsetIndexable for Write {
    type Offset = Write;
}

impl CaptureOffsetIndexable for Partial {
    type Offset = Read;
}

pub trait CaptureIndexable: CaptureOffsetIndexable<Offset=Read> {
    type Elem: Flow;
}

impl CaptureIndexable for Read {
    type Elem = Read;
}

impl CaptureIndexable for Partial {
    type Elem = Write;
}

/// The data required to obtain pointers to local variables, and clear up on scope-exit.
pub struct Environment<'ctx, 'b> {
    pub captures: &'b Captures,
    pub captures_ptr: PtrValue<'ctx, Read, tag::Captures>,
    pub arg_ptr: Option<PtrValue<'ctx, Read, tag::Unknown>>,
    pub dtors: &'b mut Destructors<'ctx>,
}

impl<'ctx> Environment<'ctx, '_> {
    /// Forwards to [`Destructors::add_dtor`].
    pub fn add_dtor(
        &mut self,
        universe_ptr: PtrValue<'ctx, Read, tag::Universe>,
        arg_ptr: PtrValue<'ctx, Write, tag::Unknown>,
    ) {
        self.dtors.add_dtor(universe_ptr, arg_ptr);
    }
}

impl<'ctx> CompilerWithBuilder<'ctx, '_> {
    /// Obtains a pointer to a given variable.
    ///
    /// Requires `var` is a valid variable in environment `env`.
    pub fn build_var_ptr(
        &mut self,
        env: &mut Environment<'ctx, '_>,
        var: VVariable,
    ) -> PtrValue<'ctx, Read, tag::Unknown> {
        use VVariable::*;
        match var {
            Global(name) => self.compiler.get_global_ptr(name).unwrap(),
            Local(level) => self.build_level_ptr(env, level),
        }
    }

    /// Obtains a pointer to the local variable with a given level.
    ///
    /// Requires `level` is a valid level in environment `env`.
    pub fn build_level_ptr(
        &mut self,
        env: &mut Environment<'ctx, '_>,
        level: Level,
    ) -> PtrValue<'ctx, Read, tag::Unknown> {
        match env.captures.var_location(level) {
            VarLocation::Arg => env.arg_ptr.unwrap(),
            VarLocation::Capture(n) => self.build_get_nth_capture_ptr(env.captures_ptr, n),
        }
    }

    /// Obtains a pointer to the nth capture offset in a given pointer to captures, or a pointer to
    /// storage for such from a pointer to storage for captures.
    ///
    /// Requires `n` is a valid index of a capture.
    pub(crate) fn build_get_nth_capture_offset_ptr<F: CaptureOffsetIndexable>(
        &mut self,
        captures_ptr: PtrValue<'ctx, F, tag::Captures>,
        n: u64,
    ) -> PtrValue<'ctx, F::Offset, tag::Usize> {
        let offset_ptr = unsafe {
            self.builder.build_gep(
                self.compiler.usize_type(),
                captures_ptr.0,
                &[self.compiler.usize_type().const_int(n, false)],
                "",
            )
        };
        PtrValue(offset_ptr, F::Offset::TAG, tag::Usize)
    }

    /// Obtains a pointer to the nth capture in a given pointer to captures, or a pointer to storage
    /// for such from a pointer to partially-initialized storage for captures.
    ///
    /// Requires `n` is a valid index of a capture, and the `n`th capture offset has been
    /// initialized, even if the rest of the captures have not.
    pub(crate) fn build_get_nth_capture_ptr<F: CaptureIndexable>(
        &mut self,
        captures_ptr: PtrValue<'ctx, F, tag::Captures>,
        n: u64,
    ) -> PtrValue<'ctx, F::Elem, tag::Unknown> {
        let offset_ptr = self.build_get_nth_capture_offset_ptr(captures_ptr, n);
        let offset = LoadStore::build_load(self, offset_ptr);
        let capture_ptr = unsafe {
            self.builder
                .build_gep(self.compiler.byte_type(), captures_ptr.0, &[offset], "")
        };
        PtrValue(capture_ptr, F::Elem::TAG, tag::Unknown)
    }

    /// Copies the capture offsets of a given pointer to captures into appropriate storage.
    ///
    /// Requires `src_ptr` points to captures described by `captures`, and `dest_ptr` points to
    /// storage that can hold the value pointed to by `src_ptr`.
    pub fn build_copy_capture_offsets(
        &mut self,
        captures: &Captures,
        src_ptr: PtrValue<'ctx, Read, tag::Captures>,
        dest_ptr: PtrValue<'ctx, Write, tag::Captures>,
    ) -> PtrValue<'ctx, Partial, tag::Captures> {
        let number = captures.number();
        let number = self.compiler.usize_type().const_int(number as u64, false);
        let num_bytes = self
            .compiler
            .size_of_int_type(self.compiler.usize_type())
            .const_nuw_mul(number);
        let src_ptr = src_ptr.as_byte_ptr();
        let dest_ptr = dest_ptr.as_byte_ptr();
        self.build_call_memcpy(MemcpyArgs {
            dest_ptr,
            src_ptr,
            num_bytes,
        });
        PtrValue(dest_ptr.0, Partial, tag::Captures)
    }
}

#[cfg(test)]
#[doc(hidden)]
mod tests;
