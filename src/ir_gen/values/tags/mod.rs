//! Tag types and traits for customising behaviour of wrappers around LLVM IR values.

pub mod args;
pub mod attributes;
pub mod builtin_type;
pub mod flow;
pub mod function;
pub mod param_flow;
pub mod ret;
pub mod structure;

/// Trait for tag types.
pub trait Tag: Copy {
    /// Tag types must be unit structs, and `TAG` must be the unit value.
    const TAG: Self;
}

/// Trait for tags which name types that can exist in memory.
pub trait Object: Tag {}

/// Tag types.
pub mod tag {
    use crate::ir_gen::values::tags::{Object, Tag};

    /// Tag for a dynamically-typed object.
    #[derive(Copy, Clone)]
    pub struct Unknown;

    impl Tag for Unknown {
        const TAG: Self = Unknown;
    }

    impl Object for Unknown {}

    /// Tag for a `usize`.
    #[derive(Copy, Clone)]
    pub struct Usize;

    impl Tag for Usize {
        const TAG: Self = Usize;
    }

    impl Object for Usize {}

    /// Tag for a value of `Type`.
    #[derive(Copy, Clone)]
    pub struct Universe;

    impl Tag for Universe {
        const TAG: Self = Universe;
    }

    impl Object for Universe {}

    /// Tag for a value of a pi type.
    #[derive(Copy, Clone)]
    pub struct Pi;

    impl Tag for Pi {
        const TAG: Self = Pi;
    }

    impl Object for Pi {}

    /// Tag for captures.
    #[derive(Copy, Clone)]
    pub struct Captures;

    impl Tag for Captures {
        const TAG: Self = Captures;
    }

    impl Object for Captures {}

    /// Tag for a size function.
    #[derive(Copy, Clone)]
    pub struct SizeFn;

    impl Tag for SizeFn {
        const TAG: Self = SizeFn;
    }

    /// Tag for a clone function.
    #[derive(Copy, Clone)]
    pub struct CloneFn;

    impl Tag for CloneFn {
        const TAG: Self = CloneFn;
    }

    /// Tag for a destroy function.
    #[derive(Copy, Clone)]
    pub struct DestroyFn;

    impl Tag for DestroyFn {
        const TAG: Self = DestroyFn;
    }

    /// Tag for a total size member.
    #[derive(Copy, Clone)]
    pub struct TotalSize;

    impl Tag for TotalSize {
        const TAG: Self = TotalSize;
    }

    /// Tag for a captures clone function.
    #[derive(Copy, Clone)]
    pub struct CapturesCloneFn;

    impl Tag for CapturesCloneFn {
        const TAG: Self = CapturesCloneFn;
    }

    /// Tag for a captures destroy function.
    #[derive(Copy, Clone)]
    pub struct CapturesDestroyFn;

    impl Tag for CapturesDestroyFn {
        const TAG: Self = CapturesDestroyFn;
    }

    /// Tag for a raw function.
    #[derive(Copy, Clone)]
    pub struct RawFn;

    impl Tag for RawFn {
        const TAG: Self = RawFn;
    }

    /// Tag for a return size function.
    #[derive(Copy, Clone)]
    pub struct RetSizeFn;

    impl Tag for RetSizeFn {
        const TAG: Self = RetSizeFn;
    }
}
