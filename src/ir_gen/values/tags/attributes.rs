//! Trait for dealing with LLVM IR enum attributes, and implementors.

use crate::ir_gen::Compiler;
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::values::FunctionValue;

/// A flag type for specifying what memory operations a function is allowed to perform.
pub type MemoryVal = u64;

/// Constants of type [`MemoryVal`].
pub mod memory_val {
    use crate::ir_gen::values::tags::attributes::MemoryVal;

    /// May read from memory pointed to be pointer arguments.
    pub const ARGMEM_READ: MemoryVal = 0b000001;

    /// May write to memory pointed to be pointer arguments.
    pub const ARGMEM_WRITE: MemoryVal = 0b000010;

    /// May read from memory inaccessible to the caller.
    ///
    /// For example, memory allocated by the function.
    pub const INACCESSIBLEMEM_READ: MemoryVal = 0b000100;

    /// May write to memory inaccessible to the caller.
    ///
    /// For example, memory allocated by the function.
    pub const INACCESSIBLEMEM_WRITE: MemoryVal = 0b001000;

    /// May read from memory which is accessible to the caller but which is not pointed to by a
    /// pointer argument.
    ///
    /// For example, globals or memory pointed to by pointers loaded from arguments.
    pub const OTHER_READ: MemoryVal = 0b010000;

    /// May write to memory which is accessible to the caller but which is not pointed to by a
    /// pointer argument.
    ///
    /// For example, globals or memory pointed to by pointers loaded from arguments.
    pub const OTHER_WRITE: MemoryVal = 0b100000;

    /// May not read or write any memory.
    ///
    /// The strongest memory contract.
    pub const NONE: MemoryVal = 0b000000;

    /// May read or write memory pointed to be pointer arguments.
    pub const ARGMEM_READ_WRITE: MemoryVal = ARGMEM_READ | ARGMEM_WRITE;

    /// May read or write memory inaccessible to the caller.
    pub const INACCESSIBLEMEM_READ_WRITE: MemoryVal = INACCESSIBLEMEM_READ | INACCESSIBLEMEM_WRITE;

    /// May read or write memory which is accessible to the caller but which is not pointed to by a
    /// pointer argument.
    pub const OTHER_READ_WRITE: MemoryVal = OTHER_READ | OTHER_WRITE;

    /// May read or write memory which is not pointed to by a pointer argument.
    pub const NON_ARGMEM_READ_WRITE: MemoryVal = INACCESSIBLEMEM_READ_WRITE | OTHER_READ_WRITE;

    /// May read or write any memory.
    ///
    /// The weakest memory contract.
    pub const READ_WRITE: MemoryVal = ARGMEM_READ_WRITE | NON_ARGMEM_READ_WRITE;
}

/// Trait for representing LLVM IR enum attributes.
pub trait EnumAttribute {
    /// Gets the name of the attribute.
    ///
    /// This function must immediately return a constant string.
    fn get_name(&self) -> &'static str;

    /// Gets the value of the attribute.
    ///
    /// For most attributes there is no value; so `0` is returned by default.
    fn get_val(&self) -> u64 {
        0
    }
}

/// Forwards members to referee.
impl<EA: EnumAttribute + ?Sized> EnumAttribute for &'_ EA {
    fn get_name(&self) -> &'static str {
        EA::get_name(self)
    }
    fn get_val(&self) -> u64 {
        EA::get_val(self)
    }
}

/// Implementors of [`EnumAttribute`].
pub mod enum_attributes {
    use crate::ir_gen::values::tags::attributes::{EnumAttribute, MemoryVal};

    /// The `memory` attribute.
    #[derive(Copy, Clone)]
    pub struct Memory(pub MemoryVal);

    impl EnumAttribute for Memory {
        fn get_name(&self) -> &'static str {
            "memory"
        }
        fn get_val(&self) -> u64 {
            let memory_val = self.0;
            assert!(memory_val < 64);
            memory_val
        }
    }

    /// The `alwaysinline` attribute.
    #[derive(Copy, Clone)]
    pub struct AlwaysInline;

    impl EnumAttribute for AlwaysInline {
        fn get_name(&self) -> &'static str {
            "alwaysinline"
        }
    }

    /// The `nofree` attribute.
    #[derive(Copy, Clone)]
    pub struct NoFree;

    impl EnumAttribute for NoFree {
        fn get_name(&self) -> &'static str {
            "nofree"
        }
    }

    /// The `norecurse` attribute.
    #[derive(Copy, Clone)]
    pub struct NoRecurse;

    impl EnumAttribute for NoRecurse {
        fn get_name(&self) -> &'static str {
            "norecurse"
        }
    }

    /// The `willreturn` attribute.
    #[derive(Copy, Clone)]
    pub struct WillReturn;

    impl EnumAttribute for WillReturn {
        fn get_name(&self) -> &'static str {
            "willreturn"
        }
    }

    /// The `nosync` attribute.
    #[derive(Copy, Clone)]
    pub struct NoSync;

    impl EnumAttribute for NoSync {
        fn get_name(&self) -> &'static str {
            "nosync"
        }
    }

    /// The `nounwind` attribute.
    #[derive(Copy, Clone)]
    pub struct NoUnwind;

    impl EnumAttribute for NoUnwind {
        fn get_name(&self) -> &'static str {
            "nounwind"
        }
    }

    /// The `mustprogress` attribute.
    #[derive(Copy, Clone)]
    pub struct MustProgress;

    impl EnumAttribute for MustProgress {
        fn get_name(&self) -> &'static str {
            "mustprogress"
        }
    }

    /// The `noalias` attribute.
    #[derive(Copy, Clone)]
    pub struct NoAlias;

    impl EnumAttribute for NoAlias {
        fn get_name(&self) -> &'static str {
            "noalias"
        }
    }

    /// The `nocapture` attribute.
    #[derive(Copy, Clone)]
    pub struct NoCapture;

    impl EnumAttribute for NoCapture {
        fn get_name(&self) -> &'static str {
            "nocapture"
        }
    }

    /// The `nonnull` attribute.
    #[derive(Copy, Clone)]
    pub struct NonNull;

    impl EnumAttribute for NonNull {
        fn get_name(&self) -> &'static str {
            "nonnull"
        }
    }

    /// The `noundef` attribute.
    #[derive(Copy, Clone)]
    pub struct NoUndef;

    impl EnumAttribute for NoUndef {
        fn get_name(&self) -> &'static str {
            "noundef"
        }
    }

    /// The `readnone` attribute.
    #[derive(Copy, Clone)]
    pub struct ReadNone;

    impl EnumAttribute for ReadNone {
        fn get_name(&self) -> &'static str {
            "readnone"
        }
    }

    /// The `readonly` attribute.
    #[derive(Copy, Clone)]
    pub struct ReadOnly;

    impl EnumAttribute for ReadOnly {
        fn get_name(&self) -> &'static str {
            "readonly"
        }
    }

    /// The `writeonly` attribute.
    #[derive(Copy, Clone)]
    pub struct WriteOnly;

    impl EnumAttribute for WriteOnly {
        fn get_name(&self) -> &'static str {
            "writeonly"
        }
    }
}

impl<'ctx> Compiler<'ctx> {
    /// Gets the [`Attribute`] associated to an [`EnumAttribute`].
    pub fn enum_attribute<EA: EnumAttribute>(&self, ea: EA) -> Attribute {
        self.context.create_enum_attribute(
            Attribute::get_named_enum_kind_id(ea.get_name()),
            ea.get_val(),
        )
    }

    /// Adds a given [`EnumAttribute`] to a given [`AttributeLoc`] within a given [`FunctionValue`].
    pub fn add_enum_attribute<EA: EnumAttribute>(
        &self,
        function: FunctionValue<'ctx>,
        loc: AttributeLoc,
        ea: EA,
    ) {
        function.add_attribute(loc, self.enum_attribute(ea));
    }
}
