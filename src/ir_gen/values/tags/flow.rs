//! Trait for read-write permissions, and implementors.

use crate::ir_gen::values::tags::Tag;

/// Trait for read-write permissions.
pub trait Flow: Tag {}

/// Read-only.
#[derive(Copy, Clone)]
pub struct Read;

impl Tag for Read {
    const TAG: Self = Read;
}

impl Flow for Read {}

/// Write-only.
#[derive(Copy, Clone)]
pub struct Write;

impl Tag for Write {
    const TAG: Self = Write;
}

impl Flow for Write {}

/// Partially-initialized.
///
/// For use with captures that have been partially initialized.
#[derive(Copy, Clone)]
pub struct Partial;

impl Tag for Partial {
    const TAG: Self = Partial;
}

impl Flow for Partial {}
