//! Core component infrastructure traits.
//!
//! These traits encode the component-based conventions used throughout the
//! codebase. Every component is expressed in terms of a named component marker,
//! a consumer trait, and a provider trait. The blanket
//! implementations glue a context to its component registry.

/// Marker trait for contexts that expose component registries.
pub trait HasProvider {
    /// Registry type that provides the concrete implementations for the
    /// components required by this context.
    type Components: ?Sized;
}

/// Marker trait connecting a component marker, a context, and any type-level
/// generic arguments.
pub trait IsProviderFor<Component, Context, Generics = ()> {}

/// Marks that a component delegates its implementation to another type. This
/// allows indirection in component registries while preserving a uniform pattern.
pub trait DelegateComponent<Component> {
    type Delegate;
}
