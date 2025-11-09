/// Extension system for custom Markdown syntax and rendering.
use crate::adapters::DefaultParsingContext;
use crate::ast::{Block, Inline};
use crate::codegen::CustomRenderer;
use crate::error::{MarkdownError, Result};
use crate::parser::Parser;
use std::collections::HashMap;
use std::fmt::Debug;

type ExtensionParser<'a> = Parser<DefaultParsingContext<'a>>;

/// Trait for custom syntax extensions that can parse new Markdown elements.
pub trait SyntaxExtension: Debug + Send + Sync {
    /// Returns the unique name of this extension
    fn name(&self) -> &str;

    /// Returns the priority of this extension (higher numbers run first)
    fn priority(&self) -> i32 {
        0
    }

    /// Attempts to parse a custom block element at the current parser position.
    fn parse_block(&self, parser: &mut ExtensionParser<'_>) -> Option<Result<Block>> {
        let _ = parser; // Suppress unused parameter warning
        None
    }

    /// Attempts to parse a custom inline element at the current parser position.
    fn parse_inline(&self, parser: &mut ExtensionParser<'_>) -> Option<Result<Inline>> {
        let _ = parser; // Suppress unused parameter warning
        None
    }

    /// Returns whether this extension handles block-level elements
    fn handles_blocks(&self) -> bool {
        false
    }

    /// Returns whether this extension handles inline elements
    fn handles_inlines(&self) -> bool {
        false
    }
}

/// Registry for managing syntax extensions.
#[derive(Debug, Default)]
pub struct ExtensionRegistry {
    /// Block-level extensions, sorted by priority
    block_extensions: Vec<Box<dyn SyntaxExtension>>,
    /// Inline extensions, sorted by priority
    inline_extensions: Vec<Box<dyn SyntaxExtension>>,
    /// Custom renderers for extension-generated elements
    custom_renderers: HashMap<String, Box<dyn CustomRenderer>>,
}

impl ExtensionRegistry {
    /// Creates a new empty extension registry
    pub fn new() -> Self {
        Self::default()
    }

    /// Registers a new syntax extension
    pub fn register_extension(&mut self, extension: Box<dyn SyntaxExtension>) -> Result<()> {
        let name = extension.name().to_string();

        // Check for duplicate names
        if self.has_extension(&name) {
            return Err(MarkdownError::Extension {
                message: format!("Extension '{}' is already registered", name),
            });
        }

        if extension.handles_blocks() {
            self.block_extensions.push(extension);
            // Sort by priority (higher priority first)
            self.block_extensions
                .sort_by_key(|b| std::cmp::Reverse(b.priority()));
        } else if extension.handles_inlines() {
            self.inline_extensions.push(extension);
            // Sort by priority (higher priority first)
            self.inline_extensions
                .sort_by_key(|b| std::cmp::Reverse(b.priority()));
        } else {
            return Err(MarkdownError::Extension {
                message: format!("Extension '{}' must handle either blocks or inlines", name),
            });
        }

        Ok(())
    }

    /// Registers a custom renderer for extension-generated elements
    pub fn register_renderer(&mut self, element_type: &str, renderer: Box<dyn CustomRenderer>) {
        self.custom_renderers
            .insert(element_type.to_string(), renderer);
    }

    /// Checks if an extension with the given name is registered
    pub fn has_extension(&self, name: &str) -> bool {
        self.block_extensions.iter().any(|ext| ext.name() == name)
            || self.inline_extensions.iter().any(|ext| ext.name() == name)
    }

    /// Returns the number of registered extensions
    pub fn extension_count(&self) -> usize {
        self.block_extensions.len() + self.inline_extensions.len()
    }

    /// Returns the names of all registered extensions
    pub fn extension_names(&self) -> Vec<String> {
        let mut names = Vec::new();
        names.extend(
            self.block_extensions
                .iter()
                .map(|ext| ext.name().to_string()),
        );
        names.extend(
            self.inline_extensions
                .iter()
                .map(|ext| ext.name().to_string()),
        );
        names
    }

    /// Attempts to parse a block using registered extensions
    pub fn try_parse_block(&self, parser: &mut ExtensionParser<'_>) -> Option<Result<Block>> {
        // Try extensions in priority order
        for extension in &self.block_extensions {
            if let Some(result) = extension.parse_block(parser) {
                return Some(result);
            }
        }
        None
    }

    /// Attempts to parse an inline element using registered extensions
    pub fn try_parse_inline(&self, parser: &mut ExtensionParser<'_>) -> Option<Result<Inline>> {
        // Try extensions in priority order
        for extension in &self.inline_extensions {
            if let Some(result) = extension.parse_inline(parser) {
                return Some(result);
            }
        }
        None
    }

    /// Returns custom renderers for extension elements
    pub fn get_custom_renderers(&self) -> &HashMap<String, Box<dyn CustomRenderer>> {
        &self.custom_renderers
    }

    /// Clears all registered extensions and renderers
    pub fn clear(&mut self) {
        self.block_extensions.clear();
        self.inline_extensions.clear();
        self.custom_renderers.clear();
    }
}

/// High-level extension manager that integrates with the parser and renderer.
#[derive(Debug)]
pub struct ExtensionManager {
    /// Extension registry
    registry: ExtensionRegistry,
    /// Whether extensions are enabled
    enabled: bool,
}

impl ExtensionManager {
    /// Creates a new extension manager
    pub fn new() -> Self {
        Self {
            registry: ExtensionRegistry::new(),
            enabled: true,
        }
    }

    /// Creates a new extension manager with a pre-configured registry
    pub fn with_registry(registry: ExtensionRegistry) -> Self {
        Self {
            registry,
            enabled: true,
        }
    }

    /// Enables or disables extension processing
    pub fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
    }

    /// Returns whether extensions are enabled
    pub fn is_enabled(&self) -> bool {
        self.enabled
    }

    /// Registers a new syntax extension
    pub fn register_extension(&mut self, extension: Box<dyn SyntaxExtension>) -> Result<()> {
        self.registry.register_extension(extension)
    }

    /// Registers a custom renderer
    pub fn register_renderer(&mut self, element_type: &str, renderer: Box<dyn CustomRenderer>) {
        self.registry.register_renderer(element_type, renderer);
    }

    /// Returns a reference to the extension registry
    pub fn registry(&self) -> &ExtensionRegistry {
        &self.registry
    }

    /// Returns a mutable reference to the extension registry
    pub fn registry_mut(&mut self) -> &mut ExtensionRegistry {
        &mut self.registry
    }

    /// Attempts to parse a block using registered extensions (if enabled)
    pub fn try_parse_block(&self, parser: &mut ExtensionParser<'_>) -> Option<Result<Block>> {
        if self.enabled {
            self.registry.try_parse_block(parser)
        } else {
            None
        }
    }

    /// Attempts to parse an inline element using registered extensions (if enabled)
    pub fn try_parse_inline(&self, parser: &mut ExtensionParser<'_>) -> Option<Result<Inline>> {
        if self.enabled {
            self.registry.try_parse_inline(parser)
        } else {
            None
        }
    }
}

impl Default for ExtensionManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Mock extension for testing
    #[derive(Debug)]
    struct MockBlockExtension {
        name: String,
        priority: i32,
    }

    impl MockBlockExtension {
        fn new(name: &str, priority: i32) -> Self {
            Self {
                name: name.to_string(),
                priority,
            }
        }
    }

    impl SyntaxExtension for MockBlockExtension {
        fn name(&self) -> &str {
            &self.name
        }

        fn priority(&self) -> i32 {
            self.priority
        }

        fn handles_blocks(&self) -> bool {
            true
        }
    }

    #[test]
    fn test_extension_registry_creation() {
        let registry = ExtensionRegistry::new();
        assert_eq!(registry.extension_count(), 0);
        assert!(registry.extension_names().is_empty());
    }

    #[test]
    fn test_extension_registration() {
        let mut registry = ExtensionRegistry::new();

        let extension = Box::new(MockBlockExtension::new("test", 0));
        let result = registry.register_extension(extension);
        assert!(result.is_ok());

        assert_eq!(registry.extension_count(), 1);
        assert!(registry.has_extension("test"));
        assert_eq!(registry.extension_names(), vec!["test"]);
    }

    #[test]
    fn test_extension_manager_creation() {
        let manager = ExtensionManager::new();
        assert!(manager.is_enabled());
        assert_eq!(manager.registry().extension_count(), 0);
    }
}
