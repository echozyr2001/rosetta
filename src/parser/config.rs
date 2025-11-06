use crate::performance::ParallelConfig;

/// Configuration for the parser behavior and options
#[derive(Debug, Clone)]
pub struct ParserConfig {
    /// Whether to strictly follow CommonMark specification
    pub strict_commonmark: bool,
    /// Maximum nesting depth to prevent stack overflow
    pub max_nesting_depth: usize,
    /// Whether to enable GitHub Flavored Markdown extensions
    pub enable_gfm_extensions: bool,
    /// Whether to collect detailed source position information
    pub track_source_positions: bool,
    /// Maximum number of errors before stopping parsing
    pub max_errors: Option<usize>,
    /// Whether to enable performance optimizations
    pub performance_optimized: bool,
    /// Parallel processing configuration
    pub parallel_config: ParallelConfig,
}

impl Default for ParserConfig {
    fn default() -> Self {
        Self {
            strict_commonmark: true,
            max_nesting_depth: 64,
            enable_gfm_extensions: false,
            track_source_positions: true,
            max_errors: Some(100),
            performance_optimized: true,
            parallel_config: ParallelConfig::default(),
        }
    }
}
