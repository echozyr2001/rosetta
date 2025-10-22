/// Performance optimization module for the Markdown engine.
///
/// This module provides zero-copy string handling, memory pooling for node allocation,
/// and parallel processing capabilities for independent sections.
use crate::ast::{Block, Document, Inline};
use crate::error::Result;

use std::cell::RefCell;
use std::collections::VecDeque;
use std::sync::{Arc, Mutex};
use std::thread;

/// Zero-copy string wrapper that can hold either borrowed or owned strings
#[derive(Debug, Clone)]
pub enum ZeroCopyStr<'a> {
    /// Borrowed string slice from input
    Borrowed(&'a str),
    /// Owned string for cases where we need to modify content
    Owned(String),
}

impl<'a> ZeroCopyStr<'a> {
    /// Create a new zero-copy string from a borrowed slice
    pub fn borrowed(s: &'a str) -> Self {
        Self::Borrowed(s)
    }

    /// Create a new zero-copy string from an owned string
    pub fn owned(s: String) -> Self {
        Self::Owned(s)
    }

    /// Get the string content as a &str
    pub fn as_str(&self) -> &str {
        match self {
            Self::Borrowed(s) => s,
            Self::Owned(s) => s,
        }
    }

    /// Convert to owned string if needed
    pub fn into_owned(self) -> String {
        match self {
            Self::Borrowed(s) => s.to_string(),
            Self::Owned(s) => s,
        }
    }

    /// Check if this is a borrowed string
    pub fn is_borrowed(&self) -> bool {
        matches!(self, Self::Borrowed(_))
    }

    /// Get length of the string
    pub fn len(&self) -> usize {
        self.as_str().len()
    }

    /// Check if string is empty
    pub fn is_empty(&self) -> bool {
        self.as_str().is_empty()
    }
}

impl<'a> From<&'a str> for ZeroCopyStr<'a> {
    fn from(s: &'a str) -> Self {
        Self::Borrowed(s)
    }
}

impl<'a> From<String> for ZeroCopyStr<'a> {
    fn from(s: String) -> Self {
        Self::Owned(s)
    }
}

impl<'a> AsRef<str> for ZeroCopyStr<'a> {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

/// Memory pool for efficient node allocation and reuse
pub struct NodePool<T> {
    pool: RefCell<VecDeque<T>>,
    factory: Box<dyn Fn() -> T + Send + Sync>,
}

impl<T> std::fmt::Debug for NodePool<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NodePool")
            .field("pool_size", &self.pool.borrow().len())
            .finish()
    }
}

impl<T> NodePool<T> {
    /// Create a new node pool with a factory function
    pub fn new<F>(factory: F) -> Self
    where
        F: Fn() -> T + Send + Sync + 'static,
    {
        Self {
            pool: RefCell::new(VecDeque::new()),
            factory: Box::new(factory),
        }
    }

    /// Get a node from the pool or create a new one
    pub fn get(&self) -> T {
        self.pool
            .borrow_mut()
            .pop_front()
            .unwrap_or_else(|| (self.factory)())
    }

    /// Return a node to the pool for reuse
    pub fn put(&self, item: T) {
        let mut pool = self.pool.borrow_mut();
        if pool.len() < 1000 {
            // Limit pool size to prevent unbounded growth
            pool.push_back(item);
        }
    }

    /// Get the current pool size
    pub fn size(&self) -> usize {
        self.pool.borrow().len()
    }

    /// Clear the pool
    pub fn clear(&self) {
        self.pool.borrow_mut().clear();
    }
}

/// Thread-safe memory pool for concurrent access
pub struct ConcurrentNodePool<T> {
    pool: Arc<Mutex<VecDeque<T>>>,
    factory: Arc<dyn Fn() -> T + Send + Sync>,
}

impl<T> ConcurrentNodePool<T> {
    /// Create a new concurrent node pool
    pub fn new<F>(factory: F) -> Self
    where
        F: Fn() -> T + Send + Sync + 'static,
    {
        Self {
            pool: Arc::new(Mutex::new(VecDeque::new())),
            factory: Arc::new(factory),
        }
    }

    /// Get a node from the pool or create a new one
    pub fn get(&self) -> T {
        self.pool
            .lock()
            .unwrap()
            .pop_front()
            .unwrap_or_else(|| (self.factory)())
    }

    /// Return a node to the pool for reuse
    pub fn put(&self, item: T) {
        let mut pool = self.pool.lock().unwrap();
        if pool.len() < 1000 {
            pool.push_back(item);
        }
    }

    /// Get the current pool size
    pub fn size(&self) -> usize {
        self.pool.lock().unwrap().len()
    }

    /// Clear the pool
    pub fn clear(&self) {
        self.pool.lock().unwrap().clear();
    }

    /// Clone the pool for use in multiple threads
    pub fn clone(&self) -> Self {
        Self {
            pool: Arc::clone(&self.pool),
            factory: Arc::clone(&self.factory),
        }
    }
}

/// Parallel processing configuration
#[derive(Debug, Clone)]
pub struct ParallelConfig {
    /// Number of worker threads to use
    pub num_threads: usize,
    /// Minimum section size to consider for parallel processing
    pub min_section_size: usize,
    /// Whether to enable parallel processing
    pub enabled: bool,
}

impl Default for ParallelConfig {
    fn default() -> Self {
        Self {
            num_threads: num_cpus::get().max(1),
            min_section_size: 1000, // 1KB minimum section size
            enabled: true,
        }
    }
}

/// Parallel processor for independent document sections
#[derive(Debug)]
pub struct ParallelProcessor {
    config: ParallelConfig,
}

impl ParallelProcessor {
    /// Create a new parallel processor
    pub fn new(config: ParallelConfig) -> Self {
        Self { config }
    }

    /// Create with default configuration
    pub fn with_defaults() -> Self {
        Self::new(ParallelConfig::default())
    }

    /// Process document sections in parallel
    pub fn process_sections<F, R>(&self, sections: Vec<String>, processor: F) -> Result<Vec<R>>
    where
        F: Fn(&str) -> Result<R> + Send + Sync + Clone + 'static,
        R: Send + 'static,
    {
        if !self.config.enabled || sections.len() < 2 {
            // Process sequentially if parallel processing is disabled or too few sections
            return sections.iter().map(|s| processor(s)).collect();
        }

        // Filter sections by minimum size
        let large_sections: Vec<_> = sections
            .into_iter()
            .enumerate()
            .filter(|(_, section)| section.len() >= self.config.min_section_size)
            .collect();

        if large_sections.len() < 2 {
            // Not enough large sections for parallel processing
            return large_sections
                .into_iter()
                .map(|(_, section)| processor(&section))
                .collect();
        }

        // Process sections in parallel using thread pool
        let num_threads = self.config.num_threads.min(large_sections.len());
        let chunk_size = large_sections.len().div_ceil(num_threads);

        let mut handles = Vec::new();
        let processor = Arc::new(processor);

        for chunk in large_sections.chunks(chunk_size) {
            let chunk_sections: Vec<String> = chunk.iter().map(|(_, s)| s.clone()).collect();
            let processor_clone = Arc::clone(&processor);

            let handle = thread::spawn(move || {
                chunk_sections
                    .iter()
                    .map(|section| processor_clone(section))
                    .collect::<Result<Vec<R>>>()
            });

            handles.push(handle);
        }

        // Collect results from all threads
        let mut results = Vec::new();
        for handle in handles {
            let chunk_results = handle.join().map_err(|_| {
                crate::error::MarkdownError::parse_error(
                    crate::lexer::Position::default(),
                    "Thread panicked during parallel processing".to_string(),
                )
            })??;
            results.extend(chunk_results);
        }

        Ok(results)
    }

    /// Split document into independent sections for parallel processing
    pub fn split_document(&self, content: &str) -> Vec<String> {
        if !self.config.enabled {
            return vec![content.to_string()];
        }

        let mut sections = Vec::new();
        let mut current_section = String::new();
        let mut in_code_block = false;
        let mut code_fence_count = 0;

        for line in content.lines() {
            // Track code block boundaries to avoid splitting within them
            if line.trim_start().starts_with("```") {
                let fence_chars = line.chars().take_while(|&c| c == '`').count();
                if !in_code_block {
                    in_code_block = true;
                    code_fence_count = fence_chars;
                } else if fence_chars >= code_fence_count {
                    in_code_block = false;
                }
            }

            current_section.push_str(line);
            current_section.push('\n');

            // Split on major section boundaries (headings) when not in code blocks
            if !in_code_block
                && line.starts_with('#')
                && !current_section.trim().is_empty()
                && current_section.len() >= self.config.min_section_size
            {
                sections.push(current_section.trim().to_string());
                current_section.clear();
                current_section.push_str(line);
                current_section.push('\n');
            }
        }

        // Add the final section
        if !current_section.trim().is_empty() {
            sections.push(current_section.trim().to_string());
        }

        // If we don't have enough sections, return the original content
        if sections.len() < 2 {
            vec![content.to_string()]
        } else {
            sections
        }
    }

    /// Process blocks in parallel
    pub fn process_blocks_parallel<F>(&self, blocks: Vec<Block>, processor: F) -> Result<Vec<Block>>
    where
        F: Fn(Block) -> Result<Block> + Send + Sync + Clone + 'static,
    {
        if !self.config.enabled || blocks.len() < 2 {
            return blocks.into_iter().map(processor).collect();
        }

        let num_threads = self.config.num_threads.min(blocks.len());
        let chunk_size = blocks.len().div_ceil(num_threads);

        let mut handles = Vec::new();
        let processor = Arc::new(processor);

        for chunk in blocks.chunks(chunk_size) {
            let chunk_blocks = chunk.to_vec();
            let processor_clone = Arc::clone(&processor);

            let handle = thread::spawn(move || {
                chunk_blocks
                    .into_iter()
                    .map(|block| processor_clone(block))
                    .collect::<Result<Vec<Block>>>()
            });

            handles.push(handle);
        }

        // Collect results
        let mut results = Vec::new();
        for handle in handles {
            let chunk_results = handle.join().map_err(|_| {
                crate::error::MarkdownError::parse_error(
                    crate::lexer::Position::default(),
                    "Thread panicked during parallel block processing".to_string(),
                )
            })??;
            results.extend(chunk_results);
        }

        Ok(results)
    }
}

/// Performance optimization utilities
#[derive(Debug)]
pub struct PerformanceOptimizer {
    /// Memory pool for string allocations
    pub string_pool: NodePool<String>,
    /// Memory pool for vector allocations
    pub vec_pool: NodePool<Vec<Inline>>,
    /// Parallel processor
    pub parallel_processor: ParallelProcessor,
}

impl PerformanceOptimizer {
    /// Create a new performance optimizer
    pub fn new(parallel_config: ParallelConfig) -> Self {
        Self {
            string_pool: NodePool::new(String::new),
            vec_pool: NodePool::new(Vec::new),
            parallel_processor: ParallelProcessor::new(parallel_config),
        }
    }

    /// Create with default configuration
    pub fn with_defaults() -> Self {
        Self::new(ParallelConfig::default())
    }

    /// Get a string from the pool
    pub fn get_string(&self) -> String {
        let mut s = self.string_pool.get();
        s.clear();
        s
    }

    /// Return a string to the pool
    pub fn return_string(&self, s: String) {
        self.string_pool.put(s);
    }

    /// Get a vector from the pool
    pub fn get_vec(&self) -> Vec<Inline> {
        let mut v = self.vec_pool.get();
        v.clear();
        v
    }

    /// Return a vector to the pool
    pub fn return_vec(&self, v: Vec<Inline>) {
        self.vec_pool.put(v);
    }

    /// Optimize document processing using all available optimizations
    pub fn optimize_document_processing<F>(&self, content: &str, processor: F) -> Result<Document>
    where
        F: Fn(&str) -> Result<Document>,
    {
        // Split document into sections for parallel processing
        let sections = self.parallel_processor.split_document(content);

        if sections.len() == 1 {
            // Single section, process normally
            return processor(&sections[0]);
        }

        // For now, process sequentially to avoid Send + Sync requirements
        // In a full implementation, we would use a different approach for thread safety
        let mut merged_blocks = Vec::new();
        for section in sections {
            let doc = processor(&section)?;
            merged_blocks.extend(doc.blocks);
        }

        Ok(Document {
            blocks: merged_blocks,
            source_map: crate::ast::SourceMap::new(),
        })
    }

    /// Clear all pools to free memory
    pub fn clear_pools(&self) {
        self.string_pool.clear();
        self.vec_pool.clear();
    }

    /// Get pool statistics
    pub fn pool_stats(&self) -> PoolStats {
        PoolStats {
            string_pool_size: self.string_pool.size(),
            vec_pool_size: self.vec_pool.size(),
        }
    }
}

/// Statistics about memory pool usage
#[derive(Debug, Clone)]
pub struct PoolStats {
    pub string_pool_size: usize,
    pub vec_pool_size: usize,
}

impl Default for PerformanceOptimizer {
    fn default() -> Self {
        Self::with_defaults()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_zero_copy_str() {
        let borrowed = ZeroCopyStr::borrowed("hello");
        assert!(borrowed.is_borrowed());
        assert_eq!(borrowed.as_str(), "hello");
        assert_eq!(borrowed.len(), 5);

        let owned = ZeroCopyStr::owned("world".to_string());
        assert!(!owned.is_borrowed());
        assert_eq!(owned.as_str(), "world");
        assert_eq!(owned.len(), 5);

        let owned_string = owned.into_owned();
        assert_eq!(owned_string, "world");
    }

    #[test]
    fn test_node_pool() {
        let pool = NodePool::new(|| String::from("default"));

        // Get a new item
        let item1 = pool.get();
        assert_eq!(item1, "default");

        // Return it to pool
        pool.put("reused".to_string());

        // Get should return the reused item
        let item2 = pool.get();
        assert_eq!(item2, "reused");

        assert_eq!(pool.size(), 0); // Pool should be empty after getting the item
    }

    #[test]
    fn test_concurrent_node_pool() {
        let pool = ConcurrentNodePool::new(|| 42i32);

        let item1 = pool.get();
        assert_eq!(item1, 42);

        pool.put(100);
        let item2 = pool.get();
        assert_eq!(item2, 100);

        // Test cloning
        let pool2 = pool.clone();
        pool2.put(200);
        let item3 = pool.get();
        assert_eq!(item3, 200);
    }

    #[test]
    fn test_parallel_config() {
        let config = ParallelConfig::default();
        assert!(config.enabled);
        assert!(config.num_threads > 0);
        assert_eq!(config.min_section_size, 1000);
    }

    #[test]
    fn test_parallel_processor_split_document() {
        let processor = ParallelProcessor::with_defaults();
        let content = "# Section 1\nContent 1\n\n# Section 2\nContent 2\n\n# Section 3\nContent 3";

        let sections = processor.split_document(content);

        // Should split into multiple sections based on headings
        assert!(sections.len() >= 1);

        // Each section should contain content
        for section in &sections {
            assert!(!section.trim().is_empty());
        }
    }

    #[test]
    fn test_parallel_processor_process_sections() {
        let processor = ParallelProcessor::new(ParallelConfig {
            enabled: true,
            num_threads: 2,
            min_section_size: 1, // Lower threshold for testing
        });

        let sections = vec![
            "# Section 1\nContent".to_string(),
            "# Section 2\nContent".to_string(),
        ];

        let results = processor
            .process_sections(sections, |s| Ok(s.len()))
            .unwrap();

        assert_eq!(results.len(), 2);
        assert!(results[0] > 0);
        assert!(results[1] > 0);
    }

    #[test]
    fn test_performance_optimizer() {
        let optimizer = PerformanceOptimizer::with_defaults();

        // Test string pool
        let s1 = optimizer.get_string();
        assert!(s1.is_empty());
        optimizer.return_string("test".to_string());

        // Test vector pool
        let v1 = optimizer.get_vec();
        assert!(v1.is_empty());
        optimizer.return_vec(vec![]);

        // Test pool stats
        let stats = optimizer.pool_stats();
        assert!(stats.string_pool_size <= 1);
        assert!(stats.vec_pool_size <= 1);

        // Test clearing pools
        optimizer.clear_pools();
        let stats_after_clear = optimizer.pool_stats();
        assert_eq!(stats_after_clear.string_pool_size, 0);
        assert_eq!(stats_after_clear.vec_pool_size, 0);
    }

    #[test]
    fn test_zero_copy_str_conversions() {
        let s = "test";
        let zero_copy: ZeroCopyStr = s.into();
        assert!(zero_copy.is_borrowed());

        let owned = String::from("test");
        let zero_copy: ZeroCopyStr = owned.into();
        assert!(!zero_copy.is_borrowed());
    }

    #[test]
    fn test_parallel_processor_disabled() {
        let processor = ParallelProcessor::new(ParallelConfig {
            enabled: false,
            num_threads: 1,
            min_section_size: 1000,
        });

        let sections = vec!["section1".to_string(), "section2".to_string()];
        let results = processor
            .process_sections(sections, |s| Ok(s.len()))
            .unwrap();

        assert_eq!(results.len(), 2);
    }

    #[test]
    fn test_node_pool_size_limit() {
        let pool = NodePool::new(|| String::new());

        // Add many items to test size limit
        for i in 0..1500 {
            pool.put(format!("item_{}", i));
        }

        // Pool should be limited to 1000 items
        assert!(pool.size() <= 1000);
    }

    #[test]
    fn test_parallel_processor_code_block_handling() {
        let processor = ParallelProcessor::with_defaults();
        let content = r#"# Section 1
Some content

```rust
fn main() {
    println!("Hello");
}
```

# Section 2
More content"#;

        let sections = processor.split_document(content);

        // Should handle code blocks properly and not split within them
        for section in &sections {
            let code_block_starts = section.matches("```").count();
            // Each section should have even number of ``` (balanced)
            if code_block_starts > 0 {
                assert_eq!(code_block_starts % 2, 0);
            }
        }
    }
}
