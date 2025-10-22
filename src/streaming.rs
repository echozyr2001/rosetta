/// Streaming parser implementation for large documents.
/// This module provides streaming capabilities for parsing large Markdown documents
/// that may not fit entirely in memory. It implements incremental parsing with
/// backpressure handling and memory management.
use crate::ast::{Block, Document, SourceMap};
use crate::error::{MarkdownError, Result};
use crate::lexer::Position;
use crate::parser::{Parser, ParserConfig};
use std::collections::VecDeque;
use std::io::{BufReader, Read};

/// Configuration for streaming parser behavior
#[derive(Debug, Clone)]
pub struct StreamingConfig {
    /// Size of each chunk to process (in bytes)
    pub chunk_size: usize,
    /// Maximum number of chunks to buffer in memory
    pub max_buffer_chunks: usize,
    /// Whether to enable backpressure when buffer is full
    pub enable_backpressure: bool,
    /// Parser configuration for each chunk
    pub parser_config: ParserConfig,
    /// Whether to preserve block boundaries across chunks
    pub preserve_block_boundaries: bool,
    /// Overlap size between chunks to handle split elements
    pub chunk_overlap: usize,
}

impl Default for StreamingConfig {
    fn default() -> Self {
        Self {
            chunk_size: 64 * 1024, // 64KB chunks
            max_buffer_chunks: 10, // Buffer up to 10 chunks (640KB)
            enable_backpressure: true,
            parser_config: ParserConfig::default(),
            preserve_block_boundaries: true,
            chunk_overlap: 1024, // 1KB overlap to handle split elements
        }
    }
}

/// Result of processing a chunk in streaming mode
#[derive(Debug, Clone)]
pub struct ChunkResult {
    /// Parsed blocks from this chunk
    pub blocks: Vec<Block>,
    /// Position information for the chunk
    pub start_position: Position,
    pub end_position: Position,
    /// Whether this is the final chunk
    pub is_final: bool,
    /// Number of bytes processed in this chunk
    pub bytes_processed: usize,
    /// Any warnings or non-fatal errors encountered
    pub warnings: Vec<String>,
}

/// Streaming parser for processing large Markdown documents incrementally
pub struct StreamingParser<R: Read> {
    /// Input reader
    reader: BufReader<R>,
    /// Streaming configuration
    config: StreamingConfig,
    /// Buffer for incomplete blocks across chunk boundaries
    boundary_buffer: String,
    /// Current position in the input stream
    current_position: Position,
    /// Whether we've reached the end of input
    is_finished: bool,
    /// Chunk buffer for backpressure handling
    chunk_buffer: VecDeque<ChunkResult>,
    /// Total bytes processed so far
    total_bytes_processed: usize,
}

impl<R: Read> StreamingParser<R> {
    /// Creates a new streaming parser with the given reader and configuration
    pub fn new(reader: R, config: StreamingConfig) -> Self {
        Self {
            reader: BufReader::new(reader),
            config,
            boundary_buffer: String::new(),
            current_position: Position::default(),
            is_finished: false,
            chunk_buffer: VecDeque::new(),
            total_bytes_processed: 0,
        }
    }

    /// Creates a new streaming parser with default configuration
    pub fn with_defaults(reader: R) -> Self {
        Self::new(reader, StreamingConfig::default())
    }

    /// Processes the next chunk of input and returns parsed blocks
    pub fn next_chunk(&mut self) -> Result<Option<ChunkResult>> {
        // Check if we have buffered results due to backpressure
        if let Some(buffered_result) = self.chunk_buffer.pop_front() {
            return Ok(Some(buffered_result));
        }

        if self.is_finished {
            return Ok(None);
        }

        // Read the next chunk of data
        let chunk_data = self.read_next_chunk()?;
        if chunk_data.is_empty() {
            self.is_finished = true;

            // Process any remaining data in boundary buffer
            if !self.boundary_buffer.is_empty() {
                return self.process_final_chunk();
            }

            return Ok(None);
        }

        // Combine with boundary buffer and process
        let full_chunk = if self.boundary_buffer.is_empty() {
            chunk_data
        } else {
            let combined = format!("{}{}", self.boundary_buffer, chunk_data);
            self.boundary_buffer.clear();
            combined
        };

        self.process_chunk(full_chunk)
    }

    /// Processes all remaining chunks and returns a complete document
    pub fn parse_complete(&mut self) -> Result<Document> {
        let mut all_blocks = Vec::new();
        let source_map = SourceMap::new();

        while let Some(chunk_result) = self.next_chunk()? {
            all_blocks.extend(chunk_result.blocks);
            // In a full implementation, we would merge source maps here
        }

        Ok(Document {
            blocks: all_blocks,
            source_map,
        })
    }

    /// Returns the current parsing position
    pub fn position(&self) -> Position {
        self.current_position
    }

    /// Returns the total number of bytes processed so far
    pub fn bytes_processed(&self) -> usize {
        self.total_bytes_processed
    }

    /// Returns whether the parser has finished processing all input
    pub fn is_finished(&self) -> bool {
        self.is_finished && self.chunk_buffer.is_empty()
    }

    /// Returns the number of chunks currently buffered
    pub fn buffered_chunks(&self) -> usize {
        self.chunk_buffer.len()
    }

    /// Enables or disables backpressure handling
    pub fn set_backpressure(&mut self, enabled: bool) {
        self.config.enable_backpressure = enabled;
    }

    /// Reads the next chunk of data from the input stream
    fn read_next_chunk(&mut self) -> Result<String> {
        let mut buffer = vec![0u8; self.config.chunk_size];
        let bytes_read = self
            .reader
            .read(&mut buffer)
            .map_err(|e| MarkdownError::Io { source: e })?;

        if bytes_read == 0 {
            return Ok(String::new());
        }

        // Truncate buffer to actual bytes read
        buffer.truncate(bytes_read);

        // Convert to UTF-8 string, handling potential incomplete characters
        match String::from_utf8(buffer) {
            Ok(string) => Ok(string),
            Err(e) => {
                // Handle incomplete UTF-8 sequences at chunk boundaries
                let valid_up_to = e.utf8_error().valid_up_to();
                let valid_bytes = e.into_bytes();

                if valid_up_to == 0 {
                    return Err(MarkdownError::parse_error(
                        self.current_position,
                        "Invalid UTF-8 sequence in input".to_string(),
                    ));
                }

                // Save incomplete bytes for next chunk
                let (valid_part, incomplete_part) = valid_bytes.split_at(valid_up_to);
                let valid_string = String::from_utf8(valid_part.to_vec()).map_err(|_| {
                    MarkdownError::parse_error(
                        self.current_position,
                        "Failed to convert valid UTF-8 bytes".to_string(),
                    )
                })?;

                // Store incomplete bytes in boundary buffer for next chunk
                if !incomplete_part.is_empty() {
                    // In a full implementation, we would handle this properly
                    // For now, we'll just ignore incomplete sequences
                }

                Ok(valid_string)
            }
        }
    }

    /// Processes a chunk of text and returns parsed blocks
    fn process_chunk(&mut self, chunk_text: String) -> Result<Option<ChunkResult>> {
        let start_position = self.current_position;

        // If we need to preserve block boundaries, find a good split point
        let preserve_boundaries = self.config.preserve_block_boundaries;
        let (process_text, remaining_text) = if preserve_boundaries {
            self.split_at_block_boundary(&chunk_text)
        } else {
            (chunk_text, String::new())
        };

        // Store remaining text for next chunk
        if !remaining_text.is_empty() {
            self.boundary_buffer = remaining_text;
        }

        // Parse the chunk
        let parser_config = self.config.parser_config.clone();
        let mut parser = Parser::new(&process_text, parser_config);
        let chunk_document = parser.parse()?;

        // Update position tracking
        self.update_position(&process_text);
        let end_position = self.current_position;

        let chunk_result = ChunkResult {
            blocks: chunk_document.blocks,
            start_position,
            end_position,
            is_final: false,
            bytes_processed: process_text.len(),
            warnings: Vec::new(), // In a full implementation, collect parser warnings
        };

        self.total_bytes_processed += process_text.len();

        // Handle backpressure
        let enable_backpressure = self.config.enable_backpressure;
        let max_buffer_chunks = self.config.max_buffer_chunks;

        if enable_backpressure && self.chunk_buffer.len() >= max_buffer_chunks {
            self.chunk_buffer.push_back(chunk_result);
            // Return the oldest buffered result
            Ok(self.chunk_buffer.pop_front())
        } else {
            Ok(Some(chunk_result))
        }
    }

    /// Processes the final chunk when end of input is reached
    fn process_final_chunk(&mut self) -> Result<Option<ChunkResult>> {
        let start_position = self.current_position;

        // Parse any remaining content in boundary buffer
        let parser_config = self.config.parser_config.clone();
        let boundary_content = self.boundary_buffer.clone();
        let mut parser = Parser::new(&boundary_content, parser_config);
        let final_document = parser.parse()?;

        self.update_position(&boundary_content);
        let end_position = self.current_position;

        let bytes_processed = boundary_content.len();
        self.total_bytes_processed += bytes_processed;
        self.boundary_buffer.clear();

        let chunk_result = ChunkResult {
            blocks: final_document.blocks,
            start_position,
            end_position,
            is_final: true,
            bytes_processed,
            warnings: Vec::new(),
        };

        Ok(Some(chunk_result))
    }

    /// Splits text at a block boundary to avoid splitting elements across chunks
    fn split_at_block_boundary(&self, text: &str) -> (String, String) {
        let chunk_overlap = self.config.chunk_overlap;
        if text.len() <= chunk_overlap {
            return (text.to_string(), String::new());
        }

        // Find the last complete block boundary within the chunk
        let split_point = text.len() - chunk_overlap;

        // Look for block boundaries (double newlines, heading markers, etc.)
        let search_start = split_point.saturating_sub(1024); // Search within 1KB
        let search_text = &text[search_start..];

        // Find block boundary markers
        let boundary_patterns = [
            "\n\n", "\n# ", "\n## ", "\n### ", "\n> ", "\n- ", "\n* ", "\n1. ",
        ];

        let mut best_split = split_point;
        for pattern in &boundary_patterns {
            if let Some(pos) = search_text.rfind(pattern) {
                let absolute_pos = search_start + pos + pattern.len();
                if absolute_pos > best_split {
                    best_split = absolute_pos;
                }
            }
        }

        // Ensure we don't split in the middle of a line
        while best_split < text.len()
            && !text.chars().nth(best_split).unwrap_or('\n').is_whitespace()
        {
            best_split += 1;
        }

        let (process_part, remaining_part) = text.split_at(best_split);
        (process_part.to_string(), remaining_part.to_string())
    }

    /// Updates the current position based on processed text
    fn update_position(&mut self, text: &str) {
        for ch in text.chars() {
            match ch {
                '\n' => {
                    self.current_position.line += 1;
                    self.current_position.column = 1;
                }
                _ => {
                    self.current_position.column += 1;
                }
            }
            self.current_position.offset += ch.len_utf8();
        }
    }
}

/// Streaming parser for string input (convenience wrapper)
pub struct StringStreamingParser {
    inner: StreamingParser<std::io::Cursor<Vec<u8>>>,
}

impl StringStreamingParser {
    /// Creates a new streaming parser for string input
    pub fn new(input: &str, config: StreamingConfig) -> Self {
        let cursor = std::io::Cursor::new(input.as_bytes().to_vec());
        Self {
            inner: StreamingParser::new(cursor, config),
        }
    }

    /// Creates a new streaming parser with default configuration
    pub fn with_defaults(input: &str) -> Self {
        Self::new(input, StreamingConfig::default())
    }

    /// Processes the next chunk
    pub fn next_chunk(&mut self) -> Result<Option<ChunkResult>> {
        self.inner.next_chunk()
    }

    /// Parses the complete input
    pub fn parse_complete(&mut self) -> Result<Document> {
        self.inner.parse_complete()
    }

    /// Returns current position
    pub fn position(&self) -> Position {
        self.inner.position()
    }

    /// Returns bytes processed
    pub fn bytes_processed(&self) -> usize {
        self.inner.bytes_processed()
    }

    /// Returns whether finished
    pub fn is_finished(&self) -> bool {
        self.inner.is_finished()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn test_streaming_config_default() {
        let config = StreamingConfig::default();
        assert_eq!(config.chunk_size, 64 * 1024);
        assert_eq!(config.max_buffer_chunks, 10);
        assert!(config.enable_backpressure);
        assert!(config.preserve_block_boundaries);
        assert_eq!(config.chunk_overlap, 1024);
    }

    #[test]
    fn test_streaming_parser_creation() {
        let input = "# Test\n\nContent";
        let cursor = Cursor::new(input.as_bytes());
        let config = StreamingConfig::default();
        let parser = StreamingParser::new(cursor, config);

        assert_eq!(parser.position().line, 1);
        assert_eq!(parser.position().column, 1);
        assert!(!parser.is_finished());
        assert_eq!(parser.bytes_processed(), 0);
    }

    #[test]
    fn test_string_streaming_parser() {
        let input = "# Heading\n\nParagraph content.";
        let mut parser = StringStreamingParser::with_defaults(input);

        assert!(!parser.is_finished());

        // Process chunks
        let mut chunk_count = 0;
        while let Ok(Some(_chunk)) = parser.next_chunk() {
            chunk_count += 1;
            if chunk_count > 10 {
                break; // Prevent infinite loop in test
            }
        }

        assert!(chunk_count > 0);
        assert!(parser.is_finished());
    }

    #[test]
    fn test_streaming_parser_complete_parsing() {
        let input = "# Title\n\nFirst paragraph.\n\n## Subtitle\n\nSecond paragraph.";
        let mut parser = StringStreamingParser::with_defaults(input);

        let document = parser.parse_complete().unwrap();

        // Should have parsed all blocks
        assert!(document.blocks.len() >= 2); // At least heading and paragraph
        assert!(parser.is_finished());
    }

    #[test]
    fn test_chunk_result_structure() {
        let input = "# Test";
        let mut parser = StringStreamingParser::with_defaults(input);

        if let Ok(Some(chunk)) = parser.next_chunk() {
            assert!(chunk.bytes_processed > 0);
            assert_eq!(chunk.start_position.line, 1);
            assert!(chunk.warnings.is_empty());
        }
    }

    #[test]
    fn test_streaming_with_custom_config() {
        let config = StreamingConfig {
            chunk_size: 10, // Very small chunks for testing
            max_buffer_chunks: 2,
            enable_backpressure: false,
            parser_config: ParserConfig::default(),
            preserve_block_boundaries: false,
            chunk_overlap: 2,
        };

        let input = "# Heading\n\nParagraph content that is longer than chunk size.";
        let mut parser = StringStreamingParser::new(input, config);

        let mut total_chunks = 0;
        while let Ok(Some(_chunk)) = parser.next_chunk() {
            total_chunks += 1;
            if total_chunks > 20 {
                break; // Prevent infinite loop
            }
        }

        // Should have processed multiple chunks due to small chunk size
        assert!(total_chunks > 1);
    }

    #[test]
    fn test_position_tracking() {
        let input = "Line 1\nLine 2\nLine 3";
        let mut parser = StringStreamingParser::with_defaults(input);

        let initial_pos = parser.position();
        assert_eq!(initial_pos.line, 1);
        assert_eq!(initial_pos.column, 1);

        // Process the input
        let _document = parser.parse_complete().unwrap();

        // Position should have advanced
        let final_pos = parser.position();
        assert!(final_pos.line >= 3 || final_pos.offset > 0);
    }

    #[test]
    fn test_backpressure_handling() {
        let config = StreamingConfig {
            chunk_size: 5, // Very small chunks
            max_buffer_chunks: 2,
            enable_backpressure: true,
            ..StreamingConfig::default()
        };

        let input = "# A\n\n# B\n\n# C\n\n# D\n\n# E";
        let mut parser = StringStreamingParser::new(input, config);

        // Process a few chunks to test buffering
        let mut chunks_processed = 0;
        while let Ok(Some(_chunk)) = parser.next_chunk() {
            chunks_processed += 1;
            if chunks_processed >= 3 {
                break;
            }
        }

        assert!(chunks_processed > 0);
    }

    #[test]
    fn test_block_boundary_preservation() {
        let config = StreamingConfig {
            chunk_size: 20, // Small chunk to force splitting
            preserve_block_boundaries: true,
            chunk_overlap: 5,
            ..StreamingConfig::default()
        };

        let input = "# Heading One\n\nParagraph one.\n\n# Heading Two\n\nParagraph two.";
        let mut parser = StringStreamingParser::new(input, config);

        let document = parser.parse_complete().unwrap();

        // Should successfully parse despite chunking
        assert!(!document.blocks.is_empty());
    }

    #[test]
    fn test_empty_input_streaming() {
        let input = "";
        let mut parser = StringStreamingParser::with_defaults(input);

        let result = parser.next_chunk().unwrap();
        assert!(result.is_none());
        assert!(parser.is_finished());

        let document = parser.parse_complete().unwrap();
        assert!(document.blocks.is_empty());
    }

    #[test]
    fn test_large_input_simulation() {
        // Simulate a large document by repeating content
        let base_content = "# Section\n\nThis is a paragraph with some content.\n\n";
        let large_input = base_content.repeat(100); // Repeat 100 times

        let config = StreamingConfig {
            chunk_size: 1024, // 1KB chunks
            ..StreamingConfig::default()
        };

        let mut parser = StringStreamingParser::new(&large_input, config);
        let document = parser.parse_complete().unwrap();

        // Should have parsed many blocks
        assert!(document.blocks.len() > 50); // Should have many sections
        assert!(parser.bytes_processed() > 1000);
    }
}
