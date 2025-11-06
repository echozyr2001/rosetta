use crate::ast::{Block, Inline};

/// Enhanced paragraph builder that handles complex inline content.
pub struct ParagraphBuilder {
    inlines: Vec<Inline>,
    consecutive_newlines: usize,
    has_content: bool,
}

impl ParagraphBuilder {
    pub fn new() -> Self {
        Self {
            inlines: Vec::new(),
            consecutive_newlines: 0,
            has_content: false,
        }
    }

    pub fn push_inline(&mut self, inline: Inline) {
        // Skip empty text inlines
        if let Inline::Text(text) = &inline
            && text.trim().is_empty()
            && !self.has_content
        {
            return;
        }

        self.inlines.push(inline);
        self.consecutive_newlines = 0;
        self.has_content = true;
    }

    pub fn push_text(&mut self, fragment: &str) {
        if fragment.trim().is_empty() && !self.has_content {
            return;
        }

        // Merge with previous text inline if possible
        if let Some(Inline::Text(last_text)) = self.inlines.last_mut() {
            if !last_text.ends_with(' ') && !fragment.starts_with(' ') {
                last_text.push(' ');
            }
            last_text.push_str(fragment.trim_end_matches('\n'));
        } else {
            self.inlines
                .push(Inline::Text(fragment.trim_end_matches('\n').to_string()));
        }

        self.consecutive_newlines = 0;
        self.has_content = true;
    }

    pub fn flush_into(&mut self, output: &mut Vec<Block>) {
        if let Some(block) = self.flush() {
            output.push(block);
        }
    }

    pub fn flush(&mut self) -> Option<Block> {
        if !self.has_content || self.inlines.is_empty() {
            self.clear();
            return None;
        }

        // Remove trailing soft breaks
        while let Some(Inline::SoftBreak) = self.inlines.last() {
            self.inlines.pop();
        }

        if self.inlines.is_empty() {
            self.clear();
            return None;
        }

        let content = std::mem::take(&mut self.inlines);
        self.clear();

        Some(Block::Paragraph {
            content,
            position: None,
        })
    }

    pub fn clear(&mut self) {
        self.inlines.clear();
        self.consecutive_newlines = 0;
        self.has_content = false;
    }
}

impl Default for ParagraphBuilder {
    fn default() -> Self {
        Self::new()
    }
}
