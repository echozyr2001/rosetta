#![allow(dead_code)]
use crate::lexer::{LexToken, Position};

/// Experimental token model that captures the richer lexical information
/// described by the CommonMark 0.31.2 specification. This design keeps the
/// lexical layer focused on syntactic markers and defers block aggregation to
/// the parser/IR pipeline.
#[derive(Debug, Clone, PartialEq)]
pub enum Token<'input> {
    /// ATX heading marker like `## Section` (CommonMark §4.2).
    AtxHeading(AtxHeadingToken<'input>),
    /// Setext heading underline `Heading\n-----` (CommonMark §4.3).
    SetextHeading(SetextHeadingToken<'input>),
    /// Indented or fenced code block marker (CommonMark §§4.4–4.5).
    CodeBlock(CodeBlockToken<'input>),
    /// Block quote marker `>` (CommonMark §5.1).
    BlockQuote(BlockQuoteToken),
    /// List marker `- item` or `1. item` (CommonMark §5.2).
    ListMarker(ListMarkerToken<'input>),
    /// Thematic break `***` (CommonMark §4.1).
    ThematicBreak(ThematicBreakToken),
    /// Raw HTML block `<div>` (CommonMark §4.6).
    HtmlBlock(HtmlBlockToken<'input>),
    /// Link reference definition `[label]: /url` (CommonMark §4.7).
    LinkReferenceDefinition(LinkReferenceDefinitionToken<'input>),
    /// Plain text run `hello` (CommonMark §6).
    Text(TextToken<'input>),
    /// Whitespace cluster (CommonMark §2.1).
    Whitespace(WhitespaceToken<'input>),
    /// Soft line break (CommonMark §6.9).
    SoftBreak(SoftBreakToken),
    /// Hard line break with trailing spaces (CommonMark §6.8).
    HardBreak(HardBreakToken),
    /// Emphasis delimiter run `***` (CommonMark §6.4).
    EmphasisDelimiter(EmphasisDelimiterToken),
    /// Inline code span `` `code` `` (CommonMark §6.5).
    CodeSpan(CodeSpanToken<'input>),
    /// Inline code delimiter run `` ``` `` (CommonMark §6.5).
    CodeSpanDelimiter(CodeSpanDelimiterToken),
    /// Autolink `<https://example.com>` (CommonMark §6.6).
    Autolink(AutolinkToken<'input>),
    /// HTML entity `&amp;` (CommonMark §6.2).
    Entity(EntityToken<'input>),
    /// Escape sequence `\*` (CommonMark §6.1).
    EscapeSequence(EscapeSequenceToken),
    /// Inline HTML `<span>` (CommonMark §6.7).
    HtmlInline(HtmlInlineToken<'input>),
    /// Link label delimiter `[` or `]` (CommonMark §6.10).
    LinkLabelDelimiter(LinkLabelDelimiter),
    /// Link destination `(destination)` (CommonMark §6.10).
    LinkDestination(LinkDestinationToken<'input>),
    /// Link title `"title"` (CommonMark §6.10).
    LinkTitle(LinkTitleToken<'input>),
    /// Image marker `!` (CommonMark §6.10).
    ImageMarker(ImageMarkerToken),
    /// Line ending `\n`, `\r`, or `\r\n` (CommonMark §2.2).
    LineEnding(LineEndingToken),
    /// Indentation prefix at the start of a line (CommonMark §2.1).
    Indent(IndentToken),
    /// Punctuation glyph used during inline parsing (CommonMark §6).
    Punctuation(PunctuationToken),
    /// End-of-file sentinel.
    Eof,
}

/// ATX heading lexeme (CommonMark §4.2).
#[derive(Debug, Clone, PartialEq)]
pub struct AtxHeadingToken<'input> {
    pub level: u8,
    pub marker_count: usize,
    pub leading_whitespace: usize,
    pub raw_marker: &'input str,
    pub raw_content: &'input str,
    pub closing_sequence: &'input str,
    pub position: crate::lexer::Position,
}

/// Setext underline marker (CommonMark §4.3).
#[derive(Debug, Clone, PartialEq)]
pub struct SetextHeadingToken<'input> {
    pub level: u8,
    pub marker_char: char,
    pub marker_count: usize,
    pub leading_whitespace: usize,
    pub raw_underline: &'input str,
    pub position: crate::lexer::Position,
}

/// Indented or fenced code block info (CommonMark §§4.4–4.5).
#[derive(Debug, Clone, PartialEq)]
pub struct CodeBlockToken<'input> {
    pub fence_char: Option<char>,
    pub fence_length: Option<usize>,
    pub info_string: Option<&'input str>,
    pub closing_fence_length: Option<usize>,
    pub raw_content: &'input str,
    pub indent_width: usize,
    pub contains_tab: bool,
    pub position: crate::lexer::Position,
}

/// Block quote marker (CommonMark §5.1).
#[derive(Debug, Clone, PartialEq)]
pub struct BlockQuoteToken {
    pub depth: usize,
    pub marker_offset: usize,
    pub spaces_after_marker: usize,
    pub position: crate::lexer::Position,
}

/// Types of list markers.
#[derive(Debug, Clone, PartialEq)]
pub enum ListKind {
    Bullet { marker: char },
    Ordered { start: u32, delimiter: char },
}

/// List item marker (CommonMark §5.2).
#[derive(Debug, Clone, PartialEq)]
pub struct ListMarkerToken<'input> {
    pub marker: ListKind,
    pub marker_offset: usize,
    pub spaces_after_marker: usize,
    pub ordinal_span: Option<&'input str>,
    pub position: crate::lexer::Position,
}

/// Thematic break marker (CommonMark §4.1).
#[derive(Debug, Clone, PartialEq)]
pub struct ThematicBreakToken {
    pub marker_char: char,
    pub marker_count: usize,
    pub leading_whitespace: usize,
    pub position: crate::lexer::Position,
}

/// HTML block categories (CommonMark §4.6).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HtmlBlockKind {
    Type1,
    Type2,
    Type3,
    Type4,
    Type5,
    Type6,
    Type7,
}

/// Raw HTML block contents (CommonMark §4.6).
#[derive(Debug, Clone, PartialEq)]
pub struct HtmlBlockToken<'input> {
    pub kind: HtmlBlockKind,
    pub raw: &'input str,
    pub position: crate::lexer::Position,
}

/// Link reference definition components (CommonMark §4.7).
#[derive(Debug, Clone, PartialEq)]
pub struct LinkReferenceDefinitionToken<'input> {
    pub label: &'input str,
    pub destination: &'input str,
    pub title: Option<&'input str>,
    pub marker_offset: usize,
    pub position: crate::lexer::Position,
}

/// Plain text lexeme emitted by the lexer (CommonMark §6).
#[derive(Debug, Clone, PartialEq)]
pub struct TextToken<'input> {
    pub lexeme: &'input str,
    pub position: crate::lexer::Position,
}

/// Runs of spaces or tabs (CommonMark §2.1).
#[derive(Debug, Clone, PartialEq)]
pub struct WhitespaceToken<'input> {
    pub lexeme: &'input str,
    pub contains_tab: bool,
    pub position: crate::lexer::Position,
}

/// Soft line break token (CommonMark §6.9).
#[derive(Debug, Clone, PartialEq)]
pub struct SoftBreakToken {
    pub position: crate::lexer::Position,
}

/// Hard line break token (CommonMark §6.8).
#[derive(Debug, Clone, PartialEq)]
pub struct HardBreakToken {
    pub position: crate::lexer::Position,
}

/// Whether an emphasis delimiter can open/close (CommonMark §6.4).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EmphasisMutation {
    Open,
    Close,
    Both,
    Neither,
}

/// Emphasis delimiter run metadata (CommonMark §6.4).
#[derive(Debug, Clone, PartialEq)]
pub struct EmphasisDelimiterToken {
    pub marker: char,
    pub run_length: usize,
    pub mutation: EmphasisMutation,
    pub position: crate::lexer::Position,
}

/// Inline code span payload (CommonMark §6.5).
#[derive(Debug, Clone, PartialEq)]
pub struct CodeSpanToken<'input> {
    pub content: &'input str,
    pub backtick_count: usize,
    pub position: crate::lexer::Position,
}

/// Inline code delimiter run (CommonMark §6.5).
#[derive(Debug, Clone, PartialEq)]
pub struct CodeSpanDelimiterToken {
    pub backtick_count: usize,
    pub position: crate::lexer::Position,
}

/// Autolink classification (CommonMark §6.6).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AutolinkKind {
    Uri,
    Email,
}

/// Autolink payload (CommonMark §6.6).
#[derive(Debug, Clone, PartialEq)]
pub struct AutolinkToken<'input> {
    pub kind: AutolinkKind,
    pub lexeme: &'input str,
    pub position: crate::lexer::Position,
}

/// Named or numeric character reference (CommonMark §6.2).
#[derive(Debug, Clone, PartialEq)]
pub struct EntityToken<'input> {
    pub raw: &'input str,
    pub resolved: Option<char>,
    pub position: crate::lexer::Position,
}

/// Escaped character (CommonMark §6.1).
#[derive(Debug, Clone, PartialEq)]
pub struct EscapeSequenceToken {
    pub escaped: char,
    pub position: crate::lexer::Position,
}

/// Raw inline HTML chunk (CommonMark §6.7).
#[derive(Debug, Clone, PartialEq)]
pub struct HtmlInlineToken<'input> {
    pub raw: &'input str,
    pub position: crate::lexer::Position,
}

/// Link label delimiter classification (CommonMark §6.10).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LinkLabelDelimiter {
    Open,
    Close,
}

/// Link destination chunk (CommonMark §6.10).
#[derive(Debug, Clone, PartialEq)]
pub struct LinkDestinationToken<'input> {
    pub raw: &'input str,
    pub position: crate::lexer::Position,
}

/// Link title chunk (CommonMark §6.10).
#[derive(Debug, Clone, PartialEq)]
pub struct LinkTitleToken<'input> {
    pub raw: &'input str,
    pub position: crate::lexer::Position,
}

/// Image marker `!` (CommonMark §6.10).
#[derive(Debug, Clone, PartialEq)]
pub struct ImageMarkerToken {
    pub position: crate::lexer::Position,
}

/// Line ending classification (CommonMark §2.2).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LineEndingKind {
    LineFeed,
    CarriageReturn,
    CarriageReturnLineFeed,
}

/// Line ending token (CommonMark §2.2).
#[derive(Debug, Clone, PartialEq)]
pub struct LineEndingToken {
    pub kind: LineEndingKind,
    pub position: crate::lexer::Position,
}

/// Indentation encountered at the start of a line (CommonMark §2.1).
#[derive(Debug, Clone, PartialEq)]
pub struct IndentToken {
    pub visual_width: usize,
    pub contains_tab: bool,
    pub position: crate::lexer::Position,
}

/// Inline punctuation marker (CommonMark §6).
#[derive(Debug, Clone, PartialEq)]
pub struct PunctuationToken {
    pub ch: char,
    pub position: crate::lexer::Position,
}

// Implement LexToken for Token from token.rs
impl<'input> LexToken for Token<'input> {
    fn position(&self) -> Option<Position> {
        match self {
            Token::AtxHeading(token) => Some(token.position),
            Token::SetextHeading(token) => Some(token.position),
            Token::CodeBlock(token) => Some(token.position),
            Token::BlockQuote(token) => Some(token.position),
            Token::ListMarker(token) => Some(token.position),
            Token::ThematicBreak(token) => Some(token.position),
            Token::HtmlBlock(token) => Some(token.position),
            Token::LinkReferenceDefinition(token) => Some(token.position),
            Token::Text(token) => Some(token.position),
            Token::Whitespace(token) => Some(token.position),
            Token::SoftBreak(token) => Some(token.position),
            Token::HardBreak(token) => Some(token.position),
            Token::EmphasisDelimiter(token) => Some(token.position),
            Token::CodeSpan(token) => Some(token.position),
            Token::CodeSpanDelimiter(token) => Some(token.position),
            Token::Autolink(token) => Some(token.position),
            Token::Entity(token) => Some(token.position),
            Token::EscapeSequence(token) => Some(token.position),
            Token::HtmlInline(token) => Some(token.position),
            Token::LinkDestination(token) => Some(token.position),
            Token::LinkTitle(token) => Some(token.position),
            Token::ImageMarker(token) => Some(token.position),
            Token::LineEnding(token) => Some(token.position),
            Token::Indent(token) => Some(token.position),
            Token::Punctuation(token) => Some(token.position),
            Token::LinkLabelDelimiter(_) => None, // LinkLabelDelimiter doesn't have position yet
            Token::Eof => None,
        }
    }

    fn is_eof(&self) -> bool {
        matches!(self, Token::Eof)
    }

    fn is_newline(&self) -> bool {
        matches!(self, Token::LineEnding(_))
    }

    fn is_whitespace(&self) -> bool {
        matches!(self, Token::Whitespace(_))
    }

    fn is_indent(&self) -> bool {
        matches!(self, Token::Indent(_))
    }
}
