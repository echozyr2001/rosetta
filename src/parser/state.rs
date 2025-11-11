use crate::error::Result;
use crate::lexer::LexToken;
use crate::traits::{CanBuildAst, ParsingContext};

/// Streaming parser state that drives token consumption directly from the
/// underlying parsing context. This avoids materialising the full token stream
/// up front and keeps lexing lazy.
pub struct ParserState<'ctx, C>
where
    C: ParsingContext,
{
    context: &'ctx mut C,
}

impl<'ctx, C> ParserState<'ctx, C>
where
    C: ParsingContext,
    C::Token: LexToken,
{
    pub fn new(context: &'ctx mut C) -> Self {
        Self { context }
    }

    /// Borrow the underlying parsing context.
    pub fn context(&mut self) -> &mut C {
        self.context
    }

    /// Returns a clone of the current token without consuming it.
    pub fn current_token(&self) -> Option<C::Token> {
        self.context.current_token().cloned()
    }

    /// Consume the current token and advance to the next one.
    pub fn consume_token(&mut self) -> Result<Option<C::Token>> {
        if self.is_exhausted() {
            return Ok(None);
        }

        let token = self.context.current_token().cloned();
        self.advance()?;
        Ok(token)
    }

    /// Advance to the next token if we are not already at EOF.
    pub fn advance(&mut self) -> Result<()> {
        if self.is_exhausted() {
            return Ok(());
        }

        self.context.advance()
    }

    /// Peek at the next token without advancing the underlying context.
    pub fn peek_token(&mut self) -> Result<Option<C::Token>> {
        if self.is_exhausted() {
            return Ok(None);
        }

        let token = self.context.peek_token()?;
        Ok(Some(token))
    }

    /// Returns true if the current token is EOF or the context ran out of
    /// tokens.
    pub fn is_exhausted(&self) -> bool {
        self.context
            .current_token()
            .map(|token| token.is_eof())
            .unwrap_or(true)
    }

    /// Shortcut access to the AST builder exposed by the context.
    pub fn ast_builder(
        &self,
    ) -> &dyn crate::parser::traits::AstBuilder<
        Inline = <C as CanBuildAst>::Inline,
        Block = <C as CanBuildAst>::Block,
        Document = <C as CanBuildAst>::Document,
    > {
        self.context.ast_builder()
    }
}
