/// Component-based traits for token layer decoupling
///
/// This module defines provider and consumer traits that decouple the lexer,
/// parser, and token implementations using component abstractions.
use crate::error::Result;
use crate::lexer::{LexToken, Position};

use super::component::{DelegateComponent, HasProvider, IsProviderFor};

/// Component marker for token provision.
pub struct TokenProviderComponent;

/// Consumer trait for contexts capable of supplying tokens.
pub trait CanProvideTokens: HasProvider {
    type Token: LexToken;

    fn current_token(&self) -> Option<&Self::Token>;
    fn next_token(&mut self) -> Result<Self::Token>;
    fn peek_token(&mut self) -> Result<Self::Token>;
    fn current_position(&self) -> Position;
}

/// Provider trait that concrete component registries implement.
pub trait TokenProvider<Context>: IsProviderFor<TokenProviderComponent, Context> {
    type Token: LexToken;

    #[allow(clippy::needless_lifetimes)]
    fn current_token<'a>(context: &'a Context) -> Option<&'a Self::Token>;
    fn next_token(context: &mut Context) -> Result<Self::Token>;
    fn peek_token(context: &mut Context) -> Result<Self::Token>;
    fn current_position(context: &Context) -> Position;
}

impl<Context> CanProvideTokens for Context
where
    Context: HasProvider,
    Context::Components: TokenProvider<Context>,
{
    type Token = <Context::Components as TokenProvider<Context>>::Token;

    fn current_token(&self) -> Option<&Self::Token> {
        <Context::Components as TokenProvider<Context>>::current_token(self)
    }

    fn next_token(&mut self) -> Result<Self::Token> {
        <Context::Components as TokenProvider<Context>>::next_token(self)
    }

    fn peek_token(&mut self) -> Result<Self::Token> {
        <Context::Components as TokenProvider<Context>>::peek_token(self)
    }

    fn current_position(&self) -> Position {
        <Context::Components as TokenProvider<Context>>::current_position(self)
    }
}

impl<Context, Component> TokenProvider<Context> for Component
where
    Context: Sized,
    Component:
        DelegateComponent<TokenProviderComponent> + IsProviderFor<TokenProviderComponent, Context>,
    Component::Delegate: TokenProvider<Context>,
{
    type Token = <Component::Delegate as TokenProvider<Context>>::Token;

    #[allow(clippy::needless_lifetimes)]
    fn current_token<'a>(context: &'a Context) -> Option<&'a Self::Token> {
        <Component::Delegate as TokenProvider<Context>>::current_token(context)
    }

    fn next_token(context: &mut Context) -> Result<Self::Token> {
        <Component::Delegate as TokenProvider<Context>>::next_token(context)
    }

    fn peek_token(context: &mut Context) -> Result<Self::Token> {
        <Component::Delegate as TokenProvider<Context>>::peek_token(context)
    }

    fn current_position(context: &Context) -> Position {
        <Component::Delegate as TokenProvider<Context>>::current_position(context)
    }
}
