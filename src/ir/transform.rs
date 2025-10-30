use crate::ast::{Block as AstBlock, Document, Inline as AstInline, ListItem, ListKind};
use crate::error::{MarkdownError, Result};

use super::types::{
    Block, BlockId, BlockKind, ContentPayload, InlineMark, InlineSpan, MetadataMap, MetadataValue,
    Space, SpaceId, Workspace, WorkspaceId,
};

/// Transformer responsible for converting the Markdown AST into Workspace IR.
#[derive(Debug, Default)]
pub struct WorkspaceTransformer;

impl WorkspaceTransformer {
    pub fn new() -> Self {
        Self
    }

    /// Transform an AST document into a Workspace IR tree.
    pub fn transform(&self, document: &Document) -> Result<Workspace> {
        let mut workspace = Workspace::new(WorkspaceId::new("workspace"));

        if document.blocks.is_empty() {
            return Ok(workspace);
        }

        let mut default_space = Space::new(SpaceId::new("space"));

        for (index, block) in document.blocks.iter().enumerate() {
            let ir_block = self.transform_block(block, index as u32)?;
            default_space.blocks.push(ir_block);
        }

        workspace.spaces.push(default_space);
        Ok(workspace)
    }

    fn transform_block(&self, block: &AstBlock, index: u32) -> Result<Block> {
        match block {
            AstBlock::Heading {
                level,
                content,
                id,
                position,
            } => {
                let mut ir_block = Block::new(
                    BlockId::new(id.clone().unwrap_or_else(|| format!("heading-{index}"))),
                    BlockKind::new("heading"),
                );

                ir_block.layout.index = Some(index);
                ir_block
                    .metadata
                    .insert("level".into(), MetadataValue::Number(f64::from(*level)));

                if let Some(position) = position {
                    ir_block.metadata.insert(
                        "position".into(),
                        MetadataValue::Object(position_metadata(position)),
                    );
                }

                ir_block.content = ContentPayload::RichText(self.transform_inline_vec(content));
                Ok(ir_block)
            }

            AstBlock::Paragraph { content, position } => {
                let mut ir_block = Block::new(
                    BlockId::new(format!("paragraph-{index}")),
                    BlockKind::new("paragraph"),
                );

                ir_block.layout.index = Some(index);

                if let Some(position) = position {
                    ir_block.metadata.insert(
                        "position".into(),
                        MetadataValue::Object(position_metadata(position)),
                    );
                }

                ir_block.content = ContentPayload::RichText(self.transform_inline_vec(content));
                Ok(ir_block)
            }

            AstBlock::CodeBlock {
                info,
                content,
                language,
                position,
            } => {
                let mut ir_block = Block::new(
                    BlockId::new(format!("code-{index}")),
                    BlockKind::new("code"),
                );
                ir_block.layout.index = Some(index);

                let mut data = MetadataMap::default();
                if let Some(lang) = language {
                    data.insert("language".into(), MetadataValue::String(lang.clone()));
                }
                if let Some(info) = info {
                    data.insert("info".into(), MetadataValue::String(info.clone()));
                }

                if let Some(position) = position {
                    ir_block.metadata.insert(
                        "position".into(),
                        MetadataValue::Object(position_metadata(position)),
                    );
                }

                ir_block.content = ContentPayload::Data(
                    data.into_iter()
                        .chain(std::iter::once((
                            "code".into(),
                            MetadataValue::String(content.clone()),
                        )))
                        .collect(),
                );
                Ok(ir_block)
            }

            AstBlock::BlockQuote { content, position } => {
                let mut ir_block = Block::new(
                    BlockId::new(format!("blockquote-{index}")),
                    BlockKind::new("blockquote"),
                );
                ir_block.layout.index = Some(index);

                if let Some(position) = position {
                    ir_block.metadata.insert(
                        "position".into(),
                        MetadataValue::Object(position_metadata(position)),
                    );
                }

                ir_block.children = self.transform_blocks(content)?;
                Ok(ir_block)
            }

            AstBlock::List {
                kind,
                tight,
                items,
                position,
            } => {
                let mut ir_block = Block::new(
                    BlockId::new(format!("list-{index}")),
                    BlockKind::new("list"),
                );

                ir_block.layout.index = Some(index);

                match kind {
                    ListKind::Bullet { marker } => {
                        ir_block
                            .metadata
                            .insert("marker".into(), MetadataValue::String(marker.to_string()));
                        ir_block
                            .metadata
                            .insert("ordered".into(), MetadataValue::Bool(false));
                    }
                    ListKind::Ordered { start, delimiter } => {
                        ir_block
                            .metadata
                            .insert("ordered".into(), MetadataValue::Bool(true));
                        ir_block
                            .metadata
                            .insert("start".into(), MetadataValue::Number(f64::from(*start)));
                        ir_block.metadata.insert(
                            "delimiter".into(),
                            MetadataValue::String(delimiter.to_string()),
                        );
                    }
                }

                ir_block
                    .metadata
                    .insert("tight".into(), MetadataValue::Bool(*tight));

                if let Some(position) = position {
                    ir_block.metadata.insert(
                        "position".into(),
                        MetadataValue::Object(position_metadata(position)),
                    );
                }

                ir_block.children = self.transform_list_items(items)?;
                Ok(ir_block)
            }

            AstBlock::ThematicBreak { position } => {
                let mut ir_block = Block::new(
                    BlockId::new(format!("thematic-break-{index}")),
                    BlockKind::new("thematic_break"),
                );
                ir_block.layout.index = Some(index);

                if let Some(position) = position {
                    ir_block.metadata.insert(
                        "position".into(),
                        MetadataValue::Object(position_metadata(position)),
                    );
                }

                Ok(ir_block)
            }

            AstBlock::HtmlBlock { content, position } => {
                let mut ir_block = Block::new(
                    BlockId::new(format!("html-{index}")),
                    BlockKind::new("html"),
                );
                ir_block.layout.index = Some(index);

                if let Some(position) = position {
                    ir_block.metadata.insert(
                        "position".into(),
                        MetadataValue::Object(position_metadata(position)),
                    );
                }

                ir_block.content = ContentPayload::Text(content.clone());
                Ok(ir_block)
            }
        }
    }

    fn transform_blocks(&self, blocks: &[AstBlock]) -> Result<Vec<Block>> {
        blocks
            .iter()
            .enumerate()
            .map(|(idx, block)| self.transform_block(block, idx as u32))
            .collect()
    }

    fn transform_list_items(&self, items: &[ListItem]) -> Result<Vec<Block>> {
        let mut results = Vec::with_capacity(items.len());

        for (index, item) in items.iter().enumerate() {
            let mut block = Block::new(
                BlockId::new(format!("list-item-{index}")),
                BlockKind::new("list_item"),
            );
            block.layout.index = Some(index as u32);
            block
                .metadata
                .insert("tight".into(), MetadataValue::Bool(item.tight));

            if let Some(checked) = item.task_list_marker {
                block
                    .metadata
                    .insert("task".into(), MetadataValue::Bool(true));
                block
                    .metadata
                    .insert("checked".into(), MetadataValue::Bool(checked));
            }

            block.children = self.transform_blocks(&item.content)?;
            results.push(block);
        }

        Ok(results)
    }

    fn transform_inline_vec(&self, inlines: &[AstInline]) -> Vec<InlineSpan> {
        inlines
            .iter()
            .flat_map(|inline| self.transform_inline(inline))
            .collect()
    }

    fn transform_inline(&self, inline: &AstInline) -> Vec<InlineSpan> {
        match inline {
            AstInline::Text(text) => vec![InlineSpan::new(text.clone())],

            AstInline::Emphasis { strong, content } => {
                let mark = if *strong {
                    InlineMark::Bold
                } else {
                    InlineMark::Italic
                };

                self.transform_inline_vec(content)
                    .into_iter()
                    .map(|mut span| {
                        span.marks.push(mark.clone());
                        span
                    })
                    .collect()
            }

            AstInline::Code(code) => vec![InlineSpan {
                text: code.clone(),
                marks: vec![InlineMark::Code],
                metadata: MetadataMap::default(),
            }],

            AstInline::Link {
                text,
                destination,
                title,
            } => self
                .transform_inline_vec(text)
                .into_iter()
                .map(|mut span| {
                    span.marks.push(InlineMark::Link);
                    span.metadata
                        .insert("href".into(), MetadataValue::String(destination.clone()));
                    if let Some(title) = title {
                        span.metadata
                            .insert("title".into(), MetadataValue::String(title.clone()));
                    }
                    span
                })
                .collect(),

            AstInline::Image {
                alt,
                destination,
                title,
            } => {
                let mut span = InlineSpan::new(alt.clone());
                span.metadata
                    .insert("src".into(), MetadataValue::String(destination.clone()));
                if let Some(title) = title {
                    span.metadata
                        .insert("title".into(), MetadataValue::String(title.clone()));
                }
                span.marks.push(InlineMark::Custom("image".into()));
                vec![span]
            }

            AstInline::HtmlInline(content) => vec![InlineSpan::new(content.clone())],

            AstInline::SoftBreak => vec![InlineSpan::new("\n")],

            AstInline::HardBreak => {
                let mut span = InlineSpan::new("\n");
                span.marks.push(InlineMark::Custom("hard_break".into()));
                vec![span]
            }
        }
    }
}

fn position_metadata(position: &crate::lexer::Position) -> MetadataMap {
    let mut map = MetadataMap::default();
    map.insert("line".into(), MetadataValue::Number(position.line as f64));
    map.insert(
        "column".into(),
        MetadataValue::Number(position.column as f64),
    );
    map.insert(
        "offset".into(),
        MetadataValue::Number(position.offset as f64),
    );
    map
}

impl Workspace {
    pub fn from_ast(document: &Document) -> Result<Self> {
        WorkspaceTransformer::new().transform(document)
    }
}

impl TryFrom<&Document> for Workspace {
    type Error = MarkdownError;

    fn try_from(document: &Document) -> Result<Self> {
        Workspace::from_ast(document)
    }
}

impl TryFrom<Document> for Workspace {
    type Error = MarkdownError;

    fn try_from(document: Document) -> Result<Self> {
        Workspace::from_ast(&document)
    }
}

impl From<&Workspace> for Workspace {
    fn from(workspace: &Workspace) -> Self {
        workspace.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::utils::NodeBuilder;
    use crate::lexer::Position;

    #[test]
    fn transforms_heading_and_paragraph() {
        let heading = NodeBuilder::heading(1, vec![NodeBuilder::text("Title".into())], None);
        let paragraph = NodeBuilder::paragraph(
            vec![
                NodeBuilder::text("Hello ".into()),
                NodeBuilder::emphasis(true, vec![NodeBuilder::text("World".into())]),
            ],
            None,
        );
        let document = NodeBuilder::document(vec![heading, paragraph]);

        let workspace = Workspace::from_ast(&document).expect("transform should succeed");

        assert_eq!(workspace.spaces.len(), 1);
        let space = &workspace.spaces[0];
        assert_eq!(space.blocks.len(), 2);

        let heading_block = &space.blocks[0];
        assert_eq!(heading_block.kind.0, "heading");
        assert_eq!(heading_block.content,
            ContentPayload::RichText(vec![InlineSpan::new("Title")])
        );

        let paragraph_block = &space.blocks[1];
        assert_eq!(paragraph_block.kind.0, "paragraph");
        match &paragraph_block.content {
            ContentPayload::RichText(spans) => {
                assert_eq!(spans.len(), 2);
                assert_eq!(spans[0].text, "Hello ");
                assert_eq!(spans[1].text, "World");
                assert!(spans[1].marks.contains(&InlineMark::Bold));
            }
            other => panic!("Unexpected content: {:?}", other),
        }
    }

    #[test]
    fn transforms_lists_and_nested_blocks() {
        let list_item = NodeBuilder::list_item(
            vec![NodeBuilder::paragraph(vec![NodeBuilder::text("Item".into())], None)],
            false,
            Some(true),
        );
        let list = NodeBuilder::list(
            ListKind::Ordered { start: 1, delimiter: '.' },
            true,
            vec![list_item],
            None,
        );
        let quote = NodeBuilder::blockquote(
            vec![NodeBuilder::paragraph(vec![NodeBuilder::text("Quote".into())], None)],
            None,
        );
        let document = NodeBuilder::document(vec![list, quote]);

        let workspace = Workspace::from_ast(&document).expect("transform should succeed");

        let space = &workspace.spaces[0];
        assert_eq!(space.blocks.len(), 2);

        let list_block = &space.blocks[0];
        assert_eq!(list_block.kind.0, "list");
        assert_eq!(list_block.children.len(), 1);
        assert_eq!(list_block.metadata.get("ordered"), Some(&MetadataValue::Bool(true)));

        let list_item_block = &list_block.children[0];
        assert_eq!(list_item_block.kind.0, "list_item");
        assert_eq!(list_item_block.metadata.get("task"), Some(&MetadataValue::Bool(true)));
        assert_eq!(list_item_block.metadata.get("checked"), Some(&MetadataValue::Bool(true)));
        assert_eq!(list_item_block.children.len(), 1);

        let quote_block = &space.blocks[1];
        assert_eq!(quote_block.kind.0, "blockquote");
        assert_eq!(quote_block.children.len(), 1);
    }

    #[test]
    fn transforms_positions_and_code_blocks() {
        let position = Some(Position { line: 1, column: 1, offset: 0 });
        let heading = NodeBuilder::heading(2, vec![NodeBuilder::text("A".into())], position);
        let code_block = NodeBuilder::code_block(
            Some("rust".into()),
            "fn main() {}".into(),
            Some("rust".into()),
            position,
        );
        let document = NodeBuilder::document(vec![heading, code_block]);

        let workspace = Workspace::from_ast(&document).expect("transform should succeed");

        let space = &workspace.spaces[0];
        assert_eq!(space.blocks.len(), 2);

        let heading_block = &space.blocks[0];
        assert!(heading_block.metadata.contains_key("position"));

        let code_block = &space.blocks[1];
        assert_eq!(code_block.kind.0, "code");
        match &code_block.content {
            ContentPayload::Data(data) => {
                assert_eq!(data.get("language"), Some(&MetadataValue::String("rust".into())));
                assert_eq!(data.get("code"), Some(&MetadataValue::String("fn main() {}".into())));
            }
            other => panic!("Unexpected code block payload: {:?}", other),
        }
    }
}
