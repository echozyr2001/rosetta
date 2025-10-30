use std::collections::HashMap;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct WorkspaceId(pub String);

impl WorkspaceId {
    pub fn new(id: impl Into<String>) -> Self {
        Self(id.into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct SpaceId(pub String);

impl SpaceId {
    pub fn new(id: impl Into<String>) -> Self {
        Self(id.into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BlockId(pub String);

impl BlockId {
    pub fn new(id: impl Into<String>) -> Self {
        Self(id.into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BlockKind(pub String);

impl BlockKind {
    pub fn new(kind: impl Into<String>) -> Self {
        Self(kind.into())
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Workspace {
    pub id: WorkspaceId,
    pub label: Option<String>,
    pub spaces: Vec<Space>,
    pub layout: LayoutMetadata,
    pub style: StyleHooks,
    pub metadata: MetadataMap,
}

impl Workspace {
    pub fn new(id: WorkspaceId) -> Self {
        Self {
            id,
            label: None,
            spaces: Vec::new(),
            layout: LayoutMetadata::default(),
            style: StyleHooks::default(),
            metadata: MetadataMap::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Space {
    pub id: SpaceId,
    pub label: Option<String>,
    pub blocks: Vec<Block>,
    pub layout: LayoutMetadata,
    pub style: StyleHooks,
    pub metadata: MetadataMap,
}

impl Space {
    pub fn new(id: SpaceId) -> Self {
        Self {
            id,
            label: None,
            blocks: Vec::new(),
            layout: LayoutMetadata::default(),
            style: StyleHooks::default(),
            metadata: MetadataMap::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Block {
    pub id: BlockId,
    pub kind: BlockKind,
    pub label: Option<String>,
    pub layout: LayoutMetadata,
    pub style: StyleHooks,
    pub metadata: MetadataMap,
    pub content: ContentPayload,
    pub children: Vec<Block>,
}

impl Block {
    pub fn new(id: BlockId, kind: BlockKind) -> Self {
        Self {
            id,
            kind,
            label: None,
            layout: LayoutMetadata::default(),
            style: StyleHooks::default(),
            metadata: MetadataMap::default(),
            content: ContentPayload::Empty,
            children: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum ContentPayload {
    Text(String),
    RichText(Vec<InlineSpan>),
    Data(MetadataMap),
    Empty,
}

impl Default for ContentPayload {
    fn default() -> Self {
        Self::Empty
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct InlineSpan {
    pub text: String,
    pub marks: Vec<InlineMark>,
    pub metadata: MetadataMap,
}

impl InlineSpan {
    pub fn new(text: impl Into<String>) -> Self {
        Self {
            text: text.into(),
            marks: Vec::new(),
            metadata: MetadataMap::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum InlineMark {
    Bold,
    Italic,
    Code,
    Link,
    Custom(String),
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct LayoutMetadata {
    pub index: Option<u32>,
    pub columns: Option<u16>,
    pub rows: Option<u16>,
    pub size: LayoutSize,
    pub alignment: LayoutAlignment,
    pub anchor: Option<String>,
    pub group: Option<String>,
    pub custom: MetadataMap,
}

impl Default for LayoutMetadata {
    fn default() -> Self {
        Self {
            index: None,
            columns: None,
            rows: None,
            size: LayoutSize::default(),
            alignment: LayoutAlignment::default(),
            anchor: None,
            group: None,
            custom: MetadataMap::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct LayoutSize {
    pub width: Dimension,
    pub height: Dimension,
}

impl Default for LayoutSize {
    fn default() -> Self {
        Self {
            width: Dimension::Auto,
            height: Dimension::Auto,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Dimension {
    Auto,
    Pixels(f32),
    Percent(f32),
    Fraction(f32),
    Content,
}

impl Default for Dimension {
    fn default() -> Self {
        Self::Auto
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct LayoutAlignment {
    pub horizontal: AlignmentAxis,
    pub vertical: AlignmentAxis,
}

impl Default for LayoutAlignment {
    fn default() -> Self {
        Self {
            horizontal: AlignmentAxis::Start,
            vertical: AlignmentAxis::Start,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum AlignmentAxis {
    Start,
    Center,
    End,
    Stretch,
}

impl Default for AlignmentAxis {
    fn default() -> Self {
        Self::Start
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct StyleHooks {
    pub tokens: HashMap<String, String>,
    pub class_list: Vec<String>,
    pub inline_styles: HashMap<String, String>,
    pub custom: MetadataMap,
}

impl Default for StyleHooks {
    fn default() -> Self {
        Self {
            tokens: HashMap::new(),
            class_list: Vec::new(),
            inline_styles: HashMap::new(),
            custom: MetadataMap::default(),
        }
    }
}

pub type MetadataMap = HashMap<String, MetadataValue>;

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum MetadataValue {
    Null,
    Bool(bool),
    Number(f64),
    String(String),
    Array(Vec<MetadataValue>),
    Object(HashMap<String, MetadataValue>),
}
