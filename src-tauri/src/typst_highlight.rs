use serde::Serialize;
use typst_syntax::{highlight, LinkedNode, Tag};

#[derive(Serialize)]
pub struct HNode {
    tag: Option<&'static str>,
    text: Option<String>,
    children: Vec<HNode>,
}

fn build(node: &LinkedNode) -> HNode {
    let tag = highlight(node)
        .filter(|tag| *tag != Tag::Error)
        .map(Tag::css_class);

    let text = node.leaf_text();
    if !text.is_empty() {
        HNode {
            tag,
            text: Some(text.to_string()),
            children: Vec::new(),
        }
    } else {
        HNode {
            tag,
            text: None,
            children: node.children().map(|child| build(&child)).collect(),
        }
    }
}

#[tauri::command]
pub fn highlight_typst(source: &str) -> HNode {
    let root = typst_syntax::parse(source);
    build(&LinkedNode::new(&root))
}
