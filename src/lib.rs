pub mod ast;
pub mod codegen;
pub mod dom;
pub mod parser;

/// Converts a Markdown string to an HTML string.
///
/// This function orchestrates the main phases of the compiler pipeline:
/// 1.  **Parsing:** The input Markdown text is parsed into an Abstract Syntax Tree (AST).
/// 2.  **Intermediate Representation:** The AST is converted into a generic Document Object Model (DOM).
/// 3.  **Target Code Generation:** The DOM is used to generate the final HTML string.
pub fn to_html(markdown: &str) -> String {
    // 1. Parsing
    let root = parser::parse(markdown);

    // 2. AST to DOM conversion (Intermediate Representation)
    let dom = dom::from_ast(root);

    // 3. Target Code Generation
    codegen::generate_html(dom)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let markdown = "# Hello, World!";
        let expected_html = "<h1>Hello, World!</h1>";
        assert_eq!(to_html(markdown), expected_html);
    }
}
