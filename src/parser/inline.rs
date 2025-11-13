use crate::parser::ast::Inline;

fn flush_text(buffer: &mut String, output: &mut Vec<Inline>) {
    if buffer.is_empty() {
        return;
    }
    output.push(Inline::Text(std::mem::take(buffer)));
}

fn emphasis_run_length(chars: &[(usize, char)], start: usize, marker: char) -> usize {
    let mut len = 0;
    let mut idx = start;
    while idx < chars.len() && chars[idx].1 == marker {
        len += 1;
        idx += 1;
    }
    len
}

fn find_matching_emphasis(
    chars: &[(usize, char)],
    start: usize,
    marker: char,
    run_len: usize,
) -> Option<usize> {
    let mut search = start + run_len;
    while search < chars.len() {
        if chars[search].1 == marker {
            let close_len = emphasis_run_length(chars, search, marker);
            if close_len == run_len && search > start + run_len {
                return Some(search);
            }
            search += max(close_len, 1);
        } else {
            search += 1;
        }
    }
    None
}

fn max(a: usize, b: usize) -> usize {
    if a > b { a } else { b }
}

fn find_char(chars: &[(usize, char)], mut start: usize, target: char) -> Option<usize> {
    while start < chars.len() {
        if chars[start].1 == target {
            return Some(start);
        }
        start += 1;
    }
    None
}

fn is_autolink(candidate: &str) -> bool {
    if candidate.is_empty() || candidate.chars().any(char::is_whitespace) {
        return false;
    }
    candidate.starts_with("http://")
        || candidate.starts_with("https://")
        || candidate.starts_with("mailto:")
        || candidate.contains('@')
}

fn parse_inlines_into(text: &str, output: &mut Vec<Inline>) {
    if text.is_empty() {
        return;
    }

    let chars: Vec<(usize, char)> = text.char_indices().collect();
    let mut buffer = String::new();
    let mut idx = 0usize;

    while idx < chars.len() {
        let (byte_idx, ch) = chars[idx];
        match ch {
            '`' => {
                if let Some(close_idx) = find_char(&chars, idx + 1, '`') {
                    flush_text(&mut buffer, output);
                    let start = byte_idx + ch.len_utf8();
                    let end = chars[close_idx].0;
                    let content = text[start..end].to_string();
                    output.push(Inline::Code(content));
                    idx = close_idx + 1;
                    continue;
                } else {
                    buffer.push(ch);
                }
            }
            '*' | '_' => {
                let run_len = emphasis_run_length(&chars, idx, ch);
                if let Some(close_idx) = find_matching_emphasis(&chars, idx, ch, run_len) {
                    flush_text(&mut buffer, output);
                    let start = chars[idx + run_len].0;
                    let end = chars[close_idx].0;
                    let inner = &text[start..end];
                    let mut nested = Vec::new();
                    parse_inlines_into(inner, &mut nested);
                    let strong = run_len >= 2;
                    output.push(Inline::Emphasis {
                        strong,
                        content: nested,
                    });
                    idx = close_idx + run_len;
                    continue;
                } else {
                    for _ in 0..run_len {
                        buffer.push(ch);
                    }
                    idx += run_len;
                    continue;
                }
            }
            '<' => {
                if let Some(close_idx) = find_char(&chars, idx + 1, '>') {
                    let start = byte_idx + ch.len_utf8();
                    let end = chars[close_idx].0;
                    let inner = &text[start..end];
                    if is_autolink(inner) {
                        flush_text(&mut buffer, output);
                        output.push(Inline::Link {
                            text: vec![Inline::Text(inner.to_string())],
                            destination: inner.to_string(),
                            title: None,
                        });
                        idx = close_idx + 1;
                        continue;
                    }
                }
                buffer.push(ch);
            }
            _ => buffer.push(ch),
        }
        idx += 1;
    }

    flush_text(&mut buffer, output);
}

/// Parses a raw inline string into `Inline` nodes.
pub fn parse_inlines_from_text(text: &str) -> Vec<Inline> {
    let mut result = Vec::new();
    parse_inlines_into(text, &mut result);
    result
}
