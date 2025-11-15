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

fn parse_link_destination(text: &str, start: usize) -> Option<(usize, String)> {
    let chars: Vec<(usize, char)> = text.char_indices().collect();
    if start >= chars.len() {
        return None;
    }

    let (byte_idx, ch) = chars[start];

    // Handle <...> destination
    if ch == '<' {
        if let Some(close_idx) = find_char(&chars, start + 1, '>') {
            let start_byte = byte_idx + ch.len_utf8();
            let end_byte = chars[close_idx].0;
            let dest = text[start_byte..end_byte].to_string();
            return Some((close_idx + 1, dest));
        }
        return None;
    }

    // Handle plain destination (no < >)
    let mut end = start;
    let mut paren_depth = 0;
    while end < chars.len() {
        let (_, ch) = chars[end];
        match ch {
            '(' => paren_depth += 1,
            ')' if paren_depth > 0 => paren_depth -= 1,
            ')' if paren_depth == 0 => break,
            ' ' | '\t' | '\n' | '\r' => break,
            _ if ch.is_control() => break,
            _ => {}
        }
        end += 1;
    }

    if end > start {
        let dest = text[byte_idx..chars[end].0].to_string();
        return Some((end, dest));
    }

    None
}

fn parse_link_title(text: &str, start: usize) -> Option<(usize, String)> {
    let chars: Vec<(usize, char)> = text.char_indices().collect();
    if start >= chars.len() {
        return None;
    }

    let (byte_idx, ch) = chars[start];

    match ch {
        '"' => {
            // Double-quoted title
            let mut end = start + 1;
            let mut escaped = false;
            while end < chars.len() {
                let (_, ch) = chars[end];
                if escaped {
                    escaped = false;
                    end += 1;
                    continue;
                }
                if ch == '\\' {
                    escaped = true;
                    end += 1;
                    continue;
                }
                if ch == '"' {
                    let start_byte = byte_idx + 1;
                    let end_byte = chars[end].0;
                    let title = text[start_byte..end_byte].to_string();
                    return Some((end + 1, title));
                }
                end += 1;
            }
            None
        }
        '\'' => {
            // Single-quoted title
            let mut end = start + 1;
            let mut escaped = false;
            while end < chars.len() {
                let (_, ch) = chars[end];
                if escaped {
                    escaped = false;
                    end += 1;
                    continue;
                }
                if ch == '\\' {
                    escaped = true;
                    end += 1;
                    continue;
                }
                if ch == '\'' {
                    let start_byte = byte_idx + 1;
                    let end_byte = chars[end].0;
                    let title = text[start_byte..end_byte].to_string();
                    return Some((end + 1, title));
                }
                end += 1;
            }
            None
        }
        '(' => {
            // Parenthesized title
            let mut end = start + 1;
            let mut paren_depth = 1;
            let mut escaped = false;
            while end < chars.len() {
                let (_, ch) = chars[end];
                if escaped {
                    escaped = false;
                    end += 1;
                    continue;
                }
                if ch == '\\' {
                    escaped = true;
                    end += 1;
                    continue;
                }
                if ch == '(' {
                    paren_depth += 1;
                } else if ch == ')' {
                    paren_depth -= 1;
                    if paren_depth == 0 {
                        let start_byte = byte_idx + 1;
                        let end_byte = chars[end].0;
                        let title = text[start_byte..end_byte].to_string();
                        return Some((end + 1, title));
                    }
                }
                end += 1;
            }
            None
        }
        _ => None,
    }
}

fn parse_inline_link(
    text: &str,
    _link_text_start_char: usize,
    link_text_end_char: usize,
) -> Option<(usize, String, Option<String>)> {
    let chars: Vec<(usize, char)> = text.char_indices().collect();

    if link_text_end_char >= chars.len() {
        return None;
    }

    // link_text_end_char points to the ']' character
    // We need to start from the character after ']'
    let mut idx = link_text_end_char + 1;

    // Skip whitespace after ]
    while idx < chars.len() {
        let (_, ch) = chars[idx];
        if ch != ' ' && ch != '\t' {
            break;
        }
        idx += 1;
    }

    if idx >= chars.len() || chars[idx].1 != '(' {
        return None;
    }

    idx += 1; // Skip (

    // Skip whitespace
    while idx < chars.len() {
        let (_, ch) = chars[idx];
        if ch != ' ' && ch != '\t' {
            break;
        }
        idx += 1;
    }

    // Parse destination
    let (dest_end, destination) = parse_link_destination(text, idx)?;
    idx = dest_end;

    // Skip whitespace
    while idx < chars.len() {
        let (_, ch) = chars[idx];
        if ch != ' ' && ch != '\t' {
            break;
        }
        idx += 1;
    }

    // Parse optional title
    let title = if idx < chars.len() {
        parse_link_title(text, idx).map(|(end, t)| {
            idx = end;
            t
        })
    } else {
        None
    };

    // Skip whitespace before closing )
    while idx < chars.len() {
        let (_, ch) = chars[idx];
        if ch != ' ' && ch != '\t' {
            break;
        }
        idx += 1;
    }

    if idx >= chars.len() || chars[idx].1 != ')' {
        return None;
    }

    // Return character index after )
    Some((idx + 1, destination, title))
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
            '!' => {
                // Check for image: ![alt](url)
                if idx + 1 < chars.len() && chars[idx + 1].1 == '[' {
                    // Try to find matching ]
                    let mut bracket_depth = 1;
                    let mut search_idx = idx + 2;
                    let mut escaped = false;
                    while search_idx < chars.len() {
                        let (_, ch) = chars[search_idx];
                        if escaped {
                            escaped = false;
                            search_idx += 1;
                            continue;
                        }
                        if ch == '\\' {
                            escaped = true;
                            search_idx += 1;
                            continue;
                        }
                        if ch == '[' {
                            bracket_depth += 1;
                        } else if ch == ']' {
                            bracket_depth -= 1;
                            if bracket_depth == 0 {
                                // Found matching ], now check for (url) or [ref]
                                let alt_start = chars[idx + 2].0;
                                let alt_end = chars[search_idx].0;
                                let alt_text = text[alt_start..alt_end].to_string();

                                // Try inline link
                                if let Some((end_idx, destination, title)) =
                                    parse_inline_link(text, idx + 2, search_idx)
                                {
                                    flush_text(&mut buffer, output);
                                    output.push(Inline::Image {
                                        alt: alt_text,
                                        destination,
                                        title,
                                    });
                                    idx = end_idx;
                                    continue;
                                }

                                // TODO: Handle reference-style images [ref]
                                break;
                            }
                        }
                        search_idx += 1;
                    }
                }
                buffer.push(ch);
            }
            '[' => {
                // Check for link: [text](url) or [text][ref]
                let mut bracket_depth = 1;
                let mut search_idx = idx + 1;
                let mut escaped = false;
                while search_idx < chars.len() {
                    let (_, ch) = chars[search_idx];
                    if escaped {
                        escaped = false;
                        search_idx += 1;
                        continue;
                    }
                    if ch == '\\' {
                        escaped = true;
                        search_idx += 1;
                        continue;
                    }
                    if ch == '[' {
                        bracket_depth += 1;
                    } else if ch == ']' {
                        bracket_depth -= 1;
                        if bracket_depth == 0 {
                            // Found matching ], now check for (url) or [ref]
                            let link_text_start = byte_idx + 1;
                            let link_text_end = chars[search_idx].0;
                            let link_text = text[link_text_start..link_text_end].to_string();

                            // Try inline link
                            if let Some((end_idx, destination, title)) =
                                parse_inline_link(text, idx + 1, search_idx)
                            {
                                flush_text(&mut buffer, output);
                                let mut nested = Vec::new();
                                parse_inlines_into(&link_text, &mut nested);
                                output.push(Inline::Link {
                                    text: nested,
                                    destination,
                                    title,
                                });
                                idx = end_idx;
                                continue;
                            }

                            // TODO: Handle reference-style links [ref]
                            break;
                        }
                    }
                    search_idx += 1;
                }
                buffer.push(ch);
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
