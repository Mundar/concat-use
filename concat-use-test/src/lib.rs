#![doc = include_str!("../README.md")]
#![forbid(future_incompatible)]
#![warn(missing_docs, missing_debug_implementations, bare_trait_objects)]

use concat_use::concat_use;
#[cfg(doc)]
use use_lines::{DisplayFormat, UseLines};

/// A macro that returns a literal string of a constant definition for the input to the assorted
/// *_expected functions.
///
/// # Examples
///
/// ## Usage
///
/// ```text
/// #[doc = concat_use!("concat_use::concat_use")]
/// ///
/// #[doc = const_expected_input!(normal)]
/// ///
/// #[doc = const_expected_input!(normal, "let expected_str = ", ";")]
/// #[doc = const_expected_input!(one_line, "# const EXPECTED_INPUT_HIDDEN: &[&str] = ", ";")]
/// /// assert_eq!(EXPECTED_INPUT, expected_str);
/// /// assert_eq!(EXPECTED_INPUT, EXPECTED_INPUT_HIDDEN);
/// ```
///
/// ## Output
///
/// ```rust
#[doc = concat_use!("concat_use::concat_use")]
///
#[doc = const_expected_input!(normal)]
///
#[doc = const_expected_input!(normal, "let expected_str = ", ";")]
#[doc = const_expected_input!(one_line, "# const EXPECTED_INPUT_HIDDEN: &[&str] = ", ";")]
/// assert_eq!(EXPECTED_INPUT, expected_str);
/// assert_eq!(EXPECTED_INPUT, EXPECTED_INPUT_HIDDEN);
/// ```
#[macro_export]
macro_rules! const_expected_input {
    (normal) => { const_expected_input!(normal, "const EXPECTED_INPUT : &[&str] = ", ";") };
    (hidden) => { const_expected_input!(one_line, "# const EXPECTED_INPUT : &[&str] = ", ";") };
    (normal, $before:literal, $after:literal) => { concat!($before, r#"&[
    "proptest::prelude::*", ";",
    "std", "{",
        "cmp", "{", "Ordering", "max", "min", "}",
        "collections", "{",
            "BTreeMap", "BTreeSet", "HashMap", "HashSet",
            "btree_set", "{", "Intersection", "Union", "}",
        "}",
        "fmt", "{", "self", "Debug", "Display", "Formatter", "}",
        "io", "{",
            "BufReader", "BufWriter", "Error as IoError", "Read",
            "Result as IoResult", "Seek", "SeekFrom", "Write",
        "}",
    "}", ";",
    "thiserror::Error"
]"#, $after)
    };
    (one_line, $before:literal, $after:literal) => { concat!($before, "&[",
        r#""proptest::prelude::*", ";", "std", "{", "cmp", "{", "Ordering", "max", "min", "}", "#,
        r#""collections", "{", "BTreeMap", "BTreeSet", "HashMap", "HashSet", "btree_set", "{", "#,
        r#""Intersection", "Union", "}", "}", "fmt", "{", "self", "Debug", "Display", "Formatter", "#,
        r#""}", "io", "{", "BufReader", "BufWriter", "Error as IoError", "Read", "#,
        r#""Result as IoResult", "Seek", "SeekFrom", "Write", "}", "}", ";", "thiserror::Error" ]"#,
        $after)
    };
}

/// A macro that returns a literal string of a constant definition for the input to the assorted
/// *_expected functions.
///
/// # Examples
///
/// ## Usage
///
/// ```text
/// #[doc = concat_use!(use concat_use::concat_use;)]
/// ///
/// #[doc = concat_use_str!(normal)]
/// ///
/// #[doc = concat_use_str!(normal, "", "const WITH_CONST_NAME: &str = ", ";")]
/// #[doc = concat_use_str!(one_line, "", "# const EXPECTED_INPUT_HIDDEN: &str = ", ";")]
/// /// assert_eq!(EXPECTED_INPUT, WITH_CONST_NAME);
/// /// assert_eq!(EXPECTED_INPUT, EXPECTED_INPUT_HIDDEN);
/// ```
///
/// ## Output
///
/// ```rust
#[doc = concat_use!(use concat_use::concat_use;)]
///
#[doc = concat_use_str!(normal)]
///
#[doc = concat_use_str!(normal, "", "const WITH_CONST_NAME: &str = ", ";")]
#[doc = concat_use_str!(one_line, "", "# const EXPECTED_OUTPUT_HIDDEN: &str = ", ";")]
/// assert_eq!(EXPECTED_OUTPUT, WITH_CONST_NAME);
/// assert_eq!(EXPECTED_OUTPUT, EXPECTED_OUTPUT_HIDDEN);
/// ```
#[macro_export]
macro_rules! concat_use_str {
    (normal) => { concat_use_str!(normal, "", "const EXPECTED_OUTPUT: &str = ", ";") };
    (hidden) => { concat_use_str!(one_line, "", "# const EXPECTED_OUTPUT: &str = ", ";") };
    (normal, $config:literal, $begin:literal, $after:literal) => { concat!($begin, "concat_use!(",
        $config, r#"
    use proptest::prelude::*;
    use std::{
        cmp::{Ordering, max, min},
        collections::{BTreeMap, BTreeSet, HashMap, HashSet, btree_set::{Intersection, Union}},
        fmt::{self, Debug, Display, Formatter},
        io::{BufReader, BufWriter, Error as IoError, Read,
            Result as IoResult, Seek, SeekFrom, Write},
    };
    use thiserror::Error;
)"#, $after)
    };
    (one_line, $config:literal, $before:literal, $after:literal) => { concat!($before,
        "concat_use!(", $config, "use proptest::prelude::*; use std::{cmp::{Ordering, ",
        "max, min}, collections::{BTreeMap, BTreeSet, HashMap, HashSet, btree_set::{Intersection, ",
        "Union}}, fmt::{self, Debug, Display, Formatter}, io::{BufReader, BufWriter, Error as ",
        "IoError, Read, Result as IoResult, Seek, SeekFrom, Write}}; use thiserror::Error;)",
        $after)
    };
    (as_str, $config:tt, $before:literal, $after:literal) => { concat!($before,
        "r#\"", concat_use!($config use proptest::prelude::*; use std::{cmp::{Ordering,
        max, min}, collections::{BTreeMap, BTreeSet, HashMap, HashSet, btree_set::{Intersection,
        Union}}, fmt::{self, Debug, Display, Formatter}, io::{BufReader, BufWriter, Error as
        IoError, Read, Result as IoResult, Seek, SeekFrom, Write}}; use thiserror::Error;), "\"#;",
        $after)
    };
}

/// Format the input slice of string slices as if the [`DisplayFormat::OneLine`] format option is
/// applied to the [`UseLines`] output.
///
/// # Examples
///
/// ```rust
#[doc = concat_use! {
    concat_use_test::one_line_expected
    concat_use::concat_use
    use_lines::UseLines
    use_lines::DisplayFormat
}]
///
#[doc = const_expected_input!(normal)]
///
#[doc = concat_use_str!(normal, "(one_line) ", "const ONE_LINE_OUTPUT: &str = ", ";")]
/// assert_eq!(&one_line_expected(EXPECTED_INPUT, ""), ONE_LINE_OUTPUT);
/// let one_line = UseLines::try_from(ONE_LINE_OUTPUT).unwrap()
///     .with_format(DisplayFormat::OneLine);
/// assert_eq!(one_line_expected(EXPECTED_INPUT, ""), one_line.to_string());
///
#[doc = concat_use_str!(normal, "(hidden) ", "const HIDDEN_OUTPUT: &str = ", ";")]
/// assert_eq!(&one_line_expected(EXPECTED_INPUT, "# "), HIDDEN_OUTPUT);
/// let hidden = one_line.with_format(DisplayFormat::Hidden);
/// assert_eq!(one_line_expected(EXPECTED_INPUT, "# "), hidden.to_string());
///
#[doc = concat_use_str!(as_str, (one_line), "const AS_STR_OUTPUT: &str = ", ";")]
/// assert_eq!(ONE_LINE_OUTPUT, AS_STR_OUTPUT);
/// ```
pub fn one_line_expected(token_list: &[&str], start: &str) -> String {
    if token_list.is_empty() { return String::new(); }
    let mut expected = String::with_capacity({
        let mut len = start.len() + 6;
        for token in token_list {
            len += token.len() + 3;
        }
        len
    });
    expected.push_str(start);
    expected.push_str("use ");
    let mut path_last = false;
    for token in token_list {
        match *token {
            "{" => {
                expected.push_str("::{");
                path_last = false;
            }
            "}" => {
                expected.push('}');
                path_last = true;
            }
            ";" => {
                expected.push_str("; use ");
                path_last = false;
            }
            path => {
                if path_last { expected.push_str(", "); }
                else { path_last = true; }
                expected.push_str(path);
            }
        }
    }
    expected.push(';');
    expected
}

/// Format the input slice of string slices as if the [`DisplayFormat::Expand`] format option is
/// applied to the [`UseLines`] output.
///
/// # Examples
///
/// ```rust
#[doc = concat_use! {
    use concat_use_test::expand_expected;
    use concat_use::concat_use;
    use use_lines::{UseLines, DisplayFormat};
}]
///
#[doc = const_expected_input!(normal)]
///
#[doc = concat_use_str!(normal, "(expand) ", "const EXPAND_OUTPUT: &str = ", ";")]
/// assert_eq!(&expand_expected(EXPECTED_INPUT, ""), EXPAND_OUTPUT);
/// let expand = UseLines::try_from(EXPAND_OUTPUT).unwrap()
///     .with_format(DisplayFormat::Expand);
/// assert_eq!(expand_expected(EXPECTED_INPUT, ""), expand.to_string());
///
#[doc = concat_use_str!(as_str, (expand), "const AS_STR_OUTPUT: &str = ", ";")]
/// assert_eq!(EXPAND_OUTPUT, AS_STR_OUTPUT);
/// ```
pub fn expand_expected(token_list: &[&str], indent: &str) -> String {
    if token_list.is_empty() { return String::new(); }
    let mut expected = String::new();
    let mut this_line = String::new();
    this_line.push_str("use ");
    let mut len_stack = Vec::new();
    let mut this_len = 4;
    let mut print = false;
    for token in token_list {
        match *token {
            "{" => {
                len_stack.push(this_len);
                this_len = this_line.len();
                print = false;
            }
            "}" => {
                if print {
                    if !expected.is_empty() { expected.push('\n'); }
                    expected.push_str(indent);
                    expected.push_str(&this_line);
                    expected.push(';');
                    print = false;
                }
                this_len = len_stack.pop().unwrap_or(4);
                this_line.truncate(this_len);
            }
            ";" => {
                if print {
                    if !expected.is_empty() { expected.push('\n'); }
                    expected.push_str(indent);
                    expected.push_str(&this_line);
                    expected.push(';');
                    print = false;
                    this_line.truncate(this_len);
                }
            }
            "self" => {
                if !expected.is_empty() { expected.push('\n'); }
                expected.push_str(indent);
                expected.push_str(&this_line);
                expected.push(';');
                print = false;
            }
            path => {
                if path.starts_with("self as ") {
                    if !expected.is_empty() { expected.push('\n'); }
                    expected.push_str(indent);
                    expected.push_str(&this_line);
                    expected.push_str(path.strip_prefix("self").unwrap());
                    expected.push(';');
                    print = false;
                }
                else {
                    if print {
                        if !expected.is_empty() { expected.push('\n'); }
                        expected.push_str(indent);
                        expected.push_str(&this_line);
                        expected.push(';');
                        this_line.truncate(this_len);
                    }
                    if 4 != this_line.len() { this_line.push_str("::"); }
                    this_line.push_str(path);
                    print = true;
                }
            }
        }
    }
    if print {
        if !expected.is_empty() { expected.push('\n'); }
        expected.push_str(indent);
        expected.push_str(&this_line);
        expected.push(';');
        this_line.truncate(this_len);
    }
    expected
}

/// Format the input slice of string slices as if the [`DisplayFormat::Normal`] format option is
/// applied to the [`UseLines`] output.
///
/// # Examples
///
/// ```rust
#[doc = concat_use! {
    "concat_use_test::normal_expected",
    "concat_use::concat_use",
    "use_lines::{UseLines, DisplayFormat}",
}]
///
#[doc = const_expected_input!(normal)]
///
#[doc = concat_use_str!(normal, "", "const EXPECTED_OUTPUT: &str = ", ";")]
/// // The default wrap length is 90.
/// assert_eq!(&normal_expected(EXPECTED_INPUT, 90, ""), EXPECTED_OUTPUT);
///
/// // The normal_expected should wrap exactly the same way as `UseLines`.
/// for wrap_len in (0..80).rev() {
///     let use_lines = UseLines::try_from(EXPECTED_OUTPUT).unwrap().with_wrap_len(wrap_len);
///     assert_eq!(normal_expected(EXPECTED_INPUT, wrap_len, ""), use_lines.to_string());
/// }
///
#[doc = concat_use_str!(as_str, (), "const AS_STR_OUTPUT: &str = ", ";")]
/// assert_eq!(EXPECTED_OUTPUT, AS_STR_OUTPUT);
/// ```
pub fn normal_expected(token_list: &[&str], wrap_len: usize, indent: &str) -> String {
    if token_list.is_empty() { return String::new(); }
    let mut expected = String::new();
    expected.push_str(indent);
    expected.push_str("use ");
    let mut this_line = String::new();
    let mut indent_level = 0;
    let mut brace_level = 0;
    let mut index = 0;
    let mut reset_index = 0;
    let mut path_last = false;
    let mut break_at_brace = false;
    let mut hidden_len = 4 + indent.len();
    while token_list.len() > index {
        match token_list[index] {
            "{" => {
                if break_at_brace {
                    expected.push_str(&this_line);
                    expected.push_str("::{\n");
                    expected.push_str(indent);
                    hidden_len = indent.len();
                    break_at_brace = false;
                    indent_level += 1;
                    this_line.clear();
                    for _ in 0..indent_level {
                        this_line.push_str("    ");
                    }
                    reset_index = index + 1;
                }
                else {
                    this_line.push_str("::{");
                    brace_level += 1;
                }
                path_last = false;
            }
            "}" => {
                if 0 < brace_level {
                    brace_level -= 1;
                    this_line.push('}');
                }
                else if 0 < indent_level {
                    expected.push_str(&this_line);
                    expected.push_str(",\n");
                    expected.push_str(indent);
                    hidden_len = indent.len();
                    reset_index = index + 1;
                    indent_level -= 1;
                    this_line.clear();
                    for _ in 0..indent_level {
                        this_line.push_str("    ");
                    }
                    this_line.push('}');
                }
                else {
                    this_line.push('}');
                }
                path_last = true;
            }
            ";" => {
                expected.push_str(&this_line);
                expected.push_str(";\n");
                expected.push_str(indent);
                expected.push_str("use ");
                hidden_len = 4 + indent.len();
                reset_index = index + 1;
                indent_level = 0;
                brace_level = 0;
                this_line.clear();
                path_last = false;
            }
            path => {
                if path_last {
                    if (0 == brace_level) && (0 < indent_level) {
                        expected.push_str(&this_line);
                        expected.push_str(",\n");
                        expected.push_str(indent);
                        hidden_len = indent.len();
                        reset_index = index;
                        this_line.clear();
                        for _ in 0..indent_level {
                            this_line.push_str("    ");
                        }
                    }
                    else {
                        this_line.push_str(", ");
                    }
                }
                else { path_last = true; }
                this_line.push_str(path);
            }
        }
        if (wrap_len > (this_line.len() + hidden_len + 1)) || break_at_brace {
            index += 1;
        }
        else {
            break_at_brace = true;
            brace_level = 0;
            index = reset_index;
            path_last = false;
            this_line.clear();
            for _ in 0..indent_level {
                this_line.push_str("    ");
            }
        }
    }
    expected.push_str(&this_line);
    expected.push(';');
    expected
}

#[cfg(test)]
mod tests {
}
