#![doc = include_str!("../README.md")]
#![forbid(future_incompatible)]
#![warn(missing_docs, missing_debug_implementations, bare_trait_objects)]

use std::{
    collections::BTreeMap,
    fmt::{self, Debug, Display, Formatter},
    iter::Iterator,
};

/// The default value for the wrapping length.
pub const DEFAULT_WRAP_LEN: usize = 90;

/// The display format used for display of the `UseLines` structure.
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub enum DisplayFormat {
    /// The normal display
    #[default]
    /// Display the use declarations in the standard way. (default)
    ///
    /// Each use declaration starts on a new line, and if the expected length of the declaration
    /// exceeds the wrap length, it will break out to multiple lines.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use use_lines::{DisplayFormat, UseLines};
    ///
    /// let mut use_decl = UseLines::try_from("std::io::{Read, Write, Seek, SeekFrom}").unwrap()
    ///     .with_format(DisplayFormat::Normal);
    /// use_decl.add("std::io::{Error, ErrorKind}").unwrap();
    /// use_decl.add("std::io::Cursor").unwrap();
    /// use_decl.add("proptest::prelude::*").unwrap();
    /// use_decl.add("std::io::{BufWriter, BufReader}").unwrap();
    ///
    /// assert_eq!(use_decl.to_string(),
    /// r#"use proptest::prelude::*;
    /// use std::io::{
    ///     BufReader,
    ///     BufWriter,
    ///     Cursor,
    ///     Error,
    ///     ErrorKind,
    ///     Read,
    ///     Seek,
    ///     SeekFrom,
    ///     Write,
    /// };"#);
    /// ```
    Normal,
    /// Display the use declarations as one line with a '#' character after the indent string.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use use_lines::{DisplayFormat, UseLines};
    ///
    /// let mut use_decl = UseLines::try_from("std::io::{Read, Write, Seek, SeekFrom}").unwrap()
    ///     .with_format(DisplayFormat::Hidden);
    /// use_decl.add("std::io::{Error, ErrorKind}").unwrap();
    /// use_decl.add("std::io::Cursor").unwrap();
    /// use_decl.add("proptest::prelude::*").unwrap();
    /// use_decl.add("std::io::{BufWriter, BufReader}").unwrap();
    ///
    /// assert_eq!(use_decl.to_string(), concat!("# use proptest::prelude::*; ",
    ///     "use std::io::{BufReader, BufWriter, Cursor, Error, ErrorKind, Read, Seek, SeekFrom, Write};"));
    /// ```
    Hidden,
    /// Display the use declarations as one line.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use use_lines::{DisplayFormat, UseLines};
    ///
    /// let mut use_decl = UseLines::try_from("std::io::{Read, Write, Seek, SeekFrom}").unwrap()
    ///     .with_format(DisplayFormat::OneLine);
    /// use_decl.add("std::io::{Error, ErrorKind}").unwrap();
    /// use_decl.add("std::io::Cursor").unwrap();
    /// use_decl.add("proptest::prelude::*").unwrap();
    /// use_decl.add("std::io::{BufWriter, BufReader}").unwrap();
    ///
    /// assert_eq!(use_decl.to_string(), concat!("use proptest::prelude::*; ",
    ///     "use std::io::{BufReader, BufWriter, Cursor, Error, ErrorKind, Read, Seek, SeekFrom, Write};"));
    /// ```
    OneLine,
    /// Display the use declarations as one line for each individual endpoint.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use use_lines::{DisplayFormat, UseLines};
    ///
    /// let mut use_decl = UseLines::try_from("std::io::{Read, Write, Seek, SeekFrom}").unwrap()
    ///     .with_format(DisplayFormat::Expand);
    /// use_decl.add("std::io::{Error, ErrorKind}").unwrap();
    /// use_decl.add("std::io::Cursor").unwrap();
    /// use_decl.add("proptest::prelude::*").unwrap();
    /// use_decl.add("std::io::{BufWriter, BufReader}").unwrap();
    ///
    /// assert_eq!(use_decl.to_string(), r#"use proptest::prelude::*;
    /// use std::io::BufReader;
    /// use std::io::BufWriter;
    /// use std::io::Cursor;
    /// use std::io::Error;
    /// use std::io::ErrorKind;
    /// use std::io::Read;
    /// use std::io::Seek;
    /// use std::io::SeekFrom;
    /// use std::io::Write;"#);
    /// ```
    Expand,
}

#[derive(Clone, Debug)]
struct Options {
    format: DisplayFormat,
    indent: String,
    line_len: usize,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            format: DisplayFormat::Normal,
            indent: String::new(),
            line_len: DEFAULT_WRAP_LEN,
        }
    }
}

#[doc = include_str!("../README.md")]
#[derive(Clone, Debug, Default)]
pub struct UseLines {
    kids: BTreeMap<String, Node>,
    options: Options,
}

/// The node use in storing the use line data. It contains the identifier for the node and the
/// children for the node.
#[derive(Clone, Debug, Default)]
struct Node {
    kids: BTreeMap<String, Node>,
    include_self: bool,
    all: bool,
    has_as: Option<String>,
}

macro_rules! impl_parse_code {
    ($self:ident, $param:ident, {}) => {
        $self.options.$param = $param;
    };
    ($self:ident, $param:ident, { .$func:ident() }) => {
        $self.options.$param = $param.$func();
    };
}

macro_rules! impl_set_with {
    ($doc:literal
    $use_line:literal
    $example:literal
    $set_fn:ident(&self, $param:ident: $t:ty)
    $with_fn:ident($params_ex:literal)
    $code:tt
    $expected:literal) => {
        #[doc = concat!("Set the ", $doc)]
        ///
        /// ```rust
        #[doc = $use_line]
        #[doc = concat!(r#"let mut use_decl = UseLines::try_from(""#, $example, r#"").unwrap();"#)]
        #[doc = concat!("use_decl.", stringify!($set_fn), "(", $params_ex, ");")]
        #[doc = concat!(r#"assert_eq!(&use_decl.to_string(), ""#, $expected, r#"");"#)]
        /// ```
        pub fn $set_fn(&mut self, $param: $t) {
            impl_parse_code!{self, $param, $code}
        }

        #[doc = concat!("Define the ", $doc)]
        ///
        /// ```rust
        #[doc = $use_line]
        #[doc = concat!(r#"let mut use_decl = UseLines::try_from(""#, $example, r#"").unwrap()"#)]
        #[doc = concat!("    .", stringify!($with_fn), "(", $params_ex, ");")]
        #[doc = concat!(r#"assert_eq!(&use_decl.to_string(), ""#, $expected, r#"");"#)]
        /// ```
        pub fn $with_fn(mut self, $param: $t) -> Self {
            impl_parse_code!{self, $param, $code}
            self
        }
    };
}

impl UseLines {
    /// Create a new empty `UseLines` structure.
    ///
    /// ```rust
    /// use use_lines::UseLines;
    ///
    /// let use_lines = UseLines::new();
    /// assert_eq!(&use_lines.to_string(), "");
    /// ```
    pub fn new() -> Self {
        Self::default()
    }

    /// Add an identifier to the `UseLines` structure.
    fn add_ident<'a, I>(&mut self, ident: &str, split: &mut I) -> ResultKind<()>
    where
        I: Iterator<Item = &'a str>,
    {
        if ident.starts_with('{') {
            self.add_idents(ident, split)?;
        }
        else {
            if let Some(node) = self.kids.get_mut(ident) {
                node.add_ident(split)?;
            }
            else {
                let mut node = Node::default();
                node.add_ident(split)?;
                self.kids.insert(ident.to_string(), node);
            }
        }
        Ok(())
    }

    /// Add multiple identifiers to the `UseLines` structure.
    fn add_idents<'a, I>(&mut self, ident: &str, split: &mut I) -> ResultKind<()>
    where
        I: Iterator<Item = &'a str>,
    {
        let ident = ident.strip_prefix('{').ok_or(ErrorKind::InvalidBraceSyntax)?;
        if ident.ends_with('}') {
            let ident = ident.strip_suffix('}').ok_or(ErrorKind::InvalidBraceSyntax)?;
            self.add_idents_inner(ident)?;
        }
        else {
            let mut joined = ident.to_string();
            for ident in split {
                joined.push_str("::");
                joined.push_str(ident);
            }
            if joined.ends_with('}') {
                let joined = joined.strip_suffix('}').ok_or(ErrorKind::InvalidBraceSyntax)?;
                self.add_idents_inner(joined)?;
            }
            else {
                return Err(ErrorKind::InvalidBraceSyntax);
            }
        }
        Ok(())
    }

    /// Add multiple identifiers to the `UseLines` structure.
    fn add_idents_inner(&mut self, ident: &str) -> ResultKind<()> {
        let mut parts = ident.split(',');
        while let Some(next) = parts.next() {
            let next = next.trim();
            if next.contains('{') {
                let mut combined = next.to_string();
                while !combined.ends_with('}') {
                    if let Some(next) = parts.next() {
                        combined.push_str(", ");
                        combined.push_str(next);
                    }
                    else {
                        return Err(ErrorKind::InvalidBraceSyntax);
                    }
                }
                self.add_single_simple(&combined)?;
            }
            else {
                self.add_single_simple(next)?;
            }
        }
        Ok(())
    }

    /// An internal function that reads in a single use string.
    fn add_single_simple(&mut self, s: &str) -> ResultKind<()> {
        let mut split = s.split("::");
        let first = match split.next() {
            None => { return Ok(()); }
            Some("") => match split.next() {
                None => { return Ok(()); }
                Some("") => { return Err(ErrorKind::EmptyIdent); }
                Some(second) => second,
            }
            Some(first) => first,
        };
        self.add_ident(first, &mut split)?;
        Ok(())
    }

    /// Add a use declaration to the list stored in the `UseLines` structure.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use use_lines::UseLines;
    ///
    /// let mut use_lines = UseLines::default();
    /// use_lines.add("super::*").unwrap();
    /// use_lines.add("proptest::prelude::*").unwrap();
    /// use_lines.add("std::collections::BTreeMap").unwrap();
    /// use_lines.add("std::collections::HashMap").unwrap();
    /// use_lines.add("std::io::Read").unwrap();
    /// use_lines.add("std::io::Write").unwrap();
    /// use_lines.add("std::io::Seek").unwrap();
    /// use_lines.add("std::io::SeekFrom").unwrap();
    /// use_lines.add("std::fmt::{self, Display, Debug, Formatter}").unwrap();
    ///
    /// assert_eq!(&use_lines.to_string(), r#"use proptest::prelude::*;
    /// use std::{
    ///     collections::{BTreeMap, HashMap},
    ///     fmt::{self, Debug, Display, Formatter},
    ///     io::{Read, Seek, SeekFrom, Write},
    /// };
    /// use super::*;"#);
    /// ```
    pub fn add(&mut self, s: &str) -> Result<()> {
        if s.contains(';') {
            for part in s.split(';') {
                let simple = part.split_once("use").unwrap_or(("", "")).1.trim();
                if !simple.is_empty() {
                    self.add_single_simple(simple).map_err(|k| Error::BadFormat(k, s.to_string()))?;
                }
            }
        }
        else {
            let simple = s.trim();
            self.add_single_simple(simple).map_err(|k| Error::BadFormat(k, s.to_string()))?;
        }
        Ok(())
    }

    /// Add a use declaration to the list stored in the `UseLines` structure during initialization.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use use_lines::UseLines;
    ///
    /// let mut use_lines = UseLines::default()
    ///     .try_with("super::*").unwrap()
    ///     .try_with("proptest::prelude::*").unwrap()
    ///     .try_with("std::collections::BTreeMap").unwrap()
    ///     .try_with("std::collections::HashMap").unwrap()
    ///     .try_with("std::io::Read").unwrap()
    ///     .try_with("std::io::Write").unwrap()
    ///     .try_with("std::io::Seek").unwrap()
    ///     .try_with("std::io::SeekFrom").unwrap()
    ///     .try_with("std::fmt::{self, Display, Debug, Formatter}").unwrap();
    ///
    /// assert_eq!(&use_lines.to_string(), r#"use proptest::prelude::*;
    /// use std::{
    ///     collections::{BTreeMap, HashMap},
    ///     fmt::{self, Debug, Display, Formatter},
    ///     io::{Read, Seek, SeekFrom, Write},
    /// };
    /// use super::*;"#);
    /// ```
    pub fn try_with(mut self, s: &str) -> Result<Self> {
        self.add(s)?;
        Ok(self)
    }

    impl_set_with!{
        "display format of the `UseLines` when it is displayed."
        "use use_lines::{DisplayFormat, UseLines};"
        "std::cmp::{min, max}"
        set_format(&self, format: DisplayFormat)
        with_format("DisplayFormat::Hidden")
        {}
        "# use std::cmp::{max, min};"
    }

    impl_set_with!{
        "text put in front of all of the lines generated by Displaying the `UseLines`"
        "use use_lines::UseLines;"
        "std::cmp::{min, max}"
        set_indent(&self, indent: &str)
        with_indent(r#""/// ""#)
        { .to_string() }
        "/// use std::cmp::{max, min};"
    }

    impl_set_with!{
        "length of line before the display of `UseLines` displays on multiple lines. (defaults to 80)"
        "use use_lines::UseLines;"
        "std::cmp::{min, max}"
        set_wrap_len(&self, line_len: usize)
        with_wrap_len("10")
        { }
        "use std::cmp::{\n    max,\n    min,\n};"
    }
}

impl TryFrom<&str> for UseLines {
    type Error = Error;

    fn try_from(s: &str) -> Result<Self> {
        let mut use_lines = UseLines::default();
        use_lines.add(s)?;
        Ok(use_lines)
    }
}

impl Display for UseLines {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let display = DisplayHelper::from(self);
        display.write(f)
    }
}

impl Node {
    fn insert_ident<'a, I: Iterator<Item=&'a str>>(
        &mut self,
        ident: &str,
        split: &mut I,
        has_as: Option<String>,
    ) -> ResultKind<()> {
        if let Some(node) = self.kids.get_mut(ident) {
            if has_as.is_some() { node.has_as = has_as; }
            node.add_ident(split)
        }
        else {
            let mut node = Node::from(has_as);
            node.add_ident(split)?;
            self.kids.insert(ident.to_string(), node);
            Ok(())
        }
    }

    /// Add an identifier to the `UseLines` structure.
    fn add_ident<'a, I: Iterator<Item=&'a str>>(&mut self, split: &mut I) -> ResultKind<()> {
        match split.next() {
            None => {
                self.include_self = true;
                Ok(())
            }
            Some(ident) => {
                let ident = ident.trim();
                if ident.is_empty() {
                    Ok(())
                }
                else if "*" == ident {
                    self.all = true;
                    Ok(())
                }
                else if "self" == ident {
                    self.include_self = true;
                    Ok(())
                }
                else if ident.starts_with('{') {
                    self.add_idents(ident, split)
                }
                else if ident.contains(char::is_whitespace) {
                    let mut ws_split = ident.split(char::is_whitespace);
                    let ident = ws_split.next().ok_or(ErrorKind::InvalidWhitespace)?.trim();
                    let as_str: String;
                    let mut seen_as = false;
                    loop {
                        match ws_split.next() {
                            Some("as") => {
                                if seen_as {
                                    return Err(ErrorKind::MultiAs);
                                }
                                else {
                                    seen_as = true;
                                }
                            }
                            Some("") => { /* ignore mutliple whitespaces between identifiers */ }
                            Some(rename) => {
                                if seen_as {
                                    as_str = rename.to_string();
                                    break;
                                }
                                else {
                                    return Err(ErrorKind::InvalidWhitespace);
                                }
                            }
                            None => {
                                return Err(ErrorKind::InvalidWhitespace);
                            }
                        }
                    }
                    if ws_split.next().is_none() {
                        if "self" == ident {
                            self.include_self = true;
                            self.has_as = Some(as_str);
                            Ok(())
                        }
                        else {
                            self.insert_ident(ident, split, Some(as_str))
                        }
                    }
                    else {
                        Err(ErrorKind::InvalidWhitespace)
                    }
                }
                else {
                    self.insert_ident(ident, split, None)
                }
            }
        }
    }

    /// Add multiple identifiers to the `UseLines` structure.
    fn add_idents<'a, I>(&mut self, ident: &str, split: &mut I) -> ResultKind<()>
    where
        I: Iterator<Item = &'a str>,
    {
        let ident = ident.strip_prefix('{').ok_or(ErrorKind::InvalidBraceSyntax)?;
        if ident.ends_with('}') {
            let ident = ident.strip_suffix('}').ok_or(ErrorKind::InvalidBraceSyntax)?;
            self.add_idents_inner(ident)?;
        }
        else {
            let mut joined = ident.to_string();
            for ident in split {
                joined.push_str("::");
                joined.push_str(ident);
            }
            if joined.ends_with('}') {
                let joined = joined.strip_suffix('}').ok_or(ErrorKind::InvalidBraceSyntax)?;
                self.add_idents_inner(joined)?;
            }
            else {
                return Err(ErrorKind::InvalidBraceSyntax);
            }
        }
        Ok(())
    }

    fn balanced_braces(s: &str) -> bool {
        let (mut open, mut close) = (0, 0);
        for brace in s.matches(&['{', '}']) {
            match brace {
                "{" => { open += 1; }
                "}" => { close += 1; }
                _ => unreachable!(),
            }
        }
        open == close
    }

    /// Add multiple identifiers to the `UseLines` structure.
    fn add_idents_inner(&mut self, ident: &str) -> ResultKind<()> {
        let mut parts = ident.split(',');
        while let Some(next) = parts.next() {
            let next = next.trim();
            if next.contains('{') {
                let mut combined = next.to_string();
                while !Self::balanced_braces(&combined) {
                    if let Some(next) = parts.next() {
                        combined.push_str(", ");
                        combined.push_str(next);
                    }
                    else {
                        return Err(ErrorKind::InvalidBraceSyntax);
                    }
                }
                self.add_ident(&mut combined.split("::"))?;
            }
            else {
                self.add_ident(&mut next.split("::"))?;
            }
        }
        Ok(())
    }

    /// Return the number of entries in this node.
    fn count(&self) -> usize {
        let self_count = if self.include_self { 1 } else { 0 };
        if self.all {
            let mut count = 1 + self_count;
            for node in self.kids.values() {
                if node.display_when_all() { count += 1; }
            }
            count
        }
        else {
            match self.kids.len() {
                0 => 0,
                count => count + self_count,
            }
        }
    }

    /// Return true if the identifier attached to this node should be displayed if the parent node
    /// includes all symbols '*'.
    fn display_when_all(&self) -> bool {
        if !self.kids.is_empty() || self.all || self.has_as.is_some() {
            true
        }
        else {
            false
        }
    }
}

impl From<Option<String>> for Node {
    fn from(has_as: Option<String>) -> Self {
        Self {
            has_as,
            ..Default::default()
        }
    }
}

/// The individual parts of displaying use declarations.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum DisplayNode<'a> {
    Use,
    Ident(&'a str),
    As(&'a str),
    SelfDec,
    All,
    OpenBrace,
    CloseBrace,
    Comma,
    SemiColon,
}

impl<'a> DisplayNode<'a> {
    fn len(&self) -> usize {
        use DisplayNode::*;
        match self {
            Use => 4,
            Ident(ident) => ident.len(),
            As(as_str) => 4 + as_str.len(),
            SelfDec => 4,
            Comma => 2,
            All | OpenBrace | CloseBrace | SemiColon => 1,
        }
    }
}

#[derive(Clone, Debug)]
struct DisplayHelper<'a> {
    nodes: Vec<DisplayNode<'a>>,
    options: &'a Options,
}

impl<'a> From<&'a UseLines> for DisplayHelper<'a> {
    fn from(use_lines: &'a UseLines) -> DisplayHelper<'a> {
        let mut display = DisplayHelper {
            nodes: Vec::default(),
            options: &use_lines.options,
        };
        for (ident, node) in use_lines.kids.iter() {
            display.add_use();   
            display.add_ident(ident);
            display.add_node(node);
            display.add_semi();
        }
        display
    }
}

/// Generate the implementation for the simple `DisplayHelper` functions that just push a
/// `DisplayNode` to the nodes.
macro_rules! impl_simple_display_helper_funcs {
    ($func:ident(), $desc:ident) => {
        #[doc = concat!("Add the ", stringify!($desc), " node to the `DisplayHelper`.")]
        fn $func(&mut self) {
            self.nodes.push(DisplayNode::$desc);
        }
    };
    ($func:ident(consume_comma), $desc:ident) => {
        #[doc = concat!("Add the ", stringify!($desc), " node to the `DisplayHelper`.")]
        ///
        /// If it follows a `Comma`, it will replace it.
        fn $func(&mut self) {
            let len = self.nodes.len();
            if 0 < len {
                let last_node = &mut self.nodes[len-1];
                if DisplayNode::Comma == *last_node {
                    *last_node = DisplayNode::$desc;
                    return;
                }
            }
            self.nodes.push(DisplayNode::$desc);
        }
    };
    ($func:ident($($param:ident: $t:ty),*), $desc:ident) => {
        #[doc = concat!("Add the ", stringify!($desc), " node to the `DisplayHelper`.")]
        fn $func(&mut self$(, $param: $t)*) {
            self.nodes.push(DisplayNode::$desc($($param),*));
        }
    };
    ($(($func:ident$params:tt, $desc:ident),)*) => {
        $(
            impl_simple_display_helper_funcs!{$func$params, $desc}
        )*
    };
}

impl<'a> DisplayHelper<'a> {
    impl_simple_display_helper_funcs!{
        (add_use(), Use),
        (add_open(), OpenBrace),
        (add_close(consume_comma), CloseBrace),
        (add_comma(consume_comma), Comma),
        (add_semi(consume_comma), SemiColon),
        (add_self(), SelfDec),
        (add_all(), All),
        (add_ident(ident: &'a str), Ident),
        (add_as(ident: &'a str), As),
    }

    fn add_node_inner(&mut self, node: &'a Node) {
        match (node.include_self, node.all, node.has_as.as_ref(), node.kids.is_empty()) {
            (true, false, None, false) => {
                self.add_self();
                self.add_comma();
            }
            (_, false, Some(as_str), true) => {
                self.add_as(as_str);
                self.add_comma();
            }
            (_, false, Some(as_str), false) => {
                self.add_self();
                self.add_as(as_str);
                self.add_comma();
            }
            (_, true, Some(as_str), _) => {
                self.add_self();
                self.add_as(as_str);
                self.add_comma();
                self.add_all();
                self.add_comma();
            }
            (true, true, None, _) => {
                self.add_self();
                self.add_comma();
                self.add_all();
                self.add_comma();
            }
            (false, true, None, _) => {
                self.add_all();
                self.add_comma();
            }
            (_, _, _, _) => { }
        }
        if node.all {
            for (ident, node) in node.kids.iter() {
                if node.display_when_all() {
                    self.add_ident(ident);
                    self.add_node(node);
                    self.add_comma();
                }
            }
        }
        else {
            for (ident, node) in node.kids.iter() {
                self.add_ident(ident);
                self.add_node(node);
                self.add_comma();
            }
        }
    }

    fn add_node(&mut self, node: &'a Node) {
        match node.count() {
            0 | 1 => { self.add_node_inner(node); }
            _ => {
                self.add_open();
                self.add_node_inner(node);
                self.add_close();
            }
        }
    }

    fn remaining_length(&self, start_index: usize) -> usize {
        use DisplayNode::*;
        let mut ident_last = false;
        let mut brace_level = 0;
        let mut length = 0;
        for index in start_index..self.nodes.len() {
            match self.nodes[index] {
                Ident(ident) => {
                    if ident_last {
                        length += 2 + ident.len();
                    }
                    else {
                        ident_last = true;
                        length += ident.len();
                    }
                }
                All => {
                    if ident_last {
                        length += 3;
                    }
                    else {
                        ident_last = true;
                        length += 1;
                    }
                }
                OpenBrace => {
                    ident_last = false;
                    brace_level += 1;
                    length += 3;
                }
                CloseBrace => {
                    ident_last = false;
                    if 0 < brace_level {
                        brace_level -= 1;
                        length += 1;
                    }
                    else {
                        length += 1;
                        break;
                    }
                }
                SemiColon => {
                    // ident_last = false;  // Unnecessary because we always break.
                    length += 1;
                    break;
                }
                Comma => {
                    ident_last = false;
                    if 0 == brace_level {
                        length += 1;
                        break;
                    }
                    else {
                        length += 2;
                    }
                }
                node => {
                    ident_last = false;
                    length += node.len();
                }
            }
        }
        length
    }

    fn write(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use DisplayNode::*;
        match self.options.format {
            DisplayFormat::Expand => {
                let mut state = DisplayExpandState::default();
                for index in 0..self.nodes.len() {
                    let node = &self.nodes[index];
                    if state.self_seen {
                        match node {
                            As(_) => {}
                            _ => {
                                state.write_line(f, "")?;
                            }
                        }
                        state.self_seen = false;
                    }
                    match node {
                        Use => { state.use_reset(); }
                        Ident(ident) => {
                            state.add_ident(ident);
                        }
                        As(as_str) => {
                            state.write_as_str(f, as_str)?;
                        }
                        SelfDec => {
                            state.self_seen = true;
                            //state.write_line(f, "")?;
                        }
                        All => {
                            state.write_line(f, "::*")?;
                        }
                        OpenBrace => { state.open_brace(); }
                        CloseBrace => { state.close_brace(f)?; }
                        Comma | SemiColon => { state.flush(f)?; }
                    }
                }
            }
            _ => {
                let mut state = DisplayState::new(self);
                for index in 0..self.nodes.len() {
                    let node = &self.nodes[index];
                    match node {
                        Use => {
                            state.write_newline(f)?;
                            state.write_str(f, "use ")?;
                        }
                        Ident(ident) => { state.write_ident(f, ident)?; }
                        As(as_str) => {
                            state.write_str(f, " as ")?;
                            state.write_str(f, as_str)?;
                        }
                        SelfDec => {
                            state.write_str(f, "self")?;
                        }
                        All => { state.write_ident(f, "*")?; }
                        OpenBrace => { state.open_brace(f, self.remaining_length(index))?; }
                        CloseBrace => { state.close_brace(f)?; }
                        Comma => { state.comma(f)?; }
                        SemiColon => { state.write_str(f, ";")?; }
                    }
                    state.next_state(node);
                }
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
struct DisplayState<'a> {
    options: &'a Options,
    cur_len: usize,
    start: bool,
    ident_last: bool,
    newline: bool,
    newline_stack: Vec<bool>,
    indent_level: usize,
}

impl<'a> DisplayState<'a> {
    fn new(helper: &DisplayHelper<'a>) -> DisplayState<'a> {
        Self {
            options: helper.options,
            cur_len: 0,
            start: true,
            ident_last: true,
            newline: false,
            newline_stack: Vec::new(),
            indent_level: 0,
        }
    }

    fn write_str(&mut self, f: &mut Formatter<'_>, s: &str) -> fmt::Result {
        self.cur_len += s.len();
        f.write_str(s)
    }

    fn write_ident(&mut self, f: &mut Formatter<'_>, ident: &str) -> fmt::Result {
        if self.ident_last {
            f.write_str("::")?;
            self.cur_len += 2;
        }
        self.ident_last = true;
        self.cur_len += ident.len();
        f.write_str(ident)
    }

    fn write_newline(&mut self, f: &mut Formatter<'_>) -> fmt::Result {
        use DisplayFormat::*;
        if !self.start {
            match self.options.format {
                Hidden | OneLine => {
                    self.cur_len += 1;
                    return f.write_str(" ");
                }
                _ => {
                    f.write_str("\n")?;
                }
            }
        }
        else {
            self.start = false;
        }
        self.cur_len = self.options.indent.len() + (self.indent_level * 4);
        f.write_str(&self.options.indent)?;
        for _ in 0..self.indent_level {
            f.write_str("    ")?;
        }
        match self.options.format {
            Hidden => {
                self.cur_len += 2;
                return f.write_str("# ");
            }
            _ => {}
        }
        Ok(())
    }

    fn comma(&mut self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.newline {
            f.write_str(",")?;
            self.write_newline(f)
        }
        else {
            self.cur_len += 2;
            f.write_str(", ")
        }
    }

    fn open_brace(&mut self, f: &mut Formatter<'_>, remaining_length: usize) -> fmt::Result {
        use DisplayFormat::*;
        self.newline_stack.push(self.newline);
        match self.options.format {
            Hidden | OneLine => {
                self.newline = false;
                self.cur_len += 3;
                f.write_str("::{")
            }
            _ => {
                if (self.cur_len + remaining_length) >= self.options.line_len {
                    self.newline = true;
                    self.indent_level += 1;
                    // No need to update the self.cur_len because the newline resets it.
                    f.write_str("::{")?;
                    self.write_newline(f)
                }
                else {
                    self.newline = false;
                    self.cur_len += 3;
                    f.write_str("::{")
                }
            }
        }
    }

    fn close_brace(&mut self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.newline {
            if 0 < self.indent_level {
                self.indent_level -= 1;
            }
            self.comma(f)?;
        }
        self.newline = self.newline_stack.pop().unwrap_or(false);
        self.cur_len += 1;
        f.write_str("}")
    }

    fn next_state(&mut self, node: &DisplayNode) {
        self.ident_last = if let DisplayNode::Ident(_) = node { true } else { false }
    }
}

#[derive(Debug, Default)]
struct DisplayExpandState<'a> {
    this_line: Vec<&'a str>,
    this_len: usize,
    len_stack: Vec<usize>,
    not_first: bool,
    printed: bool,
    self_seen: bool,
}

impl<'a> DisplayExpandState<'a> {
    fn add_ident(&mut self, ident: &'a str) {
        self.this_line.push(ident);
        self.printed = false;
    }

    fn use_reset(&mut self) {
        self.this_line.clear();
    }

    fn write_line_inner(&mut self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.not_first { f.write_str("\n")?; }
        else { self.not_first = true; }
        f.write_str("use ")?;
        let mut not_first = false;
        for ident in self.this_line.iter() {
            if not_first { f.write_str("::")?; }
            else { not_first = true; }
            f.write_str(ident)?;
        }
        self.printed = true;
        Ok(())
    }

    fn flush(&mut self, f: &mut Formatter<'_>) -> fmt::Result {
        if !self.printed {
            self.write_line_inner(f)?;
            f.write_str(";")?;
        }
        self.this_line.truncate(self.this_len);
        Ok(())
    }

    fn write_line(&mut self, f: &mut Formatter<'_>, tail: &str) -> fmt::Result {
        self.write_line_inner(f)?;
        f.write_str(tail)?;
        f.write_str(";")
    }

    fn write_as_str(&mut self, f: &mut Formatter<'_>, as_str: &str) -> fmt::Result {
        self.write_line_inner(f)?;
        f.write_str(" as ")?;
        f.write_str(as_str)?;
        f.write_str(";")
    }

    fn open_brace(&mut self) {
        self.len_stack.push(self.this_len);
        self.this_len = self.this_line.len();
    }

    fn close_brace(&mut self, f: &mut Formatter<'_>) -> fmt::Result {
        self.flush(f)?;
        self.this_line.truncate(self.this_len);
        self.this_len = self.len_stack.pop().unwrap_or(0);
        Ok(())
    }
}

/// `UseLines` errors.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum Error {
    /// An error occured while parsing the use path.
    #[error("Invalid format: {0}: {1}")]
    BadFormat(ErrorKind, String),
}

/// Generate the example for the `ErrorKind` descriminants.
macro_rules! error_kind_example {
    ($kind:literal, $err_str:literal) => {
        error_kind_example!($kind, $kind, $err_str)
    };
    ($kind:literal, $kind_expected:literal, $err_str:literal) => {
        concat!(r#"
# Examples

```rust
use use_lines::{UseLines, Error, ErrorKind::"#, $kind, r#"};

const ERROR_STR: &'static str = ""#, $err_str, r#"";
assert_eq!(UseLines::try_from(ERROR_STR).unwrap_err(),
    Error::BadFormat("#, $kind_expected, r#", ERROR_STR.to_string()));"#)
    };
}

/// The type of error encountered while parsing the use statement data.
#[derive(Copy, Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum ErrorKind {
    /// Empty identifier in use line.
    #[doc = error_kind_example!("EmptyIdent", "::::")]
    #[error("empty identifier")]
    EmptyIdent,

    /// Indicates that whitespace was found in the middle of an itendifier.
    #[doc = error_kind_example!("InvalidWhitespace", "super::bad ident")]
    #[error("whitespace in identifier")]
    InvalidWhitespace,

    /// Indicates that more than one "as" was found while processing an as rename.
    #[doc = error_kind_example!("MultiAs", "std::collections::HashMap as as Map")]
    #[error("more than one 'as' detected")]
    MultiAs,

    /// Indicates that an error occured while parsing the brace syntax.
    #[doc = error_kind_example!("InvalidBraceSyntax", "super::{bad, brace, syntax")]
    #[error("failed to process braces")]
    InvalidBraceSyntax,
}

type Result<T> = std::result::Result<T, Error>;
type ResultKind<T> = std::result::Result<T, ErrorKind>;

#[cfg(test)]
mod tests {
    use super::*;
    use concat_use_test::{
        one_line_expected,
        expand_expected,
        normal_expected,
    };

    macro_rules! normal_expected {
        ($expected:tt, $use_decl:ident) => {
            normal_expected(&$expected, $use_decl.options.line_len, &$use_decl.options.indent)
        }
    }

    macro_rules! impl_tests {
        (inputs: { $first_input:literal$(, $input:literal)*$(,)? } expected: $expected:tt) => {
            #[test]
            fn normal() {
                #[allow(unused_mut)]
                let mut use_decl = UseLines::try_from($first_input).unwrap();
                $(
                use_decl.add($input).unwrap();
                )*
                let expected_str = normal_expected!($expected, use_decl);
                assert_eq!(use_decl.to_string(), expected_str);
            }

            #[test]
            fn normal_with_use() {
                #[allow(unused_mut)]
                let mut use_decl = UseLines::try_from(concat!("use ", $first_input, ";")).unwrap();
                $(
                use_decl.add(concat!("use ", $input, ";")).unwrap();
                )*
                let expected_str = normal_expected!($expected, use_decl);
                assert_eq!(use_decl.to_string(), expected_str);
            }

            #[test]
            fn normal_try_with() {
                #[allow(unused_mut)]
                let mut use_decl = UseLines::try_from($first_input).unwrap()
                $(
                    .try_with($input).unwrap())*;
                let expected_str = normal_expected!($expected, use_decl);
                assert_eq!(use_decl.to_string(), expected_str);
            }

            #[test]
            fn normal_try_with_use() {
                #[allow(unused_mut)]
                let mut use_decl = UseLines::try_from(concat!("use ", $first_input, ";")).unwrap()
                $(
                    .try_with(concat!("use ", $input, ";")).unwrap())*;
                let expected_str = normal_expected!($expected, use_decl);
                assert_eq!(use_decl.to_string(), expected_str);
            }

            #[test]
            fn normal_with_use_combined() {
                let use_decl = UseLines::try_from(concat!("use ", $first_input, ";"$(, " use ",
                    $input, ";")*)).unwrap();
                let expected_str = normal_expected!($expected, use_decl);
                assert_eq!(use_decl.to_string(), expected_str);
            }

            #[test]
            fn one_line() {
                #[allow(unused_mut)]
                let mut use_decl = UseLines::try_from($first_input).unwrap()
                    .with_format(DisplayFormat::OneLine);
                $(
                use_decl.add($input).unwrap();
                )*
                let expected_str = one_line_expected(&$expected, "");
                assert_eq!(use_decl.to_string(), expected_str);
            }

            #[test]
            fn hidden() {
                #[allow(unused_mut)]
                let mut use_decl = UseLines::try_from($first_input).unwrap()
                    .with_format(DisplayFormat::Hidden);
                $(
                use_decl.add($input).unwrap();
                )*
                let expected_str = one_line_expected(&$expected, "# ");
                assert_eq!(use_decl.to_string(), expected_str);
            }

            #[test]
            fn expand() {
                #[allow(unused_mut)]
                let mut use_decl = UseLines::try_from($first_input).unwrap()
                    .with_format(DisplayFormat::Expand);
                $(
                use_decl.add($input).unwrap();
                )*
                let expected_str = expand_expected(&$expected, "");
                assert_eq!(use_decl.to_string(), expected_str);
            }

            #[test]
            fn zero_wrap_len() {
                #[allow(unused_mut)]
                let mut use_decl = UseLines::try_from($first_input).unwrap()
                    .with_wrap_len(0);
                $(
                use_decl.add($input).unwrap();
                )*
                let expected_str = normal_expected!($expected, use_decl);
                assert_eq!(use_decl.to_string(), expected_str);
            }

            #[test]
            fn with_wrap_len() {
                for wrap in (10..80).rev() {
                    #[allow(unused_mut)]
                    let mut use_decl = UseLines::try_from($first_input).unwrap()
                        .with_wrap_len(wrap);
                    $(
                    use_decl.add($input).unwrap();
                    )*
                    let expected_str = normal_expected!($expected, use_decl);
                    let use_decl_str = use_decl.to_string();
                    assert_eq!(use_decl_str, expected_str, "Failed at wrap length {}: use_decl.len() = {}; expected_str.len() = {}", wrap, use_decl_str.len(), expected_str.len());
                }
            }

            #[test]
            fn recursive() {
                #[allow(unused_mut)]
                let mut use_decl = UseLines::try_from($first_input).unwrap();
                $(
                use_decl.add($input).unwrap();
                )*
                let use_recurse = UseLines::try_from(use_decl.to_string().as_str()).unwrap();
                let expected_str = normal_expected!($expected, use_decl);
                assert_eq!(use_recurse.to_string(), expected_str);
            }

            #[test]
            fn recursive_one_line() {
                #[allow(unused_mut)]
                let mut use_decl = UseLines::try_from($first_input).unwrap()
                    .with_format(DisplayFormat::OneLine);
                $(
                use_decl.add($input).unwrap();
                )*
                let use_recurse = UseLines::try_from(use_decl.to_string().as_str()).unwrap();
                let expected_str = normal_expected!($expected, use_decl);
                assert_eq!(use_recurse.to_string(), expected_str);
            }

            #[test]
            fn recursive_expand() {
                #[allow(unused_mut)]
                let mut use_decl = UseLines::try_from($first_input).unwrap()
                    .with_format(DisplayFormat::Expand);
                $(
                use_decl.add($input).unwrap();
                )*
                let use_recurse = UseLines::try_from(use_decl.to_string().as_str()).unwrap();
                let expected_str = normal_expected!($expected, use_decl);
                assert_eq!(use_recurse.to_string(), expected_str);
            }
        };
        ($name:ident inputs: $inputs:tt expected: $expected:tt) => {
            mod $name {
                use super::*;
                use crate::{DisplayFormat, UseLines};

                impl_tests!{inputs: $inputs expected: $expected}
            }
        }
    }

    #[test]
    fn no_use_lines() {
        let use_lines = UseLines::new();
        assert_eq!(&format!("{}", use_lines), "");
    }

    impl_tests!{
        null
        inputs: { "" }
        expected: []
    }

    impl_tests!{
        only_one
        inputs: { "std::sync::Mutex" }
        expected: ["std::sync::Mutex"]
    }

    impl_tests!{
        bracket
        inputs: { "std::cell::{Cell, RefCell}" }
        expected: ["std::cell", "{", "Cell", "RefCell", "}"]
    }

    impl_tests!{
        bracket_trailing_comma
        inputs: { "std::cell::{Cell, RefCell,}" }
        expected: ["std::cell", "{", "Cell", "RefCell", "}"]
    }

    impl_tests!{
        nested_bracket
        inputs: { "std::{cell::{Cell, RefCell}, io::{Read, Write}}" }
        expected: ["std", "{", "cell", "{", "Cell", "RefCell", "}", "io", "{", "Read", "Write", "}", "}"]
    }

    impl_tests!{
        top_level_bracket
        inputs: { "{once_cell::sync::{OnceCell, Lazy}, std::io::{Read, Seek, SeekFrom}}" }
        expected: ["once_cell::sync", "{", "Lazy", "OnceCell", "}", ";", "std::io", "{", "Read", "Seek", "SeekFrom", "}"]
    }

    impl_tests!{
        glob
        inputs: { "proptest::prelude::*" }
        expected: ["proptest::prelude::*"]
    }

    impl_tests!{
        complex_glob
        inputs: {
            "std::collections::HashMap",
            "std::collections::btree_map",
            "std::collections::btree_map::{Union, Intersection}",
            "std::collections::*",
            "std::collections::BTreeSet"
        }
        expected: ["std::collections", "{", "*", "btree_map", "{", "self", "Intersection", "Union", "}", "}"]
    }

    impl_tests!{
        rename_as
        inputs: { "std::io::Result as IoResult" }
        expected: ["std::io::Result as IoResult"]
    }

    impl_tests!{
        multiple_as
        inputs: {
            "std::io::Result as IoResult",
            "std::io::Error as IoError",
            "std::io::ErrorKind as IoErrorKind"
        }
        expected: ["std::io", "{", "Error as IoError", "ErrorKind as IoErrorKind", "Result as IoResult", "}"]
    }

    impl_tests!{
        glob_self
        inputs: {
            "std::collections",
            "std::collections::*"
        }
        expected: ["std::collections", "{", "self", "*", "}"]
    }

    impl_tests!{
        glob_one_level
        inputs: {
            "std::collections::*",
            "std::collections::BTreeMap",
            "std::collections::BTreeSet",
            "std::collections::HashMap",
            "std::collections::HashSet"
        }
        expected: ["std::collections::*"]
    }

    impl_tests!{
        glob_one_level_self
        inputs: {
            "std::collections::*",
            "std::collections::BTreeMap",
            "std::collections::BTreeSet",
            "std::collections::HashMap",
            "std::collections::HashSet",
            "std::collections"
        }
        expected: ["std::collections", "{", "self", "*", "}"]
    }

    impl_tests!{
        glob_one_level_self_as
        inputs: {
            "std::collections::*",
            "std::collections::BTreeMap as Map",
            "std::collections::BTreeSet",
            "std::collections::HashMap",
            "std::collections::HashSet as Set",
            "std::collections"
        }
        expected: ["std::collections", "{", "self", "*", "BTreeMap as Map", "HashSet as Set", "}"]
    }

    impl_tests!{
        glob_two_levels
        inputs: {
            "std::collections::*",
            "std::collections::BTreeMap",
            "std::collections::btree_map::Intersection",
            "std::collections::btree_map::Union",
            "std::collections::HashMap",
            "std::collections::HashSet"
        }
        expected: ["std::collections", "{", "*", "btree_map", "{", "Intersection", "Union", "}", "}"]
    }

    impl_tests!{
        glob_two_levels_self
        inputs: {
            "std::collections",
            "std::collections::*",
            "std::collections::BTreeMap",
            "std::collections::btree_map::Intersection",
            "std::collections::btree_map::Union",
            "std::collections::HashMap",
            "std::collections::HashSet"
        }
        expected: ["std::collections", "{", "self", "*", "btree_map", "{", "Intersection", "Union", "}", "}"]
    }

    impl_tests!{
        glob_two_levels_self_as
        inputs: {
            "std::collections as collect",
            "std::collections::*",
            "std::collections::BTreeMap as Map",
            "std::collections::btree_map::Intersection",
            "std::collections::btree_map::Union",
            "std::collections::HashMap",
            "std::collections::HashSet"
        }
        expected: ["std::collections", "{", "self as collect", "*", "BTreeMap as Map", "btree_map", "{", "Intersection", "Union", "}", "}"]
    }

    impl_tests!{
        one_level_self_as
        inputs: {
            "std::collections::BTreeMap as Map",
            "std::collections::BTreeSet",
            "std::collections::HashMap",
            "std::collections::HashSet as Set",
            "std::collections"
        }
        expected: ["std::collections", "{", "self", "BTreeMap as Map", "BTreeSet", "HashMap", "HashSet as Set", "}"]
    }

    impl_tests!{
        one_level_explicit_self_as
        inputs: {
            "std::collections::BTreeMap as Map",
            "std::collections::BTreeSet",
            "std::collections::HashMap",
            "std::collections::HashSet as Set",
            "std::collections::self"
        }
        expected: ["std::collections", "{", "self", "BTreeMap as Map", "BTreeSet", "HashMap", "HashSet as Set", "}"]
    }

    impl_tests!{
        one_level_as
        inputs: {
            "std::collections::BTreeMap as Map",
            "std::collections::BTreeSet",
            "std::collections::HashMap",
            "std::collections::HashSet as Set"
        }
        expected: ["std::collections", "{", "BTreeMap as Map", "BTreeSet", "HashMap", "HashSet as Set", "}"]
    }

    impl_tests!{
        one_level_self
        inputs: {
            "std::collections::BTreeMap",
            "std::collections::BTreeSet",
            "std::collections::HashMap",
            "std::collections::HashSet",
            "std::collections"
        }
        expected: ["std::collections", "{", "self", "BTreeMap", "BTreeSet", "HashMap", "HashSet", "}"]
    }

    impl_tests!{
        one_level_explicit_self
        inputs: {
            "std::collections::BTreeMap",
            "std::collections::BTreeSet",
            "std::collections::HashMap",
            "std::collections::HashSet",
            "std::collections::self",
        }
        expected: ["std::collections", "{", "self", "BTreeMap", "BTreeSet", "HashMap", "HashSet", "}"]
    }
}
