#![doc = include_str!("../README.md")]
#![forbid(future_incompatible)]
#![warn(missing_docs, missing_debug_implementations, bare_trait_objects)]

use proc_macro::{Delimiter, Group, Spacing, Span, TokenStream, TokenTree};
use quote::{quote_spanned, quote};
use use_lines::{DisplayFormat, UseLines};

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
enum LastThing {
    #[default]
    Nothing,
    Colon,
    DoubleColon,
    Use,
    Ident,
    As,
    Comma,
}

#[derive(Debug, Default)]
struct ConcatUseState {
    last_thing: LastThing,
    line: String,
    use_lines: UseLines,
}

impl ConcatUseState {
    /// Convert an `UseLines::add` error into a `TokenStream` error.
    fn return_add_err(err: use_lines::Error, line: &str, span: Option<Span>) -> TokenStream {
        let err_str = err.to_string();
        match span {
            None => quote!{
                    compile_error!(
                        concat!("Failed to add ", #line, " to the use declarations: ", #err_str))
                }.into(),
            Some(span) => quote_spanned!{span.into()=>
                    compile_error!(
                        concat!("Failed to add ", #line, " to the use declarations: ", #err_str))
                }.into(),
        }
    }

    /// Add a string slice to the `UseLines`.
    fn add_str(&mut self, line: &str, span: Option<Span>) -> Result<(), TokenStream> {
        self.use_lines.add(line).map_err(|err| Self::return_add_err(err, line, span))
    }

    /// Add the current line to the `UseLines`.
    fn add_line(&mut self, span: Option<Span>) -> Result<(), TokenStream> {
        let line = self.line.as_str();
        self.use_lines.add(line).map_err(|err| Self::return_add_err(err, line, span))
    }

    /// Add the current line to the `UseLines` if the line is not empty.
    fn flush(&mut self, span: Option<Span>) -> Result<(), TokenStream> {
        if !self.line.is_empty() {
            self.add_line(span)?;
            self.line.clear();
        }
        Ok(())
    }

    fn parse_params(&mut self, stream: TokenStream) -> Result<(), TokenStream> {
        use TokenTree::*;
        let mut define_wrap_len = false;
        let mut define_indent = false;
        for token in stream {
            match token {
                Ident(ident) => {
                    define_wrap_len = false; define_indent = false;
                    let as_string = ident.to_string();
                    match as_string.as_str() {
                        "expand" => { self.use_lines.set_format(DisplayFormat::Expand); }
                        "indent" => { define_indent = true; }
                        "hidden" => { self.use_lines.set_format(DisplayFormat::Hidden); }
                        "normal" => { self.use_lines.set_format(DisplayFormat::Normal); }
                        "one_line" => { self.use_lines.set_format(DisplayFormat::OneLine); }
                        "wrap_len" => { define_wrap_len = true; }
                        _ => {
                            return Err(quote_spanned!{ident.span().into()=>
                                compile_error!("Unsuported parameter") }.into());
                        }
                    }
                }
                Punct(punct) => {
                    match punct.as_char() {
                        ',' => { /* Just ignore commas. */ }
                        '=' => {
                            if define_indent || define_wrap_len { }
                            else {
                                return Err(quote_spanned!{punct.span().into()=>
                                    compile_error!("Unsuported '=' after unrecognised parameter") }
                                    .into());
                            }
                        }
                        _ => { 
                            return Err(quote_spanned!{punct.span().into()=>
                                compile_error!("Unsuported punctuation in parameters") }.into());
                        }
                    }
                }
                Literal(literal) => {
                    if define_indent {
                        let indent = literal.to_string();
                        if indent.starts_with('"') {
                            self.use_lines.set_indent(indent.trim_matches('"'));
                        }
                        else {
                            return Err(quote_spanned!{literal.span().into()=> compile_error!(
                                "Only string literals are supported for indent parameter") }
                                .into());
                        }
                    }
                    else if define_wrap_len {
                        let wrap_len = literal.to_string();
                        match usize::from_str_radix(&wrap_len, 10) {
                            Ok(wrap_len) => { self.use_lines.set_wrap_len(wrap_len); }
                            Err(_) => {
                                return Err(quote_spanned!{literal.span().into()=> compile_error!(
                                    "Only integer literals are supported for wrap_len parameter") }
                                    .into());
                            }
                        }
                    }
                    else {
                        return Err(quote_spanned!{literal.span().into()=>
                            compile_error!("Unsuported literal in parameters") }.into());
                    }
                }
                Group(group) => {
                    let delimiter = format!("{:?}", group.delimiter());
                    return Err(quote_spanned!{group.span().into()=>
                        compile_error!(concat!("Unsuported group delimiter: ", #delimiter))}
                            .into())
                }
            }
        }
        Ok(())
    }

    fn parse_token(&mut self, token: &TokenTree) -> Result<(), TokenStream> {
        use TokenTree::*;
        match token {
            Ident(ident) => {
                let as_string = ident.to_string();
                match as_string.as_str() {
                    "use" => {
                        if LastThing::Use != self.last_thing {
                            self.last_thing = LastThing::Use;
                        }
                        else {
                            return Err(quote_spanned!{ident.span().into()=>
                                compile_error!("Two adjacent 'use' reserved words are invalid.") }
                                .into());
                        }
                    }
                    "as" => {
                        if LastThing::Ident == self.last_thing {
                            self.line.push_str(" as ");
                            self.last_thing = LastThing::As;
                        }
                        else {
                            return Err(quote_spanned!{ident.span().into()=>
                                compile_error!("Unexpected 'as' reserved word.") }
                                .into());
                        }
                    }
                    ident_str => {
                        if LastThing::Ident != self.last_thing {
                            self.line.push_str(ident_str);
                        }
                        else {
                            self.flush(Some(ident.span()))?;
                            self.line.push_str(ident_str);
                        }
                        self.last_thing = LastThing::Ident;
                    }
                }
            }
            Punct(punct) => {
                match punct.as_char() {
                    ':' => {
                        match (self.last_thing, punct.spacing()) {
                            (LastThing::Colon, _) => {
                                self.line.push_str("::");
                                self.last_thing = LastThing::DoubleColon;
                            }
                            (LastThing::DoubleColon, _) => {
                                return Err(quote_spanned!{punct.span().into()=>
                                    compile_error!("Too many adjacent colons") }.into());
                            }
                            (_, Spacing::Joint) => {
                                self.last_thing = LastThing::Colon;
                            }
                            (_, Spacing::Alone) => {
                                return Err(quote_spanned!{punct.span().into()=>
                                    compile_error!("Single colon is invalid") }.into());
                            }
                        }
                    }
                    ';' | ',' => {
                        self.flush(Some(punct.span()))?;
                        self.last_thing = LastThing::Nothing;
                    }
                    '*' => {
                        self.line.push('*');
                        self.last_thing = LastThing::Ident;
                    }
                    _ => { 
                        return Err(quote_spanned!{punct.span().into()=>
                            compile_error!("Unsuported punctuation") }.into());
                    }
                }
            }
            Literal(literal) => {
                self.flush(None)?;
                self.add_str(literal.to_string().trim_matches('"'), Some(literal.span()))?;
            }
            Group(group) => {
                match group.delimiter() {
                    Delimiter::Brace => {
                        read_group(&mut self.line, &group)?;
                        self.flush(Some(group.span()))?;
                        self.last_thing = LastThing::Nothing;
                    }
                    Delimiter::None => {
                        for token in group.stream() {
                            self.parse_token(&token)?;
                        }
                    }
                    Delimiter::Parenthesis => {
                        self.parse_params(group.stream())?;
                    }
                    Delimiter::Bracket => {
                        let delimiter = format!("{:?}", group.delimiter());
                        return Err(quote_spanned!{group.span().into()=>
                            compile_error!(concat!("Unsuported group delimiter: ", #delimiter))}
                                .into())
                    }
                }
            }
        }
        Ok(())
    }
}

/// Concatenate `use` lines for Rust documentation examples.
///
/// # Examples
///
/// The following code example starts with the following:
///
/// ```text
/// ```
///
/// ```rust
/// use concat_use::concat_use;
///
/// let use_out = concat_use!{
///     concat_use :: concat_use;
///     use use_lines::UseLines;
///     std::fs::File,
///     use std::{
///         env,
///         fs::File,
///         io::{BufWriter, Write},
///         path::Path,
///     }
///     "std::collections::HashMap",
///     "use std::collections::BTreeSet;",
/// };
///
/// assert_eq!(use_out, r#"use concat_use::concat_use;
/// use std::{
///     collections::{BTreeSet, HashMap},
///     env,
///     fs::File,
///     io::{BufWriter, Write},
///     path::Path,
/// };
/// use use_lines::UseLines;"#);
/// ```
#[proc_macro]
pub fn concat_use(input: TokenStream) -> TokenStream {
    let mut state = ConcatUseState::default();
    for token in input {
        if let Err(err_stream) = state.parse_token(&token) {
            return err_stream;
        }
    }
    if let Err(err_stream) = state.flush(None) {
        return err_stream;
    }
    let output = state.use_lines.to_string();
    quote!{#output}.into()
}

/// Read in a group into a string.
fn read_group(line: &mut String, group: &Group) -> Result<(), TokenStream> {
    if Delimiter::Brace == group.delimiter() {
        line.push('{');
        let mut last_thing = LastThing::Nothing;
        for token in group.stream() {
            match token {
                proc_macro::TokenTree::Ident(ident) => {
                    match last_thing {
                        LastThing::Ident | LastThing::Comma => {
                            line.push(' ');
                        }
                        _ => {}
                    }
                    line.push_str(&ident.to_string());
                    last_thing = LastThing::Ident;
                }
                proc_macro::TokenTree::Literal(literal) => {
                    line.push_str(&literal.to_string().trim_matches('"'));
                    last_thing = LastThing::Nothing;
                }
                proc_macro::TokenTree::Punct(punct) => {
                    line.push(punct.as_char());
                    match punct.as_char() {
                        ',' => { last_thing = LastThing::Comma; }
                        _ => { last_thing = LastThing::Nothing; }
                    }
                }
                proc_macro::TokenTree::Group(group) => {
                    read_group(line, &group)?;
                    last_thing = LastThing::Nothing;
                }
            }
        }
        line.push('}');
        Ok(())
    }
    else {
        let delimiter = format!("{:?}", group.delimiter());
        Err(quote_spanned!{group.span().into()=> compile_error!(concat!("Unsuported group delimiter: ", #delimiter))}.into())
    }
}
