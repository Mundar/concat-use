use concat_use::concat_use;
use concat_use_test::{
    one_line_expected,
    expand_expected,
    normal_expected,
};
use use_lines::DEFAULT_WRAP_LEN;

#[test]
fn no_use_lines() {
    const CONCAT_USE: &'static str = concat_use!();
    assert_eq!(CONCAT_USE, "");
}

#[test]
fn single_use_line_idents() {
    const CONCAT_USE: &'static str = concat_use!(std::collections::BTreeSet);
    assert_eq!(CONCAT_USE, "use std::collections::BTreeSet;");
}

#[test]
fn single_use_line_idents_with_use() {
    const CONCAT_USE: &'static str = concat_use!(use std::collections::BTreeSet;);
    assert_eq!(CONCAT_USE, "use std::collections::BTreeSet;");
}

#[test]
fn single_use_line_str() {
    const CONCAT_USE: &'static str = concat_use!("std::collections::BTreeSet");
    assert_eq!(CONCAT_USE, "use std::collections::BTreeSet;");
}

#[test]
fn single_use_line_str_with_use() {
    const CONCAT_USE: &'static str = concat_use!("use std::collections::BTreeSet;");
    assert_eq!(CONCAT_USE, "use std::collections::BTreeSet;");
}

macro_rules! normal_expected {
    ($expected:tt) => {
        normal_expected(&$expected, DEFAULT_WRAP_LEN, "")
    };
    ($expected:tt, $wrap_len:literal) => {
        normal_expected(&$expected, $wrap_len, "")
    };
}


macro_rules! impl_tests {
    (
        idents: { $(use $first:ident$(::$rest:tt)*;)* }
        expected: $expected:tt
    ) => {
        mod idents {
            use super::*;

            #[test]
            fn normal() {
                const USE_TEXT: &'static str = concat_use!{$($first$(::$rest)*)*};
                let expected = normal_expected!($expected);
                assert_eq!(USE_TEXT, expected);
            }

            #[test]
            fn normal_with_use() {
                const USE_TEXT: &'static str = concat_use!{$(use $first$(::$rest)*;)*};
                let expected = normal_expected!($expected);
                assert_eq!(USE_TEXT, expected);
            }
        }
    };
    (
        paths: { $(use $path:path;)* }
        expected: $expected:tt
    ) => {
        mod paths {
            use super::*;

            #[test]
            fn normal() {
                const USE_TEXT: &'static str = concat_use!{$($path)*};
                let expected = normal_expected!($expected);
                assert_eq!(USE_TEXT, expected);
            }

            #[test]
            fn normal_with_use() {
                const USE_TEXT: &'static str = concat_use!{$(use $path;)*};
                let expected = normal_expected!($expected);
                assert_eq!(USE_TEXT, expected);
            }
        }
    };
    (
        literals: { $($lit:literal)* }
        expected: $expected:tt
    ) => {
        mod literals {
            use super::*;

            #[test]
            fn normal() {
                const USE_TEXT: &'static str = concat_use!{$($lit )*};
                let expected = normal_expected!($expected);
                assert_eq!(USE_TEXT, expected);
            }

            #[test]
            fn one_line() {
                const USE_TEXT: &'static str = concat_use!{(one_line) $($lit )*};
                let expected = one_line_expected(&$expected, "");
                assert_eq!(USE_TEXT, expected);
            }

            #[test]
            fn hidden() {
                const USE_TEXT: &'static str = concat_use!{(hidden) $($lit )*};
                let expected = one_line_expected(&$expected, "# ");
                assert_eq!(USE_TEXT, expected);
            }

            #[test]
            fn expand() {
                const USE_TEXT: &'static str = concat_use!{(expand) $($lit )*};
                let expected = expand_expected(&$expected, "");
                assert_eq!(USE_TEXT, expected);
            }

            #[test]
            fn with_wrap_0() {
                const USE_TEXT: &'static str = concat_use!{(wrap_len = 0) $($lit )*};
                let expected = normal_expected!($expected, 0);
                assert_eq!(USE_TEXT, expected);
            }

            #[test]
            fn with_wrap_10000() {
                const USE_TEXT: &'static str = concat_use!{(wrap_len = 10000) $($lit )*};
                let expected = normal_expected!($expected, 10000);
                assert_eq!(USE_TEXT, expected);
            }
        }
    };
    (
        $mod_name:ident
        idents: $idents:tt
        literals: $literals:tt
        expected: $expected:tt
    ) => {
        mod $mod_name {
            use super::*;

            #[test]
            fn idents() {
                const USE_TEXT: &'static str = concat_use!$idents;
                let expected = normal_expected!($expected);
                assert_eq!(USE_TEXT, expected);
            }

            #[test]
            fn literals() {
                const USE_TEXT: &'static str = concat_use!$literals;
                let expected = normal_expected!($expected);
                assert_eq!(USE_TEXT, expected);
            }

            impl_tests!{
                idents: $idents
                expected: $expected
            }

            impl_tests!{
                literals: $literals
                expected: $expected
            }
        }
    };
    (
        $mod_name:ident
        paths: $paths:tt
        literals: $literals:tt
        expected: $expected:tt
    ) => {
        mod $mod_name {
            use super::*;

            #[test]
            fn paths() {
                const USE_TEXT: &'static str = concat_use!$paths;
                let expected = normal_expected!($expected);
                assert_eq!(USE_TEXT, expected);
            }

            #[test]
            fn literals() {
                const USE_TEXT: &'static str = concat_use!$literals;
                let expected = normal_expected!($expected);
                assert_eq!(USE_TEXT, expected);
            }

            impl_tests!{
                idents: $paths
                expected: $expected
            }

            impl_tests!{
                paths: $paths
                expected: $expected
            }

            impl_tests!{
                literals: $literals
                expected: $expected
            }
        }
    };
    (
        $mod_name:ident
        mixed: $mixed:tt
        expected: $expected:literal
    ) => {
        mod $mod_name {
            use super::*;

            #[test]
            fn mixed() {
                const USE_TEXT: &'static str = concat_use!$mixed;
                assert_eq!(USE_TEXT, $expected);
            }
        }
    };
}

impl_tests!{
    empty
    paths: {}
    literals: {}
    expected: []
}

impl_tests!{
    single
    idents: {use std::collections::BTreeSet;}
    literals: {"std::collections::BTreeSet"}
    expected: ["std::collections::BTreeSet"]
}

impl_tests!{
    single_with_braces
    idents: {use std::io::{Write, Seek, Read, SeekFrom};}
    literals: {"std::io::{Write, Seek, Read, SeekFrom}"}
    expected: ["std::io", "{", "Read", "Seek", "SeekFrom", "Write", "}"]
}

impl_tests!{
    multiple
    idents: {
        use std::io::Write;
        use std::io::Seek;
        use std::io::Read;
        use std::io::SeekFrom;
    }
    literals: {
        "std::io::Write"
        "std::io::Seek"
        "std::io::Read"
        "std::io::SeekFrom"
    }
    expected: ["std::io", "{", "Read", "Seek", "SeekFrom", "Write", "}"]
}
/*
macro_rules! impl_tests {
    (inputs: { $first_input:literal$(, $input:literal)* } expected: $expected:tt) => {
        #[test]
        fn normal() {
            #[allow(unused_mut)]
            let mut use_decl = UseLines::try_from($first_input).unwrap();
            $(
            use_decl.add($input).unwrap();
            )*
            let expected_str = parse_expected(&$expected, &use_decl.options);
            println!("use_decl = {:?}", use_decl);
            assert_eq!(use_decl.to_string(), expected_str);
        }

        #[test]
        fn normal_with_use() {
            #[allow(unused_mut)]
            let mut use_decl = UseLines::try_from(concat!("use ", $first_input, ";")).unwrap();
            $(
            use_decl.add(concat!("use ", $input, ";")).unwrap();
            )*
            let expected_str = parse_expected(&$expected, &use_decl.options);
            assert_eq!(use_decl.to_string(), expected_str);
        }

        #[test]
        fn normal_with_use_combined() {
            let use_decl = UseLines::try_from(concat!("use ", $first_input, ";"$(, " use ",
                $input, ";")*)).unwrap();
            let expected_str = parse_expected(&$expected, &use_decl.options);
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
            let expected_str = expand_expected(&$expected);
            println!("use_decl_str = \"{}\"\nexpected_str = {:?}", use_decl, expected_str);
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
            let expected_str = parse_expected(&$expected, &use_decl.options);
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
                let expected_str = parse_expected(&$expected, &use_decl.options);
                let use_decl_str = use_decl.to_string();
                assert_eq!(use_decl_str, expected_str, "Failed at wrap length {}: use_decl.len() = {}; expected_str.len() = {}", wrap, use_decl_str.len(), expected_str.len());
            }
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
*/
