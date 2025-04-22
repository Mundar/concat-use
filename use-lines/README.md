# Use declarations structure (`UseLines`)

`UseLines` is a structure that outputs a string of `use` declarations. It
is intended to be used to generate `use` lines for documentation tests
generated in a build.rs file.

# Examples

```rust
use use_lines::{DisplayFormat, UseLines};
use std::{
    env,
    fs::{File, read_to_string},
    io::{BufWriter, Write},
    path::Path,
};

let mut use_lines = UseLines::default();
use_lines.add("std::env");
use_lines.add("std::fs::File");
use_lines.add("std::fs::read_to_string");
use_lines.add("std::io");
use_lines.add("std::io::BufWriter");
use_lines.add("std::io::{Read, Seek, SeekFrom}");
use_lines.add("std::io::Write");
use_lines.add("std::path::Path");
use_lines.add("use concat_use::concat_use;");
use_lines.add(r#"
    use use_lines::UseLines;
    use proptest::prelude::*;
"#);

assert_eq!(use_lines.clone().to_string(),
r#"use concat_use::concat_use;
use proptest::prelude::*;
use std::{
    env,
    fs::{File, read_to_string},
    io::{self, BufWriter, Read, Seek, SeekFrom, Write},
    path::Path,
};
use use_lines::UseLines;"#);

assert_eq!(use_lines.clone().with_indent("    /// ").to_string(),
r#"    /// use concat_use::concat_use;
    /// use proptest::prelude::*;
    /// use std::{
    ///     env,
    ///     fs::{File, read_to_string},
    ///     io::{self, BufWriter, Read, Seek, SeekFrom, Write},
    ///     path::Path,
    /// };
    /// use use_lines::UseLines;"#);

assert_eq!(use_lines.clone().with_format(DisplayFormat::Hidden).to_string(),
"# use concat_use::concat_use; use proptest::prelude::*; use std::{env, fs::{File, read_to_string}, io::{self, BufWriter, Read, Seek, SeekFrom, Write}, path::Path}; use use_lines::UseLines;");
```
