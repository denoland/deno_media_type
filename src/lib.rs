// Copyright 2018-2025 the Deno authors. MIT license.

#![deny(clippy::print_stderr)]
#![deny(clippy::print_stdout)]

use serde::Serialize;
use serde::Serializer;
use std::fmt;
use std::path::Path;

#[cfg(feature = "data_url")]
pub mod data_url;
pub mod encoding;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum MediaType {
  JavaScript,
  Jsx,
  Mjs,
  Cjs,
  TypeScript,
  Mts,
  Cts,
  Dts,
  Dmts,
  Dcts,
  Tsx,
  Css,
  Json,
  Jsonc,
  Json5,
  Html,
  Sql,
  Wasm,
  SourceMap,
  Unknown,
}

impl MediaType {
  /// Convert a MediaType to a `ts.Extension`.
  ///
  /// *NOTE* This is defined in TypeScript as a string based enum.  Changes to
  /// that enum in TypeScript should be reflected here.
  pub fn as_ts_extension(&self) -> &'static str {
    match self {
      Self::JavaScript => ".js",
      Self::Jsx => ".jsx",
      Self::Mjs => ".mjs",
      Self::Cjs => ".cjs",
      Self::TypeScript => ".ts",
      Self::Mts => ".mts",
      Self::Cts => ".cts",
      Self::Dts => ".d.ts",
      Self::Dmts => ".d.mts",
      Self::Dcts => ".d.cts",
      Self::Tsx => ".tsx",
      Self::Css => ".css",
      Self::Json => ".json",
      Self::Jsonc => ".json",
      Self::Json5 => ".json",
      Self::Html => ".html",
      Self::Sql => ".sql",
      // We transform Wasm to a declaration file.
      Self::Wasm => ".d.mts",
      // TypeScript doesn't have an "source map", so we will treat SourceMap as
      // JS for mapping purposes, though in reality, it is unlikely to ever be
      // passed to the compiler.
      Self::SourceMap => ".js",
      // TypeScript doesn't have an "unknown", so we will treat unknowns as JS
      // for mapping purposes, though in reality, it is unlikely to ever be
      // passed to the compiler.
      Self::Unknown => ".js",
    }
  }

  /// Returns `None` only for `MediaType::Unknown`.
  /// There is no 1:1 mapping between content types and MediaType.
  /// Specifically, for some `MediaType m`
  /// ```ignore
  /// MediaType::from_content_type(module_specifier, m.as_content_type()) != m
  /// ```
  pub fn as_content_type(&self) -> Option<&'static str> {
    // https://www.iana.org/assignments/media-types/media-types.xhtml
    // Web-specific with extensions: https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Common_types
    match self {
      Self::JavaScript => Some("text/javascript"),
      Self::Jsx => Some("text/jsx"),
      Self::Mjs => Some("text/javascript"),
      Self::Cjs => Some("text/javascript"),
      Self::TypeScript => Some("text/typescript"),
      Self::Mts => Some("text/typescript"),
      Self::Cts => Some("text/typescript"),
      Self::Dts => Some("text/typescript"),
      Self::Dmts => Some("text/typescript"),
      Self::Dcts => Some("text/typescript"),
      Self::Tsx => Some("text/tsx"),
      Self::Css => Some("text/css"),
      Self::Json => Some("application/json"),
      Self::Jsonc => Some("application/jsonc"),
      Self::Json5 => Some("application/json5"),
      Self::Html => Some("text/html"),
      Self::Sql => Some("application/sql"),
      Self::Wasm => Some("application/wasm"),
      Self::SourceMap => Some("application/json"),
      Self::Unknown => None,
    }
  }

  /// Gets if this media type is for a TypeScript declaration file.
  pub fn is_declaration(&self) -> bool {
    match self {
      Self::Dts | Self::Dmts | Self::Dcts => true,
      Self::JavaScript
      | Self::Jsx
      | Self::Mjs
      | Self::Cjs
      | Self::TypeScript
      | Self::Mts
      | Self::Cts
      | Self::Tsx
      | Self::Css
      | Self::Json
      | Self::Jsonc
      | Self::Json5
      | Self::Html
      | Self::Sql
      | Self::Wasm
      | Self::SourceMap
      | Self::Unknown => false,
    }
  }

  /// If the media type can be emitted to JavaScript.
  pub fn is_emittable(&self) -> bool {
    match self {
      MediaType::TypeScript
      | MediaType::Mts
      | MediaType::Cts
      | MediaType::Jsx
      | MediaType::Tsx => true,
      MediaType::JavaScript
      | MediaType::Mjs
      | MediaType::Cjs
      | MediaType::Dts
      | MediaType::Dmts
      | MediaType::Dcts
      | MediaType::Css
      | MediaType::Json
      | MediaType::Jsonc
      | MediaType::Json5
      | MediaType::Html
      | MediaType::Sql
      | MediaType::Wasm
      | MediaType::SourceMap
      | MediaType::Unknown => false,
    }
  }

  /// Gets if the media type is a `.jsx` or `.tsx` file.
  pub fn is_jsx(&self) -> bool {
    match self {
      Self::Tsx | Self::Jsx => true,
      Self::TypeScript
      | Self::Mts
      | Self::Cts
      | Self::Dts
      | Self::Dmts
      | Self::Dcts
      | Self::Json
      | Self::Jsonc
      | Self::Json5
      | Self::Html
      | Self::Sql
      | Self::Wasm
      | Self::JavaScript
      | Self::Mjs
      | Self::Cjs
      | Self::Css
      | Self::SourceMap
      | Self::Unknown => false,
    }
  }

  /// Returns true if this media type provides types inherently.
  ///
  /// Examples are TypeScript, TSX, or DTS files. Wasm and JSON files are also
  /// considered typed.
  pub fn is_typed(&self) -> bool {
    match self {
      Self::TypeScript
      | Self::Mts
      | Self::Cts
      | Self::Dts
      | Self::Dmts
      | Self::Dcts
      | Self::Tsx
      | Self::Json
      | Self::Jsonc
      | Self::Json5
      | Self::Wasm => true,
      Self::JavaScript
      | Self::Jsx
      | Self::Mjs
      | Self::Cjs
      | Self::Css
      | Self::Html
      | Self::Sql
      | Self::SourceMap
      | Self::Unknown => false,
    }
  }

  #[cfg(feature = "url")]
  pub fn from_specifier_and_headers(
    specifier: &url::Url,
    maybe_headers: Option<&std::collections::HashMap<String, String>>,
  ) -> Self {
    Self::from_specifier_and_content_type(
      specifier,
      maybe_headers.and_then(|h| h.get("content-type").map(|v| v.as_str())),
    )
  }

  #[cfg(feature = "url")]
  pub fn from_specifier_and_content_type(
    specifier: &url::Url,
    maybe_content_type: Option<&str>,
  ) -> Self {
    if let Some(content_type) = maybe_content_type {
      MediaType::from_content_type(specifier, content_type)
    } else {
      MediaType::from_specifier(specifier)
    }
  }

  #[cfg(feature = "url")]
  pub fn from_content_type<S: AsRef<str>>(
    specifier: &url::Url,
    content_type: S,
  ) -> Self {
    let first_part = content_type
      .as_ref()
      .split(';')
      .next()
      .unwrap()
      .trim()
      .to_lowercase();
    match first_part.as_str() {
      "application/typescript"
      | "text/typescript"
      | "video/vnd.dlna.mpeg-tts"
      | "video/mp2t"
      | "application/x-typescript" => {
        // This preserves legacy behavior, where if a file is served with a
        // content type of `application/javascript`, but it ends only with a `.ts`
        // we will assume that it is JavaScript and not TypeScript, but if it ends
        // with `.d.ts` we assume it is Dts.
        //
        // This handles situations where the file is transpiled on the server and
        // is explicitly providing a media type.
        map_js_like_extension(specifier, Self::TypeScript)
      }
      "application/javascript"
      | "text/javascript"
      | "application/ecmascript"
      | "text/ecmascript"
      | "application/x-javascript"
      | "application/node" => {
        map_js_like_extension(specifier, Self::JavaScript)
      }
      "text/jscript" => map_js_like_extension(specifier, Self::Jsx),
      "text/jsx" => Self::Jsx,
      "text/tsx" => Self::Tsx,
      "application/json" | "text/json" => Self::Json,
      s if s.ends_with("+json") => Self::Json,
      "application/jsonc" | "text/jsonc" => Self::Jsonc,
      "application/json5" | "text/json5" => Self::Json5,
      "application/wasm" => Self::Wasm,
      "text/css" => Self::Css,
      // Handle plain and possibly webassembly
      "text/plain" | "application/octet-stream"
        if specifier.scheme() != "data" =>
      {
        Self::from_specifier(specifier)
      }
      _ => Self::Unknown,
    }
  }

  pub fn from_path(path: &Path) -> Self {
    match path.file_name().and_then(|f| f.to_str()) {
      Some(file_name) => Self::from_filename(file_name),
      None => Self::Unknown,
    }
  }

  pub fn from_filename(file_name: &str) -> Self {
    fn has_dts_file_stem(file_stem: &str) -> bool {
      // .ts files that contain .d. in the file name are always considered a typescript declaration file.
      // See: https://github.com/microsoft/TypeScript/issues/53319#issuecomment-1474174018
      file_stem.contains(".d.")
    }

    let first_dot_pos = match file_name.rfind('.') {
      Some(i) => i,
      None => return Self::Unknown,
    };
    // using eq_ignore_ascii_case with if/elses seems to be ~40ns
    // slower here, so continue to use to_lowercase()
    let (file_stem, ext) = file_name.split_at(first_dot_pos + 1);
    let ext = ext.to_lowercase();
    match ext.as_str() {
      "ts" => {
        if has_dts_file_stem(file_stem) {
          Self::Dts
        } else {
          Self::TypeScript
        }
      }
      "mts" => {
        if has_dts_file_stem(file_stem) {
          Self::Dmts
        } else {
          Self::Mts
        }
      }
      "cts" => {
        if has_dts_file_stem(file_stem) {
          Self::Dcts
        } else {
          Self::Cts
        }
      }
      "tsx" => Self::Tsx,
      "js" => Self::JavaScript,
      "jsx" => Self::Jsx,
      "mjs" => Self::Mjs,
      "cjs" => Self::Cjs,
      "css" => Self::Css,
      "json" => Self::Json,
      "jsonc" => Self::Jsonc,
      "json5" => Self::Json5,
      "wasm" => Self::Wasm,
      "map" => Self::SourceMap,
      _ => Self::Unknown,
    }
  }

  #[allow(clippy::should_implement_trait)]
  pub fn from_str(path: &str) -> Self {
    Self::from_path(Path::new(path))
  }

  #[cfg(feature = "url")]
  pub fn from_specifier(specifier: &url::Url) -> MediaType {
    use ::data_url::DataUrl;

    if specifier.scheme() == "data" {
      if let Ok(data_url) = DataUrl::process(specifier.as_str()) {
        Self::from_content_type(specifier, data_url.mime_type().to_string())
      } else {
        Self::Unknown
      }
    } else {
      match specifier_file_name(specifier) {
        Some(file_name) => Self::from_filename(file_name),
        None => Self::Unknown,
      }
    }
  }
}

impl Default for MediaType {
  fn default() -> Self {
    Self::Unknown
  }
}

impl Serialize for MediaType {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    Serialize::serialize(&self.to_string(), serializer)
  }
}

impl fmt::Display for MediaType {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let value = match self {
      Self::JavaScript => "JavaScript",
      Self::Jsx => "JSX",
      Self::Mjs => "Mjs",
      Self::Cjs => "Cjs",
      Self::TypeScript => "TypeScript",
      Self::Mts => "Mts",
      Self::Cts => "Cts",
      Self::Dts => "Dts",
      Self::Dmts => "Dmts",
      Self::Dcts => "Dcts",
      Self::Tsx => "TSX",
      Self::Css => "Css",
      Self::Json => "Json",
      Self::Jsonc => "Jsonc",
      Self::Json5 => "Json5",
      Self::Html => "Html",
      Self::Sql => "Sql",
      Self::Wasm => "Wasm",
      Self::SourceMap => "SourceMap",
      Self::Unknown => "Unknown",
    };
    write!(f, "{}", value)
  }
}

/// Used to augment media types by using the path part of a module specifier to
/// resolve to a more accurate media type.
#[cfg(feature = "url")]
fn map_js_like_extension(
  specifier: &url::Url,
  default: MediaType,
) -> MediaType {
  let media_type = match specifier_file_name(specifier) {
    Some(file_name) => MediaType::from_filename(file_name),
    None => MediaType::Unknown,
  };
  match media_type {
    MediaType::Jsx
    | MediaType::Mjs
    | MediaType::Cjs
    | MediaType::Tsx
    | MediaType::Dts
    | MediaType::Dmts
    | MediaType::Dcts => media_type,
    MediaType::Mts => {
      if default == MediaType::JavaScript {
        MediaType::Mjs
      } else {
        MediaType::Mts
      }
    }
    MediaType::Cts => {
      if default == MediaType::JavaScript {
        MediaType::Cjs
      } else {
        MediaType::Cts
      }
    }
    MediaType::JavaScript
    | MediaType::TypeScript
    | MediaType::Css
    | MediaType::Json
    | MediaType::Jsonc
    | MediaType::Json5
    | MediaType::Html
    | MediaType::Sql
    | MediaType::Wasm
    | MediaType::SourceMap
    | MediaType::Unknown => default,
  }
}

#[cfg(feature = "url")]
fn specifier_file_name(specifier: &url::Url) -> Option<&str> {
  let path = specifier.path();
  let path = if path.is_empty() {
    // ex. deno://lib.deno.d.ts
    specifier.domain()?
  } else {
    path
  };
  let path = path.trim_end_matches('/');
  if path.is_empty() {
    None
  } else {
    match path.rfind('/') {
      Some(last_slash_index) => Some(&path[last_slash_index + 1..]),
      None => Some(path),
    }
  }
}

/// Resolve a media type and optionally the charset from a module specifier and
/// the value of a content type header.
#[cfg(feature = "url")]
pub fn resolve_media_type_and_charset_from_content_type<'a>(
  specifier: &url::Url,
  maybe_content_type: Option<&'a str>,
) -> (MediaType, Option<&'a str>) {
  if let Some(content_type) = maybe_content_type {
    let mut content_types = content_type.split(';');
    let media_type = content_types
      .next()
      .map(|content_type| MediaType::from_content_type(specifier, content_type))
      .unwrap_or(MediaType::Unknown);
    let charset = content_types
      .map(str::trim)
      .find_map(|s| s.strip_prefix("charset="));

    (media_type, charset)
  } else {
    (MediaType::from_specifier(specifier), None)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[cfg(feature = "module_specifier")]
  type ModuleSpecifier = url::Url;

  use serde_json::json;

  /// Normalize all intermediate components of the path (ie. remove "./" and "../" components).
  /// Similar to `fs::canonicalize()` but doesn't resolve symlinks.
  ///
  /// Taken from Cargo
  /// https://github.com/rust-lang/cargo/blob/af307a38c20a753ec60f0ad18be5abed3db3c9ac/src/cargo/util/paths.rs#L60-L85
  #[cfg(feature = "module_specifier")]
  fn normalize_path<P: AsRef<Path>>(path: P) -> std::path::PathBuf {
    use std::path::Component;
    use std::path::PathBuf;

    let mut components = path.as_ref().components().peekable();
    let mut ret =
      if let Some(c @ Component::Prefix(..)) = components.peek().cloned() {
        components.next();
        PathBuf::from(c.as_os_str())
      } else {
        PathBuf::new()
      };

    for component in components {
      match component {
        Component::Prefix(..) => unreachable!(),
        Component::RootDir => {
          ret.push(component.as_os_str());
        }
        Component::CurDir => {}
        Component::ParentDir => {
          ret.pop();
        }
        Component::Normal(c) => {
          ret.push(c);
        }
      }
    }
    ret
  }

  /// Returns true if the input string starts with a sequence of characters
  /// that could be a valid URI scheme, like 'https:', 'git+ssh:' or 'data:'.
  ///
  /// According to RFC 3986 (https://tools.ietf.org/html/rfc3986#section-3.1),
  /// a valid scheme has the following format:
  ///   scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
  ///
  /// We additionally require the scheme to be at least 2 characters long,
  /// because otherwise a windows path like c:/foo would be treated as a URL,
  /// while no schemes with a one-letter name actually exist.
  #[cfg(feature = "module_specifier")]
  fn specifier_has_uri_scheme(specifier: &str) -> bool {
    let mut chars = specifier.chars();
    let mut len = 0usize;
    // The first character must be a letter.
    match chars.next() {
      Some(c) if c.is_ascii_alphabetic() => len += 1,
      _ => return false,
    }
    // Second and following characters must be either a letter, number,
    // plus sign, minus sign, or dot.
    loop {
      match chars.next() {
        Some(c) if c.is_ascii_alphanumeric() || "+-.".contains(c) => len += 1,
        Some(':') if len >= 2 => return true,
        _ => return false,
      }
    }
  }

  #[cfg(feature = "module_specifier")]
  fn resolve_url(url_str: &str) -> ModuleSpecifier {
    ModuleSpecifier::parse(url_str).expect("Invalid url.")
  }

  #[cfg(feature = "module_specifier")]
  fn resolve_path(path_str: &str) -> ModuleSpecifier {
    let path = std::env::current_dir().unwrap().join(path_str);
    let path = normalize_path(path);
    ModuleSpecifier::from_file_path(path).expect("Invalid path.")
  }

  #[cfg(feature = "module_specifier")]
  fn resolve_url_or_path(specifier: &str) -> ModuleSpecifier {
    if specifier_has_uri_scheme(specifier) {
      resolve_url(specifier)
    } else {
      resolve_path(specifier)
    }
  }

  #[test]
  fn test_map_file_extension() {
    let fixtures = vec![
      ("file:///a/b/c.ts", MediaType::TypeScript),
      ("foo/bar.ts", MediaType::TypeScript),
      ("foo/bar.TS", MediaType::TypeScript),
      ("foo/bar.mts", MediaType::Mts),
      ("foo/bar.cts", MediaType::Cts),
      ("foo/bar.tsx", MediaType::Tsx),
      ("foo/bar.d.ts", MediaType::Dts),
      ("foo/bar.d.mts", MediaType::Dmts),
      ("foo/bar.d.cts", MediaType::Dcts),
      ("foo/bar.d.css.ts", MediaType::Dts),
      ("foo/bar.js", MediaType::JavaScript),
      ("foo/bar.mjs", MediaType::Mjs),
      ("foo/bar.cjs", MediaType::Cjs),
      ("foo/bar.jsx", MediaType::Jsx),
      ("foo/bar.css", MediaType::Css),
      ("foo/bar.json", MediaType::Json),
      ("foo/bar.jsonc", MediaType::Jsonc),
      ("foo/bar.json5", MediaType::Json5),
      ("foo/bar.wasm", MediaType::Wasm),
      ("foo/bar.js.map", MediaType::SourceMap),
      ("foo/bar.txt", MediaType::Unknown),
    ];

    for (path, expected) in fixtures {
      assert_eq!(
        MediaType::from_path(Path::new(path)),
        expected,
        "path: {}",
        path
      );
      assert_eq!(MediaType::from_str(path), expected);
    }
  }

  #[cfg(feature = "module_specifier")]
  #[test]
  fn test_from_specifier() {
    let fixtures = vec![
      ("file:///a/b/c.ts", MediaType::TypeScript),
      ("file:///a/b/c.mts", MediaType::Mts),
      ("file:///a/b/c.cts", MediaType::Cts),
      ("file:///a/b/c.js", MediaType::JavaScript),
      ("file:///a/b/c.mjs", MediaType::Mjs),
      ("file:///a/b/c.cjs", MediaType::Cjs),
      ("file:///a/b/c.txt", MediaType::Unknown),
      ("file:///lib.deno.d.ts", MediaType::Dts),
      ("file:///lib.deno.d.mts", MediaType::Dmts),
      ("file:///lib.deno.d.cts", MediaType::Dcts),
      ("file:///lib.deno.ts", MediaType::TypeScript),
      ("file:///file.d.css.ts", MediaType::Dts),
      ("file:///deno.js", MediaType::JavaScript),
      ("deno://lib.deno.d.ts", MediaType::Dts),
      ("deno://deno.ts", MediaType::TypeScript),
      ("deno://deno.js", MediaType::JavaScript),
      ("https://deno.land/x/mod.ts", MediaType::TypeScript),
      ("https://deno.land/x/mod.d.ts", MediaType::Dts),
      ("https://deno.land/x/mod.d.mts", MediaType::Dmts),
      ("https://deno.land/x/mod.d.cts", MediaType::Dcts),
      ("https://deno.land/x/mod.js", MediaType::JavaScript),
      ("https://deno.land/x/mod.txt", MediaType::Unknown),
      ("https://deno.land/x/mod.css", MediaType::Css),
      ("https://deno.land/x/mod.json", MediaType::Json),
      ("https://deno.land/x/mod.jsonc", MediaType::Jsonc),
      ("https://deno.land/x/mod.json5", MediaType::Json5),
      ("data:application/typescript;base64,ZXhwb3J0IGNvbnN0IGEgPSAiYSI7CgpleHBvcnQgZW51bSBBIHsKICBBLAogIEIsCiAgQywKfQo=", MediaType::TypeScript),
      ("data:application/javascript;base64,ZXhwb3J0IGNvbnN0IGEgPSAiYSI7CgpleHBvcnQgZW51bSBBIHsKICBBLAogIEIsCiAgQywKfQo=", MediaType::JavaScript),
      ("data:text/plain;base64,ZXhwb3J0IGNvbnN0IGEgPSAiYSI7CgpleHBvcnQgZW51bSBBIHsKICBBLAogIEIsCiAgQywKfQo=", MediaType::Unknown),
    ];

    for (specifier, expected) in fixtures {
      let actual = resolve_url_or_path(specifier);
      assert_eq!(
        MediaType::from_specifier(&actual),
        expected,
        "specifier: {}",
        specifier
      );

      assert_eq!(
        MediaType::from_specifier_and_headers(&actual, None),
        expected
      );
    }
  }

  #[cfg(feature = "module_specifier")]
  #[test]
  fn test_from_content_type() {
    let fixtures = vec![
      (
        "https://deno.land/x/mod.ts",
        "application/typescript",
        MediaType::TypeScript,
      ),
      (
        "https://deno.land/x/mod.ts",
        "application/javascript",
        MediaType::JavaScript,
      ),
      (
        "https://deno.land/x/mod.mts",
        "application/javascript",
        MediaType::Mjs,
      ),
      (
        "https://deno.land/x/mod.cts",
        "application/javascript",
        MediaType::Cjs,
      ),
      (
        "https://deno.land/x/mod.mts",
        "application/typescript",
        MediaType::Mts,
      ),
      (
        "https://deno.land/x/mod.cts",
        "application/typescript",
        MediaType::Cts,
      ),
      (
        "https://deno.land/x/mod.d.ts",
        "application/typescript",
        MediaType::Dts,
      ),
      (
        "https://deno.land/x/mod.d.ts",
        "application/javascript",
        MediaType::Dts,
      ),
      (
        "https://deno.land/x/mod.d.mts",
        "application/typescript",
        MediaType::Dmts,
      ),
      (
        "https://deno.land/x/mod.d.cts",
        "application/typescript",
        MediaType::Dcts,
      ),
      ("https://deno.land/x/mod.tsx", "text/tsx", MediaType::Tsx),
      (
        "https://deno.land/x/mod.js",
        "application/javascript",
        MediaType::JavaScript,
      ),
      (
        "https://deno.land/x/mod.js",
        "application/typescript",
        MediaType::TypeScript,
      ),
      (
        "https://deno.land/x/mod.mjs",
        "application/javascript",
        MediaType::Mjs,
      ),
      (
        "https://deno.land/x/mod.cjs",
        "application/javascript",
        MediaType::Cjs,
      ),
      ("https://deno.land/x/mod.jsx", "text/jsx", MediaType::Jsx),
      (
        "https://deno.land/x/mod.ts",
        "text/plain",
        MediaType::TypeScript,
      ),
      ("https://deno.land/x/mod.mts", "text/plain", MediaType::Mts),
      ("https://deno.land/x/mod.cts", "text/plain", MediaType::Cts),
      (
        "https://deno.land/x/mod.js",
        "text/plain",
        MediaType::JavaScript,
      ),
      (
        "https://deno.land/x/mod.wasm",
        "text/plain",
        MediaType::Wasm,
      ),
      (
        "https://deno.land/x/mod.jsx",
        "text/jscript",
        MediaType::Jsx,
      ),
      ("https://deno.land/x/mod.jsx", "text/css", MediaType::Css),
      (
        "https://deno.land/x/mod.jsx",
        "application/json",
        MediaType::Json,
      ),
      (
        "https://deno.land/x/mod.jsx",
        "application/jsonc",
        MediaType::Jsonc,
      ),
      (
        "https://deno.land/x/mod.jsx",
        "application/json5",
        MediaType::Json5,
      ),
    ];

    for (specifier, content_type, expected) in fixtures {
      let specifier = resolve_url_or_path(specifier);
      assert_eq!(
        MediaType::from_content_type(&specifier, content_type),
        expected,
        "specifier: {}, content_type: {}",
        specifier,
        content_type,
      );

      let mut headers = std::collections::HashMap::<String, String>::new();
      headers.insert("content-type".to_string(), content_type.to_string());
      assert_eq!(
        MediaType::from_specifier_and_headers(&specifier, Some(&headers)),
        expected
      );
    }
  }

  #[test]
  fn test_serialization() {
    assert_eq!(json!(MediaType::JavaScript), json!("JavaScript"));
    assert_eq!(json!(MediaType::Mjs), json!("Mjs"));
    assert_eq!(json!(MediaType::Cjs), json!("Cjs"));
    assert_eq!(json!(MediaType::Jsx), json!("JSX"));
    assert_eq!(json!(MediaType::TypeScript), json!("TypeScript"));
    assert_eq!(json!(MediaType::Mts), json!("Mts"));
    assert_eq!(json!(MediaType::Dts), json!("Dts"));
    assert_eq!(json!(MediaType::Dmts), json!("Dmts"));
    assert_eq!(json!(MediaType::Dcts), json!("Dcts"));
    assert_eq!(json!(MediaType::Tsx), json!("TSX"));
    assert_eq!(json!(MediaType::Css), json!("Css"));
    assert_eq!(json!(MediaType::Json), json!("Json"));
    assert_eq!(json!(MediaType::Jsonc), json!("Jsonc"));
    assert_eq!(json!(MediaType::Json5), json!("Json5"));
    assert_eq!(json!(MediaType::Wasm), json!("Wasm"));
    assert_eq!(json!(MediaType::SourceMap), json!("SourceMap"));
    assert_eq!(json!(MediaType::Unknown), json!("Unknown"));
  }

  #[test]
  fn test_display() {
    assert_eq!(MediaType::JavaScript.to_string(), "JavaScript");
    assert_eq!(MediaType::Mjs.to_string(), "Mjs");
    assert_eq!(MediaType::Cjs.to_string(), "Cjs");
    assert_eq!(MediaType::Jsx.to_string(), "JSX");
    assert_eq!(MediaType::TypeScript.to_string(), "TypeScript");
    assert_eq!(MediaType::Mts.to_string(), "Mts");
    assert_eq!(MediaType::Cts.to_string(), "Cts");
    assert_eq!(MediaType::Dts.to_string(), "Dts");
    assert_eq!(MediaType::Dmts.to_string(), "Dmts");
    assert_eq!(MediaType::Dcts.to_string(), "Dcts");
    assert_eq!(MediaType::Tsx.to_string(), "TSX");
    assert_eq!(MediaType::Css.to_string(), "Css");
    assert_eq!(MediaType::Json.to_string(), "Json");
    assert_eq!(MediaType::Jsonc.to_string(), "Jsonc");
    assert_eq!(MediaType::Json5.to_string(), "Json5");
    assert_eq!(MediaType::Wasm.to_string(), "Wasm");
    assert_eq!(MediaType::SourceMap.to_string(), "SourceMap");
    assert_eq!(MediaType::Unknown.to_string(), "Unknown");
  }

  #[cfg(feature = "module_specifier")]
  macro_rules! file_url {
    ($path:expr) => {
      if cfg!(target_os = "windows") {
        concat!("file:///C:", $path)
      } else {
        concat!("file://", $path)
      }
    };
  }

  #[cfg(feature = "module_specifier")]
  #[test]
  fn test_resolve_media_type_and_charset_from_content_type() {
    let fixtures = vec![
      // Extension only
      (file_url!("/foo/bar.ts"), None, MediaType::TypeScript, None),
      (file_url!("/foo/bar.tsx"), None, MediaType::Tsx, None),
      (file_url!("/foo/bar.d.cts"), None, MediaType::Dcts, None),
      (file_url!("/foo/bar.d.mts"), None, MediaType::Dmts, None),
      (file_url!("/foo/bar.d.ts"), None, MediaType::Dts, None),
      (file_url!("/foo/bar.js"), None, MediaType::JavaScript, None),
      (file_url!("/foo/bar.jsx"), None, MediaType::Jsx, None),
      (file_url!("/foo/bar.json"), None, MediaType::Json, None),
      (file_url!("/foo/bar.jsonc"), None, MediaType::Jsonc, None),
      (file_url!("/foo/bar.json5"), None, MediaType::Json5, None),
      (file_url!("/foo/bar.wasm"), None, MediaType::Wasm, None),
      (file_url!("/foo/bar.cjs"), None, MediaType::Cjs, None),
      (file_url!("/foo/bar.mjs"), None, MediaType::Mjs, None),
      (file_url!("/foo/bar.cts"), None, MediaType::Cts, None),
      (file_url!("/foo/bar.mts"), None, MediaType::Mts, None),
      (file_url!("/foo/bar"), None, MediaType::Unknown, None),
      // Media type no extension
      (
        "https://deno.land/x/mod",
        Some("application/typescript".to_string()),
        MediaType::TypeScript,
        None,
      ),
      (
        "https://deno.land/x/mod",
        Some("text/typescript".to_string()),
        MediaType::TypeScript,
        None,
      ),
      (
        "https://deno.land/x/mod",
        Some("video/vnd.dlna.mpeg-tts".to_string()),
        MediaType::TypeScript,
        None,
      ),
      (
        "https://deno.land/x/mod",
        Some("video/mp2t".to_string()),
        MediaType::TypeScript,
        None,
      ),
      (
        "https://deno.land/x/mod",
        Some("application/x-typescript".to_string()),
        MediaType::TypeScript,
        None,
      ),
      (
        "https://deno.land/x/mod",
        Some("application/javascript".to_string()),
        MediaType::JavaScript,
        None,
      ),
      (
        "https://deno.land/x/mod",
        Some("text/javascript".to_string()),
        MediaType::JavaScript,
        None,
      ),
      (
        "https://deno.land/x/mod",
        Some("application/ecmascript".to_string()),
        MediaType::JavaScript,
        None,
      ),
      (
        "https://deno.land/x/mod",
        Some("text/ecmascript".to_string()),
        MediaType::JavaScript,
        None,
      ),
      (
        "https://deno.land/x/mod",
        Some("application/x-javascript".to_string()),
        MediaType::JavaScript,
        None,
      ),
      (
        "https://deno.land/x/mod",
        Some("application/node".to_string()),
        MediaType::JavaScript,
        None,
      ),
      (
        "https://deno.land/x/mod",
        Some("text/jsx".to_string()),
        MediaType::Jsx,
        None,
      ),
      (
        "https://deno.land/x/mod",
        Some("text/tsx".to_string()),
        MediaType::Tsx,
        None,
      ),
      (
        "https://deno.land/x/mod",
        Some("text/json".to_string()),
        MediaType::Json,
        None,
      ),
      (
        "https://deno.land/x/mod",
        Some("text/json; charset=utf-8".to_string()),
        MediaType::Json,
        Some("utf-8".to_string()),
      ),
      (
        "https://deno.land/x/mod",
        Some("application/geo+json".to_string()),
        MediaType::Json,
        None,
      ),
      (
        "https://deno.land/x/mod",
        Some("text/jsonc".to_string()),
        MediaType::Jsonc,
        None,
      ),
      (
        "https://deno.land/x/mod",
        Some("text/json5".to_string()),
        MediaType::Json5,
        None,
      ),
      // Extension with media type
      (
        "https://deno.land/x/mod.ts",
        Some("text/plain".to_string()),
        MediaType::TypeScript,
        None,
      ),
      (
        "https://deno.land/x/mod.ts",
        Some("foo/bar".to_string()),
        MediaType::Unknown,
        None,
      ),
      (
        "https://deno.land/x/mod.tsx",
        Some("application/typescript".to_string()),
        MediaType::Tsx,
        None,
      ),
      (
        "https://deno.land/x/mod.tsx",
        Some("application/javascript".to_string()),
        MediaType::Tsx,
        None,
      ),
      (
        "https://deno.land/x/mod.jsx",
        Some("application/javascript".to_string()),
        MediaType::Jsx,
        None,
      ),
      (
        "https://deno.land/x/mod.jsx",
        Some("application/x-typescript".to_string()),
        MediaType::Jsx,
        None,
      ),
      (
        "https://deno.land/x/mod.d.ts",
        Some("application/javascript".to_string()),
        MediaType::Dts,
        None,
      ),
      (
        "https://deno.land/x/mod.d.ts",
        Some("text/plain".to_string()),
        MediaType::Dts,
        None,
      ),
      (
        "https://deno.land/x/mod.d.ts",
        Some("application/x-typescript".to_string()),
        MediaType::Dts,
        None,
      ),
    ];

    for (specifier, maybe_content_type, media_type, maybe_charset) in fixtures {
      let specifier = ModuleSpecifier::parse(specifier).unwrap();
      assert_eq!(
        resolve_media_type_and_charset_from_content_type(
          &specifier,
          maybe_content_type.as_deref()
        ),
        (media_type, maybe_charset.as_deref())
      );
    }
  }
}
