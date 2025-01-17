// Copyright 2018-2025 the Deno authors. MIT license.

pub const BOM_CHAR: char = '\u{FEFF}';

/// Strips the byte order mark if it exists from the provided text.
pub fn strip_bom_mut(text: &mut String) {
  if text.starts_with(BOM_CHAR) {
    text.drain(..BOM_CHAR.len_utf8());
  }
}

/// Attempts to detect the character encoding of the provided bytes.
///
/// Supports UTF-8, UTF-16 Little Endian and UTF-16 Big Endian.
#[cfg(feature = "url")]
pub fn detect_charset(
  specifier: &url::Url,
  bytes: &'_ [u8]
) -> &'static str {
  const UTF16_LE_BOM: &[u8] = b"\xFF\xFE";
  const UTF16_BE_BOM: &[u8] = b"\xFE\xFF";

  if specifier.scheme() == "file" {
    if bytes.starts_with(UTF16_LE_BOM) {
      "utf-16le"
    } else if bytes.starts_with(UTF16_BE_BOM) {
      "utf-16be"
    } else {
      // Assume everything else is utf-8
      "utf-8"
    }
  } else {
    "utf-8"
  }
}

#[cfg(feature = "decoding")]
pub fn decode_owned_source(
  charset: &str,
  bytes: Vec<u8>,
) -> Result<String, std::io::Error> {
  match convert_to_utf8(&bytes, charset)? {
    std::borrow::Cow::Borrowed(text) => {
      if text.starts_with(BOM_CHAR) {
        Ok(text[BOM_CHAR.len_utf8()..].to_string())
      } else {
        Ok(
          // SAFETY: we know it's a valid utf-8 string at this point
          unsafe { String::from_utf8_unchecked(bytes) },
        )
      }
    }
    std::borrow::Cow::Owned(mut text) => {
      strip_bom_mut(&mut text);
      Ok(text)
    }
  }
}

/// Decodes the source bytes into a string handling any encoding rules
/// for local vs remote files and dealing with the charset.
#[cfg(feature = "decoding")]
pub fn decode_arc_source(
  charset: &str,
  bytes: std::sync::Arc<[u8]>,
) -> Result<std::sync::Arc<str>, std::io::Error> {
  use std::sync::Arc;

  let text = match convert_to_utf8(bytes.as_ref(), charset)? {
    std::borrow::Cow::Borrowed(text) => {
      if text.starts_with(BOM_CHAR) {
        text[BOM_CHAR.len_utf8()..].to_string()
      } else {
        return Ok(
          // SAFETY: we know it's a valid utf-8 string at this point
          unsafe {
            let raw_ptr = Arc::into_raw(bytes);
            Arc::from_raw(std::mem::transmute::<*const [u8], *const str>(
              raw_ptr,
            ))
          },
        );
      }
    }
    std::borrow::Cow::Owned(mut text) => {
      strip_bom_mut(&mut text);
      text
    }
  };
  let text: Arc<str> = Arc::from(text);
  Ok(text)
}

/// Attempts to convert the provided bytes to a UTF-8 string.
///
/// Supports all encodings supported by the encoding_rs crate, which includes
/// all encodings specified in the WHATWG Encoding Standard, and only those
/// encodings (see: <https://encoding.spec.whatwg.org/>).
#[cfg(feature = "decoding")]
pub fn convert_to_utf8<'a>(
  bytes: &'a [u8],
  charset: &'_ str,
) -> Result<std::borrow::Cow<'a, str>, std::io::Error> {
  match encoding_rs::Encoding::for_label(charset.as_bytes()) {
    Some(encoding) => Ok(encoding.decode_without_bom_handling(bytes).0),
    None => Err(std::io::Error::new(
      std::io::ErrorKind::InvalidInput,
      format!("Unsupported charset: {charset}"),
    )),
  }
}

#[cfg(test)]
mod test {

  use super::*;

  #[cfg(feature = "url")]
  mod detection_tests {
    use super::*;

    fn run_detection_test(test_data: &[u8], expected_charset: &str) {
      let detected_charset = detect_charset(
        &url::Url::parse("file:///file.txt").unwrap(),
        test_data
      );
      assert_eq!(
        expected_charset.to_lowercase(),
        detected_charset.to_lowercase()
      );
    }

    #[test]
    fn run_detection_test_utf8_no_bom() {
      let test_data = "Hello UTF-8 it is \u{23F0} for Deno!"
        .to_owned()
        .into_bytes();
      run_detection_test(&test_data, "utf-8");
    }

    #[test]
    fn run_detection_test_utf16_little_endian() {
      let test_data = b"\xFF\xFEHello UTF-16LE".to_owned().to_vec();
      run_detection_test(&test_data, "utf-16le");
    }

    #[test]
    fn run_detection_test_utf16_big_endian() {
      let test_data = b"\xFE\xFFHello UTF-16BE".to_owned().to_vec();
      run_detection_test(&test_data, "utf-16be");
    }
  }

  #[test]
  fn strip_bom_mut_with_bom() {
    let mut text = format!("{BOM_CHAR}text");
    strip_bom_mut(&mut text);
    assert_eq!(text, "text");
  }

  #[test]
  fn strip_bom_mut_without_bom() {
    let mut text = "text".to_string();
    strip_bom_mut(&mut text);
    assert_eq!(text, "text");
  }

  #[cfg(feature = "decoding")]
  #[test]
  fn test_decoding_unsupported_charset() {
    let test_data = Vec::new();
    let result = convert_to_utf8(&test_data, "utf-32le");
    assert!(result.is_err());
    let err = result.expect_err("Err expected");
    assert!(err.kind() == std::io::ErrorKind::InvalidInput);
  }

  #[cfg(feature = "decoding")]
  #[test]
  fn test_decoding_invalid_utf8() {
    let test_data = b"\xFE\xFE\xFF\xFF".to_vec();
    let result = convert_to_utf8(&test_data, "utf-8");
    assert!(result.is_ok());
  }

  
  #[cfg(feature = "decoding")]
  #[test]
  fn test_decode_owned_with_bom() {
    let bytes = format!("{}{}", BOM_CHAR, "Hello").into_bytes();
    let text = decode_owned_source(
      detect_charset(
        &url::Url::parse("file:///file.txt").unwrap(),
        &bytes
      ),
      bytes,
    )
    .unwrap();
    assert_eq!(text, "Hello");
  }

  #[cfg(feature = "decoding")]
  #[test]
  fn test_decode_with_charset_with_bom() {
    let bytes = format!("{}{}", BOM_CHAR, "Hello").into_bytes();
    let charset = "utf-8";
    let text = decode_arc_source(
      charset,
      std::sync::Arc::from(bytes),
    ).unwrap();
    assert_eq!(text.as_ref(), "Hello");
  }
}