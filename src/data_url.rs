// Copyright 2018-2025 the Deno authors. MIT license.

use url::Url;

use crate::MediaType;

pub fn get_mime_type_charset(mime_type: &str) -> Option<&str> {
  mime_type
    .split(';')
    .skip(1)
    .map(str::trim)
    .find_map(|s| s.strip_prefix("charset="))
}

#[derive(Debug, Clone)]
pub struct RawDataUrl {
  pub mime_type: String,
  pub bytes: Vec<u8>,
}

impl RawDataUrl {
  pub fn parse(specifier: &Url) -> Result<Self, std::io::Error> {
    use std::io::Error;
    use std::io::ErrorKind;

    fn unable_to_decode() -> Error {
      Error::new(ErrorKind::InvalidData, "Unable to decode data url.")
    }

    let url = data_url::DataUrl::process(specifier.as_str())
      .map_err(|_| unable_to_decode())?;
    let (bytes, _) = url.decode_to_vec().map_err(|_| unable_to_decode())?;
    Ok(RawDataUrl {
      mime_type: url.mime_type().to_string(),
      bytes,
    })
  }

  pub fn charset(&self) -> Option<&str> {
    get_mime_type_charset(&self.mime_type)
  }

  pub fn media_type(&self) -> MediaType {
    let mut content_types = self.mime_type.split(';');
    let Some(content_type) = content_types.next() else {
      return MediaType::Unknown;
    };
    MediaType::from_content_type(
      // this data url will be ignored when resolving the MediaType
      // as in this rare case the MediaType is determined solely based
      // on the provided content type
      &Url::parse("data:image/png;base64,").unwrap(),
      content_type,
    )
  }

  #[cfg(feature = "decoding")]
  pub fn decode(self) -> Result<String, std::io::Error> {
    let charset = get_mime_type_charset(&self.mime_type).unwrap_or("utf-8");
    crate::encoding::decode_owned_source(charset, self.bytes)
  }

  pub fn into_bytes_and_mime_type(self) -> (Vec<u8>, String) {
    (self.bytes, self.mime_type)
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_parse_valid_data_url() {
    let valid_data_url = "data:text/plain;base64,SGVsbG8sIFdvcmxkIQ==";
    let specifier = Url::parse(valid_data_url).unwrap();
    let raw_data_url = RawDataUrl::parse(&specifier).unwrap();
    assert_eq!(raw_data_url.mime_type, "text/plain");
    assert_eq!(raw_data_url.bytes, b"Hello, World!");
  }

  #[test]
  fn test_charset_with_valid_mime_type() {
    let raw_data_url = RawDataUrl {
      mime_type: "text/plain; charset=utf-8".to_string(),
      bytes: vec![],
    };
    assert_eq!(raw_data_url.charset(), Some("utf-8"));
  }

  #[test]
  fn test_charset_with_no_charset_in_mime_type() {
    let raw_data_url = RawDataUrl {
      mime_type: "text/plain".to_string(),
      bytes: vec![],
    };
    assert_eq!(raw_data_url.charset(), None);
  }

  #[test]
  fn test_media_type_with_known_type() {
    let raw_data_url = RawDataUrl {
      mime_type: "application/javascript;charset=utf-8".to_string(),
      bytes: vec![],
    };
    assert_eq!(raw_data_url.media_type(), MediaType::JavaScript);
  }

  #[test]
  fn test_media_type_with_unknown_type() {
    let raw_data_url = RawDataUrl {
      mime_type: "unknown/unknown".to_string(),
      bytes: vec![],
    };
    assert_eq!(raw_data_url.media_type(), MediaType::Unknown);
  }

  #[test]
  fn test_decode_with_valid_charset() {
    let raw_data_url = RawDataUrl {
      mime_type: "text/plain; charset=utf-8".to_string(),
      bytes: "Hello, World!".as_bytes().to_vec(),
    };
    assert_eq!(raw_data_url.decode().unwrap(), "Hello, World!");
  }

  #[test]
  fn test_decode_with_invalid_charset() {
    let raw_data_url = RawDataUrl {
      mime_type: "text/plain; charset=invalid-charset".to_string(),
      bytes: vec![],
    };
    assert!(raw_data_url.decode().is_err());
  }
}
