use std::{collections::VecDeque, iter::FusedIterator};

/// Characters below RuneSelf are represented as themselves in a single byte.
const RUNE_SELF: u8 = 0x80;

pub const fn is_ascii_whitespace(c: u8) -> bool {
  matches!(c, b'\t' | b'\n' | b'\x0B' | b'\x0C' | b'\r' | b' ')
}

// converted from bytes.TrimSpace from go
pub(crate) fn trim_space(s: &[u8]) -> &[u8] {
  // Fast path: if empty, return immediately
  if s.is_empty() {
    return s;
  }

  // Fast path for ASCII: look for the first ASCII non-space byte
  let mut start = 0;
  for (idx, c) in s.iter().copied().enumerate() {
    if c >= RUNE_SELF {
      // If we run into a non-ASCII byte, fall back to the
      // slower unicode-aware method on the remaining bytes
      return trim_func(&s[idx..], &char::is_whitespace);
    }

    if !is_ascii_whitespace(c) {
      break;
    }

    start = idx + 1;
  }

  // Now look for the first ASCII non-space byte from the end
  let mut stop = s.len();
  for (idx, c) in s.iter().copied().enumerate().skip(start).rev() {
    if c >= RUNE_SELF {
      // If we run into a non-ASCII byte, fall back to the
      // slower unicode-aware method on the remaining bytes
      return trim_func(&s[start..=idx], &char::is_whitespace);
    }

    if !is_ascii_whitespace(c) {
      break;
    }

    stop = idx;
  }

  // At this point s[start:stop] starts and ends with an ASCII
  // non-space bytes, so we're done. Non-ASCII cases have already
  // been handled above.
  &s[start..stop]
}

fn trim_func<'a>(s: &'a [u8], f: &impl Fn(char) -> bool) -> &'a [u8] {
  let s = std::str::from_utf8(s).unwrap();
  s.trim_start_matches(f).trim_end_matches(f).as_bytes()
}

pub(crate) struct VecDequeIterator<'a, T>(&'a mut VecDeque<T>);

impl<'a, T> VecDequeIterator<'a, T> {
  pub(crate) fn new(queue: &'a mut VecDeque<T>) -> Self {
    Self(queue)
  }
}

impl<'a, T> Iterator for VecDequeIterator<'a, T> {
  type Item = T;

  #[inline]
  fn next(&mut self) -> Option<Self::Item> {
    self.0.pop_front()
  }

  #[inline]
  fn size_hint(&self) -> (usize, Option<usize>) {
    (self.0.len(), Some(self.0.len()))
  }
}

impl<'a, T> DoubleEndedIterator for VecDequeIterator<'a, T> {
  #[inline]
  fn next_back(&mut self) -> Option<Self::Item> {
    self.0.pop_back()
  }
}

impl<'a, T> ExactSizeIterator for VecDequeIterator<'a, T> {
  #[inline]
  fn len(&self) -> usize {
    self.0.len()
  }
}

impl<'a, T> FusedIterator for VecDequeIterator<'a, T> {}

#[cfg(test)]
mod tests {
  use super::*;
  use const_format::concatcp;
  use test_case::test_case;

  const SPACE: &str = "\t\x0B\r\x0C\n\u{0085}\u{00a0}\u{2000}\u{3000}";

  #[test_case("", ""; "01")]
  #[test_case("  a", "a"; "02")]
  #[test_case("b  ", "b"; "03")]
  #[test_case("abc", "abc"; "04")]
  #[test_case(concatcp!(SPACE, "abc", SPACE), "abc"; "05")]
  #[test_case(" ", ""; "06")]
  #[test_case("\u{3000} ", ""; "07")]
  #[test_case(" \u{3000}", ""; "08")]
  #[test_case(" \t\r\n \t\t\r\r\n\n ", ""; "09")]
  #[test_case(" \t\r\n x\t\t\r\r\n\n ", "x"; "10")]
  #[test_case(" \u{2000}\t\r\n x\t\t\r\r\ny\n \u{3000}", "x\t\t\r\r\ny"; "11")]
  #[test_case("1 \t\r\n2", "1 \t\r\n2"; "12")]
  #[test_case(" x\u{80}", "x\u{80}"; "13")]
  #[test_case(" x\u{c0}", "x\u{c0}"; "14")]
  #[test_case("x \u{c0}\u{c0} ", "x \u{c0}\u{c0}"; "15")]
  #[test_case("x \u{c0}", "x \u{c0}"; "16")]
  #[test_case("x \u{c0} ", "x \u{c0}"; "17")]
  #[test_case("x \u{c0}\u{c0} ", "x \u{c0}\u{c0}"; "18")]
  #[test_case("x ☺\u{c0}\u{c0} ", "x ☺\u{c0}\u{c0}"; "19")]
  #[test_case("x ☺ ", "x ☺"; "20")]
  fn test_trim_space(s_in: &str, s_out: &str) {
    let actual = trim_space(s_in.as_bytes());
    assert_eq!(std::str::from_utf8(actual).unwrap(), s_out);
  }
}
