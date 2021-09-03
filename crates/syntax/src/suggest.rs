use levenshtein::levenshtein;

/// tries to find a name from the given slice of suggested names
/// that is close to the given name and returns it if found. If no suggestion
/// is close enough, returns the empty string.
///
/// The suggestions are tried in order, so earlier suggestions take precedence
/// if the given string is similar to two or more suggestions.
///
/// This function is intended to be used with a relatively-small number of
/// suggestions. It's not optimized for hundreds or thousands of them.
pub(crate) fn name_suggestion<'a>(given: &str, suggestions: &[&'a str]) -> Option<&'a str> {
  for suggestion in suggestions {
    let dist = levenshtein(given, *suggestion);

    // threshold determined experimentally
    if dist < 3 {
      return Some(*suggestion);
    }
  }

  None
}

#[cfg(test)]
mod tests {
  use super::*;
  use test_case::test_case;

  const KEYWORDS: &[&str] = &["false", "true", "null"];

  #[test_case("true", "true")]
  #[test_case("false", "false")]
  #[test_case("null", "null")]
  #[test_case("bananas", "")]
  #[test_case("NaN", "")]
  #[test_case("Inf", "")]
  #[test_case("Infinity", "")]
  #[test_case("void", "")]
  #[test_case("undefined", "")]
  #[test_case("ture", "true")]
  #[test_case("tru", "true")]
  #[test_case("tre", "true")]
  #[test_case("treu", "true")]
  #[test_case("rtue", "true")]
  #[test_case("flase", "false")]
  #[test_case("fales", "false")]
  #[test_case("flse", "false")]
  #[test_case("fasle", "false")]
  #[test_case("fasel", "false")]
  #[test_case("flue", "false")]
  #[test_case("nil", "null")]
  #[test_case("nul", "null")]
  #[test_case("unll", "null")]
  #[test_case("nll", "null")]
  fn test_name(input: &str, want: &str) {
    let want = (!want.is_empty()).then(|| want);
    let result = name_suggestion(input, KEYWORDS);

    assert_eq!(result, want);
  }
}
