#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ParseErrorData {
  summary: String,
  detail: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParseError(Box<ParseErrorData>);

impl ParseError {
  pub fn new(summary: impl Into<String>, detail: impl Into<String>) -> Self {
    Self(Box::new(ParseErrorData {
      summary: summary.into(),
      detail: detail.into(),
    }))
  }

  pub fn summary(&self) -> &str {
    &self.0.summary
  }

  pub fn detail(&self) -> &str {
    &self.0.detail
  }
}
