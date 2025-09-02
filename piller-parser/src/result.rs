use miette::{LabeledSpan, SourceSpan};
use piller_lexer::Span;

pub type Result<T, E = Error> = std::result::Result<T, E>;

pub trait IntoSourceSpan {
    fn into_source_span(self) -> SourceSpan;
}

impl IntoSourceSpan for Span {
    fn into_source_span(self) -> SourceSpan {
        (self.start, self.end - self.start).into()
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ParseOutput<T> {
    value: Option<T>,
    issues: Vec<ParseIssue>,
}

impl<T> Default for ParseOutput<T> {
    fn default() -> Self {
        Self {
            value: None,
            issues: vec![],
        }
    }
}

impl<T> ParseOutput<T> {
    pub fn with_value(value: T) -> Self {
        Self {
            value: Some(value),
            issues: vec![],
        }
    }

    pub fn with_issue(issue: ParseIssue) -> Self {
        Self {
            value: None,
            issues: vec![issue],
        }
    }

    pub fn merge<U>(&mut self, other: ParseOutput<U>) -> Option<U> {
        self.issues.extend(other.issues);
        other.value
    }

    pub fn has_issues(&self) -> bool {
        !self.issues.is_empty()
    }

    pub fn set_value(&mut self, value: T) {
        self.value = Some(value)
    }

    pub fn add_issue(&mut self, issue: ParseIssue) {
        self.issues.push(issue)
    }

    pub fn into_result(self, message: impl ToString) -> Result<T, Error> {
        if self.issues.is_empty() {
            Ok(self.value.expect("parser produced no value but no issues"))
        } else {
            Err(Error {
                message: message.to_string(),
                issues: self.issues,
            })
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ParseIssue {
    message: String,
    span: SourceSpan,
}

impl ParseIssue {
    pub fn new(message: impl ToString, span: impl IntoSourceSpan) -> Self {
        Self {
            message: message.to_string(),
            span: span.into_source_span(),
        }
    }
}

#[derive(Debug, thiserror::Error)]
#[error("{message}")]
pub struct Error {
    message: String,
    issues: Vec<ParseIssue>,
}

impl Error {
    pub fn into_report(self) -> miette::Report {
        miette::Report::new(self)
    }
}

impl miette::Diagnostic for Error {
    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        Some(Box::new(self.issues.iter().map(|issue| {
            LabeledSpan::new(
                Some(issue.message.clone()),
                issue.span.offset(),
                issue.span.len(),
            )
        })))
    }
}