use loy_ast::token::Span;
use miette::{LabeledSpan, SourceSpan};

pub type Result<T, E = Error> = std::result::Result<T, E>;

pub trait IntoSourceSpan {
    fn into_source_span(self) -> SourceSpan;
}

impl IntoSourceSpan for Span {
    fn into_source_span(self) -> SourceSpan {
        (self.start, self.end - self.start).into()
    }
}

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct ReportTitleWeight(u8);

impl From<u8> for ReportTitleWeight {
    fn from(val: u8) -> Self {
        ReportTitleWeight(val)
    }
}

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct ReportTitle {
    message: String,
    weight: ReportTitleWeight,
}

pub trait IntoReportTitle {
    fn into_report_title(self) -> ReportTitle
    where
        Self: Sized,
    {
        Self::into_report_title_weighted(self, 0)
    }

    fn into_report_title_weighted(self, weight: impl Into<ReportTitleWeight>) -> ReportTitle
    where
        Self: Sized;
}

impl<S> IntoReportTitle for S
where
    S: ToString,
{
    fn into_report_title_weighted(self, weight: impl Into<ReportTitleWeight>) -> ReportTitle
    where
        Self: Sized,
    {
        ReportTitle {
            message: self.to_string(),
            weight: weight.into(),
        }
    }
}

impl IntoReportTitle for ReportTitle {
    fn into_report_title_weighted(self, _: impl Into<ReportTitleWeight>) -> ReportTitle
    where
        Self: Sized,
    {
        self
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ParseIssue {
    message: String,
    span: SourceSpan,
    help: Option<String>,
    error_title: ReportTitle,
    severity: ErrorSeverity,
}

impl ParseIssue {
    pub fn new(message: impl ToString, span: impl IntoSourceSpan) -> Self {
        Self {
            message: message.to_string(),
            span: span.into_source_span(),
            help: None,
            error_title: ReportTitle::default(),
            severity: ErrorSeverity::Error,
        }
    }

    pub fn with_help(mut self, message: impl ToString) -> Self {
        self.help = Some(message.to_string());
        self
    }

    pub fn with_report_title(mut self, title: impl IntoReportTitle) -> Self {
        self.error_title = title.into_report_title();
        self
    }

    pub fn into_error<T>(self) -> Result<T> {
        Err(Error {
            title: self.error_title.clone(),
            severity: self.severity,
            issues: vec![self],
        })
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum ErrorSeverity {
    Advice,
    Warning,
    Error,
}

impl From<ErrorSeverity> for miette::Severity {
    fn from(val: ErrorSeverity) -> Self {
        match val {
            ErrorSeverity::Advice => miette::Severity::Advice,
            ErrorSeverity::Warning => miette::Severity::Warning,
            ErrorSeverity::Error => miette::Severity::Error,
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub struct Error {
    title: ReportTitle,
    issues: Vec<ParseIssue>,
    severity: ErrorSeverity,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.severity {
            ErrorSeverity::Error => write!(f, "Error:   {}", self.title.message),
            ErrorSeverity::Warning => write!(f, "Warning: {}", self.title.message),
            ErrorSeverity::Advice => write!(f, "Info:    {}", self.title.message),
        }
    }
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

    fn help<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        let strongest_issue_help = self
            .issues
            .iter()
            .max_by_key(|t| t.error_title.weight)
            .and_then(|issue| issue.help.as_ref());

        match strongest_issue_help {
            Some(message) => Some(Box::new(message)),
            None => Some(Box::new(
                self.issues.iter().find_map(|issue| issue.help.as_ref())?,
            )),
        }
    }

    fn severity(&self) -> Option<miette::Severity> {
        Some(self.severity.into())
    }
}