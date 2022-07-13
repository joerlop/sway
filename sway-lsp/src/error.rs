use thiserror::Error;
use tower_lsp::lsp_types::Diagnostic;

#[derive(Debug, Error)]
pub enum ServerError {
    #[error(transparent)]
    ManifestFileMissing(#[from] anyhow::Error),

    #[error("document not found")]
    DocumentNotFound,

    #[error("document already stored")]
    DocumentAlreadyStored,

    #[error("Failed to parse typed AST")]
    FailedToParse(Vec<Diagnostic>),
}
