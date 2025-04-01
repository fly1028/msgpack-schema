use msgpack_value::TryFromIntError;
use thiserror::Error;

/// This error type represents type mismatch errors during deserialization.
#[derive(Debug, Error)]
#[error("validation failed: {0}")]
pub struct ValidationError(pub &'static str);

/// This error type represents all possible errors during deserialization.
#[derive(Debug, Error)]
pub enum DeserializeError {
    #[error(transparent)]
    InvalidInput(#[from] InvalidInputError),
    #[error(transparent)]
    Validation(#[from] ValidationError),
}

/// This error type represents blob-to-MessegePack transcode errors.
///
/// This error type is raised during deserialization either
/// 1. when (first bytes of) given binary data is not a message pack object, or
/// 2. when it unexpectedly reaches the end of input.
#[derive(Debug, Error)]
#[error("invalid input: {0}")]
pub struct InvalidInputError(pub &'static str);

/// This error type represents errors during serialization.
// #[derive(Debug, Error)]
// #[error("failed to serialize: {0}")]
// pub struct SerializeError(#[from] pub std::io::Error);
#[derive(Debug, Error)]
pub enum SerializeError {
    #[error(transparent)]
    InvalidInput(#[from] InvalidInputError),
    #[error(transparent)]
    Validation(#[from] ValidationError),
    #[error(transparent)]
    RMP(#[from] std::io::Error),
}

impl From<rmp::encode::ValueWriteError> for SerializeError {
    fn from(err: rmp::encode::ValueWriteError) -> Self {
        SerializeError::RMP(err.into())
    }
}

impl From<TryFromIntError> for SerializeError {
    fn from(_: TryFromIntError) -> Self {
        SerializeError::InvalidInput(InvalidInputError("try from int error"))
    }
}
