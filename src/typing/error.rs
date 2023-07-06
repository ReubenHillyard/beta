#[derive(Debug)]
pub struct Error {
    pub msg: String,
}

pub type Result<T> = std::result::Result<T, Error>;
