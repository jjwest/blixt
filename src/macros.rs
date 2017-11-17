#[macro_export]
macro_rules! expected {
    ( $expected:expr, $got:expr ) => {
        return Err(format_err!("Expected '{:?}', found '{:?}'", $expected, $got))
    };

    ( $expected:expr ) => {
        return Err(format_err!("Expected '{:?}'", $expected))
    }
}
