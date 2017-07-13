#[macro_export]
macro_rules! expect_next {
    ( $context:ident, $func:ident, $expected:expr ) => {
        match $context.$func() {
            $expected => {},
            other => return Err(format!("Expected '{:?}', found '{:?}'", $expected, other).into()),
        }
    }
}

#[macro_export]
macro_rules! expected {
    ( $expected:expr, $got:expr ) => {
        return Err(format!("Expected '{:?}', found '{:?}'", $expected, $got).into())
    };

    ( $expected:expr ) => {
        return Err(format!("Expected '{:?}'", $expected).into())
    }
}
