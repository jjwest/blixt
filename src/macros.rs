#[macro_export]
macro_rules! die {
    ( $msg:expr, $($args:tt)* ) => {
        eprintln!($msg, $($args)*);
        ::std::process::exit(1);
    }
}
