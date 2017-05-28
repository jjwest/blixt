struct Variable<'a, T> {
    name: &'a str,
    value: T,
}

struct ArithExpr<T> {
    left: T,
    right: T,
    operator: ArithOperator,
}

enum ArithOperator {
    Add,
    Sub,
    Mult,
    Div,
    Mod,
}

struct LogicExpr<T> {
    left: T,
    right: T,
    operator: LogicOperator,
}

struct LogicNotExpr<T> {
    expr: LogicExpr<T>,
}

enum LogicOperator {
    And,
    Equal,
    Greater,
    GreaterEqual,
    Lesser,
    LesserEqual,
    Or,
}
