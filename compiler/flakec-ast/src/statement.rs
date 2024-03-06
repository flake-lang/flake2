use crate::expression::Expression;


#[derive(Debug, Clone)]
pub enum Statement{
    Let{
        var_name: String,
        initale_value: Option<Expression>
    },
    Return {
        value: Option<Expression>
    },
    Assignment {
        var: String,
        value: Expression
    },
}

