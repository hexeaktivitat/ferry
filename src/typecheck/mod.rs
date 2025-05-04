use std::vec;

use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use crate::{
    lexer::token::{Op, TokenType},
    parser::syntax::{
        Assign, Binary, Binding, Call, Expr, ExprVisitor, For, Function, Group, If, Import, Lit,
        Loop, Module, Unary, Variable, walk_expr,
    },
    state::{
        State,
        types::{FerryType, FerryTyping, Typing},
        value::{FuncVal, Value},
    },
};

#[derive(Error, Diagnostic, Debug)]
#[error("Type errors")]
pub enum FerryTypeError {
    #[error("asdf")]
    UnimplementedFeature {
        #[help]
        advice: String,
        #[label]
        span: SourceSpan,
    },
    #[error("conditional statement was not boolean")]
    ConditionalNotBool {
        #[help]
        advice: String,
        #[label("conditional")]
        span: SourceSpan,
    },
    #[error("unary type mismatch")]
    UnaryOpTypeMismatch {
        #[help]
        advice: String,
        #[label("operand")]
        span: SourceSpan,
        #[label("rhs")]
        rhs_span: SourceSpan,
    },
    #[error("mismatched types")]
    BinaryOpTypeMismatch {
        #[help]
        advice: String,
        #[label("operand")]
        span: SourceSpan,
        #[label("lhs")]
        lhs_span: SourceSpan,
        #[label("rhs")]
        rhs_span: SourceSpan,
    },
    #[error("mismatched if-then and else types")]
    MismatchedThenElse {
        #[help]
        advice: String,
        #[label("operand")]
        span: SourceSpan,
        #[label("if-then")]
        lhs_span: SourceSpan,
        #[label("else")]
        rhs_span: SourceSpan,
    },
    #[error("mistyped variable")]
    MistypedVariable {
        #[help]
        advice: String,
        #[label]
        span: SourceSpan,
    },
    #[error("mistyped argument")]
    MistypedArgument {
        #[help]
        advice: String,
        #[label("argument")]
        span: SourceSpan,
    },
    #[error("arity error")]
    ArityMismatch {
        #[help]
        advice: String,
        #[label("function")]
        span: SourceSpan,
    },
    #[error("unknown type")]
    UnknownType {
        #[help]
        advice: String,
        #[label]
        span: SourceSpan,
    },
    #[error("invalid operand")]
    InvalidOperand {
        #[help]
        advice: String,
        #[label]
        span: SourceSpan,
    },
    #[error("invalid assignment")]
    InvalidAssignment {
        #[help]
        advice: String,
        #[label("variable type")]
        typedef: SourceSpan,
        #[label("value type")]
        valuetype: SourceSpan,
        // #[label("binding")]
        // binding_span: SourceSpan,
    },
    #[error("not a function")]
    NotAFunction {
        #[help]
        advice: String,
        #[label("identifier")]
        span: SourceSpan,
    },
}

type FerryResult<T> = Result<T, FerryTypeError>;
type FerryTypecheckResult<T> = Result<Vec<T>, Vec<FerryTypeError>>;

#[derive(Debug)]
pub struct Typechecker;

impl Typechecker {
    pub fn new() -> Self {
        Self {}
    }

    pub fn typecheck(&mut self, syntax: &[Expr], state: &mut State) -> FerryTypecheckResult<Expr> {
        let mut result = Vec::new();
        let mut errors = Vec::new();

        for code in syntax {
            self.check_types(code, state)
                .map_or_else(|err| errors.push(err), |res| result.push(res));
        }

        if errors.is_empty() {
            Ok(result)
        } else {
            Err(errors)
        }
    }

    fn check_types(&mut self, code: &Expr, state: &mut State) -> FerryResult<Expr> {
        walk_expr(self, code, state)
    }

    fn infer(
        &mut self,
        code: &Expr,
        state: &mut State,
        infer_type: &FerryType,
    ) -> FerryResult<Expr> {
        match walk_expr(&mut *self, code, state) {
            Ok(expr) => match expr {
                Expr::Literal(l) => match l {
                    Lit::Undefined { token, expr_type } => Ok(Expr::Literal(Lit::Undefined {
                        token,
                        expr_type: set_type(expr_type, infer_type),
                    })),
                    Lit::Number {
                        token,
                        value,
                        expr_type,
                        span,
                    } => Ok(Expr::Literal(Lit::Number {
                        token,
                        value,
                        expr_type: set_type(expr_type, infer_type),
                        span,
                    })),
                    Lit::Str {
                        token,
                        value,
                        expr_type,
                        span,
                    } => Ok(Expr::Literal(Lit::Str {
                        token,
                        value,
                        expr_type: set_type(expr_type, infer_type),
                        span,
                    })),
                    Lit::Bool {
                        token,
                        value,
                        expr_type,
                        span,
                    } => Ok(Expr::Literal(Lit::Bool {
                        token,
                        value,
                        expr_type: set_type(expr_type, infer_type),
                        span,
                    })),
                    Lit::List {
                        token,
                        contents,
                        expr_type,
                        span,
                    } => Ok(Expr::Literal(Lit::List {
                        token,
                        contents,
                        expr_type: set_type(expr_type, infer_type),
                        span,
                    })),
                },
                Expr::Binary(b) => Ok(Expr::Binary(Binary {
                    lhs: b.lhs,
                    operator: b.operator,
                    rhs: b.rhs,
                    expr_type: set_type(b.expr_type, infer_type),
                })),
                Expr::Unary(u) => Ok(Expr::Unary(Unary {
                    operator: u.operator,
                    rhs: u.rhs,
                    expr_type: set_type(u.expr_type, infer_type),
                })),
                Expr::Variable(v) => Ok(Expr::Variable(Variable {
                    token: v.token,
                    name: v.name,
                    assigned_type: v.assigned_type,
                    expr_type: set_type(v.expr_type, infer_type),
                })),
                Expr::Assign(a) => Ok(Expr::Assign(Assign {
                    token: a.token,
                    var: a.var,
                    name: a.name,
                    value: a.value,
                    expr_type: set_type(a.expr_type, infer_type),
                })),
                Expr::If(i) => Ok(Expr::If(If {
                    token: i.token,
                    condition: i.condition,
                    then_expr: i.then_expr,
                    else_expr: i.else_expr,
                    expr_type: set_type(i.expr_type, infer_type),
                })),
                Expr::Group(g) => Ok(Expr::Group(Group {
                    token: g.token,
                    contents: g.contents,
                    expr_type: set_type(g.expr_type, infer_type),
                })),
                Expr::Binding(b) => Ok(Expr::Binding(Binding {
                    token: b.token,
                    name: b.name,
                    assigned_type: b.assigned_type,
                    value: b.value,
                    expr_type: set_type(b.expr_type, infer_type),
                    assigned_type_token: b.assigned_type_token,
                    value_token: b.value_token,
                    span: b.span,
                })),
                Expr::Loop(l) => Ok(Expr::Loop(Loop {
                    token: l.token,
                    condition: l.condition,
                    contents: l.contents,
                    expr_type: set_type(l.expr_type, infer_type),
                })),
                Expr::For(f) => Ok(Expr::For(For {
                    token: f.token,
                    variable: f.variable,
                    iterator: f.iterator,
                    contents: f.contents,
                    expr_type: set_type(f.expr_type, infer_type),
                    iterator_type: f.iterator_type,
                })),
                Expr::Function(f) => Ok(Expr::Function(Function {
                    token: f.token,
                    name: f.name,
                    args: f.args,
                    contents: f.contents,
                    return_type: f.return_type,
                    expr_type: set_type(f.expr_type, infer_type),
                })),
                Expr::Call(c) => Ok(Expr::Call(Call {
                    invoker: c.invoker,
                    name: c.name,
                    token: c.token,
                    args: c.args,
                    expr_type: set_type(c.expr_type, infer_type),
                })),
                Expr::Module(m) => Ok(Expr::Module(m.to_owned())),
                Expr::Import(i) => Ok(Expr::Import(i.to_owned())),
            },
            Err(e) => Err(e),
        }
    }
}

fn set_type(expr_type: FerryTyping, infer_type: &FerryType) -> FerryTyping {
    match expr_type {
        FerryTyping::Assigned(a) => FerryTyping::assign(&a),
        FerryTyping::Inferred(i) => FerryTyping::infer(&i),
        FerryTyping::Untyped => FerryTyping::infer(infer_type),
        FerryTyping::Undefined => FerryTyping::Undefined,
    }
}

impl ExprVisitor<FerryResult<Expr>, &mut State> for &mut Typechecker {
    fn visit_literal(&mut self, literal: &Lit, state: &mut State) -> FerryResult<Expr> {
        match literal {
            Lit::Number {
                value,
                expr_type: _,
                span,
                token,
            } => Ok(Expr::Literal(Lit::Number {
                value: *value,
                expr_type: FerryTyping::assign(&FerryType::Num),
                span: *span,
                token: token.clone(),
            })),
            Lit::Str {
                value,
                expr_type: _,
                span,
                token,
            } => Ok(Expr::Literal(Lit::Str {
                value: value.clone(),
                expr_type: FerryTyping::assign(&FerryType::String),
                span: *span,
                token: token.clone(),
            })),
            Lit::Bool {
                value,
                expr_type: _,
                span,
                token,
            } => Ok(Expr::Literal(Lit::Bool {
                value: *value,
                expr_type: FerryTyping::assign(&FerryType::Boolean),
                span: *span,
                token: token.clone(),
            })),
            Lit::Undefined {
                expr_type: _,
                token,
            } => Ok(Expr::Literal(Lit::Undefined {
                token: token.clone(),
                expr_type: FerryTyping::Undefined,
            })),
            Lit::List {
                token,
                contents,
                expr_type: _,
                span,
            } => {
                let mut checked_contents: Vec<Expr> = Vec::new();
                for item in contents {
                    checked_contents.push(self.check_types(item, state)?);
                }

                Ok(Expr::Literal(Lit::List {
                    token: token.clone(),
                    contents: checked_contents,
                    expr_type: FerryTyping::assign(&FerryType::List),
                    span: *span,
                }))
            }
        }
    }

    fn visit_binary(&mut self, binary: &Binary, state: &mut State) -> FerryResult<Expr> {
        match binary.operator.get_token_type() {
            TokenType::Operator(o) => match o {
                // mathematical operations
                Op::Add | Op::Subtract | Op::Multiply | Op::Divide | Op::Equals => {
                    let left = self.infer(&binary.lhs, state, &FerryType::Num)?;
                    let right = self.infer(&binary.rhs, state, &FerryType::Num)?;

                    if left.check(right.get_type()) {
                        let expr_type = FerryTyping::infer(left.get_type());

                        Ok(Expr::Binary(Binary {
                            lhs: Box::new(left.clone()),
                            operator: binary.operator.clone(),
                            rhs: Box::new(right),
                            expr_type: expr_type.clone(),
                        }))
                    } else if (left.check(&FerryType::Untyped) && right.check(&FerryType::Num))
                        || (left.check(&FerryType::Num) && right.check(&FerryType::Untyped))
                    {
                        let expr_type = FerryTyping::infer(right.get_type());

                        // need a setter to set type to left without needing to fuss with its internals
                        Ok(Expr::Binary(Binary {
                            lhs: Box::new(left.clone()),
                            operator: binary.operator.clone(),
                            rhs: Box::new(right),
                            expr_type: expr_type.clone(),
                        }))
                    } else {
                        Err(FerryTypeError::BinaryOpTypeMismatch {
                            advice: format!(
                                "Operands do not match type:\n LH type: {} \n RH type: {}",
                                left.get_type(),
                                right.get_type()
                            ),
                            span: *binary.operator.get_span(),
                            lhs_span: *left.get_token().get_span(),
                            rhs_span: *right.get_token().get_span(),
                        })
                    }
                }

                Op::LessThan
                | Op::GreaterThan
                | Op::Equality
                | Op::LessEqual
                | Op::GreaterEqual => {
                    let left = self.infer(&binary.lhs, state, &FerryType::Num)?;
                    let right = self.infer(&binary.rhs, state, &FerryType::Num)?;

                    if left.check(right.get_type()) {
                        Ok(Expr::Binary(Binary {
                            lhs: Box::new(left.clone()),
                            operator: binary.operator.clone(),
                            rhs: Box::new(right),
                            expr_type: FerryTyping::assign(&FerryType::Boolean),
                        }))
                    } else {
                        Err(FerryTypeError::BinaryOpTypeMismatch {
                            advice: format!(
                                "Operands do not match type:\n LH type: {} \n RH type: {}",
                                left.get_type(),
                                right.get_type()
                            ),
                            span: *binary.operator.get_span(),
                            lhs_span: *left.get_token().get_span(),
                            rhs_span: *right.get_token().get_span(),
                        })
                    }
                }

                Op::GetI => {
                    let left = self.infer(&binary.lhs, state, &FerryType::List)?;
                    let right = self.infer(&binary.rhs, state, &FerryType::Num)?;

                    if left.check(&FerryType::List) {
                        if right.check(&FerryType::Num) {
                            Ok(Expr::Binary(Binary {
                                lhs: Box::new(left.clone()),
                                operator: binary.operator.clone(),
                                rhs: Box::new(right),
                                expr_type: FerryTyping::assign(&FerryType::List),
                            }))
                        } else {
                            Err(FerryTypeError::BinaryOpTypeMismatch {
                                advice: format!(
                                    "Cannot index List with non-Int type:\n LHS type: {}\n RHS type: {}",
                                    left.get_type(),
                                    right.get_type()
                                ),
                                span: *binary.operator.get_span(),
                                lhs_span: *left.get_token().get_span(),
                                rhs_span: *right.get_token().get_span(),
                            })
                        }
                    } else {
                        Err(FerryTypeError::BinaryOpTypeMismatch {
                            advice: format!(
                                "Cannot index into non-List type:\n LH type: {} \n RH type: {}",
                                left.get_type(),
                                right.get_type()
                            ),
                            span: *binary.operator.get_span(),
                            lhs_span: *left.get_token().get_span(),
                            rhs_span: *right.get_token().get_span(),
                        })
                    }
                }

                Op::Cons => {
                    let left = self.infer(&binary.lhs, state, &FerryType::List)?;
                    let right = self.infer(&binary.rhs, state, &FerryType::List)?;

                    if left.check(&FerryType::List) {
                        if right.check(&FerryType::List) {
                            Ok(Expr::Binary(Binary {
                                lhs: Box::new(left.clone()),
                                operator: binary.operator.clone(),
                                rhs: Box::new(right.clone()),
                                expr_type: FerryTyping::infer(&FerryType::List),
                            }))
                        } else {
                            Err(FerryTypeError::BinaryOpTypeMismatch {
                                advice: format!(
                                    "Cannot cons a non-List object into a List:\n LHS type: {}\n RHS type: {}",
                                    left.get_type(),
                                    right.get_type()
                                ),
                                span: *binary.operator.get_span(),
                                lhs_span: *left.get_token().get_span(),
                                rhs_span: *right.get_token().get_span(),
                            })
                        }
                    } else {
                        Err(FerryTypeError::BinaryOpTypeMismatch {
                            advice: format!(
                                "Cannot cons a non-List object:\n LHS type: {}\n RHS type: {}",
                                left.get_type(),
                                right.get_type()
                            ),
                            span: *binary.operator.get_span(),
                            lhs_span: *left.get_token().get_span(),
                            rhs_span: *right.get_token().get_span(),
                        })
                    }
                }
            },

            _ => Err(FerryTypeError::InvalidOperand {
                advice: "invalid operator token".into(),
                span: *binary.operator.get_span(),
            }),
        }
    }

    fn visit_variable(&mut self, variable: &Variable, state: &mut State) -> FerryResult<Expr> {
        // we expect that the variable has been declared in some scope prior to being referenced
        if let (Some(derived_type), assigned_type) =
            (state.get_symbol_value(&variable.name), &variable.expr_type)
        {
            if derived_type.check(assigned_type.get_type()) {
                Ok(Expr::Variable(variable.clone()))
            } else if assigned_type.check(&FerryType::Untyped) {
                // type inference: if derived_type is valid, update the type
                Ok(Expr::Variable(Variable {
                    token: variable.token.clone(),
                    name: variable.name.clone(),
                    assigned_type: variable.assigned_type.clone(),
                    expr_type: FerryTyping::infer(derived_type.get_type()),
                }))
            } else {
                Err(FerryTypeError::MistypedVariable {
                    advice: "mismatched type:".into(),
                    span: *variable.token.get_span(),
                })
            }
        } else if state.get_symbol_value(&variable.name).is_none() {
            // we do not have enough information to type this variable
            // this variable will be inferred
            Ok(Expr::Variable(variable.clone()))
        } else {
            Err(FerryTypeError::UnknownType {
                advice: "variable of unknown type".into(),
                span: *variable.token.get_span(),
            })
        }
    }

    fn visit_assign(&mut self, assign: &Assign, state: &mut State) -> FerryResult<Expr> {
        // type inference first
        let Ok(value_check) = self.infer(assign.value.as_ref(), state, &FerryType::Undefined)
        else {
            return Err(FerryTypeError::UnknownType {
                advice: format!("Value type could not be defined at compile time"),
                span: *assign.token.get_span(),
            });
        };

        let typed_var = self.check_types(&assign.var, state)?;

        if typed_var.check(value_check.get_type()) {
            Ok(Expr::Assign(Assign {
                var: assign.var.clone(),
                name: assign.name.clone(),
                value: Box::new(value_check.clone()),
                expr_type: FerryTyping::infer(value_check.get_type()),
                token: assign.token.clone(),
            }))
        } else {
            Err(FerryTypeError::InvalidAssignment {
                advice: format!(
                    "Cannot assign value of this type to this variable:\n Variable type: {}\n Value type: {}",
                    typed_var.get_type(),
                    value_check.get_type()
                ),
                typedef: *assign.var.get_token().get_span(),
                valuetype: *assign.value.get_token().get_span(),
            })
        }
    }

    fn visit_if_expr(&mut self, if_expr: &If, state: &mut State) -> FerryResult<Expr> {
        let condition = self.check_types(&if_expr.condition, state)?;

        if !condition.check(&FerryType::Boolean) {
            return Err(FerryTypeError::ConditionalNotBool {
                advice: format!(
                    "expected 'Bool' conditional, found {}",
                    condition.get_type()
                ),
                span: *if_expr.condition.get_token().get_span(),
            });
        }

        let then_expr = self.check_types(&if_expr.then_expr, state)?;

        let else_expr = if let Some(else_expr_box) = &if_expr.else_expr {
            let else_expr_inner = self.check_types(else_expr_box, state)?;

            if then_expr.get_type() == else_expr_inner.get_type() {
                Some(Box::new(else_expr_inner))
            } else {
                return Err(FerryTypeError::MismatchedThenElse {
                    advice: format!(
                        "'then' and 'else' clause types do not match:\n then: {}\n else: {}",
                        then_expr.get_type(),
                        else_expr_inner.get_type()
                    ),
                    span: *if_expr.token.get_span(),
                    lhs_span: *then_expr.get_token().get_span(),
                    rhs_span: *else_expr_inner.get_token().get_span(),
                });
            }
        } else {
            None
        };

        let expr_type = FerryTyping::Inferred(*then_expr.get_type());

        Ok(Expr::If(If {
            token: if_expr.token.clone(),
            condition: Box::new(condition),
            then_expr: Box::new(then_expr),
            else_expr,
            expr_type,
        }))
    }

    fn visit_group(&mut self, group: &Group, state: &mut State) -> FerryResult<Expr> {
        let contents = Box::new(self.check_types(&group.contents, state)?);
        let expr_type = FerryTyping::infer(contents.get_type());

        Ok(Expr::Group(Group {
            token: group.token.clone(),
            contents,
            expr_type,
        }))
    }

    fn visit_binding(&mut self, binding: &Binding, state: &mut State) -> FerryResult<Expr> {
        // type inference first
        if let Some(value) = &binding.value {
            if let Ok(value_check) = self.check_types(value, state) {
                if let Some(assigned_type) = &binding.assigned_type {
                    if assigned_type.check(value_check.get_type()) {
                        let placeholder_value = set_placeholder(value_check.get_type());
                        state.add_symbol(&binding.name, Some(placeholder_value));

                        Ok(Expr::Binding(Binding {
                            token: binding.token.clone(),
                            name: binding.name.clone(),
                            assigned_type: binding.assigned_type,
                            assigned_type_token: binding.assigned_type_token.clone(),
                            value: Some(Box::new(value_check.clone())),
                            value_token: binding.value_token.clone(),
                            expr_type: FerryTyping::assign(assigned_type),
                            span: binding.span,
                        }))
                    } else {
                        Err(FerryTypeError::InvalidAssignment {
                            advice: format!(
                                "Variable type and assignment mismatch:\n Expected: {}\n Found: {}",
                                assigned_type, value_check
                            ),
                            typedef: *binding.assigned_type_token.get_span(),
                            valuetype: *value_check.get_token().get_span(),
                        })
                    }
                } else {
                    let placeholder_value = set_placeholder(value_check.get_type());
                    state.add_symbol(&binding.name, Some(placeholder_value));

                    Ok(Expr::Binding(Binding {
                        token: binding.token.clone(),
                        name: binding.name.clone(),
                        assigned_type: None,
                        assigned_type_token: binding.assigned_type_token.clone(),
                        value: Some(Box::new(value_check.clone())),
                        value_token: binding.value_token.clone(),
                        expr_type: FerryTyping::infer(value_check.get_type()),
                        span: binding.span,
                    }))
                }
            } else {
                Err(FerryTypeError::InvalidAssignment {
                    advice: format!(
                        "Variable type and assignment mismatch:\n Expected: {:?}\n Found: {:?}",
                        binding.assigned_type, binding.name
                    ),
                    typedef: *binding.assigned_type_token.get_span(),
                    valuetype: *value.get_token().get_span(),
                })
            }
        } else if let Some(assigned_type) = &binding.assigned_type {
            let placeholder_value = set_placeholder(assigned_type);
            state.add_symbol(&binding.name, Some(placeholder_value));

            Ok(Expr::Binding(Binding {
                token: binding.token.clone(),
                name: binding.name.clone(),
                assigned_type: binding.assigned_type,
                assigned_type_token: binding.assigned_type_token.clone(),
                value: None,
                value_token: binding.value_token.clone(),
                expr_type: FerryTyping::assign(assigned_type),
                span: binding.span,
            }))
        } else {
            Err(FerryTypeError::InvalidAssignment {
                advice: format!(
                    "Variable type and assignment mismatch:\n Expected: {:?}\n Found: {:?}",
                    binding.assigned_type, binding.name
                ),
                typedef: *binding.assigned_type_token.get_span(),
                valuetype: *binding.token.get_span(),
            })
        }
    }

    fn visit_loop(&mut self, loop_expr: &Loop, state: &mut State) -> FerryResult<Expr> {
        if let Some(cond) = &loop_expr.condition {
            let condition = Box::new(self.check_types(cond, state)?);

            if condition.check(&FerryType::Boolean) {
                let contents = Box::new(self.check_types(&loop_expr.contents, state)?);
                let expr_type = FerryTyping::infer(contents.get_type());

                Ok(Expr::Loop(Loop {
                    token: loop_expr.token.clone(),
                    condition: Some(condition),
                    contents,
                    expr_type,
                }))
            } else {
                Err(FerryTypeError::ConditionalNotBool {
                    advice: "loop condition was not boolean".into(),
                    span: *condition.get_token().get_span(),
                })
            }
        } else {
            let contents = Box::new(self.check_types(&loop_expr.contents, state)?);
            let expr_type = FerryTyping::infer(contents.get_type());

            Ok(Expr::Loop(Loop {
                token: loop_expr.token.clone(),
                condition: loop_expr.condition.clone(),
                contents,
                expr_type,
            }))
        }
    }

    fn visit_unary(&mut self, unary: &Unary, state: &mut State) -> FerryResult<Expr> {
        let right = self.check_types(&unary.rhs, state)?;
        if right.get_type() == &FerryType::Num {
            Ok(Expr::Unary(Unary {
                operator: unary.operator.clone(),
                rhs: Box::new(right),
                expr_type: FerryTyping::assign(&FerryType::Num),
            }))
        } else {
            Err(FerryTypeError::UnaryOpTypeMismatch {
                advice: format!("Expected Num, found {}", right.get_type()),
                span: *unary.operator.get_span(),
                rhs_span: *right.get_token().get_span(),
            })
        }
    }

    fn visit_for(&mut self, for_expr: &For, state: &mut State) -> FerryResult<Expr> {
        let iterator = self.check_types(&for_expr.iterator, state)?;

        if let Some(variable) = &for_expr.variable {
            if let Expr::Variable(v) = variable.as_ref() {
                state.add_symbol(&v.name, None);
            }

            let variable_checked = self.infer(variable, state, &FerryType::Num)?;

            if let Expr::Variable(var) = &variable_checked {
                let placeholder_value = set_placeholder(variable_checked.get_type());
                state.add_symbol(&var.name, Some(placeholder_value));
            }

            let contents = self.check_types(&for_expr.contents, state)?;

            if iterator.get_type() == &FerryType::List {
                let inf_type = contents.get_type();

                Ok(Expr::For(For {
                    token: for_expr.token.clone(),
                    // variable: Some(Box::new(variable_checked)),
                    variable: Some(Box::new(variable_checked)),
                    iterator: Box::new(iterator),
                    contents: Box::new(contents.clone()),
                    expr_type: FerryTyping::infer(inf_type),
                    iterator_type: for_expr.iterator_type,
                }))
            } else {
                Err(FerryTypeError::MistypedVariable {
                    advice: format!("Expected List, found {}", iterator.get_type()),
                    span: *for_expr.iterator.get_token().get_span(),
                })
            }
        } else {
            let contents = self.check_types(&for_expr.contents, state)?;

            if iterator.get_type() == &FerryType::List {
                let inf_type = &contents.get_type();

                Ok(Expr::For(For {
                    token: for_expr.token.clone(),
                    variable: None,
                    iterator: Box::new(iterator),
                    contents: Box::new(contents.clone()),
                    expr_type: FerryTyping::infer(inf_type),
                    iterator_type: for_expr.iterator_type,
                }))
            } else {
                Err(FerryTypeError::MistypedVariable {
                    advice: format!("Expected List, found {}", iterator.get_type()),
                    span: *for_expr.iterator.get_token().get_span(),
                })
            }
        }
    }

    fn visit_function(&mut self, function: &Function, state: &mut State) -> FerryResult<Expr> {
        // let expr_type = FerryTyping::new_assign(FerryType::Function);
        // Need to rework logic of functions in general to enable first-class functions
        let (expr_type, return_type) = if let Some(ty) = function.return_type {
            (ty, ty)
        } else {
            (FerryType::Undefined, FerryType::Undefined)
        };

        let mut fn_state = state.clone();

        fn_state.add_symbol(
            &function.name,
            Some(Value::Function(FuncVal {
                declaration: Some(Function {
                    token: function.token.clone(),
                    name: function.name.clone(),
                    args: function.args.clone(),
                    contents: function.contents.clone(),
                    return_type: Some(return_type),
                    expr_type: FerryTyping::assign(&expr_type),
                }),
                name: function.name.clone(),
                func_type: FerryType::Function,
                instructions: vec![],
                arity: 0,
            })),
        );

        let mut arity = 0;

        let args = if let Some(arguments) = &function.args {
            let mut rets = Vec::new();
            for a in arguments {
                let arg = self.check_types(a, &mut fn_state)?;
                rets.push(arg);
            }
            arity = rets.len();
            Some(rets)
        } else {
            None
        };

        let checked_contents = Box::new(self.check_types(&function.contents, &mut fn_state)?);
        // if checked_contents.get_type() != expr_type.get_type()
        //     && expr_type == FerryTyping::Inferred(FerryType::Undefined)
        // {
        //     FerryTyping::Inferred(checked_contents.get_type().to_owned());
        // }

        // let expr_type = FerryTyping::Inferred(FerryType::Function);

        let function_checked = Function {
            token: function.token.clone(),
            name: function.name.clone(),
            args,
            contents: checked_contents,
            return_type: function.return_type,
            expr_type: FerryTyping::assign(&expr_type),
        };

        state.add_symbol(
            &function.name,
            Some(Value::Function(FuncVal {
                declaration: Some(function_checked.clone()),
                name: function.name.clone(),
                func_type: FerryType::Function,
                instructions: vec![],
                arity,
            })),
        );

        Ok(Expr::Function(function_checked))
    }

    fn visit_call(&mut self, call: &Call, state: &mut State) -> FerryResult<Expr> {
        // println!("state: {:?}", state);
        if let Some(Value::Function(FuncVal {
            declaration: decl,
            name,
            func_type: _,
            instructions: _,
            arity,
        })) = &mut state.get_symbol_value(&call.name)
        {
            let Some(declaration) = decl else {
                return Err(FerryTypeError::MistypedVariable {
                    advice: format!("Expected function, found variable {name}"),
                    span: *call.token.get_span(),
                });
            };
            if let Some(decl_args) = &mut declaration.args {
                if call.args.len() != decl_args.len() {
                    return Err(FerryTypeError::ArityMismatch {
                        advice: format!(
                            "Expected {} arguments, found {} args",
                            arity,
                            decl_args.len() + 1
                        ),
                        span: *call.token.get_span(),
                    });
                }
                if call.args.is_empty() {
                    Ok(Expr::Call(Call {
                        invoker: call.invoker.clone(),
                        name: name.clone(),
                        token: call.token.clone(),
                        args: vec![],
                        expr_type: declaration.expr_type.clone(),
                    }))
                } else {
                    let mut checked_args = Vec::new();

                    for (call_arg, decl_arg) in call.args.iter().zip(decl_args.iter()) {
                        let checked_arg = self.check_types(call_arg, state)?;
                        let checked_decl_arg = self.check_types(decl_arg, state)?;

                        if checked_arg.check(checked_decl_arg.get_type()) {
                            checked_args.push(checked_arg);
                        } else {
                            return Err(FerryTypeError::MistypedArgument {
                                advice: format!(
                                    "Argument types do not match:\n expected {}\n found {}",
                                    checked_decl_arg.get_type(),
                                    checked_arg.get_type()
                                ),
                                span: *checked_arg.get_token().get_span(),
                            });
                        }
                    }

                    Ok(Expr::Call(Call {
                        invoker: call.invoker.clone(),
                        name: name.clone(),
                        token: call.token.clone(),
                        args: checked_args,
                        expr_type: declaration.expr_type.clone(),
                    }))
                }
            } else {
                Ok(Expr::Call(Call {
                    invoker: call.invoker.clone(),
                    name: name.clone(),
                    token: call.token.clone(),
                    args: call.args.clone(),
                    expr_type: declaration.expr_type.clone(),
                }))
            }
        } else {
            Err(FerryTypeError::NotAFunction {
                advice: format!(
                    "Expected function call, found unknown identifier {}",
                    call.name
                ),
                span: *call.token.get_span(),
            })
        }
    }

    fn visit_module(&mut self, module: &Module, state: &mut State) -> FerryResult<Expr> {
        let mut checked_fns = vec![];

        for function in module.functions.clone() {
            if let Expr::Function(checked_function) =
                self.check_types(&Expr::Function(function), state)?
            {
                checked_fns.push(checked_function);
            } else {
                return Err(FerryTypeError::UnimplementedFeature {
                    advice: "not supported".into(),
                    span: *module.token.get_span(),
                });
            };
        }

        Ok(Expr::Module(Module {
            name: module.name.clone(),
            token: module.token.clone(),
            functions: checked_fns,
        }))
    }

    fn visit_import(&mut self, import: &Import, state: &mut State) -> FerryResult<Expr> {
        let mut checked_fns = vec![];

        for function in import.functions.clone() {
            if let Expr::Function(checked_function) =
                self.check_types(&Expr::Function(function), state)?
            {
                checked_fns.push(checked_function);
            } else {
                return Err(FerryTypeError::UnimplementedFeature {
                    advice: format!("Typecheck issues in module {}", import.name),
                    span: *import.token.get_span(),
                });
            }
        }

        Ok(Expr::Import(Import {
            name: import.name.clone(),
            token: import.token.clone(),
            functions: checked_fns,
        }))
    }
}

fn set_placeholder(ty: &FerryType) -> Value {
    match ty {
        FerryType::Num | FerryType::Untyped => Value::Number(0),
        FerryType::String => Value::Str(String::new()),
        FerryType::Boolean => Value::Boolean(false),
        FerryType::List => Value::List(vec![]),
        FerryType::Undefined => Value::Unit,
        FerryType::Pointer => Value::Ptr(0x00),
        FerryType::Function => Value::Function(FuncVal {
            declaration: None,
            name: String::new(),
            func_type: FerryType::Function,
            instructions: vec![],
            arity: 0,
        }),
    }
}
