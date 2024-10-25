use std::vec;

use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use crate::{
    state::{FerryState, FerryValue},
    syntax::{
        walk_expr, Assign, Binary, Binding, Call, Expr, ExprVisitor, For, Function, Group, If, Lit,
        Loop, Module, Unary, Variable,
    },
    token::{Op, TokenType},
    types::{FerryType, FerryTyping, TypeCheckable, Typing},
    FerryTypeErrors,
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
        #[label]
        span: SourceSpan,
    },
    #[error("mismatched types")]
    TypeMismatch {
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
}

type FerryResult<T> = Result<T, FerryTypeError>;
type FerryTypecheckResult<T> = Result<Vec<T>, Vec<FerryTypeError>>;

#[derive(Debug)]
pub struct FerryTypechecker {
    syntax: Vec<Expr>,
}

impl FerryTypechecker {
    pub fn new(syntax: Vec<Expr>) -> Self {
        Self { syntax }
    }

    pub fn typecheck(&mut self, state: &mut FerryState) -> FerryTypecheckResult<Expr> {
        let mut result = Vec::new();
        let mut errors = Vec::new();

        for code in self.syntax.clone().iter_mut() {
            match self.check_types(code, state) {
                Ok(r) => result.push(r),
                Err(e) => errors.push(e),
            }
        }

        if errors.is_empty() {
            Ok(result)
        } else {
            Err(errors)
        }
    }

    fn check_types(&mut self, code: &mut Expr, state: &mut FerryState) -> FerryResult<Expr> {
        walk_expr(&mut *self, code, state)
    }

    fn infer(
        &mut self,
        code: &mut Expr,
        state: &mut FerryState,
        infer_type: FerryType,
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
            },
            Err(e) => Err(e),
        }
    }
}

fn set_type(expr_type: FerryTyping, infer_type: FerryType) -> FerryTyping {
    match expr_type {
        FerryTyping::Assigned(a) => FerryTyping::Assigned(a),
        FerryTyping::Inferred(i) => FerryTyping::Inferred(i),
        FerryTyping::Untyped => FerryTyping::Inferred(infer_type),
        FerryTyping::Undefined => FerryTyping::Undefined,
    }
}

impl ExprVisitor<FerryResult<Expr>, &mut FerryState> for &mut FerryTypechecker {
    fn visit_literal(&mut self, literal: &mut Lit, state: &mut FerryState) -> FerryResult<Expr> {
        match literal {
            Lit::Number {
                value,
                expr_type: _,
                span,
                token,
            } => Ok(Expr::Literal(Lit::Number {
                value: *value,
                expr_type: FerryTyping::Assigned(FerryType::Num),
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
                expr_type: FerryTyping::Assigned(FerryType::String),
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
                expr_type: FerryTyping::Assigned(FerryType::Boolean),
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
                    expr_type: FerryTyping::Assigned(FerryType::List),
                    span: *span,
                }))
            }
        }
    }

    fn visit_binary(&mut self, binary: &mut Binary, state: &mut FerryState) -> FerryResult<Expr> {
        // if left.check(&FerryType::Untyped) || right.check(&FerryType::Untyped) {
        //     return Err(FerryTypeError::MistypedVariable {
        //         advice: "variables were not assigned typed correctly".into(),
        //         span: left.get_token().get_span().clone(),
        //     });
        // }

        match binary.operator.get_token_type() {
            TokenType::Operator(o) => match o {
                Op::Add | Op::Subtract | Op::Multiply | Op::Divide | Op::Equals => {
                    let left = self.infer(&mut binary.lhs, state, FerryType::Num)?;
                    let right = self.infer(&mut binary.rhs, state, FerryType::Num)?;
                    if left.check(right.get_type()) {
                        let expr_type = FerryTyping::Inferred(left.get_type().clone());
                        Ok(Expr::Binary(Binary {
                            lhs: Box::new(left.clone()),
                            operator: binary.operator.clone(),
                            rhs: Box::new(right),
                            expr_type: expr_type.clone(),
                        }))
                    } else if (left.check(&FerryType::Untyped) && right.check(&FerryType::Num))
                        || (left.check(&FerryType::Num) && right.check(&FerryType::Untyped))
                    {
                        let expr_type = FerryTyping::Inferred(right.get_type().clone());
                        // need a setter to set type to left without needing to fuss with its internals
                        println!("hello from visit binary");
                        Ok(Expr::Binary(Binary {
                            lhs: Box::new(left.clone()),
                            operator: binary.operator.clone(),
                            rhs: Box::new(right),
                            expr_type: expr_type.clone(),
                        }))
                    } else {
                        Err(FerryTypeError::TypeMismatch {
                            advice: "operands did not match types".into(),
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
                    let left = self.infer(&mut binary.lhs, state, FerryType::Num)?;
                    let right = self.infer(&mut binary.rhs, state, FerryType::Num)?;
                    if left.check(right.get_type()) {
                        Ok(Expr::Binary(Binary {
                            lhs: Box::new(left.clone()),
                            operator: binary.operator.clone(),
                            rhs: Box::new(right),
                            expr_type: FerryTyping::Assigned(FerryType::Boolean),
                        }))
                    } else {
                        Err(FerryTypeError::TypeMismatch {
                            advice: "operands did not match types".into(),
                            span: *binary.operator.get_span(),
                            lhs_span: *left.get_token().get_span(),
                            rhs_span: *right.get_token().get_span(),
                        })
                    }
                }
                Op::GetI => {
                    let left = self.infer(&mut binary.lhs, state, FerryType::List)?;
                    let right = self.infer(&mut binary.rhs, state, FerryType::Num)?;
                    if left.check(&FerryType::List) {
                        if right.check(&FerryType::Num) {
                            Ok(Expr::Binary(Binary {
                                lhs: Box::new(left.clone()),
                                operator: binary.operator.clone(),
                                rhs: Box::new(right),
                                expr_type: FerryTyping::Assigned(FerryType::List),
                            }))
                        } else {
                            Err(FerryTypeError::TypeMismatch {
                                advice: "invalid attempt at list indexing".into(),
                                span: *binary.operator.get_span(),
                                lhs_span: *left.get_token().get_span(),
                                rhs_span: *right.get_token().get_span(),
                            })
                        }
                    } else {
                        Err(FerryTypeError::TypeMismatch {
                            advice: "invalid attempt at list indexing".into(),
                            span: *binary.operator.get_span(),
                            lhs_span: *left.get_token().get_span(),
                            rhs_span: *right.get_token().get_span(),
                        })
                    }
                }
                Op::Cons => {
                    let left = self.infer(&mut binary.lhs, state, FerryType::List)?;
                    let right = self.infer(&mut binary.rhs, state, FerryType::List)?;
                    if left.check(&FerryType::List) {
                        if right.check(&FerryType::List) {
                            Ok(Expr::Binary(Binary {
                                lhs: Box::new(left.clone()),
                                operator: binary.operator.clone(),
                                rhs: Box::new(right.clone()),
                                expr_type: FerryTyping::Inferred(FerryType::List),
                            }))
                        } else {
                            Err(FerryTypeError::TypeMismatch {
                                advice: "invalid attempt at list cons".into(),
                                span: *binary.operator.get_span(),
                                lhs_span: *left.get_token().get_span(),
                                rhs_span: *right.get_token().get_span(),
                            })
                        }
                    } else {
                        Err(FerryTypeError::TypeMismatch {
                            advice: "invalid attempt at list cons".into(),
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

    fn visit_variable(
        &mut self,
        variable: &mut Variable,
        state: &mut FerryState,
    ) -> FerryResult<Expr> {
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
                    expr_type: FerryTyping::Inferred(derived_type.get_type().clone()),
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

    fn visit_assign(&mut self, assign: &mut Assign, state: &mut FerryState) -> FerryResult<Expr> {
        // type inference first
        if let Some(value) = &mut assign.value {
            if let Ok(value_check) = self.infer(value, state, FerryType::Undefined) {
                Ok(Expr::Assign(Assign {
                    var: assign.var.clone(),
                    name: assign.name.clone(),
                    value: Some(Box::new(value_check.clone())),
                    expr_type: FerryTyping::Inferred(value_check.get_type().clone()),
                    token: assign.token.clone(),
                }))
            } else {
                Ok(Expr::Assign(Assign {
                    token: assign.token.clone(),
                    var: assign.var.clone(),
                    name: assign.name.clone(),
                    value: None,
                    expr_type: FerryTyping::Undefined,
                }))
            }
        } else {
            Ok(Expr::Assign(Assign {
                token: assign.token.clone(),
                var: assign.var.clone(),
                name: assign.name.clone(),
                value: None,
                expr_type: FerryTyping::Undefined,
            }))
        }
    }

    fn visit_if_expr(&mut self, if_expr: &mut If, state: &mut FerryState) -> FerryResult<Expr> {
        let condition = self.check_types(&mut if_expr.condition, state)?;
        if !condition.check(&FerryType::Boolean) {
            return Err(FerryTypeError::ConditionalNotBool {
                advice: "expected conditional to if statement to be of type 'bool'".into(),
                span: *if_expr.token.get_span(),
            });
        }
        let then_expr = self.check_types(&mut if_expr.then_expr, state)?;
        let else_expr = if let Some(else_expr_box) = &mut if_expr.else_expr {
            let else_expr = self.check_types(else_expr_box, state)?;
            if then_expr.get_type() != else_expr.get_type() {
                return Err(FerryTypeError::MismatchedThenElse {
                    advice: "type mismatch between two values".into(),
                    span: *if_expr.token.get_span(),
                    lhs_span: *then_expr.get_token().get_span(),
                    rhs_span: *else_expr.get_token().get_span(),
                });
            } else {
                Some(Box::new(else_expr))
            }
        } else {
            None
        };
        let expr_type = FerryTyping::Inferred(then_expr.get_type().clone());
        Ok(Expr::If(If {
            token: if_expr.token.clone(),
            condition: Box::new(condition),
            then_expr: Box::new(then_expr),
            else_expr,
            expr_type,
        }))
    }

    fn visit_group(&mut self, group: &mut Group, state: &mut FerryState) -> FerryResult<Expr> {
        let contents = Box::new(self.check_types(&mut group.contents, state)?);
        let expr_type = FerryTyping::Inferred(contents.get_type().clone());

        Ok(Expr::Group(Group {
            token: group.token.clone(),
            contents,
            expr_type,
        }))
    }

    fn visit_binding(
        &mut self,
        binding: &mut Binding,
        state: &mut FerryState,
    ) -> FerryResult<Expr> {
        // type inference first
        if let Some(value) = &mut binding.value {
            if let Ok(value_check) = self.check_types(value, state) {
                if let Some(assigned_type) = &binding.assigned_type {
                    if assigned_type.check(value_check.get_type()) {
                        let placeholder_value = match value_check.get_type() {
                            FerryType::Num => FerryValue::Number(0),
                            FerryType::String => FerryValue::Str("".into()),
                            FerryType::Boolean => FerryValue::Boolean(false),
                            FerryType::List => FerryValue::List(vec![]),
                            _ => unreachable!(),
                        };
                        state.add_symbol(&binding.name, Some(placeholder_value));
                        return Ok(Expr::Binding(Binding {
                            token: binding.token.clone(),
                            name: binding.name.clone(),
                            assigned_type: binding.assigned_type.clone(),
                            value: Some(Box::new(value_check.clone())),
                            expr_type: FerryTyping::Assigned(assigned_type.to_owned()),
                        }));
                    } else {
                        return Err(FerryTypeError::MistypedVariable {
                            advice: "cannot assign this value to this variable".into(),
                            span: *binding.token.get_span(),
                        });
                    }
                } else {
                    let placeholder_value = match value_check.get_type() {
                        FerryType::Num => FerryValue::Number(0),
                        FerryType::String => FerryValue::Str("".into()),
                        FerryType::Boolean => FerryValue::Boolean(false),
                        FerryType::List => FerryValue::List(vec![]),
                        _ => unreachable!(),
                    };
                    state.add_symbol(&binding.name, Some(placeholder_value));
                    return Ok(Expr::Binding(Binding {
                        token: binding.token.clone(),
                        name: binding.name.clone(),
                        assigned_type: None,
                        value: Some(Box::new(value_check.clone())),
                        expr_type: FerryTyping::Inferred(value_check.get_type().clone()),
                    }));
                }
            }
        } else if let Some(assigned_type) = &binding.assigned_type {
            let placeholder_value = match assigned_type {
                FerryType::Num => FerryValue::Number(0),
                FerryType::String => FerryValue::Str("".into()),
                FerryType::Boolean => FerryValue::Boolean(false),
                FerryType::List => FerryValue::List(vec![]),
                _ => unreachable!(),
            };
            state.add_symbol(&binding.name, Some(placeholder_value));

            return Ok(Expr::Binding(Binding {
                token: binding.token.clone(),
                name: binding.name.clone(),
                assigned_type: binding.assigned_type.clone(),
                value: None,
                expr_type: FerryTyping::Assigned(assigned_type.clone()),
            }));
        }

        Err(FerryTypeError::UnknownType {
            advice: "unknown type for variable".into(),
            span: *binding.token.get_span(),
        })
    }

    fn visit_loop(&mut self, loop_expr: &mut Loop, state: &mut FerryState) -> FerryResult<Expr> {
        if let Some(cond) = &mut loop_expr.condition {
            let condition = Box::new(self.check_types(cond, state)?);
            if condition.check(&FerryType::Boolean) {
                let contents = Box::new(self.check_types(&mut loop_expr.contents, state)?);
                let expr_type = FerryTyping::Inferred(contents.get_type().clone());
                Ok(Expr::Loop(Loop {
                    token: loop_expr.token.clone(),
                    condition: Some(condition),
                    contents,
                    expr_type,
                }))
            } else {
                return Err(FerryTypeError::ConditionalNotBool {
                    advice: "loop condition was not boolean".into(),
                    span: *loop_expr.token.get_span(),
                });
            }
        } else {
            let contents = Box::new(self.check_types(&mut loop_expr.contents, state)?);
            let expr_type = FerryTyping::Inferred(contents.get_type().clone());
            Ok(Expr::Loop(Loop {
                token: loop_expr.token.clone(),
                condition: loop_expr.condition.clone(),
                contents,
                expr_type,
            }))
        }
    }

    fn visit_unary(
        &mut self,
        unary: &mut crate::syntax::Unary,
        state: &mut FerryState,
    ) -> FerryResult<Expr> {
        let right = self.check_types(&mut unary.rhs, state)?;

        if right.get_type() == &FerryType::Num {
            Ok(Expr::Unary(Unary {
                operator: unary.operator.clone(),
                rhs: Box::new(right),
                expr_type: FerryTyping::Assigned(FerryType::Num),
            }))
        } else {
            Err(FerryTypeError::TypeMismatch {
                advice: "Expected Num, found".into(),
                span: *unary.operator.get_span(),
                lhs_span: *unary.operator.get_span(),
                rhs_span: *right.get_token().get_span(),
            })
        }
    }

    fn visit_for(&mut self, for_expr: &mut For, state: &mut FerryState) -> FerryResult<Expr> {
        let iterator = self.check_types(&mut for_expr.iterator, state)?;
        if let Some(variable) = &mut for_expr.variable {
            if let Expr::Variable(v) = variable.as_ref() {
                state.add_symbol(&v.name, None);
            }
            let variable_checked = self.infer(variable, state, FerryType::Num)?;
            if let Expr::Variable(var) = &variable_checked {
                let placeholder_value = match variable_checked.get_type() {
                    FerryType::Num => FerryValue::Number(0),
                    FerryType::String => FerryValue::Str("".into()),
                    FerryType::Boolean => FerryValue::Boolean(false),
                    FerryType::List => FerryValue::List(vec![]),
                    FerryType::Untyped => FerryValue::Number(0), // assume an untyped iterator value is a Num
                    FerryType::Undefined => FerryValue::Unit,
                    FerryType::Pointer => FerryValue::Ptr(0x00),
                    FerryType::Function => FerryValue::Function {
                        declaration: None,
                        name: "".into(),
                        func_type: FerryType::Function,
                        instructions: vec![],
                        arity: 0,
                    },
                };
                state.add_symbol(&var.name, Some(placeholder_value));
            }
            let contents = self.check_types(&mut for_expr.contents, state)?;

            if iterator.get_type() == &FerryType::List {
                let inf_type = contents.get_type().clone();
                Ok(Expr::For(For {
                    token: for_expr.token.clone(),
                    // variable: Some(Box::new(variable_checked)),
                    variable: Some(Box::new(variable_checked)),
                    iterator: Box::new(iterator),
                    contents: Box::new(contents),
                    expr_type: FerryTyping::Inferred(inf_type),
                    iterator_type: for_expr.iterator_type.clone(),
                }))
            } else {
                Err(FerryTypeError::MistypedVariable {
                    advice: "Expected List, found".into(),
                    span: *for_expr.token.get_span(),
                })
            }
        } else {
            let contents = self.check_types(&mut for_expr.contents, state)?;
            if iterator.get_type() == &FerryType::List {
                let inf_type = contents.get_type().clone();
                Ok(Expr::For(For {
                    token: for_expr.token.clone(),
                    variable: None,
                    iterator: Box::new(iterator),
                    contents: Box::new(contents),
                    expr_type: FerryTyping::Inferred(inf_type),
                    iterator_type: for_expr.iterator_type.clone(),
                }))
            } else {
                Err(FerryTypeError::MistypedVariable {
                    advice: "Expected List, found".into(),
                    span: *for_expr.token.get_span(),
                })
            }
        }
    }

    fn visit_function(
        &mut self,
        function: &mut Function,
        state: &mut FerryState,
    ) -> FerryResult<Expr> {
        let mut return_type = FerryType::Undefined;
        let mut expr_type = if let Some(ty) = &function.return_type {
            return_type = ty.clone();
            FerryTyping::Assigned(ty.clone())
        } else {
            FerryTyping::Inferred(FerryType::Undefined)
        };
        let mut fn_state = state.clone();
        fn_state.add_symbol(
            &function.name,
            Some(FerryValue::Function {
                declaration: Some(Function {
                    token: function.token.clone(),
                    name: function.name.clone(),
                    args: function.args.clone(),
                    contents: function.contents.clone(),
                    return_type: Some(return_type),
                    expr_type: expr_type.clone(),
                }),
                name: function.name.clone(),
                func_type: FerryType::Function,
                instructions: vec![],
                arity: 0,
            }),
        );
        let mut arity = 0;
        let args = if let Some(arguments) = &mut function.args {
            let mut rets = Vec::new();
            for a in arguments {
                let arg = self.check_types(a, &mut fn_state);
                rets.push(arg?);
            }
            arity = rets.len();
            Some(rets)
        } else {
            None
        };

        let checked_contents = Box::new(self.check_types(&mut function.contents, &mut fn_state)?);
        if checked_contents.get_type() != expr_type.get_type()
            && expr_type == FerryTyping::Inferred(FerryType::Undefined)
        {
            expr_type = FerryTyping::Inferred(checked_contents.get_type().to_owned());
        }

        let function_checked = Function {
            token: function.token.clone(),
            name: function.name.clone(),
            args,
            contents: checked_contents,
            return_type: function.return_type.clone(),
            expr_type,
        };

        state.add_symbol(
            &function.name,
            Some(FerryValue::Function {
                declaration: Some(function_checked.clone()),
                name: function.name.clone(),
                func_type: FerryType::Function,
                instructions: vec![],
                arity,
            }),
        );

        Ok(Expr::Function(function_checked))
    }

    fn visit_call(&mut self, call: &mut Call, state: &mut FerryState) -> FerryResult<Expr> {
        if let Some(FerryValue::Function {
            declaration: decl,
            name,
            func_type: _,
            instructions: _,
            arity: _,
        }) = &mut state.get_symbol_value(&call.name)
        {
            let declaration = if let Some(d) = decl {
                d
            } else {
                return Err(FerryTypeError::MistypedVariable {
                    advice: "function call wasnt a function".into(),
                    span: *call.token.get_span(),
                });
            };
            if let Some(decl_args) = &mut declaration.args {
                if call.args.len() != decl_args.len() {
                    return Err(FerryTypeError::UnimplementedFeature {
                        advice: "actually an arity error".into(),
                        span: *call.token.get_span(),
                    });
                }
                if !call.args.is_empty() {
                    let mut checked_args = Vec::new();
                    for (call_arg, decl_arg) in call.args.iter_mut().zip(decl_args.iter_mut()) {
                        let checked_arg = self.check_types(call_arg, state)?;
                        let checked_decl_arg = self.check_types(decl_arg, state)?;
                        if checked_arg.check(checked_decl_arg.get_type()) {
                            checked_args.push(checked_arg);
                        } else {
                            return Err(FerryTypeError::MistypedVariable {
                                advice: "types do not match".into(),
                                span: *call.token.get_span(),
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
                } else {
                    Ok(Expr::Call(Call {
                        invoker: call.invoker.clone(),
                        name: name.clone(),
                        token: call.token.clone(),
                        args: vec![],
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
            return Err(FerryTypeError::UnknownType {
                advice: "function type unknown at compile time".into(),
                span: *call.token.get_span(),
            });
        }
    }

    fn visit_module(&mut self, module: &mut Module, state: &mut FerryState) -> FerryResult<Expr> {
        let mut checked_fns = vec![];

        for function in module.functions.clone() {
            if let Expr::Function(checked_function) =
                self.check_types(&mut Expr::Function(function), state)?
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
}
