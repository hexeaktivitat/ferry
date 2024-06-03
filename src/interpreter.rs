use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use crate::{
    state::{FerryState, FerryValue},
    syntax::{
        walk_expr, Assign, Binary, Binding, Expr, ExprVisitor, For, Function, Group, If,
        Lit as SLit, Loop, Unary, Variable,
    },
    token::{Op, TokenType as TT},
};

#[derive(Error, Diagnostic, Debug)]
pub enum FerryInterpreterError {
    #[error("Invalid operation")]
    InvalidOperation {
        #[help]
        help: String,
        #[label]
        span: SourceSpan,
    },
    #[error("Unimplemented feature")]
    Unimplemented {
        #[help]
        help: String,
        #[label]
        span: SourceSpan,
    },
}

type FerryResult<T> = Result<Option<T>, FerryInterpreterError>;

pub struct FerryInterpreter {
    syntax: Vec<Expr>,
}

impl FerryInterpreter {
    pub fn new(syntax: Vec<Expr>) -> Self {
        Self { syntax }
    }

    pub fn interpret(
        &mut self,
        state: &mut FerryState,
    ) -> Result<Option<FerryValue>, Vec<FerryInterpreterError>> {
        let mut ret = None;
        let mut errors = vec![];
        for code in self.syntax.clone().iter_mut() {
            match self.evaluate(code, state) {
                Ok(r) => ret = r,
                Err(e) => errors.push(e),
            };
        }
        if errors.is_empty() {
            Ok(ret)
        } else {
            Err(errors)
        }
    }

    fn evaluate(&mut self, code: &mut Expr, state: &mut FerryState) -> FerryResult<FerryValue> {
        walk_expr(&mut *self, code, state)
    }
}

impl ExprVisitor<FerryResult<FerryValue>, &mut FerryState> for &mut FerryInterpreter {
    fn visit_literal(
        &mut self,
        literal: &mut SLit,
        state: &mut FerryState,
    ) -> FerryResult<FerryValue> {
        match literal {
            SLit::Number {
                value,
                expr_type: _,
                span: _,
                token: _,
            } => Ok(Some(FerryValue::Number(*value))),
            SLit::Str {
                value,
                expr_type: _,
                span: _,
                token: _,
            } => Ok(Some(FerryValue::Str(value.clone()))),
            SLit::Bool {
                value,
                expr_type: _,
                span: _,
                token: _,
            } => Ok(Some(FerryValue::Boolean(*value))),
            SLit::Undefined {
                expr_type: _,
                token: _,
            } => Ok(Some(FerryValue::Unit)),
            SLit::List {
                token: _,
                contents,
                expr_type: _,
                span: _,
            } => {
                let mut values: Vec<FerryValue> = Vec::new();
                for c in contents {
                    if let Some(value) = self.evaluate(c, state)? {
                        values.push(value);
                    } else {
                        values.push(FerryValue::Unit);
                    }
                }
                Ok(Some(FerryValue::List(values)))
            }
        }
    }

    fn visit_binary(
        &mut self,
        binary: &mut Binary,
        state: &mut FerryState,
    ) -> FerryResult<FerryValue> {
        let left = self.evaluate(&mut binary.lhs, state)?;
        let right = self.evaluate(&mut binary.rhs, state)?;

        let op = &binary.operator;

        match &op.get_token_type() {
            TT::Operator(o) => match o {
                Op::Add => match (left, right) {
                    (Some(FerryValue::Number(l)), Some(FerryValue::Number(r))) => {
                        Ok(Some(FerryValue::Number(l + r)))
                    }
                    _ => Err(FerryInterpreterError::InvalidOperation {
                        help: "Operator only takes values of type Num".into(),
                        span: *op.get_span(),
                    }),
                },
                Op::Subtract => match (left, right) {
                    (Some(FerryValue::Number(l)), Some(FerryValue::Number(r))) => {
                        Ok(Some(FerryValue::Number(l - r)))
                    }
                    _ => Err(FerryInterpreterError::InvalidOperation {
                        help: "Operator only takes values of type Num".into(),
                        span: *op.get_span(),
                    }),
                },

                Op::Multiply => match (left, right) {
                    (Some(FerryValue::Number(l)), Some(FerryValue::Number(r))) => {
                        Ok(Some(FerryValue::Number(l * r)))
                    }
                    _ => Err(FerryInterpreterError::InvalidOperation {
                        help: "Operator only takes values of type Num".into(),
                        span: *op.get_span(),
                    }),
                },
                Op::Divide => match (left, right) {
                    (Some(FerryValue::Number(l)), Some(FerryValue::Number(r))) => {
                        Ok(Some(FerryValue::Number(l / r)))
                    }
                    _ => Err(FerryInterpreterError::InvalidOperation {
                        help: "Operator only takes values of type Num".into(),
                        span: *op.get_span(),
                    }),
                },
                // Op::RightArrow => todo!(),
                Op::Equals => Ok(None),
                Op::LessThan => match (left, right) {
                    (Some(FerryValue::Number(l)), Some(FerryValue::Number(r))) => {
                        Ok(Some(FerryValue::Boolean(l < r)))
                    }
                    _ => Err(FerryInterpreterError::InvalidOperation {
                        help: "Operator only takes values of type Num".into(),
                        span: *op.get_span(),
                    }),
                },
                Op::GreaterThan => match (left, right) {
                    (Some(FerryValue::Number(l)), Some(FerryValue::Number(r))) => {
                        Ok(Some(FerryValue::Boolean(l > r)))
                    }
                    _ => Err(FerryInterpreterError::InvalidOperation {
                        help: "Operator only takes values of type Num".into(),
                        span: *op.get_span(),
                    }),
                },
                Op::Equality => match (left, right) {
                    (Some(FerryValue::Number(l)), Some(FerryValue::Number(r))) => {
                        Ok(Some(FerryValue::Boolean(l == r)))
                    }
                    _ => Err(FerryInterpreterError::InvalidOperation {
                        help: "Operator only takes values of type Num".into(),
                        span: *op.get_span(),
                    }),
                },
                Op::LessEqual => match (left, right) {
                    (Some(FerryValue::Number(l)), Some(FerryValue::Number(r))) => {
                        Ok(Some(FerryValue::Boolean(l <= r)))
                    }
                    _ => Err(FerryInterpreterError::InvalidOperation {
                        help: "Operator only takes values of type Num".into(),
                        span: *op.get_span(),
                    }),
                },
                Op::GreaterEqual => match (left, right) {
                    (Some(FerryValue::Number(l)), Some(FerryValue::Number(r))) => {
                        Ok(Some(FerryValue::Boolean(l >= r)))
                    }
                    _ => Err(FerryInterpreterError::InvalidOperation {
                        help: "Operator only takes values of type Num".into(),
                        span: *op.get_span(),
                    }),
                },
                Op::GetI => match (left, right) {
                    (Some(FerryValue::List(l)), Some(FerryValue::Number(n))) => {
                        let value = l.get(n as usize);
                        match value {
                            Some(v) => Ok(Some(v.clone())),
                            None => Err(FerryInterpreterError::InvalidOperation {
                                help: "Invalid list index access".into(),
                                span: *binary.rhs.get_token().get_span(),
                            }),
                        }
                    }

                    _ => Err(FerryInterpreterError::InvalidOperation {
                        help: "Operator only takes values of type Num".into(),
                        span: *op.get_span(),
                    }),
                },
                Op::Cons => match (left, right) {
                    (Some(FerryValue::List(a)), Some(FerryValue::List(b))) => {
                        let value = [a, b].concat();
                        Ok(Some(FerryValue::List(value)))
                    }

                    _ => Err(FerryInterpreterError::InvalidOperation {
                        help: "Operator only takes values of type List".into(),
                        span: *op.get_span(),
                    }),
                },
                // _ => Err(FerryInterpreterError::InvalidOperation {
                //     help: "this was not a binary op".into(),
                //     span: *binary.operator.get_span(),
                // }),
            },
            _ => Ok(None),
        }
    }

    fn visit_variable(
        &mut self,
        variable: &mut Variable,
        state: &mut FerryState,
    ) -> FerryResult<FerryValue> {
        Ok(state.get_symbol_value(&variable.name))
    }

    fn visit_assign(
        &mut self,
        assign: &mut Assign,
        state: &mut FerryState,
    ) -> FerryResult<FerryValue> {
        if let Some(v) = &mut assign.value {
            let value = self.evaluate(v, state).unwrap();
            state.add_symbol(&assign.name, value.clone());
            Ok(value)
        } else {
            Ok(Some(FerryValue::Number(0.)))
        }
    }

    fn visit_if_expr(
        &mut self,
        if_expr: &mut If,
        state: &mut FerryState,
    ) -> FerryResult<FerryValue> {
        if let Some(conditional) = self.evaluate(&mut if_expr.condition, state)? {
            let value = if conditional.truthiness() {
                self.evaluate(&mut if_expr.then_expr, state)?
            } else if let Some(else_expr) = &mut if_expr.else_expr {
                self.evaluate(else_expr, state)?
            } else {
                None
            };
            Ok(value)
        } else {
            Ok(None)
        }
    }

    fn visit_group(
        &mut self,
        group: &mut Group,
        state: &mut FerryState,
    ) -> FerryResult<FerryValue> {
        self.evaluate(&mut group.contents, state)
    }

    fn visit_binding(
        &mut self,
        binding: &mut Binding,
        state: &mut FerryState,
    ) -> FerryResult<FerryValue> {
        if let Some(v) = &mut binding.value {
            let value = self.evaluate(v, state)?;
            state.add_symbol(&binding.name, value.clone());
            Ok(value)
        } else {
            Ok(Some(FerryValue::Number(0.)))
        }
    }

    fn visit_loop(
        &mut self,
        loop_expr: &mut Loop,
        state: &mut FerryState,
    ) -> FerryResult<FerryValue> {
        if let Some(cond) = &mut loop_expr.condition {
            match self.evaluate(cond, state)? {
                Some(b) => {
                    if b.truthiness() {
                        loop {
                            match self.evaluate(&mut loop_expr.contents, state)? {
                                Some(res) => println!("{res}"),
                                None => (),
                            }
                        }
                    } else {
                        Ok(Some(FerryValue::Unit))
                    }
                }
                None => Ok(Some(FerryValue::Unit)),
            }
        } else {
            println!("{:?}", loop_expr.contents);
            loop {
                match self.evaluate(&mut loop_expr.contents, state)? {
                    Some(res) => println!("{res}"), // this will be replaced with FerryValue::Return
                    None => (),
                }
            }
        }
    }

    fn visit_unary(
        &mut self,
        unary: &mut Unary,
        state: &mut FerryState,
    ) -> FerryResult<FerryValue> {
        let _right = self.evaluate(&mut unary.rhs, state)?;

        match unary.operator.get_token_type() {
            _ => Err(FerryInterpreterError::InvalidOperation {
                help: "Invalid unary operator".into(),
                span: *unary.operator.get_span(),
            }),
        }
    }

    fn visit_for(&mut self, for_expr: &mut For, state: &mut FerryState) -> FerryResult<FerryValue> {
        if let Some(variable) = &mut for_expr.variable {
            let name = variable.get_token().get_id().unwrap_or("error".into());
            let var = self.evaluate(variable, state)?;
            let iterator = self.evaluate(&mut for_expr.iterator, state)?;
            if let Some(value) = iterator {
                match value {
                    FerryValue::List(list) => {
                        for l in list {
                            state.add_symbol(&name, Some(l));
                            self.evaluate(&mut for_expr.contents, state)?;
                        }
                        Ok(None)
                    }
                    _ => Err(FerryInterpreterError::InvalidOperation {
                        help: "Expected iterator to be a List".into(),
                        span: *for_expr.token.get_span(),
                    }),
                }
            } else {
                Err(FerryInterpreterError::InvalidOperation {
                    help: "Expected iterator to be a List".into(),
                    span: *for_expr.token.get_span(),
                })
            }
        } else {
            Err(FerryInterpreterError::Unimplemented {
                help: "didnt do this".into(),
                span: *for_expr.token.get_span(),
            })
        }
    }

    fn visit_function(
        &mut self,
        function: &mut Function,
        state: &mut FerryState,
    ) -> FerryResult<FerryValue> {
        Err(FerryInterpreterError::Unimplemented {
            help: "unimplemented <Function>".into(),
            span: *function.token.get_span(),
        })
    }
}

impl FerryValue {
    fn truthiness(&self) -> bool {
        match self {
            FerryValue::Number(n) => n > &0.,
            FerryValue::Str(s) => !s.is_empty(),
            FerryValue::Boolean(b) => *b,
            FerryValue::Unit => false,
            FerryValue::List(l) => !l.is_empty(),
        }
    }
}
