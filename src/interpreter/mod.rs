// Interpreter is deprecated

use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use crate::{
    lexer::token::{Op, TokenType as TT},
    parser::syntax::{
        walk_expr, Assign, Binary, Binding, Call, Expr, ExprVisitor, For, Function, Group, If,
        Import, Lit as SLit, Loop, Module, Unary, Variable,
    },
    state::{
        types::Typing,
        value::{FuncVal, Value},
        State,
    },
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

#[allow(dead_code)]
pub struct Interpreter;

#[allow(dead_code)]
impl Interpreter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn interpret(
        &mut self,
        typed_ast: &[Expr],
        state: &mut State,
    ) -> Result<Option<Value>, Vec<FerryInterpreterError>> {
        let mut ret = None;
        let mut errors = vec![];
        for code in typed_ast {
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

    fn evaluate(&mut self, code: &Expr, state: &mut State) -> FerryResult<Value> {
        walk_expr(&mut *self, code, state)
    }
}

impl ExprVisitor<FerryResult<Value>, &mut State> for &mut Interpreter {
    fn visit_literal(&mut self, literal: &SLit, state: &mut State) -> FerryResult<Value> {
        match literal {
            SLit::Number {
                value,
                expr_type: _,
                span: _,
                token: _,
            } => Ok(Some(Value::Number(*value))),
            SLit::Str {
                value,
                expr_type: _,
                span: _,
                token: _,
            } => Ok(Some(Value::Str(value.clone()))),
            SLit::Bool {
                value,
                expr_type: _,
                span: _,
                token: _,
            } => Ok(Some(Value::Boolean(*value))),
            SLit::Undefined {
                expr_type: _,
                token: _,
            } => Ok(Some(Value::Unit)),
            SLit::List {
                token: _,
                contents,
                expr_type: _,
                inner_type: _,
                span: _,
            } => {
                let mut values: Vec<Value> = Vec::new();
                for c in contents {
                    if let Some(value) = self.evaluate(c, state)? {
                        values.push(value);
                    } else {
                        values.push(Value::Unit);
                    }
                }
                Ok(Some(Value::List(values)))
            }
        }
    }

    fn visit_binary(&mut self, binary: &Binary, state: &mut State) -> FerryResult<Value> {
        let left = self.evaluate(&binary.lhs, state)?;
        let right = self.evaluate(&binary.rhs, state)?;

        let op = &binary.operator;

        match &op.get_token_type() {
            TT::Operator(o) => match o {
                Op::Add => match (left, right) {
                    (Some(Value::Number(l)), Some(Value::Number(r))) => {
                        Ok(Some(Value::Number(l + r)))
                    }
                    _ => Err(FerryInterpreterError::InvalidOperation {
                        help: "Operator only takes values of type Num".into(),
                        span: *op.get_span(),
                    }),
                },
                Op::Subtract => match (left, right) {
                    (Some(Value::Number(l)), Some(Value::Number(r))) => {
                        Ok(Some(Value::Number(l - r)))
                    }
                    _ => Err(FerryInterpreterError::InvalidOperation {
                        help: "Operator only takes values of type Num".into(),
                        span: *op.get_span(),
                    }),
                },

                Op::Multiply => match (left, right) {
                    (Some(Value::Number(l)), Some(Value::Number(r))) => {
                        Ok(Some(Value::Number(l * r)))
                    }
                    _ => Err(FerryInterpreterError::InvalidOperation {
                        help: "Operator only takes values of type Num".into(),
                        span: *op.get_span(),
                    }),
                },
                Op::Divide => match (left, right) {
                    (Some(Value::Number(l)), Some(Value::Number(r))) => {
                        Ok(Some(Value::Number(l / r)))
                    }
                    _ => Err(FerryInterpreterError::InvalidOperation {
                        help: "Operator only takes values of type Num".into(),
                        span: *op.get_span(),
                    }),
                },
                // Op::RightArrow => todo!(),
                Op::Equals => Ok(None),
                Op::LessThan => match (left, right) {
                    (Some(Value::Number(l)), Some(Value::Number(r))) => {
                        Ok(Some(Value::Boolean(l < r)))
                    }
                    _ => Err(FerryInterpreterError::InvalidOperation {
                        help: "Operator only takes values of type Num".into(),
                        span: *op.get_span(),
                    }),
                },
                Op::GreaterThan => match (left, right) {
                    (Some(Value::Number(l)), Some(Value::Number(r))) => {
                        Ok(Some(Value::Boolean(l > r)))
                    }
                    _ => Err(FerryInterpreterError::InvalidOperation {
                        help: "Operator only takes values of type Num".into(),
                        span: *op.get_span(),
                    }),
                },
                Op::Equality => match (left, right) {
                    (Some(Value::Number(l)), Some(Value::Number(r))) => {
                        Ok(Some(Value::Boolean(l == r)))
                    }
                    _ => Err(FerryInterpreterError::InvalidOperation {
                        help: "Invalid operator types".into(),
                        span: *op.get_span(),
                    }),
                },
                Op::LessEqual => match (left, right) {
                    (Some(Value::Number(l)), Some(Value::Number(r))) => {
                        Ok(Some(Value::Boolean(l <= r)))
                    }
                    _ => Err(FerryInterpreterError::InvalidOperation {
                        help: "Operator only takes values of type Num".into(),
                        span: *op.get_span(),
                    }),
                },
                Op::GreaterEqual => match (left, right) {
                    (Some(Value::Number(l)), Some(Value::Number(r))) => {
                        Ok(Some(Value::Boolean(l >= r)))
                    }
                    _ => Err(FerryInterpreterError::InvalidOperation {
                        help: "Operator only takes values of type Num".into(),
                        span: *op.get_span(),
                    }),
                },
                Op::GetI => match (left, right) {
                    (Some(Value::List(l)), Some(Value::Number(n))) => {
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
                    (Some(Value::List(a)), Some(Value::List(b))) => {
                        let value = [a, b].concat();
                        Ok(Some(Value::List(value)))
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

    fn visit_variable(&mut self, variable: &Variable, state: &mut State) -> FerryResult<Value> {
        Ok(state.get_symbol_value(&variable.name))
    }

    fn visit_assign(&mut self, assign: &Assign, state: &mut State) -> FerryResult<Value> {
        if let Some(v) = &assign.value {
            let value = self.evaluate(v, state).unwrap();
            state.add_symbol(&assign.name, value.clone());
            Ok(value)
        } else {
            Ok(Some(Value::Number(0)))
        }
    }

    fn visit_if_expr(&mut self, if_expr: &If, state: &mut State) -> FerryResult<Value> {
        if let Some(conditional) = self.evaluate(&if_expr.condition, state)? {
            let value = if conditional.truthiness() {
                self.evaluate(&if_expr.then_expr, state)?
            } else if let Some(else_expr) = &if_expr.else_expr {
                self.evaluate(else_expr, state)?
            } else {
                None
            };
            Ok(value)
        } else {
            Ok(None)
        }
    }

    fn visit_group(&mut self, group: &Group, state: &mut State) -> FerryResult<Value> {
        self.evaluate(&group.contents, state)
    }

    fn visit_binding(&mut self, binding: &Binding, state: &mut State) -> FerryResult<Value> {
        if let Some(v) = &binding.value {
            let value = self.evaluate(v, state)?;
            state.add_symbol(&binding.name, value.clone());
            Ok(value)
        } else {
            Ok(Some(Value::Number(0)))
        }
    }

    fn visit_loop(&mut self, loop_expr: &Loop, state: &mut State) -> FerryResult<Value> {
        if let Some(cond) = &loop_expr.condition {
            match self.evaluate(cond, state)? {
                Some(b) => {
                    if b.truthiness() {
                        loop {
                            self.evaluate(&loop_expr.contents.clone(), state)?;
                            if let Ok(Some(b)) = self.evaluate(cond, state) {
                                if !b.truthiness() {
                                    break;
                                }
                            }
                        }
                        Ok(None)
                        // Ok(Some(FerryValue::Unit))
                    } else {
                        Ok(None)
                    }
                }

                None => Ok(Some(Value::Unit)),
            }
        } else {
            println!("{:?}", loop_expr.contents);
            loop {
                if let Some(res) = self.evaluate(&loop_expr.contents, state)? {
                    println!("{res}");
                }
            }
        }
    }

    fn visit_unary(&mut self, unary: &Unary, state: &mut State) -> FerryResult<Value> {
        let _right = self.evaluate(&unary.rhs, state)?;

        #[expect(clippy::match_single_binding)]
        match unary.operator.get_token_type() {
            _ => Err(FerryInterpreterError::InvalidOperation {
                help: "Invalid unary operator".into(),
                span: *unary.operator.get_span(),
            }),
        }
    }

    fn visit_for(&mut self, for_expr: &For, state: &mut State) -> FerryResult<Value> {
        if let Some(variable) = &for_expr.variable {
            let name = variable.get_token().get_id().unwrap_or("error".into());
            let iterator = self.evaluate(&for_expr.iterator, state)?;
            if let Some(value) = iterator {
                match value {
                    Value::List(list) => {
                        for l in list {
                            state.add_symbol(&name, Some(l));
                            self.evaluate(&for_expr.contents, state)?;
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

    fn visit_function(&mut self, function: &Function, state: &mut State) -> FerryResult<Value> {
        let name = function.name.clone();
        let arity = if let Some(args) = function.args.as_ref() {
            args.len()
        } else {
            0
        };
        state.add_symbol(
            &name,
            Some(Value::Function(FuncVal {
                declaration: Some(function.clone()),
                name: name.clone(),
                func_type: *function.expr_type.get_type(),
                instructions: vec![],
                arity,
            })),
        );

        Ok(None)
    }

    fn visit_call(&mut self, call: &Call, state: &mut State) -> FerryResult<Value> {
        if let Some(Value::Function(FuncVal {
            declaration: Some(function),
            name: _,
            func_type: _,
            instructions: _,
            arity: _,
        })) = &state.get_symbol_value(&call.name)
        {
            if let Some(params) = &function.args {
                if call.args.is_empty() {
                    let mut param_state = state.clone();
                    for (arg, param_var) in call.args.iter().zip(params.iter()) {
                        if let Expr::Binding(var) = param_var {
                            let arg_val = self.evaluate(arg, state)?;
                            param_state.add_symbol(&var.name, arg_val);
                        }
                    }
                    self.evaluate(&function.contents, &mut param_state)
                } else {
                    self.evaluate(&function.contents, state)
                }
            } else {
                self.evaluate(&function.contents, state)
            }
        } else {
            Err(FerryInterpreterError::InvalidOperation {
                help: "function does not exist".into(),
                span: *call.token.get_span(),
            })
        }
    }

    fn visit_module(&mut self, _module: &Module, _state: &mut State) -> FerryResult<Value> {
        todo!()
    }

    fn visit_import(&mut self, _import: &Import, _state: &mut State) -> FerryResult<Value> {
        todo!()
    }
}
