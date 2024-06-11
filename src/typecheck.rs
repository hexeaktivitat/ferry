use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use crate::{
    state::{FerryState, FerryValue},
    syntax::{
        walk_expr, Assign, Binary, Binding, Call, Expr, ExprVisitor, For, Function, Group, If, Lit,
        Loop, Unary, Variable,
    },
    token::{Op, TokenType},
    types::{FerryType, FerryTyping, TypeCheckable, Typing},
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
                    expr_type: FerryTyping::Inferred(FerryType::List),
                    span: *span,
                }))
            }
        }
    }

    fn visit_binary(&mut self, binary: &mut Binary, state: &mut FerryState) -> FerryResult<Expr> {
        let left = self.check_types(&mut binary.lhs, state)?;
        let right = self.check_types(&mut binary.rhs, state)?;

        match binary.operator.get_token_type() {
            TokenType::Operator(o) => match o {
                Op::Add | Op::Subtract | Op::Multiply | Op::Divide | Op::Equals => {
                    if left.check(right.get_type()) {
                        let expr_type = FerryTyping::Inferred(left.get_type().clone());
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
                _ => Err(FerryTypeError::UnimplementedFeature {
                    advice: "aaa 1".into(),
                    span: *binary.operator.get_span(),
                }),
            },
            _ => Err(FerryTypeError::UnimplementedFeature {
                advice: "aaa 2".into(),
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
        } else if state.get_symbol_value(&variable.name) == None {
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
            if let Ok(value_check) = self.check_types(value, state) {
                return Ok(Expr::Assign(Assign {
                    var: assign.var.clone(),
                    name: assign.name.clone(),
                    value: Some(Box::new(value_check.clone())),
                    expr_type: FerryTyping::Assigned(value_check.get_type().clone()),
                    token: assign.token.clone(),
                }));
            }
        }
        Err(FerryTypeError::UnimplementedFeature {
            advice: "???".into(),
            span: *assign.token.get_span(),
        })
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
                        state.add_symbol(&binding.name, None);
                        return Ok(Expr::Binding(Binding {
                            token: binding.token.clone(),
                            name: binding.name.clone(),
                            assigned_type: binding.assigned_type.clone(),
                            value: Some(Box::new(value_check.clone())),
                            expr_type: FerryTyping::Untyped,
                        }));
                    } else {
                        return Err(FerryTypeError::UnimplementedFeature {
                            advice: "aaa 3".into(),
                            span: *binding.token.get_span(),
                        });
                    }
                } else {
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
            return Ok(Expr::Binding(Binding {
                token: binding.token.clone(),
                name: binding.name.clone(),
                assigned_type: binding.assigned_type.clone(),
                value: None,
                expr_type: FerryTyping::Assigned(assigned_type.clone()),
            }));
        }

        Err(FerryTypeError::UnimplementedFeature {
            advice: "aaa 4".into(),
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
            let variable_checked = self.check_types(variable, state)?;
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
        let return_type = if let Some(ret) = &mut function.return_type {
            Some(Box::new(self.check_types(ret, state)?))
        } else {
            None
        };
        let args = if let Some(arguments) = &mut function.args {
            let mut rets = Vec::new();
            for a in arguments {
                rets.push(self.check_types(a, state)?);
            }
            Some(rets)
        } else {
            None
        };

        let checked_contents = Box::new(self.check_types(&mut function.contents, state)?);

        Ok(Expr::Function(Function {
            token: function.token.clone(),
            name: function.name.clone(),
            args,
            contents: checked_contents,
            return_type,
            expr_type: FerryTyping::Undefined,
        }))
    }

    fn visit_call(&mut self, call: &mut Call, state: &mut FerryState) -> FerryResult<Expr> {
        if let Some(FerryValue::Function { declaration }) = &mut state.get_symbol_value(&call.name)
        {
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
                        // let checked_decl_arg = self.check_types(decl_arg, state)?;
                        // if checked_arg.check(checked_decl_arg.get_type()) {
                        checked_args.push(checked_arg);
                        // } else {
                        //     return Err(FerryTypeError::MistypedVariable {
                        //         advice: "types do not match".into(),
                        //         span: *call.token.get_span(),
                        //     });
                        // }
                    }
                    return Ok(Expr::Call(Call {
                        invoker: call.invoker.clone(),
                        name: call.name.clone(),
                        token: call.token.clone(),
                        args: checked_args,
                        expr_type: declaration.expr_type.clone(),
                    }));
                } else {
                    return Ok(Expr::Call(Call {
                        invoker: call.invoker.clone(),
                        name: call.name.clone(),
                        token: call.token.clone(),
                        args: vec![],
                        expr_type: declaration.expr_type.clone(),
                    }));
                }
            } else {
                return Ok(Expr::Call(Call {
                    invoker: call.invoker.clone(),
                    name: call.name.clone(),
                    token: call.token.clone(),
                    args: call.args.clone(),
                    expr_type: declaration.expr_type.clone(),
                }));
            }
        } else {
            return Err(FerryTypeError::UnknownType {
                advice: "function type unknown at compile time".into(),
                span: *call.token.get_span(),
            });
        }
    }
}
