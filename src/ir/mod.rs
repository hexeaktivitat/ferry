use miette::{Diagnostic, Result};
use thiserror::Error;

use crate::{
    lexer::token::{Op, TokenType},
    parser::syntax::{
        Assign, Binary, Binding, Call, Expr, ExprVisitor, For, Function, Group, If, Import, Lit,
        Loop, Module, Unary, Variable, walk_expr,
    },
    state::{
        State,
        types::FerryType,
        value::{FuncVal, Value},
    },
};

pub use opcode::Opcode;

mod opcode;

/// Intermediate Representation for Ferry VM
/// Part of compilation process, intended to be a high-level assembly language
/// similar to LLVM-IR, cranelift-IR, etc.

#[derive(Error, Diagnostic, Debug)]
pub enum FerryIrError {}

// Type alias for addressing constants, etc.
// Used for things that live outside the stack
pub type FerryAddr = u8;

#[derive(Debug)]
pub struct Ir {
    // AST to be lowered to this IR
    heap_ptr: FerryAddr,
}

type FerryResult<T> = Result<T, FerryIrError>;

impl Ir {
    pub fn new() -> Self {
        Self { heap_ptr: 0x00 }
    }

    pub fn lower(&mut self, typed_ast: &[Expr], state: &mut State) -> FerryResult<Vec<Opcode>> {
        let mut program = vec![];
        let mut functions = vec![];

        for expr in typed_ast.iter() {
            if let Expr::Function(_) = expr {
                self.assemble_opcode(expr, state).map_or_else(
                    |err| eprintln!("{err:?}"),
                    |mut instructions| functions.append(&mut instructions),
                );
            } else if let Expr::Module(module) = expr {
                for function in module.functions.clone() {
                    self.assemble_opcode(&Expr::Function(function), state)
                        .map_or_else(
                            |err| eprintln!("{err:?}"),
                            |mut instructions| functions.append(&mut instructions),
                        );
                }
            } else {
                self.assemble_opcode(expr, state).map_or_else(
                    |err| eprintln!("{err:?}"),
                    |mut instructions| program.append(&mut instructions),
                );
            }
        }

        program.push(Opcode::Return);

        for (idx, inst) in program.clone().iter().enumerate() {
            if let Opcode::Label(name) = inst {
                state.add_label(name, idx + 1);
            }
        }

        Ok(program)
    }

    fn assemble_opcode(&mut self, expr: &Expr, state: &mut State) -> FerryResult<Vec<Opcode>> {
        walk_expr(&mut *self, expr, state)
    }
}

#[expect(unused_variables)]
impl ExprVisitor<FerryResult<Vec<Opcode>>, &mut State> for &mut Ir {
    fn visit_literal(&mut self, literal: &Lit, state: &mut State) -> FerryResult<Vec<Opcode>> {
        match literal {
            // treat undefined as a 0 for now
            Lit::Undefined { token, expr_type } => Ok(vec![Opcode::LoadI(0)]),
            Lit::Number {
                token,
                value,
                expr_type,
                span,
            } => Ok(vec![Opcode::LoadI(*value)]),
            Lit::Str {
                token,
                value,
                expr_type,
                span,
            } => {
                let ptr = self.heap_ptr;
                self.heap_ptr += 1;
                Ok(vec![Opcode::Alloc(ptr, Value::Str(value.clone()))])
            }
            Lit::Bool {
                token,
                value,
                expr_type,
                span,
            } => Ok(vec![Opcode::LoadI(i64::from(*value))]),
            Lit::List {
                token,
                contents,
                expr_type,
                span,
            } => {
                let ptr = self.heap_ptr;
                self.heap_ptr += 1;

                let mut value_insts = vec![];
                for expr in contents {
                    value_insts.append(&mut self.assemble_opcode(expr, state)?);
                }

                let value_iter = value_insts.iter();
                let mut instructions = vec![Opcode::Alloc(ptr, Value::List(Vec::new()))];
                for inst in value_insts {
                    instructions.append(&mut vec![inst.clone()]);
                    instructions.append(&mut vec![Opcode::Cons]);
                }

                Ok(instructions)
            }
        }
    }

    fn visit_binary(&mut self, binary: &Binary, state: &mut State) -> FerryResult<Vec<Opcode>> {
        if let TokenType::Operator(op) = binary.operator.get_token_type() {
            match op {
                Op::Add => {
                    let mut instructions = vec![];

                    let mut left = self.assemble_opcode(&binary.lhs, state)?;
                    let mut right = self.assemble_opcode(&binary.rhs, state)?;

                    instructions.append(&mut left);
                    instructions.append(&mut right);
                    instructions.append(&mut vec![Opcode::Add]);

                    Ok(instructions)
                }
                Op::Subtract => {
                    let mut instructions = vec![];

                    let mut left = self.assemble_opcode(&binary.lhs, state)?;
                    let mut right = self.assemble_opcode(&binary.rhs, state)?;

                    instructions.append(&mut left);
                    instructions.append(&mut right);
                    instructions.append(&mut vec![Opcode::Sub]);

                    Ok(instructions)
                }
                Op::Multiply => {
                    let mut instructions = vec![];

                    let mut left = self.assemble_opcode(&binary.lhs, state)?;
                    let mut right = self.assemble_opcode(&binary.rhs, state)?;

                    instructions.append(&mut left);
                    instructions.append(&mut right);
                    instructions.append(&mut vec![Opcode::Mul]);

                    Ok(instructions)
                }
                Op::Divide => {
                    let mut instructions = vec![];

                    let mut left = self.assemble_opcode(&binary.lhs, state)?;
                    let mut right = self.assemble_opcode(&binary.rhs, state)?;

                    instructions.append(&mut left);
                    instructions.append(&mut right);
                    instructions.append(&mut vec![Opcode::Div]);

                    Ok(instructions)
                }
                Op::Equals => unreachable!(),
                Op::LessThan => {
                    let mut instructions = vec![];

                    let mut left = self.assemble_opcode(&binary.lhs, state)?;
                    let mut right = self.assemble_opcode(&binary.rhs, state)?;

                    instructions.append(&mut left);
                    instructions.append(&mut right);
                    instructions.append(&mut vec![Opcode::Lesser]);

                    Ok(instructions)
                }
                Op::GreaterThan => {
                    let mut instructions = vec![];

                    let mut left = self.assemble_opcode(&binary.lhs, state)?;
                    let mut right = self.assemble_opcode(&binary.rhs, state)?;

                    instructions.append(&mut left);
                    instructions.append(&mut right);
                    instructions.append(&mut vec![Opcode::Greater]);

                    Ok(instructions)
                }
                Op::Equality => {
                    let mut instructions = vec![];

                    let mut left = self.assemble_opcode(&binary.lhs, state)?;
                    let mut right = self.assemble_opcode(&binary.rhs, state)?;

                    instructions.append(&mut left);
                    instructions.append(&mut right);
                    instructions.append(&mut vec![Opcode::Equal]);

                    Ok(instructions)
                }
                Op::LessEqual => {
                    let mut instructions = vec![];

                    let mut left = self.assemble_opcode(&binary.lhs, state)?;
                    let mut right = self.assemble_opcode(&binary.rhs, state)?;

                    instructions.append(&mut left);
                    instructions.append(&mut right);
                    instructions.append(&mut vec![Opcode::Greater, Opcode::Not]);

                    Ok(instructions)
                }
                Op::GreaterEqual => {
                    let mut instructions = vec![];

                    let mut left = self.assemble_opcode(&binary.lhs, state)?;
                    let mut right = self.assemble_opcode(&binary.rhs, state)?;

                    instructions.append(&mut left);
                    instructions.append(&mut right);
                    instructions.append(&mut vec![Opcode::Lesser, Opcode::Not]);

                    Ok(instructions)
                }
                Op::GetI => {
                    let mut instructions = vec![];

                    let mut left = self.assemble_opcode(&binary.lhs, state)?;
                    let mut right = self.assemble_opcode(&binary.rhs, state)?;

                    instructions.append(&mut left);
                    instructions.append(&mut right);
                    instructions.push(Opcode::GetI);

                    Ok(instructions)
                }
                Op::Cons => {
                    let mut instructions = vec![];

                    let mut left = self.assemble_opcode(&binary.lhs, state)?;
                    let mut right = self.assemble_opcode(&binary.rhs, state)?;

                    instructions.append(&mut left);
                    instructions.append(&mut right);
                    instructions.push(Opcode::Cons);

                    Ok(instructions)
                }
            }
        } else {
            Ok(vec![Opcode::Nop])
        }
    }

    fn visit_unary(&mut self, unary: &Unary, state: &mut State) -> FerryResult<Vec<Opcode>> {
        match unary.operator.get_token_type() {
            TokenType::Operator(Op::Subtract) => {
                let mut instructions = vec![];

                let mut right = self.assemble_opcode(&unary.rhs, state)?;

                instructions.append(&mut right);
                instructions.append(&mut vec![Opcode::LoadI(-1), Opcode::Mul]);

                Ok(instructions)
            }
            _ => unreachable!(),
        }
    }

    fn visit_variable(
        &mut self,
        variable: &Variable,
        state: &mut State,
    ) -> FerryResult<Vec<Opcode>> {
        let instructions = vec![Opcode::Get(variable.name.clone())];

        Ok(instructions)
    }

    fn visit_assign(&mut self, assign: &Assign, state: &mut State) -> FerryResult<Vec<Opcode>> {
        let mut instructions = vec![];

        let id = assign.name.clone();
        // let mut var = self.assemble_opcode(&mut assign.var, state)?;
        let mut value_instructions = if let Some(val) = assign.value.as_ref() {
            self.assemble_opcode(val, state)?
        } else {
            vec![]
        };

        // instructions.append(&mut var);
        instructions.append(&mut value_instructions);
        instructions.append(&mut vec![Opcode::Set(id)]);

        Ok(instructions)
    }

    fn visit_if_expr(&mut self, if_expr: &If, state: &mut State) -> FerryResult<Vec<Opcode>> {
        let mut instructions = vec![];

        let mut conditional = self.assemble_opcode(&if_expr.condition, state)?;
        let mut then_expr = self.assemble_opcode(&if_expr.then_expr, state)?;
        let mut else_expr = if let Some(else_expr) = if_expr.else_expr.as_ref() {
            self.assemble_opcode(else_expr, state)?
        } else {
            vec![]
        };

        let else_offset = else_expr.len();
        then_expr.push(Opcode::Jump(else_offset));

        let then_offset = then_expr.len();

        instructions.append(&mut conditional);
        instructions.push(Opcode::JumpCond(then_offset));
        instructions.append(&mut then_expr);
        instructions.append(&mut else_expr);

        // println!("if instructions ONLY: {:?}", instructions);

        Ok(instructions)
    }

    fn visit_group(&mut self, group: &Group, state: &mut State) -> FerryResult<Vec<Opcode>> {
        let instructions = self.assemble_opcode(&group.contents, state)?;

        Ok(instructions)
    }

    fn visit_binding(&mut self, binding: &Binding, state: &mut State) -> FerryResult<Vec<Opcode>> {
        let mut instructions = vec![];

        let mut value = if let Some(v) = &binding.value {
            self.assemble_opcode(v, state)?
        } else {
            vec![Opcode::LoadI(0)]
        };

        instructions.append(&mut value);
        instructions.append(&mut vec![Opcode::Set(binding.name.clone())]);

        Ok(instructions)
    }

    fn visit_loop(&mut self, loop_expr: &Loop, state: &mut State) -> FerryResult<Vec<Opcode>> {
        let mut instructions = vec![];

        let mut cond_inst = if let Some(cond) = loop_expr.condition.as_ref() {
            self.assemble_opcode(cond, state)?
        } else {
            vec![]
        };

        let mut contents = self.assemble_opcode(&loop_expr.contents, state)?;

        instructions.append(&mut cond_inst);
        instructions.push(Opcode::JumpCond(contents.len() + 1));
        instructions.append(&mut contents);
        instructions.push(Opcode::JumpBack(instructions.len() + 1));

        Ok(instructions)
    }

    fn visit_for(&mut self, for_expr: &For, state: &mut State) -> FerryResult<Vec<Opcode>> {
        let mut instructions = vec![];

        if let Some(variable) = for_expr.variable.as_ref() {
            let name = variable.get_token().get_id().unwrap();
            let mut iter_inst = self.assemble_opcode(&for_expr.iterator, state)?;
            let mut contents_inst = self.assemble_opcode(&for_expr.contents, state)?;
            let contents_len = contents_inst.len();

            instructions.append(&mut iter_inst);
            instructions.push(Opcode::Iter);
            instructions.push(Opcode::Set(name.clone()));
            instructions.append(&mut contents_inst);
            instructions.push(Opcode::JumpCond(1));
            instructions.push(Opcode::JumpBack(contents_len + 4));
        }

        Ok(instructions)
    }

    fn visit_function(
        &mut self,
        function: &Function,
        state: &mut State,
    ) -> FerryResult<Vec<Opcode>> {
        let mut instructions = vec![];

        let mut arity = 0;

        let mut args_inst = if let Some(args) = function.args.as_ref() {
            let mut ret = vec![];

            for arg in args {
                if let Expr::Binding(binding) = arg {
                    ret.push(Opcode::Set(binding.name.clone()));
                    // ret.push(FerryOpcode::Get(binding.name.clone()));
                }
            }

            arity = ret.len();
            ret
        } else {
            vec![]
        };
        let mut function_inst = self.assemble_opcode(&function.contents, state)?;

        instructions.append(&mut args_inst);
        instructions.append(&mut function_inst);
        instructions.push(Opcode::Return);

        state.add_symbol(
            &function.name,
            Some(Value::Function(FuncVal {
                declaration: Some(function.clone()),
                name: function.name.clone(),
                func_type: FerryType::Function,
                instructions,
                arity,
            })),
        );

        Ok(vec![])
    }

    fn visit_call(&mut self, call: &Call, state: &mut State) -> FerryResult<Vec<Opcode>> {
        let mut instructions = vec![];

        let mut args_inst = {
            let mut ret = vec![];
            for arg in &mut call.args.clone() {
                ret.append(&mut self.assemble_opcode(arg, state)?);
            }
            ret
        };

        instructions.append(&mut args_inst);
        instructions.push(Opcode::Call(call.name.clone()));

        Ok(instructions)
    }

    fn visit_module(&mut self, module: &Module, state: &mut State) -> FerryResult<Vec<Opcode>> {
        todo!()
    }

    fn visit_import(&mut self, import: &Import, state: &mut State) -> FerryResult<Vec<Opcode>> {
        let instructions = vec![];

        for function in import.functions.clone() {
            self.assemble_opcode(&Expr::Function(function), state)?;
        }

        Ok(instructions)
    }
}
