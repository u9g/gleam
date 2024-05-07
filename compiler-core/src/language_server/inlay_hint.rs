use std::{collections::HashMap, sync::Arc};

use ecow::EcoString;
use itertools::Itertools;
use lsp_types::{InlayHint, InlayHintKind, InlayHintParams, Position};

use crate::{
    ast::{
        self,
        visit::{self, Visit},
        Arg, Pattern, SrcSpan, Statement, TypeAst, TypeAstVar, TypedExpr,
    },
    build::{self},
    line_numbers::LineNumbers,
    type_::{Type, TypeVar},
};

pub struct InlayHintSearcher<'a> {
    line_numbers: LineNumbers,
    params: &'a InlayHintParams,
    code: &'a EcoString,
    module: &'a ast::TypedModule,
    hints: Vec<InlayHint>,
    type_param_stack: Vec<HashMap<u64, EcoString>>,
}

const SHOULD_ADD_PARAM_ANNOTATIONS: bool = true;
const SHOULD_ADD_FINAL_STATMENT_IN_BODY_ANNOTATIONS: bool = true;
const SHOULD_ADD_LET_ANNOTATIONS: bool = true;
const SHOULD_ADD_PIPELINE_ANNOTATIONS: bool = true;
const SHOULD_ADD_USE_ANNOTATIONS: bool = true;
const SHOULD_ADD_PATTERN_ANNOTATIONS: bool = true;

impl<'ast> Visit<'ast> for InlayHintSearcher<'_> {
    fn visit_typed_function(&mut self, fun: &'ast ast::TypedFunction) {
        let type_params = extract_type_parameters_from_fn(
            &fun.return_annotation,
            &fun.return_type,
            &fun.arguments,
            &fun.body,
        );

        self.type_param_stack.push(type_params);

        self.handle_final_statement_in_body(Some(fun.body.last()));

        visit::visit_typed_function(self, fun);

        let _ = self.type_param_stack.pop().unwrap();
    }

    fn visit_typed_expr_block(
        &mut self,
        location: &'ast SrcSpan,
        statements: &'ast [ast::TypedStatement],
    ) {
        self.handle_final_statement_in_body(statements.last());

        visit::visit_typed_expr_block(self, location, statements);
    }

    fn visit_typed_expr_fn(
        &mut self,
        location: &'ast SrcSpan,
        typ: &'ast Arc<Type>,
        is_capture: &'ast bool,
        args: &'ast [ast::TypedArg],
        body: &'ast [ast::TypedStatement],
        return_annotation: &'ast Option<TypeAst>,
    ) {
        let type_params = extract_type_parameters_from_fn(return_annotation, &typ, args, body);

        self.type_param_stack.push(type_params);

        self.handle_final_statement_in_body(body.last());

        visit::visit_typed_expr_fn(
            self,
            location,
            typ,
            is_capture,
            args,
            body,
            return_annotation,
        );

        let _ = self.type_param_stack.pop().unwrap();
    }

    fn visit_argument(&mut self, arg: &'ast Arg<Arc<Type>>) {
        if arg.annotation.is_none() && SHOULD_ADD_PARAM_ANNOTATIONS {
            self.annotate_location_with_type(arg.location, &arg.type_, true);
        }

        visit::visit_argument(self, arg);
    }

    fn visit_typed_assignment(&mut self, assignment: &'ast ast::TypedAssignment) {
        if assignment.annotation.is_none() && SHOULD_ADD_LET_ANNOTATIONS {
            self.annotate_location_with_type(
                assignment.pattern.location(),
                &assignment.type_(),
                true,
            );
        }
        visit::visit_typed_assignment(self, assignment);
    }

    fn visit_typed_expr_pipeline(
        &mut self,
        location: &'ast SrcSpan,
        assignments: &'ast [ast::TypedAssignment],
        finally: &'ast TypedExpr,
    ) {
        if SHOULD_ADD_PIPELINE_ANNOTATIONS {
            if &(self.code
                [(finally.location().end as usize)..(finally.location().end as usize) + 1])
                == "\n"
            {
                self.annotate_location_with_type(finally.location(), &finally.type_(), false);
            }

            for asmt in assignments {
                if &(self.code[(asmt.location.end as usize)..(asmt.location.end as usize) + 1])
                    == "\n"
                {
                    self.annotate_location_with_type(asmt.location, &asmt.type_(), false);
                }
            }
        }

        visit::visit_typed_expr_pipeline(self, location, assignments, finally);
    }

    fn visit_typed_expr_call(
        &mut self,
        location: &'ast SrcSpan,
        typ: &'ast Arc<Type>,
        fun: &'ast TypedExpr,
        args: &'ast [crate::type_::TypedCallArg],
    ) {
        if SHOULD_ADD_USE_ANNOTATIONS {
            for arg in args {
                if arg.implicit {
                    if let TypedExpr::Fn { args: use_args, .. } = &arg.value {
                        for use_arg in use_args {
                            self.annotate_location_with_type(
                                use_arg.location,
                                &use_arg.type_,
                                true,
                            );
                        }
                    }
                }
            }
        }
        visit::visit_typed_expr_call(self, location, typ, fun, args);
    }

    fn visit_typed_clause(&mut self, clause: &'ast ast::TypedClause) {
        if SHOULD_ADD_PATTERN_ANNOTATIONS {
            for pattern in &clause.pattern {
                self.handle_pattern(pattern);
            }
            for patterns in &clause.alternative_patterns {
                for pattern in patterns {
                    self.handle_pattern(pattern);
                }
            }
        }
        visit::visit_typed_clause(self, clause);
    }
}

fn extract_type_parameters_from_fn<'ast>(
    return_annotation: &'ast Option<TypeAst>,
    return_type: &'ast Type,
    args: &'ast [ast::TypedArg],
    body: &'ast [ast::TypedStatement],
) -> HashMap<u64, EcoString> {
    let mut type_parameters = HashMap::new();

    if let Some(annotation) = &return_annotation {
        if let crate::type_::Type::Var { type_ } = &*return_type {
            if let TypeVar::Generic { id } = &*type_.borrow() {
                let _ = type_parameters.insert(*id, annotation);
            }
        }
    }

    for argument in args {
        if let Some(annotation) = &argument.annotation {
            if let crate::type_::Type::Var { type_ } = &*argument.type_ {
                if let TypeVar::Generic { id } = &*type_.borrow() {
                    let _ = type_parameters.insert(*id, annotation);
                }
            }
        }
    }

    for statement in body {
        if let Statement::Assignment(st) = statement {
            if let Some(annotation) = &st.annotation {
                // If user has provided an annotation that is a type parameter, make a note of it
                if let Type::Var { type_ } = &*st.value.type_() {
                    if let TypeVar::Generic { id } = &*type_.borrow() {
                        let _ = type_parameters.insert(*id, annotation);
                    }
                }
            }
        }
    }

    type_parameters
        .into_iter()
        .map(|x| {
            let TypeAst::Var(type_ast_var) = x.1 else {
                unreachable!()
            };
            let TypeAstVar { location: _, name } = type_ast_var;
            (x.0, name.to_owned())
        })
        .collect()
}

#[allow(unused)]
enum AnnotationPosition {
    Before,
    After,
}

impl<'ast> InlayHintSearcher<'ast> {
    pub fn new(module: &'ast build::Module, params: &'ast InlayHintParams) -> Self {
        Self {
            line_numbers: LineNumbers::new(&module.code),
            params,
            code: &module.code,
            module: &module.ast,
            hints: vec![],
            type_param_stack: vec![],
        }
    }

    fn handle_pattern(&mut self, pattern: &Pattern<Arc<Type>>) {
        match pattern {
            ast::Pattern::Discard { .. }
            | ast::Pattern::String { .. }
            | ast::Pattern::Float { .. }
            | ast::Pattern::Int { .. } => {}
            ast::Pattern::Constructor { arguments, .. } => {
                for argument in arguments {
                    self.handle_pattern(&argument.value)
                }
            }
            ast::Pattern::Variable {
                location, type_, ..
            } => self.annotate_location_with_type(*location, type_, true),
            ast::Pattern::VarUsage {
                location, type_, ..
            } => self.annotate_location_with_type(*location, type_, true),
            ast::Pattern::Assign { pattern, .. } => self.handle_pattern(pattern),
            ast::Pattern::List { elements, tail, .. } => {
                // todo: should the array's type be shown or the element types?
                for element in elements {
                    self.handle_pattern(element)
                }
                if let Some(tail) = tail {
                    self.handle_pattern(tail)
                }
            }
            ast::Pattern::Tuple { elems, .. } => {
                for element in elems {
                    self.handle_pattern(element)
                }
            }
            ast::Pattern::BitArray { .. } => { /* todo */ }
            ast::Pattern::StringPrefix { .. } => { /* todo */ }
        }
    }

    fn handle_final_statement_in_body(
        &mut self,
        statement: Option<&Statement<Arc<Type>, TypedExpr>>,
    ) {
        if !SHOULD_ADD_FINAL_STATMENT_IN_BODY_ANNOTATIONS {
            return;
        }

        if let Some(stmt) = statement {
            if let Statement::Expression(expr) = stmt {
                if !matches!(expr, crate::ast::TypedExpr::Panic { .. }) {
                    self.annotate_location_with_type(expr.location(), &expr.type_(), false);
                }
            }
        }
    }

    pub fn inlay_hints(mut self) -> Vec<InlayHint> {
        self.visit_typed_module(self.module);

        self.hints
    }

    fn type_to_string(&self, type_: &Type) -> String {
        match type_ {
            Type::Named { name, args, .. } => format!(
                "{}{}",
                name,
                if !args.is_empty() {
                    format!(
                        "({})",
                        args.iter().map(|arg| self.type_to_string(arg)).join(", ")
                    )
                } else {
                    "".into()
                }
            ),
            Type::Fn { args, retrn } => format!(
                "fn({}) -> {}",
                args.iter()
                    .map(|arg| self.type_to_string(arg.as_ref()))
                    .join(", "),
                self.type_to_string(retrn)
            ),
            Type::Var { type_ } => match &*type_.borrow() {
                TypeVar::Generic { id } | TypeVar::Unbound { id } => self
                    .type_param_stack
                    .iter()
                    .rev()
                    .find_map(|type_id_to_generic_name| type_id_to_generic_name.get(id))
                    .map(|x| x.to_string())
                    .unwrap_or("?".into()),
                TypeVar::Link { type_ } => format!("{}", self.type_to_string(&type_)),
            },
            Type::Tuple { elems } => format!(
                "#({})",
                elems
                    .iter()
                    .map(|elem| self.type_to_string(elem))
                    .join(", ")
            ),
        }
    }

    fn annotate_location_with_type(
        &mut self,
        span: SrcSpan,
        type_: &Type,
        is_let_stmt_annotation: bool,
    ) {
        let annotation = if is_let_stmt_annotation {
            format!(": {}", self.type_to_string(type_))
        } else {
            self.type_to_string(type_).into()
        };

        self.annotate_location_with_string(span, annotation, AnnotationPosition::After)
    }

    fn annotate_location_with_string(
        &mut self,
        span: SrcSpan,
        annotation: String,
        position: AnnotationPosition,
    ) {
        let linecol = self.line_numbers.line_and_column_number(match position {
            AnnotationPosition::Before => span.start,
            AnnotationPosition::After => span.end,
        });

        let position = Position::new(linecol.line - 1, linecol.column - 1);

        if !(self.params.range.start < position && self.params.range.end > position) {
            return;
        }

        if self.hints.iter().any(|x| {
            x.position.line == linecol.line - 1 && x.position.character == linecol.column - 1
        }) {
            return;
        }

        self.hints.push(InlayHint {
            position,
            label: lsp_types::InlayHintLabel::String(annotation),
            kind: Some(InlayHintKind::TYPE),
            text_edits: None,
            tooltip: None,
            padding_left: Some(true),
            padding_right: None,
            data: None,
        })
    }
}
