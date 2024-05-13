use std::sync::Arc;

use crate::{
    ast::{
        self,
        visit::{self, Visit},
        Pattern, SrcSpan, TypedClause,
    },
    build::{self, ProjectCompiler},
    io::{CommandExecutor, FileSystemReader, FileSystemWriter},
    line_numbers::LineNumbers,
    type_::{ModuleValueConstructor, Type, TypeVar, ValueConstructorVariant},
};
use ecow::EcoString;
use lsp_types::{SemanticToken, SemanticTokenType, SemanticTokensParams};

use super::src_span_to_lsp_range;

macro_rules! semantic_tokens {
    ($($name: ident => $lsp_name: ident),* $(,)?) => {
        // C-like enumerations can be casted to 0,1,2 etc in order which is exactly what the
        // LSP protocal needs for the mapping
        #[derive(Debug)]
        #[allow(non_camel_case_types)]
        pub(crate) enum SemanticTokenMapping {
            $(
            $name,
            )*
        }

        impl SemanticTokenMapping {
            pub(crate) const LSP_MAPPING: &'static [SemanticTokenType] = &[
                $(
                SemanticTokenType::$lsp_name,
                )*
            ];
        }

        $(
        pub(crate) const $name: u32 = SemanticTokenMapping::$name as u32;
        )*
    }
}

// Constructs an integer <=> string mapping according in accordance to
// https://microsoft.github.io/language-server-protocol/specifications/specification-3-17/#semanticTokensLegend
semantic_tokens! {
    SEMANTIC_TOKEN_TYPE => TYPE,
    SEMANTIC_TOKEN_PROPERTY => PROPERTY,
    SEMANTIC_TOKEN_FUNCTION => FUNCTION,
    SEMANTIC_TOKEN_STRUCT => STRUCT,
    SEMANTIC_TOKEN_ENUM_MEMBER => ENUM_MEMBER,
    SEMANTIC_TOKEN_NUMBER => NUMBER,
    SEMANTIC_TOKEN_STRING => STRING,
    SEMANTIC_TOKEN_VARIABLE => VARIABLE,
}

pub struct SemanticTokenSearcher<'a, IO>
where
    IO: CommandExecutor + FileSystemWriter + FileSystemReader + Clone,
{
    line_numbers: LineNumbers,
    params: &'a SemanticTokensParams,
    module: &'a ast::TypedModule,
    semantic_tokens: Vec<(SrcSpan, u32)>,
    project_compiler: &'a ProjectCompiler<IO>,

    last_line: u32,
    last_char: u32,
}

impl<'ast, IO> Visit<'ast> for SemanticTokenSearcher<'_, IO>
where
    IO: CommandExecutor + FileSystemWriter + FileSystemReader + Clone,
{
    fn visit_typed_expr_var(
        &mut self,
        location: &'ast SrcSpan,
        constructor: &'ast crate::type_::ValueConstructor,
        name: &'ast EcoString,
    ) {
        // could be _pipe, etc
        if !name.starts_with("_") {
            let token_type = self.type_to_token_type(
                &constructor.type_,
                matches!(constructor.variant, ValueConstructorVariant::Record { .. }),
            );
            self.push_range(*location, token_type);
        }
        visit::visit_typed_expr_var(self, location, constructor, name);
    }

    fn visit_typed_pattern(&mut self, pattern: &'ast Pattern<Arc<Type>>) {
        self.visit_pattern(&pattern);
        visit::visit_typed_pattern(self, pattern)
    }

    fn visit_typed_expr_module_select(
        &mut self,
        location: &'ast SrcSpan,
        typ: &'ast Arc<Type>,
        label: &'ast EcoString,
        module_name: &'ast EcoString,
        module_alias: &'ast EcoString,
        constructor: &'ast ModuleValueConstructor,
    ) {
        let token_type = self.type_to_token_type(
            typ,
            matches!(constructor, ModuleValueConstructor::Record { .. }),
        );
        self.push_range(
            SrcSpan {
                start: location.end - label.len() as u32,
                end: location.end,
            },
            token_type,
        );

        visit::visit_typed_expr_module_select(
            self,
            location,
            typ,
            label,
            module_name,
            module_alias,
            constructor,
        );
    }

    fn visit_typed_expr_tuple_index(
        &mut self,
        location: &'ast SrcSpan,
        typ: &'ast Arc<Type>,
        index: &'ast u64,
        tuple: &'ast ast::TypedExpr,
    ) {
        let token_type = self.type_to_token_type(typ, false);
        self.push_range(
            SrcSpan {
                start: location.end - index.to_string().len() as u32,
                end: location.end,
            },
            token_type,
        );

        visit::visit_typed_expr_tuple_index(self, location, typ, index, tuple);
    }

    fn visit_typed_expr_record_access(
        &mut self,
        location: &'ast SrcSpan,
        typ: &'ast Arc<Type>,
        label: &'ast EcoString,
        index: &'ast u64,
        record: &'ast ast::TypedExpr,
    ) {
        let token_type = self.type_to_token_type(typ, false);
        self.push_range(
            SrcSpan {
                start: location.end - label.len() as u32,
                end: location.end,
            },
            token_type,
        );

        visit::visit_typed_expr_record_access(self, location, typ, label, index, record)
    }

    fn visit_import(&mut self, import: &'ast ast::Import<EcoString>) {
        for unqualified_type in &import.unqualified_types {
            self.push_range(unqualified_type.location, SEMANTIC_TOKEN_TYPE)
        }

        for unqualified_value in &import.unqualified_values {
            let token_type = self
                .project_compiler
                .get_importable_modules()
                .get(&import.module)
                .map(|module_interface| {
                    match module_interface.values.get(&unqualified_value.name) {
                        Some(value_ctor) => match value_ctor.variant {
                            ValueConstructorVariant::LocalVariable { .. } => {
                                // unreachable!
                                None
                            }
                            ValueConstructorVariant::ModuleConstant { .. } => {
                                Some(SEMANTIC_TOKEN_PROPERTY)
                            }
                            ValueConstructorVariant::LocalConstant { .. } => {
                                Some(SEMANTIC_TOKEN_PROPERTY)
                            }
                            ValueConstructorVariant::ModuleFn { .. } => {
                                Some(SEMANTIC_TOKEN_FUNCTION)
                            }
                            ValueConstructorVariant::Record { .. } => Some(SEMANTIC_TOKEN_STRUCT),
                        },
                        None => None,
                    }
                })
                .flatten();

            if let Some(token_type) = token_type {
                self.push_range(unqualified_value.location, token_type);
            }
        }

        visit::visit_import(self, import)
    }
}

impl<'ast, IO> SemanticTokenSearcher<'ast, IO>
where
    IO: CommandExecutor + FileSystemWriter + FileSystemReader + Clone,
{
    pub fn new(
        module: &'ast build::Module,
        params: &'ast SemanticTokensParams,
        project_compiler: &'ast ProjectCompiler<IO>,
    ) -> Self {
        Self {
            line_numbers: LineNumbers::new(&module.code),
            params,
            module: &module.ast,
            semantic_tokens: vec![],
            project_compiler,

            last_line: 0,
            last_char: 0,
        }
    }

    pub fn visit_pattern(&mut self, pattern: &Pattern<Arc<Type>>) {
        match pattern {
            Pattern::Int { location, .. } => self.push_range(*location, SEMANTIC_TOKEN_NUMBER),
            Pattern::Float { location, .. } => self.push_range(*location, SEMANTIC_TOKEN_NUMBER),
            Pattern::String { location, .. } => self.push_range(*location, SEMANTIC_TOKEN_STRING),
            Pattern::Variable { location, name, .. } => {
                if !name.starts_with("_") {
                    self.push_range(*location, SEMANTIC_TOKEN_VARIABLE)
                }
            }
            Pattern::VarUsage { location, .. } => {
                self.push_range(*location, SEMANTIC_TOKEN_VARIABLE)
            }
            Pattern::Assign {
                name,
                location,
                pattern,
            } => {
                self.push_range(
                    SrcSpan {
                        start: location.end - name.len() as u32,
                        end: location.end,
                    },
                    SEMANTIC_TOKEN_VARIABLE,
                );
                self.visit_pattern(pattern)
            }
            Pattern::Discard { location, .. } => {
                self.push_range(*location, SEMANTIC_TOKEN_VARIABLE)
            }
            Pattern::List { elements, tail, .. } => {
                for element in elements {
                    self.visit_pattern(element)
                }

                if let Some(tail) = tail {
                    self.visit_pattern(tail)
                }
            }
            Pattern::Constructor {
                location,
                name,
                arguments,
                module,
                ..
            } => {
                if let Some(module_name) = module {
                    self.push_range(
                        SrcSpan {
                            start: location.start,
                            end: location.start + module_name.len() as u32,
                        },
                        SEMANTIC_TOKEN_TYPE,
                    )
                }
                self.push_range(
                    {
                        let true_start = location.start
                            + module
                                .as_ref()
                                .map(|x| x.len() + 1 /* for period */)
                                .unwrap_or(0) as u32;
                        SrcSpan {
                            start: true_start,
                            end: true_start + name.len() as u32,
                        }
                    },
                    SEMANTIC_TOKEN_ENUM_MEMBER,
                );
                for pattern in arguments {
                    self.visit_pattern(&pattern.value)
                }
            }
            Pattern::Tuple { elems, .. } => {
                for element in elems {
                    self.visit_pattern(element)
                }
            }
            Pattern::BitArray { segments, .. } => {
                for element in segments {
                    self.visit_pattern(&element.value)
                }
            }
            Pattern::StringPrefix {
                location,
                right_side_assignment,
                ..
            } => self.push_range(
                SrcSpan {
                    start: location.end - right_side_assignment.name().len() as u32,
                    end: location.end,
                },
                SEMANTIC_TOKEN_VARIABLE,
            ),
        }
    }

    fn type_to_token_type(&mut self, type_: &Type, is_record: bool) -> u32 {
        let mut token_type = SEMANTIC_TOKEN_PROPERTY;
        let mut type__ = type_.to_owned();
        loop {
            match type__ {
                Type::Named {
                    ref module,
                    ref name,
                    ..
                } => {
                    let module_interface = self
                        .project_compiler
                        .get_importable_modules()
                        .get(module.as_ref());

                    if let Some(module) = module_interface {
                        if let Some(exported_value) = module.values.get(name) {
                            match &exported_value.variant {
                                ValueConstructorVariant::LocalVariable { .. } => {
                                    // unreachable!
                                    break;
                                }
                                ValueConstructorVariant::ModuleConstant { .. } => {
                                    token_type = SEMANTIC_TOKEN_PROPERTY;
                                    break;
                                }
                                ValueConstructorVariant::LocalConstant { .. } => {
                                    token_type = SEMANTIC_TOKEN_PROPERTY;
                                    break;
                                }
                                ValueConstructorVariant::ModuleFn { .. } => {
                                    token_type = SEMANTIC_TOKEN_FUNCTION;
                                    break;
                                }
                                ValueConstructorVariant::Record { .. } => {
                                    token_type = SEMANTIC_TOKEN_PROPERTY;
                                    break;
                                }
                            }
                        }

                        if module.types.get(name).is_some() {
                            token_type = SEMANTIC_TOKEN_PROPERTY;
                            break;
                        }
                    }
                }
                Type::Fn { .. } => {
                    token_type = if is_record {
                        SEMANTIC_TOKEN_ENUM_MEMBER
                    } else {
                        SEMANTIC_TOKEN_FUNCTION
                    };
                    break;
                }
                Type::Var { type_ } => {
                    let borrowed = type_.borrow();
                    match &*borrowed {
                        TypeVar::Unbound { .. } => break,
                        TypeVar::Link { type_ } => type__ = type_.as_ref().to_owned(),
                        TypeVar::Generic { .. } => break,
                    }
                }
                Type::Tuple { .. } => {
                    token_type = SEMANTIC_TOKEN_PROPERTY;
                    break;
                }
            }
        }

        token_type
    }

    fn push_range(&mut self, location: SrcSpan, token_type: u32) {
        self.semantic_tokens.push((location, token_type))
    }

    fn remake_ranges(mut self) -> Vec<SemanticToken> {
        let mut tokens = self.semantic_tokens;
        tokens.sort_by_key(|x| x.0.start);
        let mut semantic_tokens = vec![];
        for token in tokens {
            let requested_range = src_span_to_lsp_range(token.0, &self.line_numbers);

            assert_eq!(requested_range.start.line, requested_range.end.line);

            let delta_line = requested_range.start.line - self.last_line;
            self.last_line = requested_range.start.line;

            let delta_start = if delta_line == 0 {
                requested_range.start.character - self.last_char
            } else {
                requested_range.start.character
            };
            self.last_char = requested_range.start.character;

            semantic_tokens.push(SemanticToken {
                delta_line,
                delta_start,
                length: requested_range.end.character - requested_range.start.character,
                token_type: token.1,
                token_modifiers_bitset: 0,
            })
        }
        semantic_tokens
    }

    pub fn semantic_tokens(mut self) -> Vec<SemanticToken> {
        self.visit_typed_module(self.module);

        self.remake_ranges()
    }
}
