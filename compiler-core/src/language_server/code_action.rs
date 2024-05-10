use std::sync::Arc;

use ecow::EcoString;
use lsp_types::{CodeAction, TextEdit, Url};

use crate::{
    ast::{
        self,
        visit::{self, Visit},
        Pattern, SrcSpan,
    },
    build,
    line_numbers::LineNumbers,
    type_::Type,
};

use super::src_span_to_lsp_range;

#[derive(Debug)]
pub struct CodeActionBuilder {
    action: CodeAction,
}

impl CodeActionBuilder {
    pub fn new(title: &str) -> Self {
        Self {
            action: CodeAction {
                title: title.to_string(),
                kind: None,
                diagnostics: None,
                edit: None,
                command: None,
                is_preferred: None,
                disabled: None,
                data: None,
            },
        }
    }

    pub fn kind(mut self, kind: lsp_types::CodeActionKind) -> Self {
        self.action.kind = Some(kind);
        self
    }

    pub fn changes(mut self, uri: Url, edits: Vec<lsp_types::TextEdit>) -> Self {
        let mut edit = self.action.edit.take().unwrap_or_default();
        let mut changes = edit.changes.take().unwrap_or_default();
        _ = changes.insert(uri, edits);

        edit.changes = Some(changes);
        self.action.edit = Some(edit);
        self
    }

    pub fn preferred(mut self, is_preferred: bool) -> Self {
        self.action.is_preferred = Some(is_preferred);
        self
    }

    pub fn push_to(self, actions: &mut Vec<CodeAction>) {
        actions.push(self.action);
    }
}

pub struct ReplaceWithUseSearcher<'a> {
    line_numbers: LineNumbers,
    module_alias: &'a EcoString,
    label: &'a EcoString,
    module: &'a ast::TypedModule,
    text_edits: Vec<TextEdit>,
    replace_type: ReplaceType,
}

impl<'ast> Visit<'ast> for ReplaceWithUseSearcher<'_> {
    fn visit_typed_expr_module_select(
        &mut self,
        location: &'ast SrcSpan,
        typ: &'ast Arc<Type>,
        label: &'ast EcoString,
        module_name: &'ast EcoString,
        module_alias: &'ast EcoString,
        constructor: &'ast crate::type_::ModuleValueConstructor,
    ) {
        if module_alias == self.module_alias && label == self.label {
            let range = src_span_to_lsp_range(
                SrcSpan {
                    start: location.start - module_alias.len() as u32,
                    end: location.start + 1,
                },
                &self.line_numbers,
            );
            self.text_edits.push(TextEdit {
                range,
                new_text: "".into(),
            });
        }
        visit::visit_typed_expr_module_select(
            self,
            location,
            typ,
            label,
            module_name,
            module_alias,
            constructor,
        )
    }

    fn visit_typed_pattern(&mut self, pattern: &'ast Pattern<Arc<Type>>) {
        self.visit_pattern(pattern);
        visit::visit_typed_pattern(self, pattern);
    }
}

pub enum ReplaceType {
    ModuleSelect,
    ConstructorPattern,
}

impl<'ast> ReplaceWithUseSearcher<'ast> {
    fn visit_pattern(&mut self, pattern: &Pattern<Arc<Type>>) {
        match &pattern {
            Pattern::Assign { pattern, .. } => self.visit_pattern(pattern),
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
                if matches!(self.replace_type, ReplaceType::ConstructorPattern) {
                    if let Some(module) = module {
                        if module == self.module_alias && name == self.label {
                            let range = src_span_to_lsp_range(
                                SrcSpan {
                                    start: location.start,
                                    end: location.start + module.len() as u32 + /* dot */ 1,
                                },
                                &self.line_numbers,
                            );
                            self.text_edits.push(TextEdit {
                                range,
                                new_text: "".into(),
                            });
                        }
                    }
                }

                for argument in arguments {
                    self.visit_pattern(&argument.value)
                }
            }
            Pattern::Tuple { elems, .. } => {
                for elem in elems {
                    self.visit_pattern(&elem)
                }
            }
            Pattern::BitArray { segments, .. } => {
                for segment in segments {
                    self.visit_pattern(&segment.value)
                }
            }
            _ => {}
        }
    }
    pub fn new(
        module: &'ast build::Module,
        module_alias: &'ast EcoString,
        label: &'ast EcoString,
        replace_type: ReplaceType,
    ) -> Self {
        Self {
            line_numbers: LineNumbers::new(&module.code),
            module_alias,
            label,
            module: &module.ast,
            text_edits: vec![],
            replace_type,
        }
    }

    pub fn text_edits(mut self) -> Vec<TextEdit> {
        self.visit_typed_module(self.module);

        self.text_edits
    }
}
