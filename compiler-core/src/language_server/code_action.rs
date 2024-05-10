use ecow::EcoString;
use lsp_types::{CodeAction, TextEdit, Url};

use crate::{
    ast::{
        self,
        visit::{self, Visit},
        SrcSpan,
    },
    build,
    line_numbers::LineNumbers,
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

pub struct ReplaceModuleSelectWithUseSearcher<'a> {
    line_numbers: LineNumbers,
    module_alias: &'a EcoString,
    label: &'a EcoString,
    module: &'a ast::TypedModule,
    text_edits: Vec<TextEdit>,
}

impl<'ast> Visit<'ast> for ReplaceModuleSelectWithUseSearcher<'_> {
    fn visit_typed_expr_module_select(
        &mut self,
        location: &'ast SrcSpan,
        typ: &'ast std::sync::Arc<crate::type_::Type>,
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
}

impl<'ast> ReplaceModuleSelectWithUseSearcher<'ast> {
    pub fn new(
        module: &'ast build::Module,
        module_alias: &'ast EcoString,
        label: &'ast EcoString,
    ) -> Self {
        Self {
            line_numbers: LineNumbers::new(&module.code),
            module_alias,
            label,
            module: &module.ast,
            text_edits: vec![],
        }
    }

    pub fn text_edits(mut self) -> Vec<TextEdit> {
        self.visit_typed_module(self.module);

        self.text_edits
    }
}
