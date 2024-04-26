use crate::{
    ast::{
        Arg, AssignName, Definition, Function, Import, ModuleConstant, SrcSpan, Statement,
        TypedDefinition, TypedExpr, TypedPattern,
    },
    build::{Located, Module},
    config::PackageConfig,
    io::{CommandExecutor, FileSystemReader, FileSystemWriter},
    language_server::{
        compiler::LspProjectCompiler, files::FileSystemProxy, progress::ProgressReporter,
    },
    line_numbers::LineNumbers,
    paths::ProjectPaths,
    type_::{pretty::Printer, ModuleValueConstructor, PreludeType, Type, ValueConstructorVariant},
    Error, Result, Warning,
};
use camino::Utf8PathBuf;
use ecow::EcoString;
use im::HashMap;
use itertools::Itertools;
use lsp::{InlayHintKind, Range, TextEdit};
use lsp_types::{
    self as lsp, CodeAction, Hover, HoverContents, InlayHint, MarkedString, Position, Url,
};
use std::sync::Arc;
use strum::IntoEnumIterator;
use vec1::Vec1;

use super::{
    code_action::CodeActionBuilder,
    configuration::{InlayHintsConfig, SharedConfig},
    src_span_to_lsp_range,
    type_annotations::TypeAnnotations,
    DownloadDependencies, MakeLocker,
};

#[derive(Debug, PartialEq, Eq)]
pub struct Response<T> {
    pub result: Result<T, Error>,
    pub warnings: Vec<Warning>,
    pub compilation: Compilation,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Compilation {
    /// Compilation was attempted and succeeded for these modules.
    Yes(Vec<Utf8PathBuf>),
    /// Compilation was not attempted for this operation.
    No,
}

#[derive(Debug)]
pub struct LanguageServerEngine<IO, Reporter> {
    pub(crate) paths: ProjectPaths,

    /// A compiler for the project that supports repeat compilation of the root
    /// package.
    /// In the event the the project config changes this will need to be
    /// discarded and reloaded to handle any changes to dependencies.
    pub(crate) compiler: LspProjectCompiler<FileSystemProxy<IO>>,

    modules_compiled_since_last_feedback: Vec<Utf8PathBuf>,
    compiled_since_last_feedback: bool,

    // Used to publish progress notifications to the client without waiting for
    // the usual request-response loop.
    progress_reporter: Reporter,

    /// Used to know if to show the "View on HexDocs" link
    /// when hovering on an imported value
    hex_deps: std::collections::HashSet<EcoString>,

    /// Configuration the user has set in their editor.
    pub(crate) user_config: SharedConfig,
}

impl<'a, IO, Reporter> LanguageServerEngine<IO, Reporter>
where
    // IO to be supplied from outside of gleam-core
    IO: FileSystemReader
        + FileSystemWriter
        + CommandExecutor
        + DownloadDependencies
        + MakeLocker
        + Clone,
    // IO to be supplied from inside of gleam-core
    Reporter: ProgressReporter + Clone + 'a,
{
    pub fn new(
        config: PackageConfig,
        progress_reporter: Reporter,
        io: FileSystemProxy<IO>,
        paths: ProjectPaths,
        user_config: SharedConfig,
    ) -> Result<Self> {
        let locker = io.inner().make_locker(&paths, config.target)?;

        // Download dependencies to ensure they are up-to-date for this new
        // configuration and new instance of the compiler
        progress_reporter.dependency_downloading_started();
        let manifest = io.inner().download_dependencies(&paths);
        progress_reporter.dependency_downloading_finished();

        // NOTE: This must come after the progress reporter has finished!
        let manifest = manifest?;

        let compiler =
            LspProjectCompiler::new(manifest, config, paths.clone(), io.clone(), locker)?;

        let hex_deps = compiler
            .project_compiler
            .packages
            .iter()
            .flat_map(|(k, v)| match &v.source {
                crate::manifest::ManifestPackageSource::Hex { .. } => {
                    Some(EcoString::from(k.as_str()))
                }

                _ => None,
            })
            .collect();

        Ok(Self {
            modules_compiled_since_last_feedback: vec![],
            compiled_since_last_feedback: false,
            progress_reporter,
            compiler,
            paths,
            hex_deps,
            user_config,
        })
    }

    pub fn compile_please(&mut self) -> Response<()> {
        self.respond(Self::compile)
    }

    /// Compile the project if we are in one. Otherwise do nothing.
    fn compile(&mut self) -> Result<(), Error> {
        self.compiled_since_last_feedback = true;

        self.progress_reporter.compilation_started();
        let result = self.compiler.compile();
        self.progress_reporter.compilation_finished();

        let modules = result?;
        self.modules_compiled_since_last_feedback.extend(modules);

        Ok(())
    }

    fn take_warnings(&mut self) -> Vec<Warning> {
        self.compiler.take_warnings()
    }

    // TODO: test local variables
    // TODO: test same module constants
    // TODO: test imported module constants
    // TODO: test unqualified imported module constants
    // TODO: test same module records
    // TODO: test imported module records
    // TODO: test unqualified imported module records
    // TODO: test same module functions
    // TODO: test module function calls
    // TODO: test different package module function calls
    //
    //
    //
    // TODO: implement unqualified imported module functions
    // TODO: implement goto definition of modules that do not belong to the top
    // level package.
    //
    pub fn goto_definition(
        &mut self,
        params: lsp::GotoDefinitionParams,
    ) -> Response<Option<lsp::Location>> {
        self.respond(|this| {
            let params = params.text_document_position_params;
            let (line_numbers, node) = match this.node_at_position(&params) {
                Some(location) => location,
                None => return Ok(None),
            };

            let location = match node.definition_location() {
                Some(location) => location,
                None => return Ok(None),
            };

            let (uri, line_numbers) = match location.module {
                None => (params.text_document.uri, &line_numbers),
                Some(name) => {
                    let module = match this.compiler.get_source(name) {
                        Some(module) => module,
                        // TODO: support goto definition for functions defined in
                        // different packages. Currently it is not possible as the
                        // required LineNumbers and source file path information is
                        // not stored in the module metadata.
                        None => return Ok(None),
                    };
                    let url = Url::parse(&format!("file:///{}", &module.path))
                        .expect("goto definition URL parse");
                    (url, &module.line_numbers)
                }
            };
            let range = src_span_to_lsp_range(location.span, line_numbers);

            Ok(Some(lsp::Location { uri, range }))
        })
    }

    pub fn completion(
        &mut self,
        params: lsp::TextDocumentPositionParams,
    ) -> Response<Option<Vec<lsp::CompletionItem>>> {
        self.respond(|this| {
            let module = match this.module_for_uri(&params.text_document.uri) {
                Some(m) => m,
                None => return Ok(None),
            };

            let line_numbers = LineNumbers::new(&module.code);
            let byte_index =
                line_numbers.byte_index(params.position.line, params.position.character);

            let Some(found) = module.find_node(byte_index) else {
                return Ok(None);
            };

            let completions = match found {
                Located::Pattern(_pattern) => None,

                Located::Statement(_) | Located::Expression(_) => {
                    Some(this.completion_values(module))
                }

                Located::ModuleStatement(Definition::Function(_)) => {
                    Some(this.completion_types(module))
                }

                Located::FunctionBody(_) => Some(this.completion_values(module)),

                Located::ModuleStatement(Definition::TypeAlias(_) | Definition::CustomType(_)) => {
                    Some(this.completion_types(module))
                }

                Located::ModuleStatement(Definition::Import(_) | Definition::ModuleConstant(_)) => {
                    None
                }

                Located::Arg(_) => None,
            };

            Ok(completions)
        })
    }

    pub fn action(&mut self, params: lsp::CodeActionParams) -> Response<Option<Vec<CodeAction>>> {
        self.respond(|this| {
            let mut actions = vec![];
            let Some(module) = this.module_for_uri(&params.text_document.uri) else {
                return Ok(None);
            };

            let type_qualifiers = get_type_qualifiers(module);
            let module_qualifiers = get_module_qualifiers(module);

            code_action_unused_imports(module, &params, &mut actions);
            code_action_annotate_types(
                module,
                &params,
                &mut actions,
                &type_qualifiers,
                &module_qualifiers,
            );
            // Replace qualified type with import
            code_action_add_import(module, &params, &mut actions);
            // code_action_(
            //     module,
            //     &params,
            //     &mut actions,
            //     &type_qualifiers,
            //     &module_qualifiers,
            // );

            Ok(if actions.is_empty() {
                None
            } else {
                Some(actions)
            })
        })
    }

    fn respond<T>(&mut self, handler: impl FnOnce(&mut Self) -> Result<T>) -> Response<T> {
        let result = handler(self);
        let warnings = self.take_warnings();
        // TODO: test. Ensure hover doesn't report as compiled
        let compilation = if self.compiled_since_last_feedback {
            let modules = std::mem::take(&mut self.modules_compiled_since_last_feedback);
            self.compiled_since_last_feedback = false;
            Compilation::Yes(modules)
        } else {
            Compilation::No
        };
        Response {
            result,
            warnings,
            compilation,
        }
    }

    pub fn hover(&mut self, params: lsp::HoverParams) -> Response<Option<Hover>> {
        self.respond(|this| {
            let params = params.text_document_position_params;

            let (lines, found) = match this.node_at_position(&params) {
                Some(value) => value,
                None => return Ok(None),
            };

            // tracing::info!(">> {found:#?}");

            Ok(match found {
                Located::Statement(_) => None, // TODO: hover for statement
                Located::ModuleStatement(Definition::Function(fun)) => {
                    Some(hover_for_function_head(fun, lines))
                }
                Located::ModuleStatement(Definition::ModuleConstant(constant)) => {
                    Some(hover_for_module_constant(constant, lines))
                }
                Located::ModuleStatement(_) => None,
                Located::Pattern(pattern) => Some(hover_for_pattern(pattern, lines)),
                Located::Expression(expression) => {
                    let module = this.module_for_uri(&params.text_document.uri);

                    Some(hover_for_expression(
                        expression,
                        lines,
                        module,
                        &this.hex_deps,
                        &this.compiler.modules,
                    ))
                }
                Located::Arg(arg) => Some(hover_for_function_argument(arg, lines)),
                Located::FunctionBody(_) => None,
            })
        })
    }

    fn node_at_position(
        &self,
        params: &lsp::TextDocumentPositionParams,
    ) -> Option<(LineNumbers, Located<'_>)> {
        let module = self.module_for_uri(&params.text_document.uri)?;
        module_node_at_position(&params.position, module)
    }

    fn module_for_uri(&self, uri: &Url) -> Option<&Module> {
        // The to_file_path method is available on these platforms
        #[cfg(any(unix, windows, target_os = "redox", target_os = "wasi"))]
        let path = uri.to_file_path().expect("URL file");

        #[cfg(not(any(unix, windows, target_os = "redox", target_os = "wasi")))]
        let path: Utf8PathBuf = uri.path().into();

        let components = path
            .strip_prefix(self.paths.root())
            .ok()?
            .components()
            .skip(1)
            .map(|c| c.as_os_str().to_string_lossy());
        let module_name: EcoString = Itertools::intersperse(components, "/".into())
            .collect::<String>()
            .strip_suffix(".gleam")?
            .into();

        self.compiler.modules.get(&module_name)
    }

    fn completion_types<'b>(&'b self, module: &'b Module) -> Vec<lsp::CompletionItem> {
        let mut completions = vec![];

        // Prelude types
        for type_ in PreludeType::iter() {
            completions.push(lsp::CompletionItem {
                label: type_.name().into(),
                detail: Some("Type".into()),
                kind: Some(lsp::CompletionItemKind::CLASS),
                ..Default::default()
            });
        }

        // Module types
        for (name, type_) in &module.ast.type_info.types {
            completions.push(type_completion(None, name, type_));
        }

        // Imported modules
        for import in module.ast.definitions.iter().filter_map(get_import) {
            // The module may not be known of yet if it has not previously
            // compiled yet in this editor session.
            // TODO: test getting completions from modules defined in other packages
            let Some(module) = self.compiler.get_module_inferface(&import.module) else {
                continue;
            };

            // Qualified types
            for (name, type_) in &module.types {
                if !type_.public {
                    continue;
                }

                let module = import.used_name();
                if module.is_some() {
                    completions.push(type_completion(module.as_ref(), name, type_));
                }
            }

            // Unqualified types
            for unqualified in &import.unqualified_types {
                let Some(type_) = module.get_public_type(&unqualified.name) else {
                    continue;
                };
                completions.push(type_completion(None, unqualified.used_name(), type_));
            }
        }

        completions
    }

    fn completion_values<'b>(&'b self, module: &'b Module) -> Vec<lsp::CompletionItem> {
        let mut completions = vec![];

        // Module functions
        for (name, value) in &module.ast.type_info.values {
            completions.push(value_completion(None, name, value));
        }

        // Imported modules
        for import in module.ast.definitions.iter().filter_map(get_import) {
            // The module may not be known of yet if it has not previously
            // compiled yet in this editor session.
            // TODO: test getting completions from modules defined in other packages
            let Some(module) = self.compiler.get_module_inferface(&import.module) else {
                continue;
            };

            // Qualified values
            for (name, value) in &module.values {
                if !value.public {
                    continue;
                }

                let module = import.used_name();
                if module.is_some() {
                    completions.push(value_completion(module.as_deref(), name, value));
                }
            }

            // Unqualified values
            for unqualified in &import.unqualified_values {
                let Some(value) = module.get_public_value(&unqualified.name) else {
                    continue;
                };
                completions.push(value_completion(None, unqualified.used_name(), value));
            }
        }

        completions
    }

    pub fn inlay_hint(&mut self, params: lsp::InlayHintParams) -> Response<Vec<InlayHint>> {
        self.respond(|this| {
            let Some(module) = this.module_for_uri(&params.text_document.uri) else {
                return Ok(vec![]);
            };
            let line_numbers = LineNumbers::new(&module.code);
            let mut hints = vec![];
            let requested_range = line_numbers.src_span(params.range);

            let type_qualifiers = get_type_qualifiers(module);
            let module_qualifiers = get_module_qualifiers(module);

            let config = this.user_config.read().expect("lock is poisoned");
            add_hints_for_definitions(
                &config.inlay_hints,
                module.ast.definitions.iter().filter(|stmt| {
                    // Only consider definitions in the requested range
                    requested_range.overlaps(&stmt.location())
                }),
                &line_numbers,
                &mut hints,
                &type_qualifiers,
                &module_qualifiers,
            );

            let code = &module.code;

            fn annotate_location_with_type(
                type_: &Type,
                span: SrcSpan,
                line_numbers: &LineNumbers,
                type_parameters: &HashMap<u64, crate::ast::TypeAst>,
                type_qualifiers: &HashMap<EcoString, EcoString>,
                module_qualifiers: &HashMap<EcoString, EcoString>,
                hints: &mut Vec<InlayHint>,
                is_let_statement_annotation: bool,
            ) {
                let linecol = line_numbers.line_and_column_number(span.end);
                if hints.iter().any(|x| {
                    x.position.line == linecol.line - 1
                        && x.position.character == linecol.column - 1
                }) {
                    return;
                }
                let position = Position::new(linecol.line - 1, linecol.column - 1);
                let mut type_text = TypeAnnotations::generate_type_text(
                    &type_,
                    type_qualifiers,
                    module_qualifiers,
                    type_parameters,
                );

                if is_let_statement_annotation {
                    type_text = format!(": {}", type_text).to_owned()
                }

                hints.push(InlayHint {
                    position: position,
                    label: lsp::InlayHintLabel::String(type_text.trim_start().to_owned()),
                    kind: Some(InlayHintKind::TYPE),
                    text_edits: if is_let_statement_annotation {
                        Some(vec![TextEdit::new(
                            Range::new(position, position),
                            type_text,
                        )])
                    } else {
                        None
                    },
                    tooltip: None,
                    padding_left: Some(true),
                    padding_right: None,
                    data: None,
                })
            }

            fn handle_function(
                args: &Vec<Arg<Arc<Type>>>,
                body: &Vec1<Statement<Arc<Type>, TypedExpr>>,
                code: &EcoString,
                hints: &mut Vec<InlayHint>,
                line_numbers: &LineNumbers,
                type_parameters: &HashMap<u64, crate::ast::TypeAst>,
                type_qualifiers: &HashMap<EcoString, EcoString>,
                module_qualifiers: &HashMap<EcoString, EcoString>,
            ) {
                args.iter().for_each(|arg| {
                    annotate_location_with_type(
                        &arg.type_,
                        arg.location,
                        line_numbers,
                        type_parameters,
                        type_qualifiers,
                        module_qualifiers,
                        hints,
                        true,
                    )
                });

                handle_body(
                    body,
                    code,
                    hints,
                    line_numbers,
                    type_parameters,
                    type_qualifiers,
                    module_qualifiers,
                )
            }

            fn handle_body(
                statements: &Vec1<Statement<Arc<Type>, TypedExpr>>,
                code: &EcoString,
                hints: &mut Vec<InlayHint>,
                line_numbers: &LineNumbers,
                type_parameters: &HashMap<u64, crate::ast::TypeAst>,
                type_qualifiers: &HashMap<EcoString, EcoString>,
                module_qualifiers: &HashMap<EcoString, EcoString>,
            ) {
                for stmt in statements {
                    // annotate pipe expressions
                    handle_statement(
                        stmt,
                        code,
                        hints,
                        &line_numbers,
                        &type_parameters,
                        &type_qualifiers,
                        &module_qualifiers,
                    )
                }

                // annotate final item in body
                if let Some(last_stmt) = statements.iter().last() {
                    match last_stmt {
                        crate::ast::Statement::Expression(expr) => {
                            // We don't annotate implicit returns of Panic because
                            // Panic is the only thing in the body when you add only a
                            // method signature and use @external's for implementations
                            if !matches!(expr, crate::ast::TypedExpr::Panic { .. }) {
                                annotate_location_with_type(
                                    &expr.type_(),
                                    expr.location(),
                                    &line_numbers,
                                    &type_parameters,
                                    &type_qualifiers,
                                    &module_qualifiers,
                                    hints,
                                    false,
                                );
                            }
                        }
                        _ => {}
                    }
                }
            }

            fn handle_statement(
                stmt: &crate::ast::Statement<Arc<Type>, TypedExpr>,
                code: &EcoString,
                hints: &mut Vec<InlayHint>,
                line_numbers: &LineNumbers,
                type_parameters: &HashMap<u64, crate::ast::TypeAst>,
                type_qualifiers: &HashMap<EcoString, EcoString>,
                module_qualifiers: &HashMap<EcoString, EcoString>,
            ) {
                match stmt {
                    crate::ast::Statement::Expression(expr) => handle_expression(
                        &expr,
                        code,
                        hints,
                        line_numbers,
                        type_parameters,
                        type_qualifiers,
                        module_qualifiers,
                    ),
                    crate::ast::Statement::Assignment(asmt) => {
                        if asmt.annotation.is_none() {
                            // annotate let statement
                            annotate_location_with_type(
                                &asmt.type_(),
                                asmt.pattern.location(),
                                &line_numbers,
                                &type_parameters,
                                &type_qualifiers,
                                &module_qualifiers,
                                hints,
                                true,
                            );
                        }
                        // annotate parts of expression being assigned
                        handle_expression(
                            &asmt.value,
                            code,
                            hints,
                            line_numbers,
                            type_parameters,
                            type_qualifiers,
                            module_qualifiers,
                        )
                    }
                    crate::ast::Statement::Use(use_) => {
                        // Handled in handle_expression -> TypedExpr::Call.args -> arg where arg.implicit == true
                    }
                }
            }

            fn handle_expression(
                expr: &TypedExpr,
                code: &EcoString,
                hints: &mut Vec<InlayHint>,
                line_numbers: &LineNumbers,
                type_parameters: &HashMap<u64, crate::ast::TypeAst>,
                type_qualifiers: &HashMap<EcoString, EcoString>,
                module_qualifiers: &HashMap<EcoString, EcoString>,
            ) {
                match expr {
                    TypedExpr::Block {
                        location,
                        statements,
                    } => handle_body(
                        statements,
                        code,
                        hints,
                        line_numbers,
                        type_parameters,
                        type_qualifiers,
                        module_qualifiers,
                    ),
                    TypedExpr::Pipeline {
                        location,
                        assignments,
                        finally,
                    } => {
                        if &(code[(finally.location().end as usize)
                            ..(finally.location().end as usize) + 1])
                            == "\n"
                        {
                            annotate_location_with_type(
                                &finally.type_(),
                                finally.location(),
                                line_numbers,
                                type_parameters,
                                type_qualifiers,
                                module_qualifiers,
                                hints,
                                false,
                            );
                        }
                        handle_expression(
                            &finally,
                            code,
                            hints,
                            line_numbers,
                            type_parameters,
                            type_qualifiers,
                            module_qualifiers,
                        );
                        for asmt in assignments {
                            if &(code
                                [(asmt.location.end as usize)..(asmt.location.end as usize) + 1])
                                == "\n"
                            {
                                annotate_location_with_type(
                                    &asmt.type_(),
                                    asmt.location,
                                    line_numbers,
                                    type_parameters,
                                    type_qualifiers,
                                    module_qualifiers,
                                    hints,
                                    false,
                                );
                            }

                            handle_expression(
                                &asmt.value,
                                code,
                                hints,
                                line_numbers,
                                type_parameters,
                                type_qualifiers,
                                module_qualifiers,
                            );
                        }
                    }
                    TypedExpr::Var {
                        location,
                        constructor,
                        name,
                    } => {
                        // IdentifierReference
                    }
                    TypedExpr::Fn {
                        location,
                        typ,
                        is_capture,
                        args,
                        body,
                        return_annotation,
                    } => {
                        // todo: handle merging the type params of the upper function with this fn's
                        handle_function(
                            args,
                            body,
                            code,
                            hints,
                            line_numbers,
                            type_parameters,
                            type_qualifiers,
                            module_qualifiers,
                        )
                    }
                    TypedExpr::List {
                        location,
                        typ,
                        elements,
                        tail,
                    } => {
                        elements.iter().for_each(|expr| {
                            handle_expression(
                                expr,
                                code,
                                hints,
                                line_numbers,
                                type_parameters,
                                type_qualifiers,
                                module_qualifiers,
                            )
                        });
                        tail.iter().for_each(|expr| {
                            handle_expression(
                                expr,
                                code,
                                hints,
                                line_numbers,
                                type_parameters,
                                type_qualifiers,
                                module_qualifiers,
                            )
                        });
                    }
                    TypedExpr::Call {
                        location,
                        typ,
                        fun,
                        args,
                    } => {
                        handle_expression(
                            &fun,
                            code,
                            hints,
                            line_numbers,
                            type_parameters,
                            type_qualifiers,
                            module_qualifiers,
                        );
                        args.iter().for_each(|x| {
                            handle_expression(
                                &x.value,
                                code,
                                hints,
                                line_numbers,
                                type_parameters,
                                type_qualifiers,
                                module_qualifiers,
                            );

                            if x.implicit {
                                if let TypedExpr::Fn {
                                    location,
                                    typ,
                                    is_capture,
                                    args,
                                    body,
                                    return_annotation,
                                } = &x.value
                                {
                                    args.iter().for_each(|y| {
                                        annotate_location_with_type(
                                            &y.type_,
                                            y.location,
                                            line_numbers,
                                            type_parameters,
                                            type_qualifiers,
                                            module_qualifiers,
                                            hints,
                                            true,
                                        )
                                    })
                                }
                            }
                        });
                    }
                    TypedExpr::BinOp {
                        location,
                        typ,
                        name,
                        left,
                        right,
                    } => {
                        handle_expression(
                            left,
                            code,
                            hints,
                            line_numbers,
                            type_parameters,
                            type_qualifiers,
                            module_qualifiers,
                        );
                        handle_expression(
                            right,
                            code,
                            hints,
                            line_numbers,
                            type_parameters,
                            type_qualifiers,
                            module_qualifiers,
                        )
                    }
                    TypedExpr::Case {
                        location,
                        typ,
                        subjects,
                        clauses,
                    } => {
                        subjects.iter().for_each(|expr| {
                            handle_expression(
                                expr,
                                code,
                                hints,
                                line_numbers,
                                type_parameters,
                                type_qualifiers,
                                module_qualifiers,
                            )
                        });
                        clauses.iter().for_each(|clause| {
                            handle_expression(
                                &clause.then,
                                code,
                                hints,
                                line_numbers,
                                type_parameters,
                                type_qualifiers,
                                module_qualifiers,
                            )
                        })
                    }
                    TypedExpr::ModuleSelect {
                        location,
                        typ,
                        label,
                        module_name,
                        module_alias,
                        constructor,
                    } => {}
                    TypedExpr::Tuple {
                        location,
                        typ,
                        elems,
                    } => elems.iter().for_each(|expr| {
                        handle_expression(
                            expr,
                            code,
                            hints,
                            line_numbers,
                            type_parameters,
                            type_qualifiers,
                            module_qualifiers,
                        )
                    }),
                    TypedExpr::Todo { message, .. } | TypedExpr::Panic { message, .. } => {
                        message.iter().for_each(|x| {
                            handle_expression(
                                &x,
                                code,
                                hints,
                                line_numbers,
                                type_parameters,
                                type_qualifiers,
                                module_qualifiers,
                            )
                        });
                    }
                    TypedExpr::BitArray {
                        location,
                        typ,
                        segments,
                    } => segments.iter().for_each(|segment| {
                        handle_expression(
                            &segment.value,
                            code,
                            hints,
                            line_numbers,
                            type_parameters,
                            type_qualifiers,
                            module_qualifiers,
                        )
                    }),
                    TypedExpr::RecordUpdate {
                        location,
                        typ,
                        spread,
                        args,
                    } => {
                        handle_expression(
                            &spread,
                            code,
                            hints,
                            line_numbers,
                            type_parameters,
                            type_qualifiers,
                            module_qualifiers,
                        );
                        args.iter().for_each(|arg| {
                            handle_expression(
                                &arg.value,
                                code,
                                hints,
                                line_numbers,
                                type_parameters,
                                type_qualifiers,
                                module_qualifiers,
                            )
                        })
                    }
                    TypedExpr::RecordAccess { record: value, .. }
                    | TypedExpr::TupleIndex { tuple: value, .. }
                    | TypedExpr::NegateBool { value, .. }
                    | TypedExpr::NegateInt { value, .. } => handle_expression(
                        &value,
                        code,
                        hints,
                        line_numbers,
                        type_parameters,
                        type_qualifiers,
                        module_qualifiers,
                    ),
                    _ => {}
                }
            }

            module
                .ast
                .definitions
                .iter()
                .filter(|stmt| {
                    // Only consider definitions in the requested range
                    requested_range.overlaps(&stmt.location())
                })
                .for_each(|x| match x {
                    Definition::Function(f) => {
                        let type_parameters = TypeAnnotations::extract_type_parameters_from_fn(f);

                        handle_function(
                            &f.arguments,
                            &f.body,
                            code,
                            &mut hints,
                            &line_numbers,
                            &type_parameters,
                            &type_qualifiers,
                            &module_qualifiers,
                        );
                    }
                    _ => {}
                });

            Ok(hints)
        })
    }
}

// Given a module, returns a hashMap mapping from module`.`type  to qualified_name
// eg: import mod1.{type Value as V1} => key: mod1.Value, value: V1
fn get_type_qualifiers(module: &Module) -> HashMap<EcoString, EcoString> {
    let mut type_qualifiers = HashMap::new();
    for def in &module.ast.definitions {
        if let Definition::Import(import) = def {
            for unqualified_type in &import.unqualified_types {
                let _ = unqualified_type.as_name.as_ref().map(|elem| {
                    let module_name = import
                        .used_name()
                        .map_or(import.module.clone(), |used_name| used_name.clone());
                    type_qualifiers.insert(
                        module_name + "." + unqualified_type.name.clone(),
                        elem.clone(),
                    )
                });
            }
        }
    }
    type_qualifiers
}

// Given a module, returns a hashMap mapping between module names and qualified names
// eg: import mod1 as m => key: mod1, value: m
fn get_module_qualifiers(module: &Module) -> HashMap<EcoString, EcoString> {
    let mut module_qualifiers = HashMap::new();
    for def in &module.ast.definitions {
        if let Definition::Import(import) = def {
            if let Some((AssignName::Variable(v), _)) = &import.as_name {
                let _ = module_qualifiers.insert(import.module.clone(), v.clone());
            }
        }
    }
    module_qualifiers
}

fn module_node_at_position<'a>(
    position: &Position,
    module: &'a Module,
) -> Option<(LineNumbers, Located<'a>)> {
    let line_numbers = LineNumbers::new(&module.code);
    let byte_index = line_numbers.byte_index(position.line, position.character);
    let node = module.find_node(byte_index);
    let node = node?;
    Some((line_numbers, node))
}

fn type_completion(
    module: Option<&EcoString>,
    name: &str,
    type_: &crate::type_::TypeConstructor,
) -> lsp::CompletionItem {
    let label = match module {
        Some(module) => format!("{module}.{name}"),
        None => name.to_string(),
    };

    let kind = Some(if type_.typ.is_variable() {
        lsp::CompletionItemKind::VARIABLE
    } else {
        lsp::CompletionItemKind::CLASS
    });

    lsp::CompletionItem {
        label,
        kind,
        detail: Some("Type".into()),
        ..Default::default()
    }
}

fn value_completion(
    module: Option<&str>,
    name: &str,
    value: &crate::type_::ValueConstructor,
) -> lsp::CompletionItem {
    let label = match module {
        Some(module) => format!("{module}.{name}"),
        None => name.to_string(),
    };

    let type_ = Printer::new().pretty_print(&value.type_, 0);

    let kind = Some(match value.variant {
        ValueConstructorVariant::LocalVariable { .. } => lsp::CompletionItemKind::VARIABLE,
        ValueConstructorVariant::ModuleConstant { .. } => lsp::CompletionItemKind::CONSTANT,
        ValueConstructorVariant::LocalConstant { .. } => lsp::CompletionItemKind::CONSTANT,
        ValueConstructorVariant::ModuleFn { .. } => lsp::CompletionItemKind::FUNCTION,
        ValueConstructorVariant::Record { arity: 0, .. } => lsp::CompletionItemKind::ENUM_MEMBER,
        ValueConstructorVariant::Record { .. } => lsp::CompletionItemKind::CONSTRUCTOR,
    });

    let documentation = value.get_documentation().map(|d| {
        lsp::Documentation::MarkupContent(lsp::MarkupContent {
            kind: lsp::MarkupKind::Markdown,
            value: d.to_string(),
        })
    });

    lsp::CompletionItem {
        label,
        kind,
        detail: Some(type_),
        documentation,
        ..Default::default()
    }
}

fn get_import(statement: &TypedDefinition) -> Option<&Import<EcoString>> {
    match statement {
        Definition::Import(import) => Some(import),
        _ => None,
    }
}

fn hover_for_pattern(pattern: &TypedPattern, line_numbers: LineNumbers) -> Hover {
    let documentation = pattern.get_documentation().unwrap_or_default();

    // Show the type of the hovered node to the user
    let type_ = Printer::new().pretty_print(pattern.type_().as_ref(), 0);
    let contents = format!(
        "```gleam
{type_}
```
{documentation}"
    );
    Hover {
        contents: HoverContents::Scalar(MarkedString::String(contents)),
        range: Some(src_span_to_lsp_range(pattern.location(), &line_numbers)),
    }
}

fn hover_for_function_head(
    fun: &Function<Arc<Type>, TypedExpr>,
    line_numbers: LineNumbers,
) -> Hover {
    let empty_str = EcoString::from("");
    let documentation = fun.documentation.as_ref().unwrap_or(&empty_str);
    let function_type = Type::Fn {
        args: fun.arguments.iter().map(|arg| arg.type_.clone()).collect(),
        retrn: fun.return_type.clone(),
    };
    let formatted_type = Printer::new().pretty_print(&function_type, 0);
    let contents = format!(
        "```gleam
{formatted_type}
```
{documentation}"
    );
    Hover {
        contents: HoverContents::Scalar(MarkedString::String(contents)),
        range: Some(src_span_to_lsp_range(fun.location, &line_numbers)),
    }
}

fn hover_for_function_argument(argument: &Arg<Arc<Type>>, line_numbers: LineNumbers) -> Hover {
    let type_ = Printer::new().pretty_print(&argument.type_, 0);
    let contents = format!("```gleam\n{type_}\n```");
    Hover {
        contents: HoverContents::Scalar(MarkedString::String(contents)),
        range: Some(src_span_to_lsp_range(argument.location, &line_numbers)),
    }
}

fn hover_for_module_constant(
    constant: &ModuleConstant<Arc<Type>, EcoString>,
    line_numbers: LineNumbers,
) -> Hover {
    let empty_str = EcoString::from("");
    let type_ = Printer::new().pretty_print(&constant.type_, 0);
    let documentation = constant.documentation.as_ref().unwrap_or(&empty_str);
    let contents = format!("```gleam\n{type_}\n```\n{documentation}");
    Hover {
        contents: HoverContents::Scalar(MarkedString::String(contents)),
        range: Some(src_span_to_lsp_range(constant.location, &line_numbers)),
    }
}

fn hover_for_expression(
    expression: &TypedExpr,
    line_numbers: LineNumbers,
    module: Option<&Module>,
    hex_deps: &std::collections::HashSet<EcoString>,
    modules: &std::collections::HashMap<EcoString, Module>,
) -> Hover {
    let documentation = expression.get_documentation().unwrap_or_default();

    let link_section = module
        .and_then(|m: &Module| {
            let (module_name, name) = get_expr_qualified_name(expression)?;
            get_hexdocs_link_section(module_name, name, &m.ast, hex_deps)
        })
        .unwrap_or("".to_string());

    // Show the type of the hovered node to the user
    let module_with_location = match expression {
        TypedExpr::Var { constructor, .. } => {
            match &constructor.variant {
                ValueConstructorVariant::Record {
                    location, module, ..
                } => Some((module, location.to_owned())),
                ValueConstructorVariant::ModuleFn {
                    module, location, ..
                } => Some((module, location.to_owned())),
                ValueConstructorVariant::ModuleConstant {
                    location, module, ..
                } => Some((module, location.to_owned())),
                // todo: add hover for LocalConstant
                ValueConstructorVariant::LocalConstant { .. } => None,
                _ => None,
            }
        }
        TypedExpr::ModuleSelect {
            module_name,
            constructor,
            ..
        } if modules.get(module_name).is_some() => {
            let location = match constructor {
                ModuleValueConstructor::Fn { location, .. }
                | ModuleValueConstructor::Record { location, .. } => location.to_owned(),
                ModuleValueConstructor::Constant {
                    location, literal, ..
                } => SrcSpan {
                    start: location.start,
                    end: literal.location().end,
                },
            };
            Some((module_name, location))
        }
        _ => None,
    };

    if let Some((module_name, location)) = module_with_location {
        if let Some(module) = modules.get(module_name) {
            let contents = format!(
                "```gleam\n{}\n```",
                &module.code[location.start as usize..location.end as usize]
            );
            return Hover {
                contents: HoverContents::Scalar(MarkedString::String(contents)),
                range: Some(src_span_to_lsp_range(expression.location(), &line_numbers)),
            };
        }
    }

    let type_ = Printer::new().pretty_print(expression.type_().as_ref(), 0);
    let contents = format!("```gleam\n{type_}\n```\n{documentation}{link_section}");

    Hover {
        contents: HoverContents::Scalar(MarkedString::String(contents)),
        range: Some(src_span_to_lsp_range(expression.location(), &line_numbers)),
    }
}

// Check if the inner range is included in the outer range.
fn range_includes(outer: &lsp_types::Range, inner: &lsp_types::Range) -> bool {
    (outer.start >= inner.start && outer.start <= inner.end)
        || (outer.end >= inner.start && outer.end <= inner.end)
}

fn code_action_add_import(
    module: &Module,
    params: &lsp::CodeActionParams,
    actions: &mut Vec<CodeAction>,
) {
    let uri = &params.text_document.uri;
    let Some((line_numbers, located)) = module_node_at_position(&params.range.start, module) else {
        tracing::info!("returning bc hover=none");
        return;
    };

    match located {
        Located::Expression(TypedExpr::ModuleSelect {
            location,
            typ,
            label,
            module_name,
            module_alias,
            constructor,
        }) => {
            let range = src_span_to_lsp_range(
                SrcSpan {
                    start: location.start - module_alias.len() as u32,
                    end: location.start + 1,
                },
                &line_numbers,
            );

            let mut changes = vec![TextEdit {
                range,
                new_text: "".into(),
            }];

            let import = module.ast.definitions.iter().find(|x| {
                if let Definition::Import(import) = x {
                    // todo: handle aliased imports
                    return import.module.as_ref() == module_name.as_ref();
                };
                false
            });

            // todo: handle type imports
            if let Some(Definition::Import(import)) = import {
                if import.unqualified_types.is_empty() && import.unqualified_values.is_empty() {
                    changes.push(TextEdit {
                        range: src_span_to_lsp_range(
                            SrcSpan {
                                start: import.location.end,
                                end: import.location.end,
                            },
                            &line_numbers,
                        ),
                        new_text: format!(".{{{}}}", label.to_owned()).to_owned(),
                    })
                } else {
                    changes.push(TextEdit {
                        range: src_span_to_lsp_range(
                            SrcSpan {
                                start: import.location.end - 1,
                                end: import.location.end - 1,
                            },
                            &line_numbers,
                        ),
                        new_text: format!(", {}", label.to_owned()).to_owned(),
                    })
                }
            }

            CodeActionBuilder::new("Replace qualified type with import")
                .kind(lsp_types::CodeActionKind::QUICKFIX)
                .changes(uri.clone(), changes)
                .preferred(true)
                .push_to(actions);
        }
        _ => {}
    }
}

fn code_action_unused_imports(
    module: &Module,
    params: &lsp::CodeActionParams,
    actions: &mut Vec<CodeAction>,
) {
    let uri = &params.text_document.uri;
    let unused = &module.ast.type_info.unused_imports;

    if unused.is_empty() {
        return;
    }

    // Convert src spans to lsp range
    let line_numbers = LineNumbers::new(&module.code);
    let mut hovered = false;
    let mut edits = Vec::with_capacity(unused.len());

    for unused in unused {
        let range = src_span_to_lsp_range(*unused, &line_numbers);
        // Keep track of whether any unused import has is where the cursor is
        hovered = hovered || range_includes(&params.range, &range);

        edits.push(lsp_types::TextEdit {
            range,
            new_text: "".into(),
        });
    }

    // If none of the imports are where the cursor is we do nothing
    if !hovered {
        return;
    }
    edits.sort_by_key(|edit| edit.range.start);

    CodeActionBuilder::new("Remove unused imports")
        .kind(lsp_types::CodeActionKind::QUICKFIX)
        .changes(uri.clone(), edits)
        .preferred(true)
        .push_to(actions);
}

fn get_expr_qualified_name(expression: &TypedExpr) -> Option<(&EcoString, &EcoString)> {
    match expression {
        TypedExpr::Var {
            name, constructor, ..
        } if constructor.public => match &constructor.variant {
            ValueConstructorVariant::ModuleFn {
                module: module_name,
                ..
            } => Some((module_name, name)),

            ValueConstructorVariant::ModuleConstant {
                module: module_name,
                ..
            } => Some((module_name, name)),

            _ => None,
        },

        TypedExpr::ModuleSelect {
            label, module_name, ..
        } => Some((module_name, label)),

        _ => None,
    }
}

fn get_hexdocs_link_section(
    module_name: &str,
    name: &str,
    ast: &crate::ast::TypedModule,
    hex_deps: &std::collections::HashSet<EcoString>,
) -> Option<String> {
    let package_name = ast.definitions.iter().find_map(|def| match def {
        Definition::Import(p) if p.module == module_name && hex_deps.contains(&p.package) => {
            Some(&p.package)
        }
        _ => None,
    })?;

    let link = format!("https://hexdocs.pm/{package_name}/{module_name}.html#{name}");
    Some(format!("\nView on [HexDocs]({link})"))
}

fn code_action_annotate_types(
    module: &Module,
    params: &lsp::CodeActionParams,
    actions: &mut Vec<CodeAction>,
    type_qualifiers: &HashMap<EcoString, EcoString>,
    module_qualifiers: &HashMap<EcoString, EcoString>,
) {
    let uri = &params.text_document.uri;
    let Some((line_numbers, located)) = module_node_at_position(&params.range.start, module) else {
        return;
    };

    match located {
        Located::ModuleStatement(Definition::Function(function)) => {
            let type_parameters = TypeAnnotations::extract_type_parameters_from_fn(function);
            if let Some(annotation) = TypeAnnotations::from_function_definition(
                function,
                &line_numbers,
                type_qualifiers,
                module_qualifiers,
                &type_parameters,
            )
            .into_code_action(uri)
            {
                annotation.push_to(actions)
            }
        }
        Located::ModuleStatement(Definition::ModuleConstant(constant)) => {
            if let Some(annotation) =
                TypeAnnotations::from_module_constant(constant, &line_numbers).into_code_action(uri)
            {
                annotation.push_to(actions)
            }
        }
        _ => {}
    }
}

/// Collect inlay hints for top level definitions such as functions and constants.
fn add_hints_for_definitions<'a>(
    config: &InlayHintsConfig,
    definitions: impl Iterator<Item = &'a TypedDefinition>,
    line_numbers: &LineNumbers,
    hints: &mut Vec<InlayHint>,
    type_qualifiers: &HashMap<EcoString, EcoString>,
    module_qualifiers: &HashMap<EcoString, EcoString>,
) {
    for def in definitions {
        match def {
            Definition::Function(function) => {
                // let type_parameters = TypeAnnotations::extract_type_parameters_from_fn(function);
                if config.function_definitions {
                    /*hints.extend(
                        TypeAnnotations::from_function_definition(
                            function,
                            line_numbers,
                            type_qualifiers,
                            module_qualifiers,
                            &type_parameters,
                        )
                        .into_inlay_hints(),
                    );*/
                }
                if config.variable_assignments {
                    // doesn't properly walk into TypedExpr::Fn
                    /*hints.extend(
                        TypeAnnotations::from_function_body(
                            function,
                            line_numbers,
                            type_qualifiers,
                            module_qualifiers,
                            &type_parameters,
                        )
                        .into_inlay_hints(),
                    );*/
                }
            }
            Definition::ModuleConstant(constant) if config.module_constants => hints.extend(
                TypeAnnotations::from_module_constant(constant, line_numbers).into_inlay_hints(),
            ),
            _ => {}
        }
    }
}
