use git2::Repository;
use itertools::Itertools;
use ratatui::{
    style::Style,
    text::{Line, Span, Text},
};
use similar::{udiff::UnifiedDiffHunk, Algorithm, ChangeTag, TextDiff};
use std::{
    fs,
    iter::{self},
    ops::Range,
    path::PathBuf,
    rc::Rc,
    str,
};
use tree_sitter_highlight::Highlighter;

use crate::{config::Config, Res};

#[derive(Debug, Clone)]
pub(crate) struct Diff {
    pub deltas: Vec<Delta>,
}

#[derive(Debug, Clone)]
pub(crate) struct Delta {
    pub file_header: String,
    pub old_file: PathBuf,
    pub new_file: PathBuf,
    pub hunks: Vec<Rc<Hunk>>,
    pub status: git2::Delta,
}

#[derive(Debug, Clone)]
pub(crate) struct Hunk {
    pub file_header: String,
    pub new_file: PathBuf,
    pub new_start: u32,
    pub header: String,
    pub content: Text<'static>,
}

#[derive(Debug)]
pub(crate) enum PatchMode {
    Normal,
    Reverse,
}

impl Hunk {
    pub(crate) fn format_patch(&self) -> String {
        format!("{}{}\n{}\n", &self.file_header, self.header, self.content)
    }

    pub(crate) fn format_line_patch(&self, line_range: Range<usize>, mode: PatchMode) -> String {
        let modified_content = self
            .content
            .lines
            .iter()
            .enumerate()
            .filter_map(|(i, line)| {
                let add = match mode {
                    PatchMode::Normal => '+',
                    PatchMode::Reverse => '-',
                };

                let remove = match mode {
                    PatchMode::Normal => '-',
                    PatchMode::Reverse => '+',
                };

                let patch_line = format!("{line}");

                if line_range.contains(&i) {
                    Some(patch_line)
                } else if patch_line.starts_with(add) {
                    None
                } else if let Some(stripped) = patch_line.strip_prefix(remove) {
                    Some(format!(" {}", stripped))
                } else {
                    Some(patch_line)
                }
            })
            .join("\n");

        format!(
            "{}{}\n{}\n",
            &self.file_header, self.header, modified_content
        )
    }

    pub(crate) fn first_diff_line(&self) -> u32 {
        self.content
            .lines
            .iter()
            .enumerate()
            .filter(|(_, line)| {
                let start = &line.spans.first().unwrap().content;
                start.starts_with('+') || start.starts_with('-')
            })
            .map(|(i, _)| i)
            .next()
            .unwrap_or(0) as u32
            + self.new_start
    }
}

pub(crate) fn convert_diff(
    config: &Config,
    repo: &Repository,
    diff: git2::Diff,
    workdir: bool,
) -> Res<Diff> {
    let mut deltas = vec![];

    diff.print(
        git2::DiffFormat::PatchHeader,
        |diffdelta, _maybe_hunk, line| {
            let line_content = str::from_utf8(line.content()).unwrap();
            let is_new_header = line_content.starts_with("diff")
                && line.origin_value() == git2::DiffLineType::FileHeader;

            if is_new_header {
                let mut delta = Delta {
                    file_header: line_content.to_string(),
                    old_file: path(&diffdelta.old_file()),
                    new_file: path(&diffdelta.new_file()),
                    hunks: vec![],
                    status: diffdelta.status(),
                };

                if let Ok(hunks) = diff_files(repo, diffdelta, workdir, config, &delta) {
                    delta.hunks = hunks;
                }

                deltas.push(delta);
            } else {
                let delta = deltas.last_mut().unwrap();
                delta.file_header.push_str(line_content);
            }

            true
        },
    )?;

    Ok(Diff { deltas })
}

fn diff_files(
    repo: &Repository,
    diffdelta: git2::DiffDelta<'_>,
    workdir: bool,
    config: &Config,
    delta: &Delta,
) -> Res<Vec<Rc<Hunk>>> {
    let old_content = read_blob(repo, &diffdelta.old_file())?;
    let new_content = if workdir {
        read_workdir(repo, &diffdelta.new_file())?
    } else {
        read_blob(repo, &diffdelta.new_file())?
    };

    diff_content(config, delta, &old_content, &new_content)
}

mod syntax_highlight {
    use std::ops::Range;

    use ratatui::style::{Color, Style};
    use tree_sitter_highlight::{Highlight, HighlightConfiguration, HighlightEvent, Highlighter};

    pub(crate) fn create_config() -> HighlightConfiguration {
        const HIGHLIGHT_NAMES: &[&str] = &[
            "attribute",
            "constant",
            "function.builtin",
            "function",
            "keyword",
            "operator",
            "property",
            "punctuation",
            "punctuation.bracket",
            "punctuation.delimiter",
            "string",
            "string.special",
            "tag",
            "type",
            "type.builtin",
            "variable",
            "variable.builtin",
            "variable.parameter",
        ];

        // TODO Add more languages, only Rust is used for now
        let mut rust_config = HighlightConfiguration::new(
            tree_sitter_rust::language(),
            tree_sitter_rust::HIGHLIGHT_QUERY,
            tree_sitter_rust::INJECTIONS_QUERY,
            "",
        )
        .unwrap();

        rust_config.configure(HIGHLIGHT_NAMES);
        rust_config
    }

    pub(crate) fn iter_highlights<'a>(
        old_highlighter: &'a mut Highlighter,
        syntax_highlight_config: &'a HighlightConfiguration,
        new_content: &'a [u8],
    ) -> impl Iterator<Item = (Range<usize>, Style)> + 'a {
        old_highlighter
            .highlight(syntax_highlight_config, new_content, None, |_| None)
            .unwrap()
            .scan((0..0, Style::new()), |current, event| {
                match event.unwrap() {
                    HighlightEvent::Source { start, end } => {
                        current.0 = start..end;
                        Some(None)
                    }
                    HighlightEvent::HighlightStart(Highlight(highlight)) => {
                        current.1 = Style::new().fg(Color::Indexed(highlight as u8));
                        Some(None)
                    }
                    HighlightEvent::HighlightEnd => Some(Some(current.clone())),
                }
            })
            .flatten()
    }
}

fn diff_content(
    config: &Config,
    delta: &Delta,
    old_content: &str,
    new_content: &str,
) -> Res<Vec<Rc<Hunk>>> {
    let text_diff = TextDiff::configure()
        .algorithm(Algorithm::Patience)
        .diff_lines(old_content, new_content);

    let syntax_highlight_config = syntax_highlight::create_config();
    let old_highlighter = &mut Highlighter::new();
    let new_highlighter = &mut Highlighter::new();
    let old_syntax_highlights = &mut syntax_highlight::iter_highlights(
        old_highlighter,
        &syntax_highlight_config,
        old_content.as_bytes(),
    );
    let new_syntax_highlights = &mut syntax_highlight::iter_highlights(
        new_highlighter,
        &syntax_highlight_config,
        new_content.as_bytes(),
    );

    Ok(text_diff
        .unified_diff()
        .iter_hunks()
        .map(|hunk| {
            // TODO Find a way to merge old/new syntax highlight ranges into the diff

            let formatted_hunk = format_hunk(config, &hunk, &text_diff);

            let new_start = hunk
                .header()
                .to_string()
                .strip_prefix("@@ -")
                .unwrap()
                .split(|c| c == ' ' || c == ',')
                .next()
                .unwrap()
                .parse()
                .unwrap();

            Rc::new(Hunk {
                file_header: delta.file_header.clone(),
                new_file: delta.new_file.clone(),
                new_start,
                header: format!("{}", hunk.header()),
                content: formatted_hunk,
            })
        })
        .collect::<Vec<_>>())
}

fn format_hunk<'diff, 'old, 'new, 'bufs>(
    config: &Config,
    hunk: &UnifiedDiffHunk<'diff, 'old, 'new, 'bufs, str>,
    text_diff: &'diff TextDiff<'old, 'new, 'bufs, str>,
) -> Text<'static>
where
    'diff: 'old + 'new,
{
    let formatted_hunk = hunk.ops().iter().flat_map(|op| {
        text_diff
            .iter_inline_changes(op)
            .map(|change| format_line_change(config, &change))
    });

    formatted_hunk.collect::<Vec<_>>().into()
}

fn format_line_change(config: &Config, change: &similar::InlineChange<str>) -> Line<'static> {
    let style = &config.style;

    let line_style = match change.tag() {
        ChangeTag::Equal => Style::new(),
        ChangeTag::Delete => (&style.line_removed).into(),
        ChangeTag::Insert => (&style.line_added).into(),
    };

    let some_emph = change.iter_strings_lossy().any(|(emph, _value)| emph);

    let spans = iter::once(Span::styled(format!("{}", change.tag()), line_style))
        .chain(change.iter_strings_lossy().map(|(emph, value)| {
            Span::styled(
                value.trim_end_matches('\n').to_string(),
                if some_emph {
                    if emph {
                        line_style.patch(&style.line_highlight.changed)
                    } else {
                        line_style.patch(&style.line_highlight.unchanged)
                    }
                } else {
                    line_style
                },
            )
        }))
        .collect::<Vec<_>>();

    Line::from(spans)
}

fn read_workdir(repo: &Repository, new_file: &git2::DiffFile<'_>) -> Res<String> {
    Ok(fs::read_to_string(
        repo.workdir()
            .expect("No workdir")
            .join(new_file.path().unwrap()),
    )?)
}

fn read_blob(repo: &Repository, file: &git2::DiffFile<'_>) -> Res<String> {
    let blob = repo.find_blob(file.id());
    blob.map(|blob| Ok(String::from_utf8(blob.content().to_vec())?))
        .unwrap_or(Ok("".to_string()))
}

fn path(file: &git2::DiffFile) -> PathBuf {
    file.path().unwrap().to_path_buf()
}
