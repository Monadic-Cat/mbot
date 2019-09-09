/// Mbot-centric extension for displaying dice expressions how I want.
/// A better formatting API and control of information propagation
/// are in the docket, but I serve my needs first. This will serve
/// as a proving ground for those features.
///
/// `<term total>` := `N`
/// `<term summary>` := `EXP -> N`
/// `<term>` := `EXP -> N0 [+ N$]*`
/// `<verbose term>` := `EXP -> N0 [+ N$]* = N`
///
/// Concise:
/// `<total>`
/// Less concise:
/// `<total> = (<term total> [, <term total>]*)`
/// Even less concise:
/// `<total> = (<term summary> [, <term summary>]*)`
///
/// Verbosity Levels:
/// | Verbosity Level | Structure                                  |
/// | 0               | `T`                                        |
/// | 1               | `T = N [, N]*`                             |
/// | 2               | `T = EXP -> N [, EXP -> N]*`               |
/// | 3               | `T = EXP -> N [+ N]* [, EXP -> N [+ N]*]*` |
///
/// Alternative scheme:
/// Verbosity Levels:
/// | Verbosity Level | Structure                                      |
/// | 0               | `T`                                            |
/// | 1               | `T = N [+ N]*`                                 |
/// | 2               | `T = (EXP -> N) [+ (EXP -> N)]*`               |
/// | 3               | `T = (EXP -> N [+ N]*) [+ (EXP -> N [+ N]*)]*` |
///
/// The alternative scheme is preferred for its more explicit addition.
/// Use the "→" character in place of "->".
/// Use verbosity level 3 for mbot.
use crate::parse::{Expr, Term};
use crate::post::{EvaluatedTerm, ExpressionResult, FormatOptions, TermSeparator, TotalPosition};

/// `[T[ = ]](EXP → N [+ N]*) [+ (EXP → N [+ N]*)]*[[ = ]T]`
pub(crate) fn format(e: &ExpressionResult, options: FormatOptions) -> String {
    let FormatOptions {
        total_position,
        term_separators,
        term_list_parentheses,
        ..
    } = options;
    let pairs = e.pairs();
    let listing = pairs.len() > 1 || pairs[0].1.parts().len() > 1;
    let total_sep = if listing { " = " } else { "" };
    let mut nstr = match total_position {
        TotalPosition::Left => format!("{}{}", e.total(), total_sep),
        _ => String::new(),
    };
    nstr = if pairs.is_empty() {
        nstr
    } else if listing {
        // VERBOSE TIME
        if term_list_parentheses {
            nstr.push('(');
        }
        let mut iter = pairs.iter();
        let first = iter.next().unwrap();
        let mut formatting = options;
        if let TermSeparator::PlusSign = term_separators {
            formatting = options.exclude_sign();
        }
        let form = |a, b| format_dice_term(a, b, formatting);
        nstr.push_str(&form(&first.0, &first.1));
        for (before, after) in iter {
            if let TermSeparator::PlusSign = term_separators {
                nstr.push_str(&format!(" {} ", after.sign()));
            } else {
                nstr.push_str(", ");
            }
            nstr.push_str(&form(before, &after));
        }
        if term_list_parentheses {
            nstr.push(')');
        }
        nstr
    } else {
        nstr
    };
    if let TotalPosition::Right = total_position {
        nstr.push_str(&format!("{}{}", total_sep, e.total()))
    }
    nstr
}

fn format_dice_term(prior: &Expr, val: &EvaluatedTerm, f: FormatOptions) -> String {
    let FormatOptions {
        term_parentheses, ..
    } = f;
    match prior.term {
        Term::Constant(_) => val.format(f),
        Term::Die(_) => {
            let dice_term = format!("{} → {}", prior.format(f), val.format(f));
            if term_parentheses {
                format!("({})", dice_term)
            } else {
                dice_term
            }
        }
    }
}
