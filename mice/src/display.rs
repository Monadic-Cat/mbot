//! Formatting for dice expression results.
use crate::parse::{Expr, Sign, Term};
use crate::post::{EvaluatedTerm, ExpressionResult, FormatOptions, TermSeparator, TotalPosition};

/// `[T[ = ]](EXP → N [+ N]*) [+ (EXP → N [+ N]*)]*[[ = ]T]`
/// Main entry point for formatting the results of dice expressions.
pub fn format(e: &ExpressionResult, options: FormatOptions) -> String {
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
        let (before, after) = iter.next().unwrap();
        let mut formatting = options;
        if let TermSeparator::PlusSign = term_separators {
            formatting = options.exclude_sign();
        }
        let form = |a, b| format_dice_term(a, b, formatting);
        if let TermSeparator::PlusSign = term_separators {
            if let Sign::Negative = after.sign() {
                nstr.push('-')
            }
        }
        nstr.push_str(&form(&before, &after));
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
    } else if let TotalPosition::Suppressed = total_position {
        format!("{}", e.total())
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
        Term::Dice(_) => {
            let dice_term = format!("{} → {}", prior.format(f), val.format(f));
            if term_parentheses {
                format!("({})", dice_term)
            } else {
                dice_term
            }
        }
    }
}
