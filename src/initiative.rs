use mice::unstable::parse::{integer, operator, whitespace, Sign};
use nom::{
    bytes::complete::{tag, take_while1},
    combinator::opt,
    multi::{many0, many1},
    sequence::tuple,
    IResult,
};
use rand::{thread_rng, Rng};
/*
!pinit <char>, <init bonus>
<char>, <init bonus>
<char>, <init bonus>
...

 */

#[derive(Debug)]
struct CharName<'a>(&'a str);

fn char_name(input: &str) -> IResult<&str, CharName> {
    let (input, name) = take_while1(|c| c != ',')(input)?;
    Ok((input, CharName(name)))
}

#[derive(Debug)]
struct Entry<'a> {
    name: CharName<'a>,
    initiative_bonus: i64,
}

fn char_entry(input: &str) -> IResult<&str, Entry> {
    let (input, (name, _, _, sign, initiative_bonus)) = tuple((
        char_name,
        tag(","),
        many0(whitespace),
        opt(operator),
        integer,
    ))(input)?;
    let sign = if let Some(x) = sign {
        x
    } else {
        Sign::Positive
    };
    let initiative_bonus = sign * initiative_bonus;
    Ok((
        input,
        Entry {
            name,
            initiative_bonus,
        },
    ))
}

#[derive(Debug)]
struct InitiativeList<'a> {
    entries: Vec<Entry<'a>>,
}

fn char_entry_list(input: &str) -> IResult<&str, InitiativeList> {
    let (input, list) = many1(tuple((char_entry, opt(tag("\n")))))(input)?;
    let heck = list.into_iter().map(|(a, _)| a).collect();
    Ok((input, InitiativeList { entries: heck }))
}

fn roll_init_list(inits: &InitiativeList) -> Vec<i64> {
    let mut rng = thread_rng();
    inits
        .entries
        .iter()
        .map(|x| rng.gen_range(1, 21) + x.initiative_bonus)
        .collect()
}

/// Take message string after the command prefix has
/// been removed from the front.
pub fn pathfinder_initiative(input: &str) -> IResult<&str, String> {
    let (input, list) = char_entry_list(input)?;
    let totals = roll_init_list(&list);

    let mut sort_thing = list.entries.into_iter().zip(totals).collect::<Vec<_>>();
    sort_thing.sort_unstable_by(|a, b| {
        let answer = a.1.cmp(&b.1);
        match answer {
            std::cmp::Ordering::Equal => a.0.initiative_bonus.cmp(&b.0.initiative_bonus),
            x => x,
        }
    });
    sort_thing.reverse();
    let (list, totals): (Vec<Entry>, Vec<i64>) = sort_thing.into_iter().unzip();

    let mut chars_column = Column::new();
    for c in list {
        chars_column.add_row(TableEntry::Borrowed(c.name.0))
    }
    let mut totals_column = Column::new();
    for x in totals {
        totals_column.add_row(TableEntry::Owned(format!("{}", x)))
    }
    let table = Table {
        columns: vec![chars_column, totals_column],
    };

    Ok((input, format!("```{}```", format_table(table))))
}

struct Table<'a> {
    columns: Vec<Column<'a>>,
}

struct Column<'a> {
    cells: Vec<TableEntry<'a>>,
    min_width: usize,
}
enum TableEntry<'a> {
    Borrowed(&'a str),
    Owned(String),
}
impl TableEntry<'_> {
    fn len(&self) -> usize {
        match self {
            TableEntry::Borrowed(x) => x.len(),
            TableEntry::Owned(x) => x.len(),
        }
    }
}
use std::fmt;
impl fmt::Display for TableEntry<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TableEntry::Borrowed(x) => write!(f, "{}", x),
            TableEntry::Owned(x) => write!(f, "{}", x),
        }
    }
}

impl<'a> Column<'a> {
    fn new() -> Column<'a> {
        Column {
            cells: Vec::new(),
            min_width: 0,
        }
    }
    fn add_row(&mut self, cell: TableEntry<'a>) {
        let length = cell.len();
        self.cells.push(cell);
        if length > self.min_width {
            self.min_width = length;
        }
    }
}

impl<'a> From<Vec<&'a str>> for Column<'a> {
    fn from(v: Vec<&'a str>) -> Self {
        let mut col = Column::new();
        for x in v {
            col.add_row(TableEntry::Borrowed(x));
        }
        col
    }
}

impl<'a> From<InitiativeList<'a>> for Table<'a> {
    fn from(l: InitiativeList<'a>) -> Table<'a> {
        let mut left_column = Column::new();
        let mut right_column = Column::new();
        for Entry {
            name,
            initiative_bonus,
        } in l.entries
        {
            left_column.add_row(TableEntry::Borrowed(name.0));
            right_column.add_row(TableEntry::Owned(format!("{}", initiative_bonus)));
        }
        Table {
            columns: vec![left_column, right_column],
        }
    }
}

fn format_table(table: Table) -> String {
    let row_count = table
        .columns
        .iter()
        .map(|c| c.cells.len())
        .fold(0, |a, x| if x > a { x } else { a });
    let mut rows = Vec::with_capacity(row_count);
    for column in table.columns {
        for (r, cell) in column.cells.into_iter().enumerate() {
            let row = if let Some(x) = rows.get_mut(r) {
                x
            } else {
                rows.push(String::new());
                &mut rows[r]
            };
            row.push_str(" | ");
            row.push_str(&right_pad(&format!("{}", cell), column.min_width));
        }
    }
    rows.into_iter()
        .map(|x| x + " |")
        .collect::<Vec<_>>()
        .join("\n")
}

fn right_pad(string: &str, length: usize) -> String {
    let mut x = String::from(string);
    for _ in 0..(length - string.len()) {
        x.push(' ');
    }
    x
}

#[allow(dead_code)]
fn left_pad(string: &str, length: usize) -> String {
    let mut x = String::new();
    for _ in 0..(length - string.len()) {
        x.push(' ');
    }
    x.push_str(string);
    x
}
