use std::cmp;
use super::lexer::Token;

#[derive(Debug)]
pub struct PrecedenceTable {
    entries: Vec<PrecedenceEntry>,
    max_precedence: i8,
}

impl PrecedenceTable {

    pub fn new() -> PrecedenceTable {
        PrecedenceTable {
            entries: Vec::new(),
            max_precedence: -1,
        }
    }

   pub fn insert(&mut self, entry: PrecedenceEntry) {
       for own_entry in &self.entries {
           if own_entry.operator == entry.operator {
               return;
           }
       }

       self.max_precedence = cmp::max(self.max_precedence, entry.precedence());
       self.entries.push(entry);
   }

   pub fn lookup(&self, t: Token) -> Option<PrecedenceEntry> {
       match self.entries.iter().find(|e| e.operator == t) {
           Some(entry) => Some(entry.clone()),
           None => None,
       }
   }

   pub fn max_precedence(&self) -> i8 {
       self.max_precedence
   }
}

impl Default for PrecedenceTable {
    fn default() -> Self {
        let entries = vec![
            PrecedenceEntry::new(Token::Or, 0, Associativity::Left),
            PrecedenceEntry::new(Token::And, 1, Associativity::Left),
            PrecedenceEntry::new(Token::Not, 2, Associativity::Right),
            PrecedenceEntry::new(Token::Operator("=".to_string()), 3, Associativity::Right),
            PrecedenceEntry::new(Token::Operator("==".to_string()), 3, Associativity::Right),
            PrecedenceEntry::new(Token::Operator("<>".to_string()), 3, Associativity::Right),
            PrecedenceEntry::new(Token::Operator("!=".to_string()), 3, Associativity::Right),
            PrecedenceEntry::new(Token::Operator("<".to_string()), 4, Associativity::None),
            PrecedenceEntry::new(Token::Operator("<=".to_string()), 4, Associativity::None),
            PrecedenceEntry::new(Token::Operator(">".to_string()), 4, Associativity::None),
            PrecedenceEntry::new(Token::Operator(">=".to_string()), 4, Associativity::None),
            PrecedenceEntry::new(Token::Like, 5, Associativity::None),
            PrecedenceEntry::new(Token::Ilike, 5, Associativity::None),
            PrecedenceEntry::new(Token::Between, 6, Associativity::None),
            PrecedenceEntry::new(Token::In, 7, Associativity::None),
            PrecedenceEntry::new(Token::Is, 8, Associativity::None),
            PrecedenceEntry::new(Token::Operator("+".to_string()), 9, Associativity::Left),
            PrecedenceEntry::new(Token::Operator("-".to_string()), 9, Associativity::Left),
            PrecedenceEntry::new(Token::Operator("*".to_string()), 10, Associativity::Left),
            PrecedenceEntry::new(Token::Operator("/".to_string()), 10, Associativity::Left),
            PrecedenceEntry::new(Token::Operator("%".to_string()), 10, Associativity::Left),
            PrecedenceEntry::new(Token::Operator("^".to_string()), 11, Associativity::Left),
        ];

        PrecedenceTable { entries, max_precedence: 11 }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct PrecedenceEntry {
    operator: Token,
    precedence: i8,
    associativity: Associativity,
}

impl PrecedenceEntry {
    fn new(operator: Token, precedence: i8, associativity: Associativity) -> PrecedenceEntry {
        PrecedenceEntry {
            operator,
            precedence,
            associativity,
        }
    }

    pub fn precedence(&self) -> i8 {
        self.precedence
    }

    // Gets the precedence value to use in the next iteration of the expression
    // parsing loop.
    pub fn next(&self) -> i8 {
        match self.associativity {
            Associativity::Left => self.precedence,
            Associativity::Right | Associativity::None => self.precedence -1,
        }
    }

    // Gets the minimum precedence allowed for any operators in the righthand of
    // the current binary operator.
    pub fn right(&self) -> i8 {
        match self.associativity {
            Associativity::Left | Associativity::None => self.precedence + 1,
            Associativity::Right => self.precedence,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Associativity {
    Left,
    Right,
    None,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn creates_entry() {
        let e = PrecedenceEntry::new(Token::Operator("+".to_string()), 1, Associativity::Left);
        assert_eq!(e.operator, Token::Operator("+".to_string()));
    }

    #[test]
    fn gets_precedence_through_getter() {
        let e = PrecedenceEntry::new(Token::And, 1, Associativity::Left);
        assert_eq!(e.precedence(), 1);
    }

    #[test]
    fn calculates_next_for_entry() {
        let l_assoc = PrecedenceEntry::new(Token::And, 1, Associativity::Left);
        let r_assoc = PrecedenceEntry::new(Token::Operator("=".to_string()), 4, Associativity::Right);
        let nonassoc = PrecedenceEntry::new(Token::Operator("<".to_string()), 4, Associativity::None);
        
        assert_eq!(l_assoc.next(), 1);
        assert_eq!(r_assoc.next(), 3);
        assert_eq!(nonassoc.next(), 3);
    }

    #[test]
    fn calculates_right_for_entry() {
        let l_assoc = PrecedenceEntry::new(Token::And, 1, Associativity::Left);
        let r_assoc = PrecedenceEntry::new(Token::Operator("=".to_string()), 4, Associativity::Right);
        let nonassoc = PrecedenceEntry::new(Token::Operator("<".to_string()), 4, Associativity::None);
        
        assert_eq!(l_assoc.right(), 2);
        assert_eq!(r_assoc.right(), 4);
        assert_eq!(nonassoc.right(), 5);
    }

        #[test]
    fn creates_table() {
        let tbl = PrecedenceTable::new();
        assert!(tbl.entries.is_empty());
    }

    #[test]
    fn inserts_entry() {
        let mut tbl = PrecedenceTable::new();
        tbl.insert(PrecedenceEntry::new(Token::Operator("+".to_string()), 1, Associativity::Left));
        assert_eq!(tbl.entries.len(), 1);
    }

    #[test]
    fn inserts_entry_only_once() {
        let mut tbl = PrecedenceTable::new();
        tbl.insert(PrecedenceEntry::new(Token::Operator("+".to_string()), 1, Associativity::Left));
        tbl.insert(PrecedenceEntry::new(Token::Operator("+".to_string()), 1, Associativity::Left));
        assert_eq!(tbl.entries.len(), 1);
    }

    #[test]
    fn looks_up_entry_by_token() {
        let mut tbl = PrecedenceTable::new();
        let e = PrecedenceEntry::new(Token::And, 1, Associativity::Left);
        tbl.insert(e.clone());

        assert_eq!(tbl.lookup(Token::And), Some(e));
        assert_eq!(tbl.lookup(Token::Or), None);
    }

    #[test]
    fn gets_max_precedence_for_table() {
        let mut tbl = PrecedenceTable::new();
        assert_eq!(tbl.max_precedence(), -1);

        tbl.insert(PrecedenceEntry::new(Token::And, 1, Associativity::Left));
        assert_eq!(tbl.max_precedence(), 1);

        tbl.insert(PrecedenceEntry::new(Token::Or, 0, Associativity::Left));
        assert_eq!(tbl.max_precedence(), 1);

        tbl.insert(PrecedenceEntry::new(Token::Like, 5, Associativity::None));
        assert_eq!(tbl.max_precedence(), 5);
    }

    #[test]
    fn default_table() {
        let tbl: PrecedenceTable = Default::default();
        let ne_entry = tbl
            .lookup(Token::Operator("<>".to_string()))
            .expect("Should get <> operator from default table");

        assert_eq!(tbl.max_precedence(), 11);

        assert_eq!(ne_entry.precedence(), 3);
        assert_eq!(ne_entry.next(), 2);
        assert_eq!(ne_entry.right(), 3);
    }
}