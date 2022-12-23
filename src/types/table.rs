//! A simple trait for interacting with various types of table used internally.

use std::collections::HashMap;

/// # Table
///
/// A simple trait used for accessing table-like objects.
///
/// This trait is used internally for the machine's constant table.  As long as
/// your table type implements this trait then you'll be cool.  Meaning you can
/// choose whatever language semantics you want with regards constants.
/// From `stack-vm`
pub trait Table {
    /// The type for items stored and retrieved from the table.
    type Item;

    /// Insert a value into the table using a string key.
    fn insert(&mut self, name: &str, value: Self::Item);

    /// Is the table empty or not?
    fn is_empty(&self) -> bool;

    /// Does the table contain the key or not?
    fn contains_key(&self, name: &str) -> bool;

    /// Retrieve a reference to a value stored in the table by key.
    fn get(&self, name: &str) -> Option<&Self::Item>;
}

/// Internal Dictionary
/// Used to connect keys to values. Used for symbol tables,
/// litteral tables
#[derive(Debug, Default)]
pub struct Dictionary<T>(HashMap<String, T>, bool);

impl<T> Dictionary<T>{
    pub fn new(is_unique: bool) -> Dictionary<T> {
        Dictionary(HashMap::new(), is_unique)
    }
    fn already_exists_guard(&self, name: &str) {
        if self.0.contains_key(name) {
            panic!("Error: redefining constant {} not allowed", name);
        }
    }
    pub fn keys(&self) -> Vec<String> {
        let mut result = vec![];
        self.0.keys().for_each(|ref k| result.push(k.to_string()));
        result
    }
}

impl<T> Table for Dictionary<T> {
    type Item = T;

    fn insert(&mut self, name: &str, value: T) {
        if self.1 {self.already_exists_guard(name);}
        
        let name = String::from(name);
        self.0.insert(name, value);
    }

    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    fn contains_key(&self, name: &str) -> bool {
        self.0.contains_key(name)
    }

    fn get(&self, name: &str) -> Option<&T> {
        self.0.get(name)
    }

}
