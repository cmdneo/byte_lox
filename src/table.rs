use crate::{
    object::{obj_as, GcObject, ObjectKind},
    strings::hash_string,
};

const TABLE_MAX_LOAD: f32 = 0.70;

/// Hash table for storing key-value pairs.
/// Key should be a string and value can be any generic type T.
///
/// It uses Open addressing, so the hash table is a single continguous block
/// of memory. Linear probing is used for resolving collisions.
pub struct Table<T> {
    /// Vector for buckets of the hash table.
    buckets: Vec<Bucket<T>>,
    /// Counts the number of non-empty(Deleted and Filled) buckets.
    count: usize,
    /// Counts the number of filled buckets
    filled_count: usize,
}

/// Iterator for iterating over the table entries in a mutable way
pub struct TableIterator<'a, T>(std::slice::Iter<'a, Bucket<T>>);

impl<'a, T> Iterator for TableIterator<'a, T> {
    type Item = (GcObject, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        // Return if a filled entry is found, continue on deleted or empty
        loop {
            if let Some(slot) = self.0.next() {
                if let Bucket::Filled(kv) = slot {
                    return Some((kv.key, &kv.value));
                }
            } else {
                return None;
            }
        }
    }
}

enum Bucket<T> {
    Filled(Entry<T>),
    Empty,
    Deleted,
}

struct Entry<T> {
    key: GcObject,
    value: T,
}

impl<T: Clone + Copy> Table<T> {
    pub fn copy_from(&mut self, from: &Table<T>) {
        for entry in from.buckets.iter() {
            if let Bucket::Filled(entry) = entry {
                self.insert(entry.key, entry.value.clone());
            }
        }
    }
}

impl<'a, T> Table<T> {
    pub fn iter(&'a self) -> TableIterator<'a, T> {
        TableIterator(self.buckets.iter())
    }
}

impl<T> Table<T> {
    pub fn new() -> Self {
        // Initially create with capacity 0, so that the vector does not allocate.
        Self {
            buckets: vec![],
            count: 0,
            filled_count: 0,
        }
    }

    /// Inserts a key-value pair.
    /// If the key did not already exist then inserts it and returns true,
    /// otherwise it overwrites the value of the key and returns false.
    pub fn insert(&mut self, key: GcObject, value: T) -> bool {
        // Resize the table if load factor exceeds the max load factor
        let load = (self.count as f32 + 1.0) / (self.capacity() as f32);
        if load > TABLE_MAX_LOAD {
            let cap = next_capacity(self.capacity());
            self.adjust_capacity(cap);
        }

        let index = self.entry_index(key);
        let entry = &mut self.buckets[index];

        // Since deleted buckets are also counted as full buckets,
        // only increment the count if an *ACTUAL* empty bucket is found.
        if matches!(entry, Bucket::Empty) {
            self.count += 1;
        }
        self.filled_count += 1;

        let is_new_key = matches!(entry, Bucket::Empty | Bucket::Deleted);
        *entry = Bucket::Filled(Entry { key, value });

        is_new_key
    }

    /// Deletes an entry with the given key.
    /// Returns true if the entry was present and deleted otherwise false.
    ///
    /// NOTE: Deleting entries does not lower the load factor of the table due to
    /// implementation constraints. So use the delete operation sparingly.
    pub fn delete(&mut self, key: GcObject) -> bool {
        if self.capacity() == 0 {
            return false;
        }

        // We do not consider Deleted entries(tombstones) empty buckets,
        // therefore, we do not reduce the count after deleting an entry.
        let index = self.entry_index(key);
        if let Bucket::Filled(_) = self.buckets[index] {
            self.buckets[index] = Bucket::Deleted;
            self.filled_count -= 1;
            true
        } else {
            false
        }
    }

    /// Retains the only entries which satisfy the predicate.
    pub fn retain(&mut self, predicate: impl Fn(GcObject, &T) -> bool) {
        for bucket in self.buckets.iter_mut() {
            if let Bucket::Filled(entry) = bucket {
                if predicate(entry.key, &entry.value) {
                    continue;
                }

                self.filled_count -= 1;
                *bucket = Bucket::Deleted;
            }
        }
    }

    pub fn find(&self, key: GcObject) -> Option<&T> {
        if self.capacity() == 0 {
            return None;
        }

        let index = self.entry_index(key);
        if let Bucket::Filled(entry) = &self.buckets[index] {
            Some(&entry.value)
        } else {
            None
        }
    }

    /// Finds the string(key) and return it.
    /// All keys in the table should be of string type for this method to work.
    ///
    /// This method is used for string interning purposes.
    pub fn find_string(&self, string: &str) -> Option<GcObject> {
        if self.capacity() == 0 {
            return None;
        }

        let hash = hash_string(string);
        let mut index = hash as usize % self.capacity();

        loop {
            match &self.buckets[index] {
                Bucket::Filled(entry) if entry.key.hash == hash => {
                    let key = obj_as!(String from entry.key);
                    // The string being searched may not be interned as this method
                    // is used for string interning purposes, so comparison is needed.
                    if string.as_bytes() == key.as_bytes() {
                        return Some(entry.key);
                    }
                }

                Bucket::Empty => return None,

                // Continue searching until an empty slot is found.
                _ => (),
            }

            index = (index + 1) % self.capacity();
        }
    }

    /// Returns the number of entries in the table.
    pub fn len(&self) -> usize {
        self.filled_count
    }

    /// Returns the index of the corresponding bucket for the entry
    fn entry_index(&self, key: GcObject) -> usize {
        assert!(self.capacity() != 0);

        let mut index = key.hash as usize % self.capacity();
        let mut tombstone: Option<usize> = None;

        loop {
            match &self.buckets[index] {
                // We only use strings for keys as of now.
                // All strings are interned so, only comparing the pointers works.
                Bucket::Filled(entry) => {
                    if entry.key.is_same_as(&key) {
                        return index;
                    }
                }

                Bucket::Empty => return tombstone.unwrap_or(index),

                Bucket::Deleted => tombstone = Some(index),
            }

            index = (index + 1) % self.capacity();
        }
    }

    /// Rebuilds the table with the given capacity(number of buckets).
    /// capacity must not be less than the number of items in the table.
    fn adjust_capacity(&mut self, capacity: usize) {
        // Allocate a new table and swap it with the old table.
        // We need the contents of the old table for building the new table
        let mut entries: Vec<Bucket<T>> = Vec::with_capacity(capacity);
        for _ in 0..capacity {
            entries.push(Bucket::Empty);
        }

        std::mem::swap(&mut self.buckets, &mut entries);
        self.count = 0;

        for entry in entries {
            if let Bucket::Filled(entry) = entry {
                // Find the bucket in the new table for the entry and fill it.
                let index = self.entry_index(entry.key);
                self.buckets[index] = Bucket::Filled(entry);
                self.count += 1;
            }
        }
    }

    #[inline]
    fn capacity(&self) -> usize {
        self.buckets.len()
    }
}

fn next_capacity(capacity: usize) -> usize {
    if capacity * 2 < 8 {
        8
    } else {
        capacity * 2
    }
}
