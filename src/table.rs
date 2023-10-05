use crate::{
    object::{GcObject, ObjectKind},
    strings::hash_string,
};

const TABLE_MAX_LOAD: f32 = 0.70;

/// Hash table for storing key-value pairs.
/// Key should be a string and value is a generic parameter T.
///
/// It uses Open addressing, so the hash table is a single continguous block
/// of memory. Linear probing is used for resolving collisions.
pub struct Table<T> {
    buckets: Vec<Bucket<T>>,
    count: usize,
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

impl<T> Table<T> {
    pub fn new() -> Self {
        // Initially create with capacity 0, so that the vector does not allocate.
        Self {
            buckets: vec![],
            count: 0,
        }
    }

    /// Inserts a key-value pair.
    /// If the key did not already exist then inserts it and returns true,
    /// otherwise it overwrites the value of the key and returns false.
    pub fn insert(&mut self, key: GcObject, value: T) -> bool {
        // Resize the table if load factor exceeds the max load factor
        if (self.count + 1) as f32 > (self.capacity() as f32) * TABLE_MAX_LOAD {
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

        let is_new_key = matches!(entry, Bucket::Empty | Bucket::Deleted);
        *entry = Bucket::Filled(Entry { key, value });

        is_new_key
    }

    /// Deletes an entry with the given key.
    /// Returns true if the entry was present and deleted otherwise false.
    ///
    /// NOTE: Deleting entries does not lower the load factor table due to
    /// implementation constraints. So use the delete operation sparingly.
    pub fn delete(&mut self, key: GcObject) -> bool {
        // We do not consider Deleted entries(tombstones) empty buckets,
        // therefore, we do not reduce the count after deleting an entry.
        let index = self.entry_index(key);
        if let Bucket::Filled(_) = self.buckets[index] {
            self.buckets[index] = Bucket::Deleted;
            true
        } else {
            false
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

    /// Finds the string(key) and returns the associated object.
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
                    if let ObjectKind::String(key) = &entry.key.kind {
                        if string.as_bytes() == key.as_bytes() {
                            return Some(entry.key);
                        }
                    } else {
                        panic!("Key not of string type");
                    }
                }

                Bucket::Empty => return None,

                // Continue searching if hash did not match or a deleted slot is found.
                _ => (),
            }

            index = (index + 1) % self.capacity();
        }
    }

    /// Returns the index of the corresponding bucket for the entry
    fn entry_index(&self, key: GcObject) -> usize {
        debug_assert!(self.capacity() != 0);

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
        entries.fill_with(|| Bucket::Empty);
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
    if capacity == 0 {
        8
    } else {
        capacity * 2
    }
}
