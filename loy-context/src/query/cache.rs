use std::cell::RefCell;
use std::hash::Hash;

use fxhash::FxHashMap;

#[derive(Debug)]
pub struct QueryCache<K, V> {
    inner: RefCell<FxHashMap<K, V>>,
}

impl<K, V> Default for QueryCache<K, V> {
    fn default() -> Self {
        Self {
            inner: RefCell::new(FxHashMap::default()),
        }
    }
}

impl<K, V> QueryCache<K, V>
where
    K: Hash + Eq + Clone + Copy,
    V: Clone,
{
    pub fn get(&self, key: K) -> Option<V> {
        self.inner.borrow().get(&key).cloned()
    }

    pub fn insert(&self, key: K, value: V) {
        self.inner.borrow_mut().insert(key, value);
    }
}
