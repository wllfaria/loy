use std::sync::Arc;

use parking_lot::RawRwLock;
use parking_lot::lock_api::MappedRwLockReadGuard;

#[derive(Debug, Clone)]
pub struct Steal<T> {
    value: Arc<parking_lot::RwLock<Option<T>>>,
}

impl<T> Steal<T> {
    pub fn new(value: T) -> Self {
        Self {
            value: Arc::new(parking_lot::RwLock::new(Some(value))),
        }
    }

    pub fn borrow(&self) -> MappedRwLockReadGuard<'_, RawRwLock, T> {
        let borrow = self.value.read();
        if borrow.is_none() {
            panic!(
                "attempted to read from stolen value: {}",
                std::any::type_name::<T>()
            );
        }

        parking_lot::RwLockReadGuard::map(borrow, |b| b.as_ref().unwrap())
    }

    pub fn steal(&self) -> T {
        let value_ref = &mut *self
            .value
            .try_write()
            .expect("stealing value which is locked");

        let value = value_ref.take();
        value.expect("attempt to steal from stolen value")
    }
}
