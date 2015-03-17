use std::mem;
use core::nonzero::NonZero;
use std::borrow::{Borrow, BorrowMut};
use std::ops::Deref;

/// Non-dereferencable pointer
#[unsafe_no_drop_flag]
pub struct OpaqueBox<T> {
	data: NonZero<*const T>
}

impl<T> OpaqueBox<T> {
	pub fn new(value: Box<T>) -> OpaqueBox<T> {
		OpaqueBox { data: unsafe { mem::transmute(value) } }
	}
}

#[unsafe_destructor]
impl<T> Drop for OpaqueBox<T> {
	fn drop(&mut self) {
		if self.data.is_null() { return; }
		drop(unsafe { mem::transmute::<NonZero<*const T>, Box<T>>(self.data) });
	}
}

unsafe impl<T: 'static> Send for OpaqueBox<T> {}

impl<T> Borrow<T> for OpaqueBox<T> {
	fn borrow<'a>(&'a self) -> &'a T {
		unsafe { mem::transmute::<NonZero<*const T>, &_>(self.data) }
	}
}

impl<T> BorrowMut<T> for OpaqueBox<T> {
	fn borrow_mut<'a>(&'a mut self) -> &'a mut T {
		unsafe { mem::transmute::<NonZero<*const T>, &mut _>(self.data) }
	}
}

impl<T> Deref for OpaqueBox<T> {
    type Target = T;

    fn deref<'a>(&'a self) -> &'a T {
        &self.borrow()
    }
}

#[cfg(test)]
mod tests {
	use std::mem;
	use super::OpaqueBox;
	#[test]
	fn size_check() {
		assert!(mem::size_of::<*const usize>() == mem::size_of::<OpaqueBox<usize>>());
		assert!(mem::size_of::<OpaqueBox<usize>>() == mem::size_of::<Option<OpaqueBox<usize>>>());
	}

	#[test]
	fn opaque_box_is_sendable() {
		Box::new(OpaqueBox::new(Box::new(1usize))) as Box<Send>;
	}
}