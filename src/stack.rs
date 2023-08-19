use std::{
    mem::{replace, MaybeUninit},
    ops::{Index, IndexMut},
};

/// Implementation of stack using an ArrayVec,
/// that is a vector whose capacity is fixed.
pub struct Stack<T, const CAP: usize> {
    array: [MaybeUninit<T>; CAP],
    sp: usize,
}

impl<T, const CAP: usize> Stack<T, CAP> {
    pub fn new() -> Self {
        Self {
            // It is safe because an array of MaybeUninit does not requite any inits
            array: unsafe { MaybeUninit::uninit().assume_init() },
            sp: 0,
        }
    }

    pub fn push(&mut self, value: T) {
        self.array[self.sp].write(value);
        self.sp += 1;
    }

    pub fn pop(&mut self) -> T {
        self.sp -= 1;

        // All values before `self.sp` are guranteed to be initialized by
        // `self.push`, so this is safe.
        unsafe { replace(&mut self.array[self.sp], MaybeUninit::uninit()).assume_init() }
    }

    pub fn top_mut(&mut self) -> &mut T {
        unsafe { self.array[self.sp - 1].assume_init_mut() }
    }

    pub fn top(&self) -> &T {
        unsafe { self.array[self.sp - 1].assume_init_ref() }
    }

    pub fn iter(&self) -> StackIter<'_, T> {
        StackIter {
            ref_array: &self.array,
            current: 0,
            end: self.sp,
        }
    }

    pub fn len(&self) -> usize {
        self.sp
    }

    pub fn clear(&mut self) {
        for i in (0..self.sp).rev() {
            unsafe {
                self.array[i].assume_init_drop();
            }
        }

        self.sp = 0;
    }
}

impl<T, const CAP: usize> Index<usize> for Stack<T, CAP> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        if index >= self.sp {
            panic!(
                "Index out of bounds for stack. Index is {} for length {}",
                index, self.sp
            )
        }

        // self.sp is guranteed by self.push to be not greater than CAP
        unsafe { self.array[index].assume_init_ref() }
    }
}

impl<T, const CAP: usize> IndexMut<usize> for Stack<T, CAP> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        if index >= self.sp {
            panic!(
                "Index out of bounds for stack. Index {} is for length {}",
                index, self.sp
            )
        }

        // self.sp is guranteed by self.push to be not greater than CAP
        unsafe { self.array[index].assume_init_mut() }
    }
}

pub struct StackIter<'a, T> {
    ref_array: &'a [MaybeUninit<T>],
    current: usize,
    end: usize,
}

impl<'a, T> Iterator for StackIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        // `self.end` is set to `self.sp` when making the iterator, so
        // values only before `self.end` are initialized and valid.
        if self.current == self.end {
            None
        } else {
            self.current += 1;
            Some(unsafe { self.ref_array[self.current - 1].assume_init_ref() })
        }
    }
}
