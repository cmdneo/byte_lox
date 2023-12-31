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
        debug_assert!(self.sp < CAP);
        self.array[self.sp].write(value);
        self.sp += 1;
    }

    pub fn pop(&mut self) -> T {
        debug_assert!(self.sp > 0);

        self.sp -= 1;

        // All values before `self.sp` are guranteed to be initialized by
        // `self.push`, so this is safe.
        unsafe { replace(&mut self.array[self.sp], MaybeUninit::uninit()).assume_init() }
    }

    pub fn top_mut(&mut self) -> &mut T {
        debug_assert!(self.sp > 0);
        unsafe { self.array[self.sp - 1].assume_init_mut() }
    }

    pub fn top(&self) -> &T {
        debug_assert!(self.sp > 0);
        unsafe { self.array[self.sp - 1].assume_init_ref() }
    }

    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        self.window(0, self.sp).iter()
    }

    /// Returns a slice of stack
    pub fn window(&self, start: usize, end: usize) -> &[T] {
        debug_assert!(start <= end);
        debug_assert!(end <= self.sp);

        // Everything before self.sp is initialized, and range is left inclusive only.
        unsafe { std::mem::transmute::<&[MaybeUninit<T>], &[T]>(&self.array[start..end]) }
    }

    /// Returns a mut slice of stack
    pub fn window_mut(&mut self, start: usize, end: usize) -> &mut [T] {
        debug_assert!(start <= end);
        debug_assert!(end <= self.sp);

        // Everything before self.sp is initialized, and range is left inclusive only.
        unsafe {
            std::mem::transmute::<&mut [MaybeUninit<T>], &mut [T]>(&mut self.array[start..end])
        }
    }

    pub fn len(&self) -> usize {
        self.sp
    }

    pub fn cap(&self) -> usize {
        CAP
    }

    pub fn set_len(&mut self, new_len: usize) {
        debug_assert!(new_len <= self.sp);

        for i in (new_len..self.sp).rev() {
            // Everything before self.sp is initialized
            unsafe {
                self.array[i].assume_init_drop();
            }
        }

        self.sp = new_len;
    }

    pub fn clear(&mut self) {
        self.set_len(0);
    }
}

impl<T, const CAP: usize> Index<usize> for Stack<T, CAP> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        debug_assert!(
            index < self.sp,
            "Index out of bounds for stack. Index {} is for length {}",
            index,
            self.sp
        );

        // self.sp is guranteed by self.push to be not greater than CAP
        unsafe { self.array[index].assume_init_ref() }
    }
}

impl<T, const CAP: usize> IndexMut<usize> for Stack<T, CAP> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        debug_assert!(
            index < self.sp,
            "Index out of bounds for stack. Index {} is for length {}",
            index,
            self.sp
        );

        // self.sp is guranteed by self.push to be not greater than CAP
        unsafe { self.array[index].assume_init_mut() }
    }
}
