use crate::delphi::DynArraySetLength;
use std::{
    arch::asm,
    ops::{Deref, DerefMut},
    ptr, slice,
};

#[repr(transparent)]
pub struct DelphiList<T, const P: usize>(pub *mut T);

impl<T, const P: usize> Default for DelphiList<T, P> {
    fn default() -> Self {
        Self(ptr::null_mut())
    }
}

impl<T, const P: usize> Deref for DelphiList<T, P> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        if self.0.is_null() {
            &[]
        } else {
            unsafe { slice::from_raw_parts(self.0, self.0.cast::<usize>().sub(1).read()) }
        }
    }
}

impl<T, const P: usize> DerefMut for DelphiList<T, P> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        if self.0.is_null() {
            &mut []
        } else {
            unsafe { slice::from_raw_parts_mut(self.0, self.0.cast::<usize>().sub(1).read()) }
        }
    }
}

impl<'a, T, const P: usize> IntoIterator for &'a DelphiList<T, P> {
    type IntoIter = slice::Iter<'a, T>;
    type Item = &'a T;

    fn into_iter(self) -> Self::IntoIter {
        self.deref().into_iter()
    }
}

impl<T, const P: usize> DelphiList<T, P> {
    pub unsafe fn alloc_evil(&self, len: usize) {
        asm!(
            "push {d}",
            "call {call}",
            "add esp,4",
            call = in(reg) 0x409be0,
            d = in(reg) len,
            in("eax") &self.0,
            in("edx") P,
            in("ecx") 1,
            clobber_abi("C"),
        );
    }

    pub fn alloc(&mut self, len: usize) {
        unsafe {
            DynArraySetLength(&mut self.0, P as _, 1, len);
        }
    }

    pub fn alloc_fill(&mut self, len: usize, f: impl Fn() -> T) {
        self.alloc(len);
        unsafe {
            for dst in self.get_unchecked_mut(..len) {
                let dst = dst as *mut T;
                dst.write(f());
            }
        }
    }
}

unsafe impl<T, const P: usize> Sync for DelphiList<T, P> {}
