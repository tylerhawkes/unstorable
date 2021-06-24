//! # Unstorable
//! This crate provides a type that is meant to be used with config that can be updated. The Unstorable type
//! is meant to only be referenced on the stack and made very hard to store in any struct or on the heap. It is
//! an unnamed type which makes it impossible to declare as part of a struct without generics.
#![cfg_attr(
  not(test),
  deny(warnings, clippy::all, clippy::pedantic, clippy::cargo, missing_docs, missing_crate_level_docs)
)]
#![deny(unsafe_code)]
#![cfg_attr(not(test), no_std)]

use unnamed::UnstorableImpl;

mod unnamed {
  /// The layout of this struct is guaranteed to always have `inner` before `_unsized` because of the unsized type constraints  
  pub struct UnstorableImpl<'a, T: Sized + 'a> {
    pub(crate) inner: T,
    _not_send: core::marker::PhantomData<*const ()>,
    _lifetime: core::marker::PhantomData<&'a ()>,
    _unsized: [u8],
  }
}

/// Represents the types that can be returned by the [`AsUnstorable`] trait.
pub trait Unstorable<'a> {
  /// The type that this `Unstorable` represents
  type T;
  /// Applies a function with parameters to the inner value.
  fn apply<U>(self, u: &U, f: fn(t: &Self::T, u: &U));
  /// Get a reference to the inner value. This method violates
  /// the contract of this trait.
  /// # Safety
  /// There aren't actually any memory safety or other safety requirements for calling this, however,
  /// it does violate the intent of this trait.
  #[allow(unsafe_code)]
  unsafe fn to_inner(self) -> &'a Self::T;
}

impl<'a, T> Unstorable<'a> for &'a UnstorableImpl<'_, T> {
  type T = T;
  fn apply<U: ?Sized>(self, u: &U, f: fn(&T, &U)) {
    #[cfg(test)]
    {
      assert_eq!(self as *const _ as *const T, &self.inner as *const T);
    }
    f(&self.inner, u);
  }

  #[allow(unsafe_code)]
  unsafe fn to_inner(self) -> &'a Self::T {
    #[cfg(test)]
    {
      assert_eq!(self as *const _ as *const T, &self.inner as *const T);
    }
    &self.inner
  }
}

impl<T> core::fmt::Debug for UnstorableImpl<'_, T>
where
  T: core::fmt::Debug,
{
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.inner.fmt(f)
  }
}

impl<T> core::fmt::Display for UnstorableImpl<'_, T>
where
  T: core::fmt::Display,
{
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.inner.fmt(f)
  }
}

impl<T, U: ?Sized> core::cmp::PartialEq<U> for UnstorableImpl<'_, T>
where
  T: core::cmp::PartialEq<U>,
{
  fn eq(&self, other: &U) -> bool {
    self.inner.eq(other)
  }
}

impl<T, U: ?Sized> core::cmp::PartialOrd<U> for UnstorableImpl<'_, T>
where
  T: core::cmp::PartialOrd<U>,
{
  fn partial_cmp(&self, other: &U) -> Option<core::cmp::Ordering> {
    self.inner.partial_cmp(other)
  }
}

impl<T, U: ?Sized> AsRef<U> for UnstorableImpl<'_, T>
where
  T: AsRef<U>,
{
  fn as_ref(&self) -> &U {
    self.inner.as_ref()
  }
}

/// A trait with a blanket implementation for all types that allows references to them to be made into
/// an unstorable type.
pub trait AsUnstorable<'a>: Sized {
  /// The type that is returned by `as_unstorable`.
  type R: Unstorable<'a> + ?Sized;
  /// Turns this type into an Unstorable type
  fn as_unstorable(&'a self) -> Self::R;
}

impl<'a, T: 'a> AsUnstorable<'a> for T {
  type R = &'a UnstorableImpl<'a, T>;
  fn as_unstorable(&'a self) -> &'a UnstorableImpl<'a, T> {
    // Repr(C) guarantees that the layout is in the order that we expect.
    #[repr(C)]
    struct FatPointer<'p, P> {
      ptr: &'p P,
      size: usize,
    }
    // # Safety
    // Unstorable is an unsized type with only a single sized field. We can create a reference of type
    // Unstorable from any type as long as the unsized end is 0 (or it is never referenced, which the privacy
    // rules guarantee it won't since it is in a different module).
    // As far as the way the pointers work and what order they're in, we just have to assume that the pointer is always first
    // We use the FatPointer struct to ensure that this is always the case.
    #[allow(unsafe_code)]
    unsafe {
      core::mem::transmute(FatPointer { ptr: self, size: 0 })
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;
  #[test]
  fn test_primitives() {
    #[derive(Default, Clone, PartialEq, PartialOrd, Debug)]
    struct Types {
      a: u8,
      b: bool,
      c: i8,
      d: i16,
      e: u16,
      f: u32,
      g: u64,
      s: String,
    }
    for i in u16::MIN..=u16::MAX {
      let t = Types {
        a: i as u8,
        b: (i & 1) != 0,
        c: (i >> 8) as i8,
        d: i as i16,
        e: i,
        f: ((i as u32) << 16) & (i as u32),
        g: i as u64,
        s: i.to_string(),
      };
      macro_rules! test_unstorable {
        ($e:expr) => {
          // test PartialEq
          assert_eq!($e.as_unstorable(), &$e);
          // test to_inner()
          assert_eq!($e.as_unstorable().to_inner(), &$e);
          // test apply()
          $e.as_unstorable().apply(&$e, |u, e| assert_eq!(u, e));
          // test PartialOrd
          if $e.as_unstorable() < &$e || $e.as_unstorable() > &$e {
            panic!("Invalid PartialOrd impl");
          }
        };
      }
      #[allow(unsafe_code)]
      unsafe {
        test_unstorable!(t);
        test_unstorable!(t.a);
        test_unstorable!(t.b);
        test_unstorable!(t.c);
        test_unstorable!(t.d);
        test_unstorable!(t.e);
        test_unstorable!(t.f);
        test_unstorable!(t.g);
        test_unstorable!(t.s);
      }
    }
  }
}
