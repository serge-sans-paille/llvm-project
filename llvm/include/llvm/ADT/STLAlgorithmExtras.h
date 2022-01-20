//===- llvm/ADT/STLExtras.h - Useful STL related functions ------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains some templates that are useful if you are working with the
// STL at all.
//
// No library is required when using these functions.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_ADT_STLALGORITHMEXTRAS_H
#define LLVM_ADT_STLALGORITHMEXTRAS_H

#include "llvm/ADT/STLForwardCompat.h"
#include "llvm/ADT/STLIteratorExtras.h"
#include "llvm/ADT/STLTypeTraitsExtras.h"
#include "llvm/ADT/iterator.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Config/abi-breaking.h"
#include "llvm/Support/ErrorHandling.h"
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <initializer_list>
#include <iterator>
#include <limits>
#include <memory>
#include <tuple>
#include <type_traits>
#include <utility>

#ifdef EXPENSIVE_CHECKS
#include <random> // for std::mt19937
#endif

namespace llvm {

/// Adapt std::less<T> for array_pod_sort.
template<typename T>
inline int array_pod_sort_comparator(const void *P1, const void *P2) {
  if (std::less<T>()(*reinterpret_cast<const T*>(P1),
                     *reinterpret_cast<const T*>(P2)))
    return -1;
  if (std::less<T>()(*reinterpret_cast<const T*>(P2),
                     *reinterpret_cast<const T*>(P1)))
    return 1;
  return 0;
}

/// get_array_pod_sort_comparator - This is an internal helper function used to
/// get type deduction of T right.
template<typename T>
inline int (*get_array_pod_sort_comparator(const T &))
             (const void*, const void*) {
  return array_pod_sort_comparator<T>;
}

#ifdef EXPENSIVE_CHECKS
namespace detail {

inline unsigned presortShuffleEntropy() {
  static unsigned Result(std::random_device{}());
  return Result;
}

template <class IteratorTy>
inline void presortShuffle(IteratorTy Start, IteratorTy End) {
  std::mt19937 Generator(presortShuffleEntropy());
  llvm::shuffle(Start, End, Generator);
}

} // end namespace detail
#endif

/// array_pod_sort - This sorts an array with the specified start and end
/// extent.  This is just like std::sort, except that it calls qsort instead of
/// using an inlined template.  qsort is slightly slower than std::sort, but
/// most sorts are not performance critical in LLVM and std::sort has to be
/// template instantiated for each type, leading to significant measured code
/// bloat.  This function should generally be used instead of std::sort where
/// possible.
///
/// This function assumes that you have simple POD-like types that can be
/// compared with std::less and can be moved with memcpy.  If this isn't true,
/// you should use std::sort.
///
/// NOTE: If qsort_r were portable, we could allow a custom comparator and
/// default to std::less.
template<class IteratorTy>
inline void array_pod_sort(IteratorTy Start, IteratorTy End) {
  // Don't inefficiently call qsort with one element or trigger undefined
  // behavior with an empty sequence.
  auto NElts = End - Start;
  if (NElts <= 1) return;
#ifdef EXPENSIVE_CHECKS
  detail::presortShuffle<IteratorTy>(Start, End);
#endif
  qsort(&*Start, NElts, sizeof(*Start), get_array_pod_sort_comparator(*Start));
}

template <class IteratorTy>
inline void array_pod_sort(
    IteratorTy Start, IteratorTy End,
    int (*Compare)(
        const typename std::iterator_traits<IteratorTy>::value_type *,
        const typename std::iterator_traits<IteratorTy>::value_type *)) {
  // Don't inefficiently call qsort with one element or trigger undefined
  // behavior with an empty sequence.
  auto NElts = End - Start;
  if (NElts <= 1) return;
#ifdef EXPENSIVE_CHECKS
  detail::presortShuffle<IteratorTy>(Start, End);
#endif
  qsort(&*Start, NElts, sizeof(*Start),
        reinterpret_cast<int (*)(const void *, const void *)>(Compare));
}

namespace detail {
template <typename T>
// We can use qsort if the iterator type is a pointer and the underlying value
// is trivially copyable.
using sort_trivially_copyable = conjunction<
    std::is_pointer<T>,
    std::is_trivially_copyable<typename std::iterator_traits<T>::value_type>>;
} // namespace detail

// Provide wrappers to std::sort which shuffle the elements before sorting
// to help uncover non-deterministic behavior (PR35135).
template <typename IteratorTy,
          std::enable_if_t<!detail::sort_trivially_copyable<IteratorTy>::value,
                           int> = 0>
inline void sort(IteratorTy Start, IteratorTy End) {
#ifdef EXPENSIVE_CHECKS
  detail::presortShuffle<IteratorTy>(Start, End);
#endif
  std::sort(Start, End);
}

// Forward trivially copyable types to array_pod_sort. This avoids a large
// amount of code bloat for a minor performance hit.
template <typename IteratorTy,
          std::enable_if_t<detail::sort_trivially_copyable<IteratorTy>::value,
                           int> = 0>
inline void sort(IteratorTy Start, IteratorTy End) {
  array_pod_sort(Start, End);
}

template <typename Container> inline void sort(Container &&C) {
  llvm::sort(adl_begin(C), adl_end(C));
}

template <typename IteratorTy, typename Compare>
inline void sort(IteratorTy Start, IteratorTy End, Compare Comp) {
#ifdef EXPENSIVE_CHECKS
  detail::presortShuffle<IteratorTy>(Start, End);
#endif
  std::sort(Start, End, Comp);
}

template <typename Container, typename Compare>
inline void sort(Container &&C, Compare Comp) {
  llvm::sort(adl_begin(C), adl_end(C), Comp);
}

//===----------------------------------------------------------------------===//
//     Extra additions to <algorithm>
//===----------------------------------------------------------------------===//

/// Get the size of a range. This is a wrapper function around std::distance
/// which is only enabled when the operation is O(1).
template <typename R>
auto size(R &&Range,
          std::enable_if_t<
              std::is_base_of<std::random_access_iterator_tag,
                              typename std::iterator_traits<decltype(
                                  Range.begin())>::iterator_category>::value,
              void> * = nullptr) {
  return std::distance(Range.begin(), Range.end());
}

/// Provide wrappers to std::for_each which take ranges instead of having to
/// pass begin/end explicitly.
template <typename R, typename UnaryFunction>
UnaryFunction for_each(R &&Range, UnaryFunction F) {
  return std::for_each(adl_begin(Range), adl_end(Range), F);
}

/// Provide wrappers to std::all_of which take ranges instead of having to pass
/// begin/end explicitly.
template <typename R, typename UnaryPredicate>
bool all_of(R &&Range, UnaryPredicate P) {
  return std::all_of(adl_begin(Range), adl_end(Range), P);
}

/// Provide wrappers to std::any_of which take ranges instead of having to pass
/// begin/end explicitly.
template <typename R, typename UnaryPredicate>
bool any_of(R &&Range, UnaryPredicate P) {
  return std::any_of(adl_begin(Range), adl_end(Range), P);
}

/// Provide wrappers to std::none_of which take ranges instead of having to pass
/// begin/end explicitly.
template <typename R, typename UnaryPredicate>
bool none_of(R &&Range, UnaryPredicate P) {
  return std::none_of(adl_begin(Range), adl_end(Range), P);
}

/// Provide wrappers to std::find which take ranges instead of having to pass
/// begin/end explicitly.
template <typename R, typename T> auto find(R &&Range, const T &Val) {
  return std::find(adl_begin(Range), adl_end(Range), Val);
}

/// Provide wrappers to std::find_if which take ranges instead of having to pass
/// begin/end explicitly.
template <typename R, typename UnaryPredicate>
auto find_if(R &&Range, UnaryPredicate P) {
  return std::find_if(adl_begin(Range), adl_end(Range), P);
}

template <typename R, typename UnaryPredicate>
auto find_if_not(R &&Range, UnaryPredicate P) {
  return std::find_if_not(adl_begin(Range), adl_end(Range), P);
}

/// Provide wrappers to std::remove_if which take ranges instead of having to
/// pass begin/end explicitly.
template <typename R, typename UnaryPredicate>
auto remove_if(R &&Range, UnaryPredicate P) {
  return std::remove_if(adl_begin(Range), adl_end(Range), P);
}

/// Provide wrappers to std::copy_if which take ranges instead of having to
/// pass begin/end explicitly.
template <typename R, typename OutputIt, typename UnaryPredicate>
OutputIt copy_if(R &&Range, OutputIt Out, UnaryPredicate P) {
  return std::copy_if(adl_begin(Range), adl_end(Range), Out, P);
}

template <typename R, typename OutputIt>
OutputIt copy(R &&Range, OutputIt Out) {
  return std::copy(adl_begin(Range), adl_end(Range), Out);
}

/// Provide wrappers to std::move which take ranges instead of having to
/// pass begin/end explicitly.
template <typename R, typename OutputIt>
OutputIt move(R &&Range, OutputIt Out) {
  return std::move(adl_begin(Range), adl_end(Range), Out);
}

/// Wrapper function around std::find to detect if an element exists
/// in a container.
template <typename R, typename E>
bool is_contained(R &&Range, const E &Element) {
  return std::find(adl_begin(Range), adl_end(Range), Element) != adl_end(Range);
}

/// Wrapper function around std::is_sorted to check if elements in a range \p R
/// are sorted with respect to a comparator \p C.
template <typename R, typename Compare> bool is_sorted(R &&Range, Compare C) {
  return std::is_sorted(adl_begin(Range), adl_end(Range), C);
}

/// Wrapper function around std::is_sorted to check if elements in a range \p R
/// are sorted in non-descending order.
template <typename R> bool is_sorted(R &&Range) {
  return std::is_sorted(adl_begin(Range), adl_end(Range));
}

/// Wrapper function around std::count to count the number of times an element
/// \p Element occurs in the given range \p Range.
template <typename R, typename E> auto count(R &&Range, const E &Element) {
  return std::count(adl_begin(Range), adl_end(Range), Element);
}

/// Wrapper function around std::count_if to count the number of times an
/// element satisfying a given predicate occurs in a range.
template <typename R, typename UnaryPredicate>
auto count_if(R &&Range, UnaryPredicate P) {
  return std::count_if(adl_begin(Range), adl_end(Range), P);
}

/// Wrapper function around std::transform to apply a function to a range and
/// store the result elsewhere.
template <typename R, typename OutputIt, typename UnaryFunction>
OutputIt transform(R &&Range, OutputIt d_first, UnaryFunction F) {
  return std::transform(adl_begin(Range), adl_end(Range), d_first, F);
}

/// Provide wrappers to std::partition which take ranges instead of having to
/// pass begin/end explicitly.
template <typename R, typename UnaryPredicate>
auto partition(R &&Range, UnaryPredicate P) {
  return std::partition(adl_begin(Range), adl_end(Range), P);
}

/// Provide wrappers to std::lower_bound which take ranges instead of having to
/// pass begin/end explicitly.
template <typename R, typename T> auto lower_bound(R &&Range, T &&Value) {
  return std::lower_bound(adl_begin(Range), adl_end(Range),
                          std::forward<T>(Value));
}

template <typename R, typename T, typename Compare>
auto lower_bound(R &&Range, T &&Value, Compare C) {
  return std::lower_bound(adl_begin(Range), adl_end(Range),
                          std::forward<T>(Value), C);
}

/// Provide wrappers to std::upper_bound which take ranges instead of having to
/// pass begin/end explicitly.
template <typename R, typename T> auto upper_bound(R &&Range, T &&Value) {
  return std::upper_bound(adl_begin(Range), adl_end(Range),
                          std::forward<T>(Value));
}

template <typename R, typename T, typename Compare>
auto upper_bound(R &&Range, T &&Value, Compare C) {
  return std::upper_bound(adl_begin(Range), adl_end(Range),
                          std::forward<T>(Value), C);
}

template <typename R>
void stable_sort(R &&Range) {
  std::stable_sort(adl_begin(Range), adl_end(Range));
}

template <typename R, typename Compare>
void stable_sort(R &&Range, Compare C) {
  std::stable_sort(adl_begin(Range), adl_end(Range), C);
}

/// Binary search for the first iterator in a range where a predicate is false.
/// Requires that C is always true below some limit, and always false above it.
template <typename R, typename Predicate,
          typename Val = decltype(*adl_begin(std::declval<R>()))>
auto partition_point(R &&Range, Predicate P) {
  return std::partition_point(adl_begin(Range), adl_end(Range), P);
}

template<typename Range, typename Predicate>
auto unique(Range &&R, Predicate P) {
  return std::unique(adl_begin(R), adl_end(R), P);
}

/// Wrapper function around std::equal to detect if pair-wise elements between
/// two ranges are the same.
template <typename L, typename R> bool equal(L &&LRange, R &&RRange) {
  return std::equal(adl_begin(LRange), adl_end(LRange), adl_begin(RRange),
                    adl_end(RRange));
}

/// Wrapper function around std::equal to detect if all elements
/// in a container are same.
template <typename R>
bool is_splat(R &&Range) {
  size_t range_size = size(Range);
  return range_size != 0 && (range_size == 1 ||
         std::equal(adl_begin(Range) + 1, adl_end(Range), adl_begin(Range)));
}

/// Provide a container algorithm similar to C++ Library Fundamentals v2's
/// `erase_if` which is equivalent to:
///
///   C.erase(remove_if(C, pred), C.end());
///
/// This version works for any container with an erase method call accepting
/// two iterators.
template <typename Container, typename UnaryPredicate>
void erase_if(Container &C, UnaryPredicate P) {
  C.erase(remove_if(C, P), C.end());
}

/// Wrapper function to remove a value from a container:
///
/// C.erase(remove(C.begin(), C.end(), V), C.end());
template <typename Container, typename ValueType>
void erase_value(Container &C, ValueType V) {
  C.erase(std::remove(C.begin(), C.end(), V), C.end());
}

/// Wrapper function to append a range to a container.
///
/// C.insert(C.end(), R.begin(), R.end());
template <typename Container, typename Range>
inline void append_range(Container &C, Range &&R) {
  C.insert(C.end(), R.begin(), R.end());
}

/// Given a sequence container Cont, replace the range [ContIt, ContEnd) with
/// the range [ValIt, ValEnd) (which is not from the same container).
template<typename Container, typename RandomAccessIterator>
void replace(Container &Cont, typename Container::iterator ContIt,
             typename Container::iterator ContEnd, RandomAccessIterator ValIt,
             RandomAccessIterator ValEnd) {
  while (true) {
    if (ValIt == ValEnd) {
      Cont.erase(ContIt, ContEnd);
      return;
    } else if (ContIt == ContEnd) {
      Cont.insert(ContIt, ValIt, ValEnd);
      return;
    }
    *ContIt++ = *ValIt++;
  }
}

/// Given a sequence container Cont, replace the range [ContIt, ContEnd) with
/// the range R.
template<typename Container, typename Range = std::initializer_list<
                                 typename Container::value_type>>
void replace(Container &Cont, typename Container::iterator ContIt,
             typename Container::iterator ContEnd, Range R) {
  replace(Cont, ContIt, ContEnd, R.begin(), R.end());
}

/// An STL-style algorithm similar to std::for_each that applies a second
/// functor between every pair of elements.
///
/// This provides the control flow logic to, for example, print a
/// comma-separated list:
/// \code
///   interleave(names.begin(), names.end(),
///              [&](StringRef name) { os << name; },
///              [&] { os << ", "; });
/// \endcode
template <typename ForwardIterator, typename UnaryFunctor,
          typename NullaryFunctor,
          typename = typename std::enable_if<
              !std::is_constructible<StringRef, UnaryFunctor>::value &&
              !std::is_constructible<StringRef, NullaryFunctor>::value>::type>
inline void interleave(ForwardIterator begin, ForwardIterator end,
                       UnaryFunctor each_fn, NullaryFunctor between_fn) {
  if (begin == end)
    return;
  each_fn(*begin);
  ++begin;
  for (; begin != end; ++begin) {
    between_fn();
    each_fn(*begin);
  }
}

template <typename Container, typename UnaryFunctor, typename NullaryFunctor,
          typename = typename std::enable_if<
              !std::is_constructible<StringRef, UnaryFunctor>::value &&
              !std::is_constructible<StringRef, NullaryFunctor>::value>::type>
inline void interleave(const Container &c, UnaryFunctor each_fn,
                       NullaryFunctor between_fn) {
  interleave(c.begin(), c.end(), each_fn, between_fn);
}

/// Overload of interleave for the common case of string separator.
template <typename Container, typename UnaryFunctor, typename StreamT,
          typename T = detail::ValueOfRange<Container>>
inline void interleave(const Container &c, StreamT &os, UnaryFunctor each_fn,
                       const StringRef &separator) {
  interleave(c.begin(), c.end(), each_fn, [&] { os << separator; });
}
template <typename Container, typename StreamT,
          typename T = detail::ValueOfRange<Container>>
inline void interleave(const Container &c, StreamT &os,
                       const StringRef &separator) {
  interleave(
      c, os, [&](const T &a) { os << a; }, separator);
}

template <typename Container, typename UnaryFunctor, typename StreamT,
          typename T = detail::ValueOfRange<Container>>
inline void interleaveComma(const Container &c, StreamT &os,
                            UnaryFunctor each_fn) {
  interleave(c, os, each_fn, ", ");
}
template <typename Container, typename StreamT,
          typename T = detail::ValueOfRange<Container>>
inline void interleaveComma(const Container &c, StreamT &os) {
  interleaveComma(c, os, [&](const T &a) { os << a; });
}


} // end namespace llvm

#endif // LLVM_ADT_STLEXTRAS_H
