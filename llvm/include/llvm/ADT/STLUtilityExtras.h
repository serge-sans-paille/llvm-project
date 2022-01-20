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

#ifndef LLVM_ADT_STLUTILITY_EXTRAS_H
#define LLVM_ADT_STLUTILITY_EXTRAS_H

#include "llvm/ADT/STLForwardCompat.h"
#include "llvm/ADT/STLIteratorExtras.h"
#include "llvm/ADT/STLTypeTraitsExtras.h"
#include "llvm/Config/abi-breaking.h"
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


//===----------------------------------------------------------------------===//
//     Extra additions to <memory>
//===----------------------------------------------------------------------===//

struct FreeDeleter {
  void operator()(void* v) {
    ::free(v);
  }
};

template<typename First, typename Second>
struct pair_hash {
  size_t operator()(const std::pair<First, Second> &P) const {
    return std::hash<First>()(P.first) * 31 + std::hash<Second>()(P.second);
  }
};

/// Binary functor that adapts to any other binary functor after dereferencing
/// operands.
template <typename T> struct deref {
  T func;

  // Could be further improved to cope with non-derivable functors and
  // non-binary functors (should be a variadic template member function
  // operator()).
  template <typename A, typename B> auto operator()(A &lhs, B &rhs) const {
    assert(lhs);
    assert(rhs);
    return func(*lhs, *rhs);
  }
};

namespace detail {

template <typename R> class enumerator_iter;

template <typename R> struct result_pair {
  using value_reference =
      typename std::iterator_traits<IterOfRange<R>>::reference;

  friend class enumerator_iter<R>;

  result_pair() = default;
  result_pair(std::size_t Index, IterOfRange<R> Iter)
      : Index(Index), Iter(Iter) {}

  result_pair(const result_pair<R> &Other)
      : Index(Other.Index), Iter(Other.Iter) {}
  result_pair &operator=(const result_pair &Other) {
    Index = Other.Index;
    Iter = Other.Iter;
    return *this;
  }

  std::size_t index() const { return Index; }
  value_reference value() const { return *Iter; }

private:
  std::size_t Index = std::numeric_limits<std::size_t>::max();
  IterOfRange<R> Iter;
};

template <typename R>
class enumerator_iter
    : public iterator_facade_base<enumerator_iter<R>, std::forward_iterator_tag,
                                  const result_pair<R>> {
  using result_type = result_pair<R>;

public:
  explicit enumerator_iter(IterOfRange<R> EndIter)
      : Result(std::numeric_limits<size_t>::max(), EndIter) {}

  enumerator_iter(std::size_t Index, IterOfRange<R> Iter)
      : Result(Index, Iter) {}

  const result_type &operator*() const { return Result; }

  enumerator_iter &operator++() {
    assert(Result.Index != std::numeric_limits<size_t>::max());
    ++Result.Iter;
    ++Result.Index;
    return *this;
  }

  bool operator==(const enumerator_iter &RHS) const {
    // Don't compare indices here, only iterators.  It's possible for an end
    // iterator to have different indices depending on whether it was created
    // by calling std::end() versus incrementing a valid iterator.
    return Result.Iter == RHS.Result.Iter;
  }

  enumerator_iter(const enumerator_iter &Other) : Result(Other.Result) {}
  enumerator_iter &operator=(const enumerator_iter &Other) {
    Result = Other.Result;
    return *this;
  }

private:
  result_type Result;
};

template <typename R> class enumerator {
public:
  explicit enumerator(R &&Range) : TheRange(std::forward<R>(Range)) {}

  enumerator_iter<R> begin() {
    return enumerator_iter<R>(0, std::begin(TheRange));
  }
  enumerator_iter<R> begin() const {
    return enumerator_iter<R>(0, std::begin(TheRange));
  }

  enumerator_iter<R> end() {
    return enumerator_iter<R>(std::end(TheRange));
  }
  enumerator_iter<R> end() const {
    return enumerator_iter<R>(std::end(TheRange));
  }

private:
  R TheRange;
};

} // end namespace detail

/// Given an input range, returns a new range whose values are are pair (A,B)
/// such that A is the 0-based index of the item in the sequence, and B is
/// the value from the original sequence.  Example:
///
/// std::vector<char> Items = {'A', 'B', 'C', 'D'};
/// for (auto X : enumerate(Items)) {
///   printf("Item %d - %c\n", X.index(), X.value());
/// }
///
/// Output:
///   Item 0 - A
///   Item 1 - B
///   Item 2 - C
///   Item 3 - D
///
template <typename R> detail::enumerator<R> enumerate(R &&TheRange) {
  return detail::enumerator<R>(std::forward<R>(TheRange));
}

namespace detail {

template <typename F, typename Tuple, std::size_t... I>
decltype(auto) apply_tuple_impl(F &&f, Tuple &&t, std::index_sequence<I...>) {
  return std::forward<F>(f)(std::get<I>(std::forward<Tuple>(t))...);
}

} // end namespace detail

/// Given an input tuple (a1, a2, ..., an), pass the arguments of the
/// tuple variadically to f as if by calling f(a1, a2, ..., an) and
/// return the result.
template <typename F, typename Tuple>
decltype(auto) apply_tuple(F &&f, Tuple &&t) {
  using Indices = std::make_index_sequence<
      std::tuple_size<typename std::decay<Tuple>::type>::value>;

  return detail::apply_tuple_impl(std::forward<F>(f), std::forward<Tuple>(t),
                                  Indices{});
}

namespace detail {

template <typename Predicate, typename... Args>
bool all_of_zip_predicate_first(Predicate &&P, Args &&...args) {
  auto z = zip(args...);
  auto it = z.begin();
  auto end = z.end();
  while (it != end) {
    if (!apply_tuple([&](auto &&...args) { return P(args...); }, *it))
      return false;
    ++it;
  }
  return it.all_equals(end);
}

// Just an adaptor to switch the order of argument and have the predicate before
// the zipped inputs.
template <typename... ArgsThenPredicate, size_t... InputIndexes>
bool all_of_zip_predicate_last(
    std::tuple<ArgsThenPredicate...> argsThenPredicate,
    std::index_sequence<InputIndexes...>) {
  auto constexpr OutputIndex =
      std::tuple_size<decltype(argsThenPredicate)>::value - 1;
  return all_of_zip_predicate_first(std::get<OutputIndex>(argsThenPredicate),
                             std::get<InputIndexes>(argsThenPredicate)...);
}

} // end namespace detail

/// Compare two zipped ranges using the provided predicate (as last argument).
/// Return true if all elements satisfy the predicate and false otherwise.
//  Return false if the zipped iterator aren't all at end (size mismatch).
template <typename... ArgsAndPredicate>
bool all_of_zip(ArgsAndPredicate &&...argsAndPredicate) {
  return detail::all_of_zip_predicate_last(
      std::forward_as_tuple(argsAndPredicate...),
      std::make_index_sequence<sizeof...(argsAndPredicate) - 1>{});
}

/// Return true if the sequence [Begin, End) has exactly N items. Runs in O(N)
/// time. Not meant for use with random-access iterators.
/// Can optionally take a predicate to filter lazily some items.
template <typename IterTy,
          typename Pred = bool (*)(const decltype(*std::declval<IterTy>()) &)>
bool hasNItems(
    IterTy &&Begin, IterTy &&End, unsigned N,
    Pred &&ShouldBeCounted =
        [](const decltype(*std::declval<IterTy>()) &) { return true; },
    std::enable_if_t<
        !std::is_base_of<std::random_access_iterator_tag,
                         typename std::iterator_traits<std::remove_reference_t<
                             decltype(Begin)>>::iterator_category>::value,
        void> * = nullptr) {
  for (; N; ++Begin) {
    if (Begin == End)
      return false; // Too few.
    N -= ShouldBeCounted(*Begin);
  }
  for (; Begin != End; ++Begin)
    if (ShouldBeCounted(*Begin))
      return false; // Too many.
  return true;
}

/// Return true if the sequence [Begin, End) has N or more items. Runs in O(N)
/// time. Not meant for use with random-access iterators.
/// Can optionally take a predicate to lazily filter some items.
template <typename IterTy,
          typename Pred = bool (*)(const decltype(*std::declval<IterTy>()) &)>
bool hasNItemsOrMore(
    IterTy &&Begin, IterTy &&End, unsigned N,
    Pred &&ShouldBeCounted =
        [](const decltype(*std::declval<IterTy>()) &) { return true; },
    std::enable_if_t<
        !std::is_base_of<std::random_access_iterator_tag,
                         typename std::iterator_traits<std::remove_reference_t<
                             decltype(Begin)>>::iterator_category>::value,
        void> * = nullptr) {
  for (; N; ++Begin) {
    if (Begin == End)
      return false; // Too few.
    N -= ShouldBeCounted(*Begin);
  }
  return true;
}

/// Returns true if the sequence [Begin, End) has N or less items. Can
/// optionally take a predicate to lazily filter some items.
template <typename IterTy,
          typename Pred = bool (*)(const decltype(*std::declval<IterTy>()) &)>
bool hasNItemsOrLess(
    IterTy &&Begin, IterTy &&End, unsigned N,
    Pred &&ShouldBeCounted = [](const decltype(*std::declval<IterTy>()) &) {
      return true;
    }) {
  assert(N != std::numeric_limits<unsigned>::max());
  return !hasNItemsOrMore(Begin, End, N + 1, ShouldBeCounted);
}

/// Returns true if the given container has exactly N items
template <typename ContainerTy> bool hasNItems(ContainerTy &&C, unsigned N) {
  return hasNItems(std::begin(C), std::end(C), N);
}

/// Returns true if the given container has N or more items
template <typename ContainerTy>
bool hasNItemsOrMore(ContainerTy &&C, unsigned N) {
  return hasNItemsOrMore(std::begin(C), std::end(C), N);
}

/// Returns true if the given container has N or less items
template <typename ContainerTy>
bool hasNItemsOrLess(ContainerTy &&C, unsigned N) {
  return hasNItemsOrLess(std::begin(C), std::end(C), N);
}

/// Returns a raw pointer that represents the same address as the argument.
///
/// This implementation can be removed once we move to C++20 where it's defined
/// as std::to_address().
///
/// The std::pointer_traits<>::to_address(p) variations of these overloads has
/// not been implemented.
template <class Ptr> auto to_address(const Ptr &P) { return P.operator->(); }
template <class T> constexpr T *to_address(T *P) { return P; }

} // end namespace llvm

#endif // LLVM_ADT_STLEXTRAS_H
