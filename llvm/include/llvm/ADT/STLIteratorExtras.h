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

#ifndef LLVM_ADT_STLITERATOREXTRAS_H
#define LLVM_ADT_STLITERATOREXTRAS_H

#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLForwardCompat.h"
#include "llvm/ADT/STLFunctionalExtras.h"
#include "llvm/ADT/STLTypeTraitsExtras.h"
#include "llvm/ADT/iterator.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Config/abi-breaking.h"
#include <iterator>

#ifdef EXPENSIVE_CHECKS
#include <random> // for std::mt19937
#endif

namespace llvm {

//===----------------------------------------------------------------------===//
//     Extra additions to <iterator>
//===----------------------------------------------------------------------===//

namespace adl_detail {

using std::begin;

template <typename ContainerTy>
decltype(auto) adl_begin(ContainerTy &&container) {
  return begin(std::forward<ContainerTy>(container));
}

using std::end;

template <typename ContainerTy>
decltype(auto) adl_end(ContainerTy &&container) {
  return end(std::forward<ContainerTy>(container));
}

using std::swap;

template <typename T>
void adl_swap(T &&lhs, T &&rhs) noexcept(noexcept(swap(std::declval<T>(),
                                                       std::declval<T>()))) {
  swap(std::forward<T>(lhs), std::forward<T>(rhs));
}

} // end namespace adl_detail

template <typename ContainerTy>
decltype(auto) adl_begin(ContainerTy &&container) {
  return adl_detail::adl_begin(std::forward<ContainerTy>(container));
}

template <typename ContainerTy>
decltype(auto) adl_end(ContainerTy &&container) {
  return adl_detail::adl_end(std::forward<ContainerTy>(container));
}

template <typename T>
void adl_swap(T &&lhs, T &&rhs) noexcept(
    noexcept(adl_detail::adl_swap(std::declval<T>(), std::declval<T>()))) {
  adl_detail::adl_swap(std::forward<T>(lhs), std::forward<T>(rhs));
}

/// Test whether \p RangeOrContainer is empty. Similar to C++17 std::empty.
template <typename T>
constexpr bool empty(const T &RangeOrContainer) {
  return adl_begin(RangeOrContainer) == adl_end(RangeOrContainer);
}

/// Returns true if the given container only contains a single element.
template <typename ContainerTy> bool hasSingleElement(ContainerTy &&C) {
  auto B = std::begin(C), E = std::end(C);
  return B != E && std::next(B) == E;
}

/// Return a range covering \p RangeOrContainer with the first N elements
/// excluded.
template <typename T> auto drop_begin(T &&RangeOrContainer, size_t N = 1) {
  return make_range(std::next(adl_begin(RangeOrContainer), N),
                    adl_end(RangeOrContainer));
}

// mapped_iterator - This is a simple iterator adapter that causes a function to
// be applied whenever operator* is invoked on the iterator.

template <typename ItTy, typename FuncTy,
          typename ReferenceTy =
              decltype(std::declval<FuncTy>()(*std::declval<ItTy>()))>
class mapped_iterator
    : public iterator_adaptor_base<
          mapped_iterator<ItTy, FuncTy>, ItTy,
          typename std::iterator_traits<ItTy>::iterator_category,
          std::remove_reference_t<ReferenceTy>,
          typename std::iterator_traits<ItTy>::difference_type,
          std::remove_reference_t<ReferenceTy> *, ReferenceTy> {
public:
  mapped_iterator(ItTy U, FuncTy F)
    : mapped_iterator::iterator_adaptor_base(std::move(U)), F(std::move(F)) {}

  ItTy getCurrent() { return this->I; }

  const FuncTy &getFunction() const { return F; }

  ReferenceTy operator*() const { return F(*this->I); }

private:
  FuncTy F;
};

// map_iterator - Provide a convenient way to create mapped_iterators, just like
// make_pair is useful for creating pairs...
template <class ItTy, class FuncTy>
inline mapped_iterator<ItTy, FuncTy> map_iterator(ItTy I, FuncTy F) {
  return mapped_iterator<ItTy, FuncTy>(std::move(I), std::move(F));
}

template <class ContainerTy, class FuncTy>
auto map_range(ContainerTy &&C, FuncTy F) {
  return make_range(map_iterator(C.begin(), F), map_iterator(C.end(), F));
}

/// A base type of mapped iterator, that is useful for building derived
/// iterators that do not need/want to store the map function (as in
/// mapped_iterator). These iterators must simply provide a `mapElement` method
/// that defines how to map a value of the iterator to the provided reference
/// type.
template <typename DerivedT, typename ItTy, typename ReferenceTy>
class mapped_iterator_base
    : public iterator_adaptor_base<
          DerivedT, ItTy,
          typename std::iterator_traits<ItTy>::iterator_category,
          std::remove_reference_t<ReferenceTy>,
          typename std::iterator_traits<ItTy>::difference_type,
          std::remove_reference_t<ReferenceTy> *, ReferenceTy> {
public:
  using BaseT = mapped_iterator_base;

  mapped_iterator_base(ItTy U)
      : mapped_iterator_base::iterator_adaptor_base(std::move(U)) {}

  ItTy getCurrent() { return this->I; }

  ReferenceTy operator*() const {
    return static_cast<const DerivedT &>(*this).mapElement(*this->I);
  }
};

/// Helper to determine if type T has a member called rbegin().
template <typename Ty> class has_rbegin_impl {
  using yes = char[1];
  using no = char[2];

  template <typename Inner>
  static yes& test(Inner *I, decltype(I->rbegin()) * = nullptr);

  template <typename>
  static no& test(...);

public:
  static const bool value = sizeof(test<Ty>(nullptr)) == sizeof(yes);
};

/// Metafunction to determine if T& or T has a member called rbegin().
template <typename Ty>
struct has_rbegin : has_rbegin_impl<typename std::remove_reference<Ty>::type> {
};

// Returns an iterator_range over the given container which iterates in reverse.
// Note that the container must have rbegin()/rend() methods for this to work.
template <typename ContainerTy>
auto reverse(ContainerTy &&C,
             std::enable_if_t<has_rbegin<ContainerTy>::value> * = nullptr) {
  return make_range(C.rbegin(), C.rend());
}

// Returns an iterator_range over the given container which iterates in reverse.
// Note that the container must have begin()/end() methods which return
// bidirectional iterators for this to work.
template <typename ContainerTy>
auto reverse(ContainerTy &&C,
             std::enable_if_t<!has_rbegin<ContainerTy>::value> * = nullptr) {
  return make_range(std::make_reverse_iterator(std::end(C)),
                    std::make_reverse_iterator(std::begin(C)));
}

/// An iterator adaptor that filters the elements of given inner iterators.
///
/// The predicate parameter should be a callable object that accepts the wrapped
/// iterator's reference type and returns a bool. When incrementing or
/// decrementing the iterator, it will call the predicate on each element and
/// skip any where it returns false.
///
/// \code
///   int A[] = { 1, 2, 3, 4 };
///   auto R = make_filter_range(A, [](int N) { return N % 2 == 1; });
///   // R contains { 1, 3 }.
/// \endcode
///
/// Note: filter_iterator_base implements support for forward iteration.
/// filter_iterator_impl exists to provide support for bidirectional iteration,
/// conditional on whether the wrapped iterator supports it.
template <typename WrappedIteratorT, typename PredicateT, typename IterTag>
class filter_iterator_base
    : public iterator_adaptor_base<
          filter_iterator_base<WrappedIteratorT, PredicateT, IterTag>,
          WrappedIteratorT,
          typename std::common_type<
              IterTag, typename std::iterator_traits<
                           WrappedIteratorT>::iterator_category>::type> {
  using BaseT = typename filter_iterator_base::iterator_adaptor_base;

protected:
  WrappedIteratorT End;
  PredicateT Pred;

  void findNextValid() {
    while (this->I != End && !Pred(*this->I))
      BaseT::operator++();
  }

  // Construct the iterator. The begin iterator needs to know where the end
  // is, so that it can properly stop when it gets there. The end iterator only
  // needs the predicate to support bidirectional iteration.
  filter_iterator_base(WrappedIteratorT Begin, WrappedIteratorT End,
                       PredicateT Pred)
      : BaseT(Begin), End(End), Pred(Pred) {
    findNextValid();
  }

public:
  using BaseT::operator++;

  filter_iterator_base &operator++() {
    BaseT::operator++();
    findNextValid();
    return *this;
  }
};

/// Specialization of filter_iterator_base for forward iteration only.
template <typename WrappedIteratorT, typename PredicateT,
          typename IterTag = std::forward_iterator_tag>
class filter_iterator_impl
    : public filter_iterator_base<WrappedIteratorT, PredicateT, IterTag> {
public:
  filter_iterator_impl(WrappedIteratorT Begin, WrappedIteratorT End,
                       PredicateT Pred)
      : filter_iterator_impl::filter_iterator_base(Begin, End, Pred) {}
};

/// Specialization of filter_iterator_base for bidirectional iteration.
template <typename WrappedIteratorT, typename PredicateT>
class filter_iterator_impl<WrappedIteratorT, PredicateT,
                           std::bidirectional_iterator_tag>
    : public filter_iterator_base<WrappedIteratorT, PredicateT,
                                  std::bidirectional_iterator_tag> {
  using BaseT = typename filter_iterator_impl::filter_iterator_base;

  void findPrevValid() {
    while (!this->Pred(*this->I))
      BaseT::operator--();
  }

public:
  using BaseT::operator--;

  filter_iterator_impl(WrappedIteratorT Begin, WrappedIteratorT End,
                       PredicateT Pred)
      : BaseT(Begin, End, Pred) {}

  filter_iterator_impl &operator--() {
    BaseT::operator--();
    findPrevValid();
    return *this;
  }
};

namespace detail {

template <bool is_bidirectional> struct fwd_or_bidi_tag_impl {
  using type = std::forward_iterator_tag;
};

template <> struct fwd_or_bidi_tag_impl<true> {
  using type = std::bidirectional_iterator_tag;
};

/// Helper which sets its type member to forward_iterator_tag if the category
/// of \p IterT does not derive from bidirectional_iterator_tag, and to
/// bidirectional_iterator_tag otherwise.
template <typename IterT> struct fwd_or_bidi_tag {
  using type = typename fwd_or_bidi_tag_impl<std::is_base_of<
      std::bidirectional_iterator_tag,
      typename std::iterator_traits<IterT>::iterator_category>::value>::type;
};

} // namespace detail

/// Defines filter_iterator to a suitable specialization of
/// filter_iterator_impl, based on the underlying iterator's category.
template <typename WrappedIteratorT, typename PredicateT>
using filter_iterator = filter_iterator_impl<
    WrappedIteratorT, PredicateT,
    typename detail::fwd_or_bidi_tag<WrappedIteratorT>::type>;

/// Convenience function that takes a range of elements and a predicate,
/// and return a new filter_iterator range.
///
/// FIXME: Currently if RangeT && is a rvalue reference to a temporary, the
/// lifetime of that temporary is not kept by the returned range object, and the
/// temporary is going to be dropped on the floor after the make_iterator_range
/// full expression that contains this function call.
template <typename RangeT, typename PredicateT>
iterator_range<filter_iterator<detail::IterOfRange<RangeT>, PredicateT>>
make_filter_range(RangeT &&Range, PredicateT Pred) {
  using FilterIteratorT =
      filter_iterator<detail::IterOfRange<RangeT>, PredicateT>;
  return make_range(
      FilterIteratorT(std::begin(std::forward<RangeT>(Range)),
                      std::end(std::forward<RangeT>(Range)), Pred),
      FilterIteratorT(std::end(std::forward<RangeT>(Range)),
                      std::end(std::forward<RangeT>(Range)), Pred));
}

/// A pseudo-iterator adaptor that is designed to implement "early increment"
/// style loops.
///
/// This is *not a normal iterator* and should almost never be used directly. It
/// is intended primarily to be used with range based for loops and some range
/// algorithms.
///
/// The iterator isn't quite an `OutputIterator` or an `InputIterator` but
/// somewhere between them. The constraints of these iterators are:
///
/// - On construction or after being incremented, it is comparable and
///   dereferencable. It is *not* incrementable.
/// - After being dereferenced, it is neither comparable nor dereferencable, it
///   is only incrementable.
///
/// This means you can only dereference the iterator once, and you can only
/// increment it once between dereferences.
template <typename WrappedIteratorT>
class early_inc_iterator_impl
    : public iterator_adaptor_base<early_inc_iterator_impl<WrappedIteratorT>,
                                   WrappedIteratorT, std::input_iterator_tag> {
  using BaseT = typename early_inc_iterator_impl::iterator_adaptor_base;

  using PointerT = typename std::iterator_traits<WrappedIteratorT>::pointer;

protected:
#if LLVM_ENABLE_ABI_BREAKING_CHECKS
  bool IsEarlyIncremented = false;
#endif

public:
  early_inc_iterator_impl(WrappedIteratorT I) : BaseT(I) {}

  using BaseT::operator*;
  decltype(*std::declval<WrappedIteratorT>()) operator*() {
#if LLVM_ENABLE_ABI_BREAKING_CHECKS
    assert(!IsEarlyIncremented && "Cannot dereference twice!");
    IsEarlyIncremented = true;
#endif
    return *(this->I)++;
  }

  using BaseT::operator++;
  early_inc_iterator_impl &operator++() {
#if LLVM_ENABLE_ABI_BREAKING_CHECKS
    assert(IsEarlyIncremented && "Cannot increment before dereferencing!");
    IsEarlyIncremented = false;
#endif
    return *this;
  }

  friend bool operator==(const early_inc_iterator_impl &LHS,
                         const early_inc_iterator_impl &RHS) {
#if LLVM_ENABLE_ABI_BREAKING_CHECKS
    assert(!LHS.IsEarlyIncremented && "Cannot compare after dereferencing!");
#endif
    return (const BaseT &)LHS == (const BaseT &)RHS;
  }
};

/// Make a range that does early increment to allow mutation of the underlying
/// range without disrupting iteration.
///
/// The underlying iterator will be incremented immediately after it is
/// dereferenced, allowing deletion of the current node or insertion of nodes to
/// not disrupt iteration provided they do not invalidate the *next* iterator --
/// the current iterator can be invalidated.
///
/// This requires a very exact pattern of use that is only really suitable to
/// range based for loops and other range algorithms that explicitly guarantee
/// to dereference exactly once each element, and to increment exactly once each
/// element.
template <typename RangeT>
iterator_range<early_inc_iterator_impl<detail::IterOfRange<RangeT>>>
make_early_inc_range(RangeT &&Range) {
  using EarlyIncIteratorT =
      early_inc_iterator_impl<detail::IterOfRange<RangeT>>;
  return make_range(EarlyIncIteratorT(std::begin(std::forward<RangeT>(Range))),
                    EarlyIncIteratorT(std::end(std::forward<RangeT>(Range))));
}

// forward declarations required by zip_shortest/zip_first/zip_longest
template <typename R, typename UnaryPredicate>
bool all_of(R &&range, UnaryPredicate P);
template <typename R, typename UnaryPredicate>
bool any_of(R &&range, UnaryPredicate P);

namespace detail {

using std::declval;

// We have to alias this since inlining the actual type at the usage site
// in the parameter list of iterator_facade_base<> below ICEs MSVC 2017.
template<typename... Iters> struct ZipTupleType {
  using type = std::tuple<decltype(*declval<Iters>())...>;
};

template <typename ZipType, typename... Iters>
using zip_traits = iterator_facade_base<
    ZipType, typename std::common_type<std::bidirectional_iterator_tag,
                                       typename std::iterator_traits<
                                           Iters>::iterator_category...>::type,
    // ^ TODO: Implement random access methods.
    typename ZipTupleType<Iters...>::type,
    typename std::iterator_traits<typename std::tuple_element<
        0, std::tuple<Iters...>>::type>::difference_type,
    // ^ FIXME: This follows boost::make_zip_iterator's assumption that all
    // inner iterators have the same difference_type. It would fail if, for
    // instance, the second field's difference_type were non-numeric while the
    // first is.
    typename ZipTupleType<Iters...>::type *,
    typename ZipTupleType<Iters...>::type>;

template <typename ZipType, typename... Iters>
struct zip_common : public zip_traits<ZipType, Iters...> {
  using Base = zip_traits<ZipType, Iters...>;
  using value_type = typename Base::value_type;

  std::tuple<Iters...> iterators;

protected:
  template <size_t... Ns> value_type deref(std::index_sequence<Ns...>) const {
    return value_type(*std::get<Ns>(iterators)...);
  }

  template <size_t... Ns>
  decltype(iterators) tup_inc(std::index_sequence<Ns...>) const {
    return std::tuple<Iters...>(std::next(std::get<Ns>(iterators))...);
  }

  template <size_t... Ns>
  decltype(iterators) tup_dec(std::index_sequence<Ns...>) const {
    return std::tuple<Iters...>(std::prev(std::get<Ns>(iterators))...);
  }

  template <size_t... Ns>
  bool test_all_equals(const zip_common &other,
            std::index_sequence<Ns...>) const {
    return all_of(std::initializer_list<bool>{std::get<Ns>(this->iterators) ==
                                              std::get<Ns>(other.iterators)...},
                  identity<bool>{});
  }

public:
  zip_common(Iters &&... ts) : iterators(std::forward<Iters>(ts)...) {}

  value_type operator*() const {
    return deref(std::index_sequence_for<Iters...>{});
  }

  ZipType &operator++() {
    iterators = tup_inc(std::index_sequence_for<Iters...>{});
    return *reinterpret_cast<ZipType *>(this);
  }

  ZipType &operator--() {
    static_assert(Base::IsBidirectional,
                  "All inner iterators must be at least bidirectional.");
    iterators = tup_dec(std::index_sequence_for<Iters...>{});
    return *reinterpret_cast<ZipType *>(this);
  }

  /// Return true if all the iterator are matching `other`'s iterators.
  bool all_equals(zip_common &other) {
    return test_all_equals(other, std::index_sequence_for<Iters...>{});
  }
};

template <typename... Iters>
struct zip_first : public zip_common<zip_first<Iters...>, Iters...> {
  using Base = zip_common<zip_first<Iters...>, Iters...>;

  bool operator==(const zip_first<Iters...> &other) const {
    return std::get<0>(this->iterators) == std::get<0>(other.iterators);
  }

  zip_first(Iters &&... ts) : Base(std::forward<Iters>(ts)...) {}
};

template <typename... Iters>
class zip_shortest : public zip_common<zip_shortest<Iters...>, Iters...> {
  template <size_t... Ns>
  bool test(const zip_shortest<Iters...> &other,
            std::index_sequence<Ns...>) const {
    return all_of(std::initializer_list<bool>{std::get<Ns>(this->iterators) !=
                                              std::get<Ns>(other.iterators)...},
                  identity<bool>{});
  }

public:
  using Base = zip_common<zip_shortest<Iters...>, Iters...>;

  zip_shortest(Iters &&... ts) : Base(std::forward<Iters>(ts)...) {}

  bool operator==(const zip_shortest<Iters...> &other) const {
    return !test(other, std::index_sequence_for<Iters...>{});
  }
};

template <template <typename...> class ItType, typename... Args> class zippy {
public:
  using iterator = ItType<decltype(std::begin(std::declval<Args>()))...>;
  using iterator_category = typename iterator::iterator_category;
  using value_type = typename iterator::value_type;
  using difference_type = typename iterator::difference_type;
  using pointer = typename iterator::pointer;
  using reference = typename iterator::reference;

private:
  std::tuple<Args...> ts;

  template <size_t... Ns>
  iterator begin_impl(std::index_sequence<Ns...>) const {
    return iterator(std::begin(std::get<Ns>(ts))...);
  }
  template <size_t... Ns> iterator end_impl(std::index_sequence<Ns...>) const {
    return iterator(std::end(std::get<Ns>(ts))...);
  }

public:
  zippy(Args &&... ts_) : ts(std::forward<Args>(ts_)...) {}

  iterator begin() const {
    return begin_impl(std::index_sequence_for<Args...>{});
  }
  iterator end() const { return end_impl(std::index_sequence_for<Args...>{}); }
};

} // end namespace detail

/// zip iterator for two or more iteratable types.
template <typename T, typename U, typename... Args>
detail::zippy<detail::zip_shortest, T, U, Args...> zip(T &&t, U &&u,
                                                       Args &&... args) {
  return detail::zippy<detail::zip_shortest, T, U, Args...>(
      std::forward<T>(t), std::forward<U>(u), std::forward<Args>(args)...);
}

/// zip iterator that, for the sake of efficiency, assumes the first iteratee to
/// be the shortest.
template <typename T, typename U, typename... Args>
detail::zippy<detail::zip_first, T, U, Args...> zip_first(T &&t, U &&u,
                                                          Args &&... args) {
  return detail::zippy<detail::zip_first, T, U, Args...>(
      std::forward<T>(t), std::forward<U>(u), std::forward<Args>(args)...);
}

namespace detail {
template <typename Iter>
Iter next_or_end(const Iter &I, const Iter &End) {
  if (I == End)
    return End;
  return std::next(I);
}

template <typename Iter>
auto deref_or_none(const Iter &I, const Iter &End) -> llvm::Optional<
    std::remove_const_t<std::remove_reference_t<decltype(*I)>>> {
  if (I == End)
    return None;
  return *I;
}

template <typename Iter> struct ZipLongestItemType {
  using type =
      llvm::Optional<typename std::remove_const<typename std::remove_reference<
          decltype(*std::declval<Iter>())>::type>::type>;
};

template <typename... Iters> struct ZipLongestTupleType {
  using type = std::tuple<typename ZipLongestItemType<Iters>::type...>;
};

template <typename... Iters>
class zip_longest_iterator
    : public iterator_facade_base<
          zip_longest_iterator<Iters...>,
          typename std::common_type<
              std::forward_iterator_tag,
              typename std::iterator_traits<Iters>::iterator_category...>::type,
          typename ZipLongestTupleType<Iters...>::type,
          typename std::iterator_traits<typename std::tuple_element<
              0, std::tuple<Iters...>>::type>::difference_type,
          typename ZipLongestTupleType<Iters...>::type *,
          typename ZipLongestTupleType<Iters...>::type> {
public:
  using value_type = typename ZipLongestTupleType<Iters...>::type;

private:
  std::tuple<Iters...> iterators;
  std::tuple<Iters...> end_iterators;

  template <size_t... Ns>
  bool test(const zip_longest_iterator<Iters...> &other,
            std::index_sequence<Ns...>) const {
    return llvm::any_of(
        std::initializer_list<bool>{std::get<Ns>(this->iterators) !=
                                    std::get<Ns>(other.iterators)...},
        identity<bool>{});
  }

  template <size_t... Ns> value_type deref(std::index_sequence<Ns...>) const {
    return value_type(
        deref_or_none(std::get<Ns>(iterators), std::get<Ns>(end_iterators))...);
  }

  template <size_t... Ns>
  decltype(iterators) tup_inc(std::index_sequence<Ns...>) const {
    return std::tuple<Iters...>(
        next_or_end(std::get<Ns>(iterators), std::get<Ns>(end_iterators))...);
  }

public:
  zip_longest_iterator(std::pair<Iters &&, Iters &&>... ts)
      : iterators(std::forward<Iters>(ts.first)...),
        end_iterators(std::forward<Iters>(ts.second)...) {}

  value_type operator*() const {
    return deref(std::index_sequence_for<Iters...>{});
  }

  zip_longest_iterator<Iters...> &operator++() {
    iterators = tup_inc(std::index_sequence_for<Iters...>{});
    return *this;
  }

  bool operator==(const zip_longest_iterator<Iters...> &other) const {
    return !test(other, std::index_sequence_for<Iters...>{});
  }
};

template <typename... Args> class zip_longest_range {
public:
  using iterator =
      zip_longest_iterator<decltype(adl_begin(std::declval<Args>()))...>;
  using iterator_category = typename iterator::iterator_category;
  using value_type = typename iterator::value_type;
  using difference_type = typename iterator::difference_type;
  using pointer = typename iterator::pointer;
  using reference = typename iterator::reference;

private:
  std::tuple<Args...> ts;

  template <size_t... Ns>
  iterator begin_impl(std::index_sequence<Ns...>) const {
    return iterator(std::make_pair(adl_begin(std::get<Ns>(ts)),
                                   adl_end(std::get<Ns>(ts)))...);
  }

  template <size_t... Ns> iterator end_impl(std::index_sequence<Ns...>) const {
    return iterator(std::make_pair(adl_end(std::get<Ns>(ts)),
                                   adl_end(std::get<Ns>(ts)))...);
  }

public:
  zip_longest_range(Args &&... ts_) : ts(std::forward<Args>(ts_)...) {}

  iterator begin() const {
    return begin_impl(std::index_sequence_for<Args...>{});
  }
  iterator end() const { return end_impl(std::index_sequence_for<Args...>{}); }
};
} // namespace detail

/// Iterate over two or more iterators at the same time. Iteration continues
/// until all iterators reach the end. The llvm::Optional only contains a value
/// if the iterator has not reached the end.
template <typename T, typename U, typename... Args>
detail::zip_longest_range<T, U, Args...> zip_longest(T &&t, U &&u,
                                                     Args &&... args) {
  return detail::zip_longest_range<T, U, Args...>(
      std::forward<T>(t), std::forward<U>(u), std::forward<Args>(args)...);
}

/// Iterator wrapper that concatenates sequences together.
///
/// This can concatenate different iterators, even with different types, into
/// a single iterator provided the value types of all the concatenated
/// iterators expose `reference` and `pointer` types that can be converted to
/// `ValueT &` and `ValueT *` respectively. It doesn't support more
/// interesting/customized pointer or reference types.
///
/// Currently this only supports forward or higher iterator categories as
/// inputs and always exposes a forward iterator interface.
template <typename ValueT, typename... IterTs>
class concat_iterator
    : public iterator_facade_base<concat_iterator<ValueT, IterTs...>,
                                  std::forward_iterator_tag, ValueT> {
  using BaseT = typename concat_iterator::iterator_facade_base;

  /// We store both the current and end iterators for each concatenated
  /// sequence in a tuple of pairs.
  ///
  /// Note that something like iterator_range seems nice at first here, but the
  /// range properties are of little benefit and end up getting in the way
  /// because we need to do mutation on the current iterators.
  std::tuple<IterTs...> Begins;
  std::tuple<IterTs...> Ends;

  /// Attempts to increment a specific iterator.
  ///
  /// Returns true if it was able to increment the iterator. Returns false if
  /// the iterator is already at the end iterator.
  template <size_t Index> bool incrementHelper() {
    auto &Begin = std::get<Index>(Begins);
    auto &End = std::get<Index>(Ends);
    if (Begin == End)
      return false;

    ++Begin;
    return true;
  }

  /// Increments the first non-end iterator.
  ///
  /// It is an error to call this with all iterators at the end.
  template <size_t... Ns> void increment(std::index_sequence<Ns...>) {
    // Build a sequence of functions to increment each iterator if possible.
    bool (concat_iterator::*IncrementHelperFns[])() = {
        &concat_iterator::incrementHelper<Ns>...};

    // Loop over them, and stop as soon as we succeed at incrementing one.
    for (auto &IncrementHelperFn : IncrementHelperFns)
      if ((this->*IncrementHelperFn)())
        return;

    llvm_unreachable("Attempted to increment an end concat iterator!");
  }

  /// Returns null if the specified iterator is at the end. Otherwise,
  /// dereferences the iterator and returns the address of the resulting
  /// reference.
  template <size_t Index> ValueT *getHelper() const {
    auto &Begin = std::get<Index>(Begins);
    auto &End = std::get<Index>(Ends);
    if (Begin == End)
      return nullptr;

    return &*Begin;
  }

  /// Finds the first non-end iterator, dereferences, and returns the resulting
  /// reference.
  ///
  /// It is an error to call this with all iterators at the end.
  template <size_t... Ns> ValueT &get(std::index_sequence<Ns...>) const {
    // Build a sequence of functions to get from iterator if possible.
    ValueT *(concat_iterator::*GetHelperFns[])() const = {
        &concat_iterator::getHelper<Ns>...};

    // Loop over them, and return the first result we find.
    for (auto &GetHelperFn : GetHelperFns)
      if (ValueT *P = (this->*GetHelperFn)())
        return *P;

    llvm_unreachable("Attempted to get a pointer from an end concat iterator!");
  }

public:
  /// Constructs an iterator from a sequence of ranges.
  ///
  /// We need the full range to know how to switch between each of the
  /// iterators.
  template <typename... RangeTs>
  explicit concat_iterator(RangeTs &&... Ranges)
      : Begins(std::begin(Ranges)...), Ends(std::end(Ranges)...) {}

  using BaseT::operator++;

  concat_iterator &operator++() {
    increment(std::index_sequence_for<IterTs...>());
    return *this;
  }

  ValueT &operator*() const {
    return get(std::index_sequence_for<IterTs...>());
  }

  bool operator==(const concat_iterator &RHS) const {
    return Begins == RHS.Begins && Ends == RHS.Ends;
  }
};

namespace detail {

/// Helper to store a sequence of ranges being concatenated and access them.
///
/// This is designed to facilitate providing actual storage when temporaries
/// are passed into the constructor such that we can use it as part of range
/// based for loops.
template <typename ValueT, typename... RangeTs> class concat_range {
public:
  using iterator =
      concat_iterator<ValueT,
                      decltype(std::begin(std::declval<RangeTs &>()))...>;

private:
  std::tuple<RangeTs...> Ranges;

  template <size_t... Ns>
  iterator begin_impl(std::index_sequence<Ns...>) {
    return iterator(std::get<Ns>(Ranges)...);
  }
  template <size_t... Ns>
  iterator begin_impl(std::index_sequence<Ns...>) const {
    return iterator(std::get<Ns>(Ranges)...);
  }
  template <size_t... Ns> iterator end_impl(std::index_sequence<Ns...>) {
    return iterator(make_range(std::end(std::get<Ns>(Ranges)),
                               std::end(std::get<Ns>(Ranges)))...);
  }
  template <size_t... Ns> iterator end_impl(std::index_sequence<Ns...>) const {
    return iterator(make_range(std::end(std::get<Ns>(Ranges)),
                               std::end(std::get<Ns>(Ranges)))...);
  }

public:
  concat_range(RangeTs &&... Ranges)
      : Ranges(std::forward<RangeTs>(Ranges)...) {}

  iterator begin() {
    return begin_impl(std::index_sequence_for<RangeTs...>{});
  }
  iterator begin() const {
    return begin_impl(std::index_sequence_for<RangeTs...>{});
  }
  iterator end() {
    return end_impl(std::index_sequence_for<RangeTs...>{});
  }
  iterator end() const {
    return end_impl(std::index_sequence_for<RangeTs...>{});
  }
};

} // end namespace detail

/// Concatenated range across two or more ranges.
///
/// The desired value type must be explicitly specified.
template <typename ValueT, typename... RangeTs>
detail::concat_range<ValueT, RangeTs...> concat(RangeTs &&... Ranges) {
  static_assert(sizeof...(RangeTs) > 1,
                "Need more than one range to concatenate!");
  return detail::concat_range<ValueT, RangeTs...>(
      std::forward<RangeTs>(Ranges)...);
}

/// A utility class used to implement an iterator that contains some base object
/// and an index. The iterator moves the index but keeps the base constant.
template <typename DerivedT, typename BaseT, typename T,
          typename PointerT = T *, typename ReferenceT = T &>
class indexed_accessor_iterator
    : public llvm::iterator_facade_base<DerivedT,
                                        std::random_access_iterator_tag, T,
                                        std::ptrdiff_t, PointerT, ReferenceT> {
public:
  ptrdiff_t operator-(const indexed_accessor_iterator &rhs) const {
    assert(base == rhs.base && "incompatible iterators");
    return index - rhs.index;
  }
  bool operator==(const indexed_accessor_iterator &rhs) const {
    return base == rhs.base && index == rhs.index;
  }
  bool operator<(const indexed_accessor_iterator &rhs) const {
    assert(base == rhs.base && "incompatible iterators");
    return index < rhs.index;
  }

  DerivedT &operator+=(ptrdiff_t offset) {
    this->index += offset;
    return static_cast<DerivedT &>(*this);
  }
  DerivedT &operator-=(ptrdiff_t offset) {
    this->index -= offset;
    return static_cast<DerivedT &>(*this);
  }

  /// Returns the current index of the iterator.
  ptrdiff_t getIndex() const { return index; }

  /// Returns the current base of the iterator.
  const BaseT &getBase() const { return base; }

protected:
  indexed_accessor_iterator(BaseT base, ptrdiff_t index)
      : base(base), index(index) {}
  BaseT base;
  ptrdiff_t index;
};

namespace detail {
/// The class represents the base of a range of indexed_accessor_iterators. It
/// provides support for many different range functionalities, e.g.
/// drop_front/slice/etc.. Derived range classes must implement the following
/// static methods:
///   * ReferenceT dereference_iterator(const BaseT &base, ptrdiff_t index)
///     - Dereference an iterator pointing to the base object at the given
///       index.
///   * BaseT offset_base(const BaseT &base, ptrdiff_t index)
///     - Return a new base that is offset from the provide base by 'index'
///       elements.
template <typename DerivedT, typename BaseT, typename T,
          typename PointerT = T *, typename ReferenceT = T &>
class indexed_accessor_range_base {
public:
  using RangeBaseT = indexed_accessor_range_base;

  /// An iterator element of this range.
  class iterator : public indexed_accessor_iterator<iterator, BaseT, T,
                                                    PointerT, ReferenceT> {
  public:
    // Index into this iterator, invoking a static method on the derived type.
    ReferenceT operator*() const {
      return DerivedT::dereference_iterator(this->getBase(), this->getIndex());
    }

  private:
    iterator(BaseT owner, ptrdiff_t curIndex)
        : iterator::indexed_accessor_iterator(owner, curIndex) {}

    /// Allow access to the constructor.
    friend indexed_accessor_range_base<DerivedT, BaseT, T, PointerT,
                                       ReferenceT>;
  };

  indexed_accessor_range_base(iterator begin, iterator end)
      : base(offset_base(begin.getBase(), begin.getIndex())),
        count(end.getIndex() - begin.getIndex()) {}
  indexed_accessor_range_base(const iterator_range<iterator> &range)
      : indexed_accessor_range_base(range.begin(), range.end()) {}
  indexed_accessor_range_base(BaseT base, ptrdiff_t count)
      : base(base), count(count) {}

  iterator begin() const { return iterator(base, 0); }
  iterator end() const { return iterator(base, count); }
  ReferenceT operator[](size_t Index) const {
    assert(Index < size() && "invalid index for value range");
    return DerivedT::dereference_iterator(base, static_cast<ptrdiff_t>(Index));
  }
  ReferenceT front() const {
    assert(!empty() && "expected non-empty range");
    return (*this)[0];
  }
  ReferenceT back() const {
    assert(!empty() && "expected non-empty range");
    return (*this)[size() - 1];
  }

  /// Compare this range with another.
  template <typename OtherT> bool operator==(const OtherT &other) const {
    return size() ==
               static_cast<size_t>(std::distance(other.begin(), other.end())) &&
           std::equal(begin(), end(), other.begin());
  }
  template <typename OtherT> bool operator!=(const OtherT &other) const {
    return !(*this == other);
  }

  /// Return the size of this range.
  size_t size() const { return count; }

  /// Return if the range is empty.
  bool empty() const { return size() == 0; }

  /// Drop the first N elements, and keep M elements.
  DerivedT slice(size_t n, size_t m) const {
    assert(n + m <= size() && "invalid size specifiers");
    return DerivedT(offset_base(base, n), m);
  }

  /// Drop the first n elements.
  DerivedT drop_front(size_t n = 1) const {
    assert(size() >= n && "Dropping more elements than exist");
    return slice(n, size() - n);
  }
  /// Drop the last n elements.
  DerivedT drop_back(size_t n = 1) const {
    assert(size() >= n && "Dropping more elements than exist");
    return DerivedT(base, size() - n);
  }

  /// Take the first n elements.
  DerivedT take_front(size_t n = 1) const {
    return n < size() ? drop_back(size() - n)
                      : static_cast<const DerivedT &>(*this);
  }

  /// Take the last n elements.
  DerivedT take_back(size_t n = 1) const {
    return n < size() ? drop_front(size() - n)
                      : static_cast<const DerivedT &>(*this);
  }

  /// Allow conversion to any type accepting an iterator_range.
  template <typename RangeT, typename = std::enable_if_t<std::is_constructible<
                                 RangeT, iterator_range<iterator>>::value>>
  operator RangeT() const {
    return RangeT(iterator_range<iterator>(*this));
  }

  /// Returns the base of this range.
  const BaseT &getBase() const { return base; }

private:
  /// Offset the given base by the given amount.
  static BaseT offset_base(const BaseT &base, size_t n) {
    return n == 0 ? base : DerivedT::offset_base(base, n);
  }

protected:
  indexed_accessor_range_base(const indexed_accessor_range_base &) = default;
  indexed_accessor_range_base(indexed_accessor_range_base &&) = default;
  indexed_accessor_range_base &
  operator=(const indexed_accessor_range_base &) = default;

  /// The base that owns the provided range of values.
  BaseT base;
  /// The size from the owning range.
  ptrdiff_t count;
};
} // end namespace detail

/// This class provides an implementation of a range of
/// indexed_accessor_iterators where the base is not indexable. Ranges with
/// bases that are offsetable should derive from indexed_accessor_range_base
/// instead. Derived range classes are expected to implement the following
/// static method:
///   * ReferenceT dereference(const BaseT &base, ptrdiff_t index)
///     - Dereference an iterator pointing to a parent base at the given index.
template <typename DerivedT, typename BaseT, typename T,
          typename PointerT = T *, typename ReferenceT = T &>
class indexed_accessor_range
    : public detail::indexed_accessor_range_base<
          DerivedT, std::pair<BaseT, ptrdiff_t>, T, PointerT, ReferenceT> {
public:
  indexed_accessor_range(BaseT base, ptrdiff_t startIndex, ptrdiff_t count)
      : detail::indexed_accessor_range_base<
            DerivedT, std::pair<BaseT, ptrdiff_t>, T, PointerT, ReferenceT>(
            std::make_pair(base, startIndex), count) {}
  using detail::indexed_accessor_range_base<
      DerivedT, std::pair<BaseT, ptrdiff_t>, T, PointerT,
      ReferenceT>::indexed_accessor_range_base;

  /// Returns the current base of the range.
  const BaseT &getBase() const { return this->base.first; }

  /// Returns the current start index of the range.
  ptrdiff_t getStartIndex() const { return this->base.second; }

  /// See `detail::indexed_accessor_range_base` for details.
  static std::pair<BaseT, ptrdiff_t>
  offset_base(const std::pair<BaseT, ptrdiff_t> &base, ptrdiff_t index) {
    // We encode the internal base as a pair of the derived base and a start
    // index into the derived base.
    return std::make_pair(base.first, base.second + index);
  }
  /// See `detail::indexed_accessor_range_base` for details.
  static ReferenceT
  dereference_iterator(const std::pair<BaseT, ptrdiff_t> &base,
                       ptrdiff_t index) {
    return DerivedT::dereference(base.first, base.second + index);
  }
};

namespace detail {
/// Return a reference to the first or second member of a reference. Otherwise,
/// return a copy of the member of a temporary.
///
/// When passing a range whose iterators return values instead of references,
/// the reference must be dropped from `decltype((elt.first))`, which will
/// always be a reference, to avoid returning a reference to a temporary.
template <typename EltTy, typename FirstTy> class first_or_second_type {
public:
  using type =
      typename std::conditional_t<std::is_reference<EltTy>::value, FirstTy,
                                  std::remove_reference_t<FirstTy>>;
};
} // end namespace detail

/// Given a container of pairs, return a range over the first elements.
template <typename ContainerTy> auto make_first_range(ContainerTy &&c) {
  using EltTy = decltype((*std::begin(c)));
  return llvm::map_range(std::forward<ContainerTy>(c),
                         [](EltTy elt) -> typename detail::first_or_second_type<
                                           EltTy, decltype((elt.first))>::type {
                           return elt.first;
                         });
}

/// Given a container of pairs, return a range over the second elements.
template <typename ContainerTy> auto make_second_range(ContainerTy &&c) {
  using EltTy = decltype((*std::begin(c)));
  return llvm::map_range(
      std::forward<ContainerTy>(c),
      [](EltTy elt) ->
      typename detail::first_or_second_type<EltTy,
                                            decltype((elt.second))>::type {
        return elt.second;
      });
}


} // end namespace llvm

#endif // LLVM_ADT_STLEXTRAS_H
