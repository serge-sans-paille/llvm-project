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

#ifndef LLVM_ADT_STLTYPETRAITSEXTRAS_H
#define LLVM_ADT_STLTYPETRAITSEXTRAS_H

#include "llvm/ADT/STLForwardCompat.h"
#include <iterator>
#include <type_traits>
#include <utility>


namespace llvm {

// Only used by compiler if both template types are the same.  Useful when
// using SFINAE to test for the existence of member functions.
template <typename T, T> struct SameType;

namespace detail {

template <typename RangeT>
using IterOfRange = decltype(std::begin(std::declval<RangeT &>()));

template <typename RangeT>
using ValueOfRange = typename std::remove_reference<decltype(
    *std::begin(std::declval<RangeT &>()))>::type;

} // end namespace detail

//===----------------------------------------------------------------------===//
//     Extra additions to <type_traits>
//===----------------------------------------------------------------------===//

template <typename T> struct make_const_ptr {
  using type =
      typename std::add_pointer<typename std::add_const<T>::type>::type;
};

template <typename T> struct make_const_ref {
  using type = typename std::add_lvalue_reference<
      typename std::add_const<T>::type>::type;
};

namespace detail {
template <typename...> using void_t = void;
template <class, template <class...> class Op, class... Args> struct detector {
  using value_t = std::false_type;
};
template <template <class...> class Op, class... Args>
struct detector<void_t<Op<Args...>>, Op, Args...> {
  using value_t = std::true_type;
};
} // end namespace detail

/// Detects if a given trait holds for some set of arguments 'Args'.
/// For example, the given trait could be used to detect if a given type
/// has a copy assignment operator:
///   template<class T>
///   using has_copy_assign_t = decltype(std::declval<T&>()
///                                                 = std::declval<const T&>());
///   bool fooHasCopyAssign = is_detected<has_copy_assign_t, FooClass>::value;
template <template <class...> class Op, class... Args>
using is_detected = typename detail::detector<void, Op, Args...>::value_t;

namespace detail {
template <typename Callable, typename... Args>
using is_invocable =
    decltype(std::declval<Callable &>()(std::declval<Args>()...));
} // namespace detail

/// Check if a Callable type can be invoked with the given set of arg types.
template <typename Callable, typename... Args>
using is_invocable = is_detected<detail::is_invocable, Callable, Args...>;

/// This class provides various trait information about a callable object.
///   * To access the number of arguments: Traits::num_args
///   * To access the type of an argument: Traits::arg_t<Index>
///   * To access the type of the result:  Traits::result_t
template <typename T, bool isClass = std::is_class<T>::value>
struct function_traits : public function_traits<decltype(&T::operator())> {};

/// Overload for class function types.
template <typename ClassType, typename ReturnType, typename... Args>
struct function_traits<ReturnType (ClassType::*)(Args...) const, false> {
  /// The number of arguments to this function.
  enum { num_args = sizeof...(Args) };

  /// The result type of this function.
  using result_t = ReturnType;

  /// The type of an argument to this function.
  template <size_t Index>
  using arg_t = typename std::tuple_element<Index, std::tuple<Args...>>::type;
};
/// Overload for class function types.
template <typename ClassType, typename ReturnType, typename... Args>
struct function_traits<ReturnType (ClassType::*)(Args...), false>
    : function_traits<ReturnType (ClassType::*)(Args...) const> {};
/// Overload for non-class function types.
template <typename ReturnType, typename... Args>
struct function_traits<ReturnType (*)(Args...), false> {
  /// The number of arguments to this function.
  enum { num_args = sizeof...(Args) };

  /// The result type of this function.
  using result_t = ReturnType;

  /// The type of an argument to this function.
  template <size_t i>
  using arg_t = typename std::tuple_element<i, std::tuple<Args...>>::type;
};
/// Overload for non-class function type references.
template <typename ReturnType, typename... Args>
struct function_traits<ReturnType (&)(Args...), false>
    : public function_traits<ReturnType (*)(Args...)> {};

/// traits class for checking whether type T is one of any of the given
/// types in the variadic list.
template <typename T, typename... Ts>
using is_one_of = disjunction<std::is_same<T, Ts>...>;

/// traits class for checking whether type T is a base class for all
///  the given types in the variadic list.
template <typename T, typename... Ts>
using are_base_of = conjunction<std::is_base_of<T, Ts>...>;

namespace detail {
template <typename T, typename... Us> struct TypesAreDistinct;
template <typename T, typename... Us>
struct TypesAreDistinct
    : std::integral_constant<bool, !is_one_of<T, Us...>::value &&
                                       TypesAreDistinct<Us...>::value> {};
template <typename T> struct TypesAreDistinct<T> : std::true_type {};
} // namespace detail

/// Determine if all types in Ts are distinct.
///
/// Useful to statically assert when Ts is intended to describe a non-multi set
/// of types.
///
/// Expensive (currently quadratic in sizeof(Ts...)), and so should only be
/// asserted once per instantiation of a type which requires it.
template <typename... Ts> struct TypesAreDistinct;
template <> struct TypesAreDistinct<> : std::true_type {};
template <typename... Ts>
struct TypesAreDistinct
    : std::integral_constant<bool, detail::TypesAreDistinct<Ts...>::value> {};

/// Find the first index where a type appears in a list of types.
///
/// FirstIndexOfType<T, Us...>::value is the first index of T in Us.
///
/// Typically only meaningful when it is otherwise statically known that the
/// type pack has no duplicate types. This should be guaranteed explicitly with
/// static_assert(TypesAreDistinct<Us...>::value).
///
/// It is a compile-time error to instantiate when T is not present in Us, i.e.
/// if is_one_of<T, Us...>::value is false.
template <typename T, typename... Us> struct FirstIndexOfType;
template <typename T, typename U, typename... Us>
struct FirstIndexOfType<T, U, Us...>
    : std::integral_constant<size_t, 1 + FirstIndexOfType<T, Us...>::value> {};
template <typename T, typename... Us>
struct FirstIndexOfType<T, T, Us...> : std::integral_constant<size_t, 0> {};

/// Find the type at a given index in a list of types.
///
/// TypeAtIndex<I, Ts...> is the type at index I in Ts.
template <size_t I, typename... Ts>
using TypeAtIndex = std::tuple_element_t<I, std::tuple<Ts...>>;


} // end namespace llvm

#endif // LLVM_ADT_STLEXTRAS_H
