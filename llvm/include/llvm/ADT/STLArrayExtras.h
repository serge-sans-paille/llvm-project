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

#ifndef LLVM_ADT_STLARRAYEXTRAS_H
#define LLVM_ADT_STLARRAYEXTRAS_H

#include <iterator>

namespace llvm {


//===----------------------------------------------------------------------===//
//     Extra additions for arrays
//===----------------------------------------------------------------------===//

// We have a copy here so that LLVM behaves the same when using different
// standard libraries.
template <class Iterator, class RNG>
void shuffle(Iterator first, Iterator last, RNG &&g) {
  using std::swap;  // Not using std::iter_swap to avoid <algorithm>
  // It would be better to use a std::uniform_int_distribution,
  // but that would be stdlib dependent.
  typedef
      typename std::iterator_traits<Iterator>::difference_type difference_type;
  for (auto size = last - first; size > 1; ++first, (void)--size) {
    difference_type offset = g() % size;
    // Avoid self-assignment due to incorrect assertions in libstdc++
    // containers (https://gcc.gnu.org/bugzilla/show_bug.cgi?id=85828).
    if (offset != difference_type(0))
      std::swap(*first, *(first + offset));
  }
}

/// Find the length of an array.
template <class T, std::size_t N>
constexpr inline size_t array_lengthof(T (&)[N]) {
  return N;
}

} // end namespace llvm

#endif // LLVM_ADT_STLEXTRAS_H
