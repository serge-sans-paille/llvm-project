#ifndef LLVM_TRANSFORMS_IPO_STACKCLASHPROTECTOR_H
#define LLVM_TRANSFORMS_IPO_STACKCLASHPROTECTOR_H

#include "llvm/IR/PassManager.h"

namespace llvm {

Pass *createStackClashProtector();

}

#endif // LLVM_TRANSFORMS_IPO_STACKCLASHPROTECTOR_H
