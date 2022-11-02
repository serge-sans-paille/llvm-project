//===-- MoveAutoInit.cpp - move auto-init inst closer to their use site----===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This pass moves instruction maked as auto-init closer to the basic block that
// use it, eventually removing it from some control path of the function.
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Utils/MoveAutoInit.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Analysis/MemorySSA.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Transforms/Utils.h"
#include "llvm/Transforms/Utils/LoopUtils.h"

using namespace llvm;

#define DEBUG_TYPE "move-auto-init"

STATISTIC(NumMoved, "Number of instructions moved");

namespace {

bool hasAutoInitMetadata(Instruction const &I) {
  return I.hasMetadata(LLVMContext::MD_annotation) &&
         any_of(I.getMetadata(LLVMContext::MD_annotation)->operands(),
                [](const MDOperand &Op) {
                  return cast<MDString>(Op.get())->getString() == "auto-init";
                });
}

BasicBlock *usersDominator(Instruction *I, DominatorTree &DT, MemorySSA &MSSA) {
  BasicBlock *CurrentDominator = nullptr;

  MemoryUseOrDef *IMA = MSSA.getMemoryAccess(I);
  assert(IMA && "stack init is a memory access");

  SmallPtrSet<MemoryAccess *, 8> Visited;
  SmallVector<MemoryAccess *> WorkList;
  for (User *U : IMA->users()) {
    auto *MA = cast<MemoryAccess>(U);
    WorkList.push_back(MA);
  }

  while (!WorkList.empty()) {
    MemoryAccess *MA = WorkList.pop_back_val();
    if (!Visited.insert(MA).second)
      continue;

    if (auto *M = dyn_cast<MemoryUseOrDef>(MA)) {
      if (auto *MI = M->getMemoryInst()) {
        if (MI->isLifetimeStartOrEnd() || MI == I)
          continue;
        CurrentDominator = CurrentDominator
                               ? DT.findNearestCommonDominator(CurrentDominator,
                                                               MI->getParent())
                               : MI->getParent();
      }
    } else if (auto *M = dyn_cast<MemoryPhi>(MA)) {
      for (unsigned i = 0, n = M->getNumIncomingValues(); i != n; ++i)
        WorkList.push_back(M->getIncomingValue(i));
    }
  }

  return CurrentDominator;
}

bool runMoveAutoInit(Function &F, DominatorTree &DT, MemorySSA &MSSA) {
  BasicBlock &EntryBB = F.getEntryBlock();
  SmallVector<std::pair<Instruction *, Instruction *>> JobList;

  //
  // Compute movable instructions.
  //
  for (Instruction &I : EntryBB) {

    // FIXME: this guards against generalization of this pass to any statement.
    // The remaining logic should be pretty similar.
    if (!hasAutoInitMetadata(I))
      continue;

    BasicBlock *UsersDominator = usersDominator(&I, DT, MSSA);
    if (!UsersDominator)
      continue;

    if (UsersDominator == &EntryBB)
      continue;

    // Traverse the CFG to detect cycles `UsersDominator` would be part of.
    SmallPtrSet<BasicBlock *, 8> TransitiveSuccessors;
    SmallVector<BasicBlock *> WorkList(succ_begin(UsersDominator),
                                       succ_end(UsersDominator));
    bool HasCycle = false;
    while (!WorkList.empty()) {
      BasicBlock *CurrBB = WorkList.pop_back_val();
      if (CurrBB == UsersDominator)
        // No early exit because we want to compute the full set of transitive
        // successors.
        HasCycle = true;
      for (BasicBlock *Successor : successors(CurrBB)) {
        if (!TransitiveSuccessors.insert(Successor).second)
          continue;
        WorkList.push_back(Successor);
      }
    }

    // Don't insert if that could create multiple execution of I,
    // but we can insert it in the non back-edge predecessors, if it exists.

    if (HasCycle) {
      BasicBlock *UsersDominatorHead = UsersDominator;
      while (BasicBlock *UniquePredecessor =
                 UsersDominatorHead->getUniquePredecessor())
        UsersDominatorHead = UniquePredecessor;

      if (UsersDominatorHead == &EntryBB)
        continue;

      BasicBlock *DominatingPredecessor = nullptr;
      for (BasicBlock *Pred : predecessors(UsersDominatorHead)) {
        // If one of the predecessor also transitively is a successor, that's
        // not the path we don't consider that path.
        if (TransitiveSuccessors.count(Pred))
          continue;

        DominatingPredecessor =
            DominatingPredecessor
                ? DT.findNearestCommonDominator(DominatingPredecessor, Pred)
                : Pred;
      }

      if (!DominatingPredecessor || DominatingPredecessor == &EntryBB)
        continue;

      UsersDominator = DominatingPredecessor;
    }

    // We finally found a place where I can be moved while not introducing extra
    // execution, and guarded by at least one condition.
    JobList.emplace_back(&I, &*UsersDominator->getFirstInsertionPt());
  }

  //
  // Perform the actual substitution.
  //
  if (JobList.empty())
    return false;

  for (auto Job : JobList) {
    Job.first->moveBefore(Job.second);
  }

  NumMoved += JobList.size();

  return true;
}

} // namespace

PreservedAnalyses MoveAutoInitPass::run(Function &F,
                                        FunctionAnalysisManager &AM) {
  auto &DT = AM.getResult<DominatorTreeAnalysis>(F);
  auto &MSSA = AM.getResult<MemorySSAAnalysis>(F).getMSSA();
  if (!runMoveAutoInit(F, DT, MSSA))
    return PreservedAnalyses::all();

  PreservedAnalyses PA;
  PA.preserve<DominatorTreeAnalysis>();
  PA.preserve<MemorySSAAnalysis>();
  PA.preserveSet<CFGAnalyses>();
  return PA;
}
