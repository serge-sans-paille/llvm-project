//===- StackObjectCanary.cpp - Stack object canaries ------------------*- C++
//-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/SetVector.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/IVDescriptors.h"
#include "llvm/Analysis/LazyValueInfo.h"
#include "llvm/Analysis/MemoryBuiltins.h"
#include "llvm/Analysis/OptimizationRemarkEmitter.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/CodeGen/TargetLowering.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/ConstantRange.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instruction.h"
#include "llvm/InitializePasses.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"

using namespace llvm;

#define DEBUG_TYPE "stack-object-canary"

namespace {

bool isDynamicallyIndexableType(Type *Ty) {
  if (Ty->isArrayTy())
    return true;
  if (Ty->isVectorTy())
    return true;
  if (Ty->isStructTy()) {
    for (unsigned i = 0, n = Ty->getStructNumElements(); i != n; ++i)
      if (isDynamicallyIndexableType(Ty->getStructElementType(i)))
        return true;
  }
  return false;
}

bool isDynamicallyIndexableAlloca(AllocaInst *AI) {
  if (AI->isArrayAllocation())
    return true;
  return isDynamicallyIndexableType(AI->getAllocatedType());
}

template<class UIntTy>
GlobalVariable* makeObjectSizeHolder(Module& M,  uint64_t MaxAlign, SmallVectorImpl<AllocaInst*> const& AllStaticAllocaInsts) {
  auto const& DL = M.getDataLayout();
  SmallVector<UIntTy, 8> ObjSizes;
  for(auto* AI : AllStaticAllocaInsts) {
    UIntTy AllocationSizeInBits = *AI->getAllocationSizeInBits(DL);
    UIntTy ObjectSize = ((AllocationSizeInBits + MaxAlign - 1) / MaxAlign) * MaxAlign;
    ObjSizes.push_back(ObjectSize);
  }

  auto* GVObjSizes = new GlobalVariable(
      M,
      ArrayType::get(IntegerType::get(M.getContext(), 8 * sizeof(UIntTy)), AllStaticAllocaInsts.size()),
      true,
      GlobalVariable::InternalLinkage,
      ConstantDataArray::get(M.getContext(), ObjSizes)
  );
  return GVObjSizes;
}


bool runStackObjectCanary(Function &F, AliasAnalysis &AA, TargetMachine &TM,
                          LazyValueInfo &LVI, TargetLibraryInfo &TLI) {
  Module& M = *F.getParent();

  OptimizationRemarkEmitter ORE(&F);
  auto const &DL = F.getParent()->getDataLayout();

  bool hasNonScalarAllocations = false;

  SmallVector<AllocaInst *, 8> AllStaticAllocaInsts;
  for (auto &II : F.getEntryBlock()) {
    if (auto *AI = dyn_cast<AllocaInst>(&II)) {
      hasNonScalarAllocations |= isDynamicallyIndexableAlloca(AI);
        if(!AI->getAllocationSizeInBits(DL))
          continue;
        AllStaticAllocaInsts.push_back(AI);
      }
  }
  if(!hasNonScalarAllocations)
    return false;

  auto* PRNGState = M.getGlobalVariable("__seed");
  if(!PRNGState) {
    PRNGState = new GlobalVariable(
        M,
        IntegerType::get(F.getContext(), 64),
        false,
        GlobalVariable::InternalLinkage,
        ConstantInt::get(IntegerType::get(F.getContext(), 64), 1213457));
  }

  SmallVector<uint64_t, 8> Range(AllStaticAllocaInsts.size());
  for(uint64_t i = 0; i < AllStaticAllocaInsts.size(); ++i)
    Range[i] = i;

  auto* GVRange = new GlobalVariable(
      M,
      ArrayType::get(IntegerType::get(F.getContext(), 64), AllStaticAllocaInsts.size()),
      false,
      GlobalVariable::InternalLinkage,
      ConstantDataArray::get(M.getContext(), Range)
  );
  uint64_t MaxAlign = 1;
  uint64_t MaxObjectSize = 0;
  uint64_t CumulatedSize = 0;
  for(auto* AI : AllStaticAllocaInsts) {
    uint64_t Alignment = AI->getAlignment();
    uint64_t AllocationSizeInBits = *AI->getAllocationSizeInBits(DL);
    MaxAlign = std::max(MaxAlign, Alignment);
    uint64_t ObjectSize = ((AllocationSizeInBits + MaxAlign - 1) / MaxAlign) * MaxAlign;
    MaxObjectSize = std::max(MaxObjectSize, ObjectSize);
    CumulatedSize += ObjectSize;
  }

  GlobalVariable* GVObjSizes = nullptr;
  if(MaxObjectSize <= std::numeric_limits<uint8_t>::max())
    GVObjSizes = makeObjectSizeHolder<uint8_t>(M, MaxAlign, AllStaticAllocaInsts);
  else if(MaxObjectSize <= std::numeric_limits<uint16_t>::max())
    GVObjSizes = makeObjectSizeHolder<uint16_t>(M, MaxAlign, AllStaticAllocaInsts);
  else if(MaxObjectSize <= std::numeric_limits<int32_t>::max())
    GVObjSizes = makeObjectSizeHolder<uint32_t>(M, MaxAlign, AllStaticAllocaInsts);
  else if(MaxObjectSize <= std::numeric_limits<int64_t>::max())
    GVObjSizes = makeObjectSizeHolder<uint64_t>(M, MaxAlign, AllStaticAllocaInsts);
  else
    assert(false);

  IntegerType* GVElementTy = cast<IntegerType>(cast<ArrayType>(cast<PointerType>(GVObjSizes->getType())->getElementType())->getElementType());

  auto* GV = new GlobalVariable(
      M,
      ArrayType::get(GVElementTy, AllStaticAllocaInsts.size()),
      false,
      GlobalVariable::InternalLinkage,
      Constant::getNullValue(ArrayType::get(GVElementTy, AllStaticAllocaInsts.size()))
  );



  auto* PermInit = Function::Create(FunctionType::get(Type::getVoidTy(M.getContext()), {}), GlobalVariable::InternalLinkage, "update_perm", M);
  BasicBlock* PermBB = BasicBlock::Create(M.getContext(), "", PermInit);
  IRBuilder<> PermBuilder(PermBB);

  // FIXME: shuffle GVRange

  Value* Acc = ConstantInt::get(GVElementTy, 0);
  Value* CurrState = PermBuilder.CreateLoad(PRNGState);
  for(uint64_t i = 0; i < AllStaticAllocaInsts.size(); ++i) {
    Value * Index = nullptr;
    Value* TargetPtr = PermBuilder.CreateInBoundsGEP(nullptr, GVRange, {ConstantInt::get(IntegerType::get(F.getContext(), 64), 0), ConstantInt::get(IntegerType::get(F.getContext(), 64), i)});
    // faster: see https://lemire.me/blog/2016/06/30/fast-random-shuffling/
    if( i != AllStaticAllocaInsts.size() - 1) {
      Value * SwapIndex = PermBuilder.CreateAdd(
          ConstantInt::get(IntegerType::get(F.getContext(), 64), i),
          PermBuilder.CreateURem(CurrState, ConstantInt::get(IntegerType::get(F.getContext(), 64), AllStaticAllocaInsts.size() - i))
      );

      Value* SwapPtr = PermBuilder.CreateInBoundsGEP(nullptr, GVRange, {ConstantInt::get(IntegerType::get(F.getContext(), 64), 0), SwapIndex});
      Index = PermBuilder.CreateLoad(SwapPtr);
      PermBuilder.CreateStore(PermBuilder.CreateLoad(TargetPtr), SwapPtr);
      PermBuilder.CreateStore(Index, TargetPtr);
      CurrState =  PermBuilder.CreateAdd(
          ConstantInt::get(IntegerType::get(F.getContext(), 64), 1442695040888963407ULL),
          PermBuilder.CreateMul(CurrState, ConstantInt::get(IntegerType::get(F.getContext(), 64), 6364136223846793005ULL))
          );
    }
    else {
      Index = PermBuilder.CreateLoad(TargetPtr);
    }

    PermBuilder.CreateStore(Acc, PermBuilder.CreateInBoundsGEP(nullptr, GV, {ConstantInt::get(IntegerType::get(F.getContext(), 64), 0), Index}));
    if(i != AllStaticAllocaInsts.size() - 1) {
      Value* ObjectSize = PermBuilder.CreateLoad(PermBuilder.CreateInBoundsGEP(nullptr, GVObjSizes, {ConstantInt::get(IntegerType::get(F.getContext(), 64), 0), Index}));
      if(i == 0)
        Acc = ObjectSize;
      else
        Acc = PermBuilder.CreateAdd(Acc, ObjectSize);
    }
  }
  PermBuilder.CreateStore(CurrState, PRNGState);

  //CumulatedSize = ((CumulatedSize + MaxAlign - 1 ) / MaxAlign) * MaxAlign;
  PermBuilder.CreateRetVoid();
  appendToGlobalCtors(M, PermInit, 0);

  IRBuilder<> Builder(&*F.getEntryBlock().begin());
  AllocaInst* StackEmulation = Builder.CreateAlloca(IntegerType::get(F.getContext(), 8),
      ConstantInt::get(IntegerType::get(F.getContext(), 64), CumulatedSize));
  StackEmulation->setAlignment(MaybeAlign{MaxAlign});


  uint64_t index = 0;
  for(auto* AI : AllStaticAllocaInsts) {
    Value* NewAI = Builder.CreateBitCast(
        Builder.CreateInBoundsGEP(nullptr, StackEmulation,
          Builder.CreateLoad(Builder.CreateInBoundsGEP(nullptr, GV, {ConstantInt::get(IntegerType::get(F.getContext(), 64), 0), ConstantInt::get(IntegerType::get(F.getContext(), 64), index)}))
          ),
        AI->getType());
    AI->replaceAllUsesWith(NewAI);
    index += 1;
  }
  for (auto *AI : AllStaticAllocaInsts)
    AI->eraseFromParent();
#if 0

  StoreProcessor SP{AllAllocaInsts, AllDynamicallyIndexableAllocaInsts, AA, LVI,
                    TLI};
  SP.process(F);

  if (SP.getInstrumentedAllocas().empty())
    return false;

  for (auto *AI : SP.getInstrumentedAllocas()) {
    ORE.emit([&]() {
      return OptimizationRemark(DEBUG_TYPE, "StackViary", AI)
             << AI->getName() << " selected for stack protection";
    });
  }

  TargetLoweringBase const *TL = TM.getSubtargetImpl(F)->getTargetLowering();
  TL->insertSSPDeclarations(*F.getParent());
  Type *CanaryTy = IntegerType::get(F.getContext(), CanaryNumBits);
  IRBuilder<> Builder(&*F.getEntryBlock().begin());
  Value *Canary = getCanary(F, Builder, TL, CanaryTy);

  std::map<AllocaInst *,
           std::pair<Value *, std::function<Value *(IRBuilder<> &)>>>
      NewAllocaInsts;

  for (auto *AI : SP.getInstrumentedAllocas()) {

    auto *AITy = AI->getAllocatedType();

    IRBuilder<> Builder(AI);
    Value *NewAI = nullptr;
    std::function<Value *(IRBuilder<> &)> CanaryLoader;
    if (AI->isArrayAllocation()) {
      auto const &DL = F.getParent()->getDataLayout();
      auto ExtraObjectsRequired =
          (DL.getTypeSizeInBits(AITy) + CanaryNumBits - 1) /
          DL.getTypeSizeInBits(AITy);
      AllocaInst *BaseAI = Builder.CreateAlloca(
          AITy,
          Builder.CreateAdd(AI->getArraySize(),
                            ConstantInt::get(AI->getArraySize()->getType(),
                                             ExtraObjectsRequired)));
      copyAttributes(BaseAI, AI);
      NewAI = BaseAI;
      CanaryLoader = [=](IRBuilder<> &Builder) {
        return Builder.CreateBitCast(
            Builder.CreateInBoundsGEP(nullptr, NewAI, {AI->getArraySize()}),
            PointerType::getUnqual(CanaryTy));
      };
    } else {
      auto *AIWithCanaryTy =
          StructType::create({AITy, CanaryTy}, "stack_object_canary_ty");
      auto *BaseAI = Builder.CreateAlloca(AIWithCanaryTy);
      copyAttributes(BaseAI, AI);
      CanaryLoader = [=](IRBuilder<> &Builder) {
        return Builder.CreateInBoundsGEP(
            nullptr, BaseAI,
            {ConstantInt::get(IntegerType::get(Builder.getContext(), 32), 0),
             ConstantInt::get(IntegerType::get(Builder.getContext(), 32), 1)});
      };
      NewAI = Builder.CreateInBoundsGEP(
          nullptr, BaseAI,
          {ConstantInt::get(IntegerType::get(F.getContext(), 32), 0),
           ConstantInt::get(IntegerType::get(F.getContext(), 32), 0)});
    }
    NewAllocaInsts[AI] = std::make_pair(NewAI, CanaryLoader);

    AI->replaceAllUsesWith(NewAI);

    Builder.CreateStore(Canary, CanaryLoader(Builder), true /*volatile*/);
  }

  if (NewAllocaInsts.empty())
    return false;

  BasicBlock *ExitBlock =
      BasicBlock::Create(F.getContext(), "stack_check_exit_node", &F);
  IRBuilder<> ExitBuilder(ExitBlock);
  auto *TrapF = Intrinsic::getDeclaration(F.getParent(), Intrinsic::trap);
  ExitBuilder.CreateCall(TrapF, {});
  ExitBuilder.CreateUnreachable();

  for (auto &KV : SP.getFlushPointMap()) {
    auto *FlushPoint = KV.first;
    auto &AllocasToCheck = KV.second;

    IRBuilder<> Builder(FlushPoint);
    Value *CheckState = nullptr;
    // I'd like to avoid materializing it, but if I don't it may end up spilled
    // :-/
    // Value *Canary = getCanary(F, Builder, TL, CanaryTy);
    for (auto *AI : AllocasToCheck) {
      auto &NewAILoader = NewAllocaInsts[AI];
      CheckState = insertCheck(CheckState, Canary, Builder, NewAILoader.first,
                               NewAILoader.second, CanaryTy);
    }
    if (CheckState) {
      auto *IsNull = Builder.CreateIsNull(CheckState);
      BasicBlock *BB = FlushPoint->getParent();
      BasicBlock *Tail = BB->splitBasicBlock(cast<Instruction>(FlushPoint));

      auto *OldTerminator = BB->getTerminator();
      IRBuilder<> Builder(OldTerminator);
      Builder.CreateCondBr(IsNull, Tail, ExitBlock);
      OldTerminator->eraseFromParent();
    }
  }
  for (auto *AI : SP.getInstrumentedAllocas())
    AI->eraseFromParent();
#endif

  return true;
}

} // namespace

namespace {
class StackObjectCanary : public FunctionPass {
public:
  static char ID;

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.addRequired<AAResultsWrapperPass>();
    AU.addRequired<DominatorTreeWrapperPass>();
    AU.addRequired<TargetLibraryInfoWrapperPass>();
    AU.addRequired<LazyValueInfoWrapperPass>();
    AU.addRequired<ScalarEvolutionWrapperPass>();
    AU.addRequired<LazyValueInfoWrapperPass>();
    AU.addRequired<TargetPassConfig>();
  }

  StackObjectCanary() : FunctionPass(ID) {
    initializeStackObjectCanaryPass(*PassRegistry::getPassRegistry());
  }

  bool runOnFunction(Function &F) override {
    if (skipFunction(F))
      return false;
    if (!F.hasFnAttribute(Attribute::StackProtectObject))
      return false;
    AliasAnalysis &AA = getAnalysis<AAResultsWrapperPass>().getAAResults();
    TargetMachine &TM = getAnalysis<TargetPassConfig>().getTM<TargetMachine>();
    DominatorTree &DT = getAnalysis<DominatorTreeWrapperPass>().getDomTree();
    ScalarEvolution &SE = getAnalysis<ScalarEvolutionWrapperPass>().getSE();
    LazyValueInfo &LVI = getAnalysis<LazyValueInfoWrapperPass>().getLVI();
    TargetLibraryInfo &TLI =
        getAnalysis<TargetLibraryInfoWrapperPass>().getTLI(F);
    LVI.enableDT();
    return runStackObjectCanary(F, AA, TM, LVI, TLI);
  }
};
} // namespace

char StackObjectCanary::ID = 0;
INITIALIZE_PASS_BEGIN(StackObjectCanary, "stack-object-canary",
                      "Stack Object Canary Protection", false, false)
INITIALIZE_PASS_DEPENDENCY(TargetPassConfig)
INITIALIZE_PASS_DEPENDENCY(AAResultsWrapperPass)
INITIALIZE_PASS_DEPENDENCY(DominatorTreeWrapperPass)
INITIALIZE_PASS_DEPENDENCY(TargetLibraryInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(ScalarEvolutionWrapperPass)
INITIALIZE_PASS_DEPENDENCY(LazyValueInfoWrapperPass)
INITIALIZE_PASS_END(StackObjectCanary, "stack-object-canary",
                    "Stack Object Canary Protection", false, false)

FunctionPass *llvm::createStackObjectCanaryPass() {
  return new StackObjectCanary();
}
