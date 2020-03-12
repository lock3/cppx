#include "CompileRun.h"
#include "ParseUtil.h"

using namespace llvm;
using namespace gold;


TEST(GoldClassExec, AssignTest) {
  StringRef Code = R"(
c : type = class:
  x : int
  y : bool
main() : int!
  q : c
  q.x = 4
  return q.x
)";
  llvm::LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
  using MainSig = int(*)();
  MainSig CB = (MainSig)EE->getPointerToNamedFunction("main");
  ASSERT_TRUE(CB);
  int result = CB();
  ASSERT_EQ(result, 4);
}

TEST(GoldClassExec, SimpleComparisonTest) {
  StringRef Code = R"(
x : int = 5
y = 32
main() : int!
  return y > x
)";
  llvm::LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
  using MainSig = int(*)();
  MainSig CB = (MainSig)EE->getPointerToNamedFunction("main");
  ASSERT_TRUE(CB);
  int result = CB();
  ASSERT_EQ(result, 1);
}