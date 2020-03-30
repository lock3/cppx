#include "CompileRun.h"
#include "ParseUtil.h"

using namespace llvm;
using namespace gold;

TEST(GoldBool, InitializationTest_AsTrue) {
  StringRef Code = R"(
main() : int!
  b : bool = 1
  return b
)";
  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getPointerToNamedFunction("main"));
  ASSERT_TRUE(CB);
  int result = CB();
  ASSERT_EQ(result, 1);
}

TEST(GoldBool, InitializationTest_AsFalse) {
  StringRef Code = R"(
main() : int!
  b : bool = 0
  return b
)";
  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getPointerToNamedFunction("main"));
  ASSERT_TRUE(CB);
  int result = CB();
  ASSERT_EQ(result, 0);
}