#include "GoldCompileRun.h"
#include "GoldParseUtil.h"

using namespace llvm;
using namespace gold;

TEST(GoldBuiltinConstructors, BuiltinConstructorCall_Int) {
  StringRef Code = R"(
main() : int!
  x = int(5)
  return x
)";
  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getFunctionAddress("main"));
  ASSERT_TRUE(CB);
  int Result = CB();
  ASSERT_EQ(Result, 5);
}

TEST(GoldBuiltinConstructors, BuiltinConstructorCall_float) {
  StringRef Code = R"(
main() : int!
  x = float32(5)
  return int(x)
)";
  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getFunctionAddress("main"));
  ASSERT_TRUE(CB);
  int result = CB();
  ASSERT_EQ(result, 5);
}

TEST(GoldBuiltinConstructors, BuiltinConstructorCall_FailedReturnConversion) {
  // TODO: Fix me when I have a chance.
  StringRef Code = R"(
main() : int!
  x = float32(5)
  return x
)";
  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getFunctionAddress("main"));
  ASSERT_TRUE(CB);
  int result = CB();
  ASSERT_EQ(result, 5);
}
