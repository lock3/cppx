#include "CompileRun.h"
#include "ParseUtil.h"

using namespace llvm;
using namespace gold;

TEST(GoldFunctionTemplate, Execution) {
  StringRef Code = R"(
f[z : int]() : int!
  return z

main() : int!
  return f[4]()
)";
  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getPointerToNamedFunction("main"));
  ASSERT_TRUE(CB);
  int result = CB();
  ASSERT_EQ(result, 4);
}