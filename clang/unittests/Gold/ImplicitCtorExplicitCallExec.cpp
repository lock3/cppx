#include "CompileRun.h"
#include "ParseUtil.h"

using namespace llvm;
using namespace gold;

TEST(GoldRun, ImplicitCtorExplicitCall) {
  StringRef Code = R"(
c : type = class:
  x : int = 5
  y : bool = 3
main() : int!
  q = c()
  return x
)";
  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getPointerToNamedFunction("main"));
  ASSERT_TRUE(CB);
  int result = CB();
  ASSERT_EQ(result, 5);
}