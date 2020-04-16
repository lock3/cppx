#include "CompileRun.h"
#include "ParseUtil.h"

using namespace llvm;
using namespace gold;

// TODO: Fix me!
// TEST(MemberFunctionTemplate, MemberFunctionAccess_TemplatedMemberCall) {
//   StringRef Code = R"(
// c : type = class:
//   y : bool = 0
//   foo[i:int]() :int!
//     return i


// main() : int!
//   q : c
//   return q.foo[3]()
// )";
//   LLVMContext Context;
//   std::unique_ptr<ExecutionEngine> EE;
//   ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
//   MainSig CB = MainSig(EE->getPointerToNamedFunction("main"));
//   ASSERT_TRUE(CB);
//   int result = CB();
//   ASSERT_EQ(result, 3);
// }