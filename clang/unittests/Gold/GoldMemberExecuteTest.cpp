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
  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getPointerToNamedFunction("main"));
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
  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getPointerToNamedFunction("main"));
  ASSERT_TRUE(CB);
  int result = CB();
  ASSERT_EQ(result, 1);
}


TEST(GoldClassExec, MemberInitializer) {
  StringRef Code = R"(
c : type = class:
  x : int = 69
main() : int!
  q : c
  return q.x
)";
  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getPointerToNamedFunction("main"));
  ASSERT_TRUE(CB);
  int result = CB();
  ASSERT_EQ(result, 69);
}

TEST(GoldClassExec, ImplicitConversionToBoolMemberInit) {
  StringRef Code = R"(
c : type = class:
  x : int = 69
  z : bool = 1
main() : int!
  q : c
  return q.z
)";
  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getPointerToNamedFunction("main"));
  ASSERT_TRUE(CB);
  int result = CB();
  ASSERT_EQ(result, 1);
}

TEST(GoldClassExec, MemberFunctionCall) {
  StringRef Code = R"(
c : type = class:
  x : int = 5
  y : bool
  foo() : int!
    return x
  
main() : int!
  q : c = c()
  return q.foo()
)";
  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getPointerToNamedFunction("main"));
  ASSERT_TRUE(CB);
  int result = CB();
  ASSERT_EQ(result, 5);
}

TEST(GoldClassExec, DefaultConstructorCall_WithSpecialInitializer) {
  StringRef Code = R"(
c : type = class:
  x : int = 5
  y : bool
  constructor() : void!
    x = 3
  foo() : int!
    return x
  

main() : int!
  q : c
  return q.foo()
)";
  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getPointerToNamedFunction("main"));
  ASSERT_TRUE(CB);
  int result = CB();
  ASSERT_EQ(result, 3);
}