#include "BlueCompileRun.h"
#include "BlueParseUtil.h"

using namespace llvm;
using namespace blue;

TEST(BlueMemInitExec, BlueBaseInitializer) {
  StringRef Code = R"(
main: () -> int = {
  x : derived = (4, 4);
  return x.y + x.x + x.p;
}

point: type = {
    public x: int = 2;
    public y: int = 1;
    operator=: (out this, xx : int, yy : int) = {
      x = xx;
      y = yy;
    }
}

derived: type = {
    :point;
    operator=: (out this, xx:int, yy:int) = { point = (xx,yy); }
    public p : int = 2;
}
)";
  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileBlueCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getFunctionAddress("main"));
  ASSERT_TRUE(CB);
  int Result = CB();
  ASSERT_EQ(Result, 10);
}

TEST(BlueMemInitExec, CopyConstructorWithThat) {
  StringRef Code = R"(
point: type = {
    x: int = 0;
    y: int = 0;
    operator=: (out this)                 = { x=0;      y=0;      }
    operator=: (out this, xx:int, yy:int) = { x=xx;     y=yy;     }
    operator=: (out this, that)           = { x=that.x; y=that.y; }
    sum: (this) = x+y;
}

square: (p: point) = {
    return p.sum() * p.sum();
}

main: () -> int = {
    mp: point = (1,2);
    mp2 := mp;
    return square(mp2);
}
)";
  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileBlueCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getFunctionAddress("main"));
  ASSERT_TRUE(CB);
  int Result = CB();
  ASSERT_EQ(Result, 9);
}

TEST(BlueMemInitExec, CopyConstructorWithExplicitThat) {
  StringRef Code = R"(
point: type = {
    x: int = 0;
    y: int = 0;
    operator=: (out this)                 = { x=0;      y=0;      }
    operator=: (out this, xx:int, yy:int) = { x=xx;     y=yy;     }
    operator=: (out this, that: point)    = { x=that.x; y=that.y; }
    sum: (this) = x+y;
}

square: (p: point) = {
    return p.sum() * p.sum();
}

main: () -> int = {
    mp: point = (1,2);
    mp2 := mp;
    return square(mp2);
}
)";
  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileBlueCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getFunctionAddress("main"));
  ASSERT_TRUE(CB);
  int Result = CB();
  ASSERT_EQ(Result, 9);
}

TEST(BlueMemInitExec, InClassInits) {
  StringRef Code = R"(
main: () -> int = {
    p: point = ();
    return p.x + p.y;
}

point: type = {
    public x: int = 4;
    public y: int = 2;
    operator=: (out this) = { x = 1; }
}

)";
  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileBlueCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getFunctionAddress("main"));
  ASSERT_TRUE(CB);
  int Result = CB();
  ASSERT_EQ(Result, 3);
}
