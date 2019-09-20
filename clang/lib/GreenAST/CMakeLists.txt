set(LLVM_LINK_COMPONENTS
  Support
  )

add_clang_library(greenAst
  Syntax.cpp

  LINK_LIBS
  clangBasic
  clangLex
  clangAST
  )
