set (LLVM_LINK_COMPONENTS
  Core
  Support
  )

add_clang_library(greenSema
  GreenSema.cpp

  LINK_LIBS
  clangAST
  greenAst
  )
