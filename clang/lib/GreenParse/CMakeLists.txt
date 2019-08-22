set(LLVM_LINK_COMPONENTS
  Support
  )

add_clang_library(greenParse
  ParseGreenAST.cpp

  LINK_LIBS
  clangAST
  clangLex
  )
