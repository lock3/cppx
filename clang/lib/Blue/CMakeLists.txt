set(LLVM_LINK_COMPONENTS
  Core
  Support
  )

add_clang_library(blue
  ParseBlueAST.cpp

  LINK_LIBS
  clangAST
  clangBasic
  clangFrontend
  clangLex
  clangSema
  )
