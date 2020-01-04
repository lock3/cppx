set(LLVM_LINK_COMPONENTS
  Core
  Support
  )

add_clang_library(blue
  BlueTokens.cpp
  ParseBlueAST.cpp

  LINK_LIBS
  clangAST
  clangBasic
  clangFrontend
  clangLex
  clangSema
  )
