set(LLVM_LINK_COMPONENTS
  Core
  Support
  )

add_clang_library(blue
  BlueSymbol.cpp
  BlueFile.cpp
  BlueFrontend.cpp
  BlueTokens.cpp
  BlueLexer.cpp
  BlueParser.cpp
  BlueSyntax.cpp
  ParseBlueAST.cpp

  LINK_LIBS
  clangAST
  clangBasic
  clangFrontend
  clangLex
  clangSema
  )
