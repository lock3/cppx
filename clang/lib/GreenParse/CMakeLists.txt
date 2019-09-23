set(LLVM_LINK_COMPONENTS
  Core
  Support
  )

add_clang_library(greenParse
  GreenParser.cpp
  ParseGreenAST.cpp
  SymbolTable.cpp
  TokenInfo.cpp

  LINK_LIBS
  clangAST
  clangLex
  greenSema
  )
