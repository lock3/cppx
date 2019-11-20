set(LLVM_LINK_COMPONENTS
  Core
  Support
  )

add_clang_library(greenParse
  GreenParser.cpp
  ParseGreenAST.cpp

  LINK_LIBS
  clangAST
  clangLex
  greenBasic
  greenLexer
  greenSema
  )
