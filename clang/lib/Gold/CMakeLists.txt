set(LLVM_LINK_COMPONENTS
  Core
  Support
  )

add_clang_library(gold
  GoldElaborator.cpp
  GoldExprElaborator.cpp
  GoldFile.cpp
  GoldFrontend.cpp
  GoldLexer.cpp
  GoldParser.cpp
  GoldScope.cpp
  GoldSema.cpp
  GoldStmtElaborator.cpp
  GoldSymbol.cpp
  GoldSyntax.cpp
  GoldTokens.cpp
  ParseGoldAST.cpp

  LINK_LIBS
  clangAST
  clangBasic
  clangFrontend
  clangLex
  clangSema
  )
