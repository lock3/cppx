set(LLVM_LINK_COMPONENTS
  Core
  Support
  )

add_clang_library(green
  File.cpp
  GreenFrontend.cpp
  GreenLexer.cpp
  GreenParser.cpp
  GreenScope.cpp
  GreenSema.cpp
  Elaborator.cpp
  ExprElaborator.cpp
  IdentifierMapper.cpp
  ParseGreenAST.cpp
  StmtElaborator.cpp
  Symbol.cpp
  Syntax.cpp
  Tokens.cpp

  LINK_LIBS
  clangAST
  clangBasic
  clangFrontend
  clangLex
  clangSema
  )
