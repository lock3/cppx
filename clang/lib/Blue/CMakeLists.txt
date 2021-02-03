set(LLVM_LINK_COMPONENTS
  Core
  Support
  )

add_clang_library(blue
  BlueSymbol.cpp
  BlueFile.cpp
  BlueFrontend.cpp
  BlueLateElaboration.cpp # disabled
  BlueTokens.cpp
  BlueLexer.cpp
  BlueParser.cpp          # disabled
  BlueScope.cpp           # disabled
  BlueSema.cpp            # disabled
  BlueSyntax.cpp
  BlueSyntaxVisitor.cpp
  BlueDeclarator.cpp      # disabled
  BlueDeclaration.cpp     # disabled
  BlueElaborator.cpp      # disabled
  ParseBlueAST.cpp

  LINK_LIBS
  clangAST
  clangBasic
  clangFrontend
  clangLex
  clangSema
  )
