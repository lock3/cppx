set (LLVM_LINK_COMPONENTS
  Core
  Support
  )

add_clang_library(greenSema
  GreenSema.cpp
  IdentifierMapper.cpp
  IdentifierTree.cpp

  LINK_LIBS
  clangAST
  clangSema
  greenAst
  )
