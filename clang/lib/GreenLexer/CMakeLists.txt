set(LLVM_LINK_COMPONENTS
  Core
  Support
  )

add_clang_library(greenLexer
  GreenLexer.cpp
  GreenTokens.cpp

  LINK_LIBS
  greenBasic
  )
