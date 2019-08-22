set(LLVM_LINK_COMPONENTS
  Support
  )

add_clang_library(greenFrontend
  GreenFrontend.cpp

  LINK_LIBS
  clangBasic
  clangFrontend
  greenParse
  )
