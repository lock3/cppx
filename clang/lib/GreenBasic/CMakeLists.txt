set(LLVM_LINK_COMPONENTS
  Support
  )

add_clang_library(greenBasic
  GreenBasic.cpp
  File.cpp
  Symbol.cpp

  LINK_LIBS
  clangBasic
)
