set(LLVM_LINK_COMPONENTS
  Support
  )

add_clang_unittest(GoldTests
  GoldParsingTest.cpp
  )

clang_target_link_libraries(GoldTests
  PRIVATE
  clangAST
  clangASTMatchers
  clangBasic
  clangFrontend
  clangParse
  clangSema
  clangSerialization
  clangTooling
  gold
  )
