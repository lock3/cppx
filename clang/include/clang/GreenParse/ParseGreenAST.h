#ifndef LLVM_GREEN_GREENPARSE_PARSEGREENAST
#define LLVM_GREEN_GREENPARSE_PARSEGREENAST

#include "clang/Lex/Preprocessor.h"

#include "clang/GreenParse/ParseGreenAST.h"
#include "clang/GreenParse/GreenParser.h"
#include "clang/GreenSema/Cppify.h"
#include "clang/GreenSema/GreenSema.h"

#include <string>
#include <cstdlib>
#include <fstream>

using namespace clang;

namespace lock3 {

inline void ParseGreenAST(Preprocessor &PP) {
  using namespace std;
  using namespace usyntax;

  string src_filename{"../usyntax/test.usyntax"};
  ifstream src_file{src_filename};
  string src_text{istreambuf_iterator<char>(src_file),
                  istreambuf_iterator<char>()};

  auto src_syntaxs =
    GreenParser<GenerateSyntax>(
      GenerateSyntax{make_shared<string>(src_filename)},
      (char8 *)&src_text[0], (char8 *)&src_text[src_text.size()])
    .File();

  GreenSema Actions;
  Actions.FindIdentifiers(src_syntaxs);
}

} // namespace lock3

#endif
