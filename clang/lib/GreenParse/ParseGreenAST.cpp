#include "clang/GreenParse/ParseGreenAST.h"

// #include "clang/GreenParse/ParseGreenAST.h"
// #include "clang/GreenParse/GreenParser.h"
// #include "clang/GreenSema/Cppify.h"
// #include "clang/GreenSema/GreenSema.h"

// #include <string>
// #include <cstdlib>
// #include <fstream>

// namespace usyntax {

// void ParseGreenAST(Preprocessor &PP) {
//   using namespace std;
//   using namespace usyntax;

//   string src_filename{"test.usyntax"};
//   ifstream src_file{src_filename};
//   string src_text{istreambuf_iterator<char>(src_file),
//                   istreambuf_iterator<char>()};

//   auto src_syntaxs =
//     GreenParser<GenerateSyntax>(
//       GenerateSyntax{make_shared<string>(src_filename)},
//       (char8 *)&src_text[0], (char8 *)&src_text[src_text.size()])
//     .File();

//   GreenSema Actions;
//   Actions.FindIdentifiers(src_syntaxs);

//   // ofstream dst_file{"test.out"};

//   // GenerateCpp{1, dst_file}.GenerateFile(src_syntaxs);
//   // dst_file << "/* size " << src_text.size() << " */ \n";
// }

// }
