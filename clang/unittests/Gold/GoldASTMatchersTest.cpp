#include "GoldASTMatchersTest.h"
namespace clang {
namespace ast_matchers {
const internal::VariadicDynCastAllOfMatcher<Decl, VarTemplateDecl> varTemplateDecl;
const internal::VariadicDynCastAllOfMatcher<Decl,
                   VarTemplateSpecializationDecl> varTemplateSpecializationDecl;
const internal::VariadicDynCastAllOfMatcher<
    Decl,
    VarTemplatePartialSpecializationDecl
  > varTemplatePartialSpecializationDecl;
}}