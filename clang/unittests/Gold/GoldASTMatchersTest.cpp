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

const internal::VariadicDynCastAllOfMatcher<Decl, TemplateTemplateParmDecl>
    templateTemplateParmDecl;

const internal::VariadicDynCastAllOfMatcher<Stmt, PackExpansionExpr>
    packExpansionExpr;

const internal::VariadicDynCastAllOfMatcher<Stmt, CXXFoldExpr>
    cxxFoldExpr;
const internal::VariadicDynCastAllOfMatcher<Stmt, SizeOfPackExpr> sizeOfPackExpr;
}}