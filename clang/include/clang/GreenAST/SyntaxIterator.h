#ifndef LLVM_GREEN_GREENAST_SYNTAXITERATOR_H
#define LLVM_GREEN_GREENAST_SYNTAXITERATOR_H

#include <iterator>

namespace usyntax {

struct Syntax;

class SyntaxIteratorBase {
protected:
  Syntax **S;

  SyntaxIteratorBase() : S(nullptr) {}
  SyntaxIteratorBase(Syntax **S) : S(S) {}
};

template <typename DERIVED, typename REFERENCE>
class SyntaxIteratorImpl : public SyntaxIteratorBase,
                           public std::iterator<std::forward_iterator_tag,
                                                REFERENCE, std::ptrdiff_t,
                                                REFERENCE, REFERENCE> {
protected:
  SyntaxIteratorImpl(const SyntaxIteratorBase &RHS) : SyntaxIteratorBase(RHS) {}

public:
  SyntaxIteratorImpl() = default;
  SyntaxIteratorImpl(Syntax **S) : SyntaxIteratorBase(S) {}

  DERIVED &operator++() {
    ++S;

    return static_cast<DERIVED&>(*this);
  }

  bool operator++(int) {
    DERIVED tmp = static_cast<DERIVED&>(*this);
    operator++();
    return tmp;
  }

  bool operator==(const DERIVED &RHS) const {
    return S == RHS.S;
  }

  bool operator!=(const DERIVED &RHS) const {
    return !(*this == RHS);
  }

  REFERENCE operator*() const {
    return *S;
  }

  REFERENCE operator->() const { return operator*(); }
};

class ConstSyntaxIterator;

class SyntaxIterator : public SyntaxIteratorImpl<SyntaxIterator, Syntax *&> {
  SyntaxIterator(const SyntaxIteratorBase &RHS)
    : SyntaxIteratorImpl<SyntaxIterator, Syntax *&>(RHS) {}

  inline friend SyntaxIterator
  cast_away_const(const ConstSyntaxIterator &RHS);

public:
  explicit SyntaxIterator() = default;
  SyntaxIterator(Syntax **S) : SyntaxIteratorImpl<SyntaxIterator, Syntax *&>(S) {}
};

class ConstSyntaxIterator : public SyntaxIteratorImpl<ConstSyntaxIterator,
                                                        const Syntax*> {
public:
  explicit ConstSyntaxIterator() = default;
  ConstSyntaxIterator(const SyntaxIterator &RHS)
    : SyntaxIteratorImpl<ConstSyntaxIterator, const Syntax *>(RHS) {}


  ConstSyntaxIterator(Syntax *const *S)
    : SyntaxIteratorImpl<ConstSyntaxIterator, const Syntax *>(
          const_cast<Syntax **>(S)) {}
};

inline SyntaxIterator cast_away_const(const ConstSyntaxIterator &RHS) {
  return RHS;
}

} // namespace usyntax
#endif
