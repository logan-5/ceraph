#ifndef CMP_OPS_HPP
#define CMP_OPS_HPP

#include <functional>
#include <tuple>

namespace detail {
// comparable reference wrapper to be used in comparison tuples. read:
// workaround for it being too hard to strip off trailing commas after the
// variadic macro args below. could just use std::tie(a, b, c) instead of
// std::tuple{cmp_ref(a), cmp_ref(b), cmp_ref(c),} otherwise
template <typename T>
struct cmp_ref_ {
    std::reference_wrapper<T> ref;

    bool operator<(const cmp_ref_ other) const {
        return ref.get() < other.ref.get();
    }
    bool operator==(const cmp_ref_ other) const {
        return ref.get() == other.ref.get();
    }
};
template <typename T>
cmp_ref_<const T> cmp_ref(const T& t) {
    return cmp_ref_<const T>{t};
}
}  // namespace detail

#define FE_1(WHAT, X) WHAT(X)
#define FE_2(WHAT, X, ...) WHAT(X) FE_1(WHAT, __VA_ARGS__)
#define FE_3(WHAT, X, ...) WHAT(X) FE_2(WHAT, __VA_ARGS__)
#define FE_4(WHAT, X, ...) WHAT(X) FE_3(WHAT, __VA_ARGS__)
#define FE_5(WHAT, X, ...) WHAT(X) FE_4(WHAT, __VA_ARGS__)
//... add more as needed

#define GET_MACRO(_1, _2, _3, _4, _5, NAME, ...) NAME
#define FOR_EACH(action, ...) \
    GET_MACRO(__VA_ARGS__, FE_5, FE_4, FE_3, FE_2, FE_1, )(action, __VA_ARGS__)

#define CMP_OPS_RHS_(A) detail::cmp_ref(rhs.A),
#define CMP_OPS_RHS(...) FOR_EACH(CMP_OPS_RHS_, __VA_ARGS__)
#define CMP_OPS_LHS_(A) detail::cmp_ref(lhs.A),
#define CMP_OPS_LHS(...) FOR_EACH(CMP_OPS_LHS_, __VA_ARGS__)

#define CMP_OPS(TYPE, ...)                                                 \
    inline bool operator<(util::arg_t<TYPE> lhs, util::arg_t<TYPE> rhs) {  \
        return std::tuple{CMP_OPS_LHS(__VA_ARGS__)} <                      \
               std::tuple{CMP_OPS_RHS(__VA_ARGS__)};                       \
    }                                                                      \
    inline bool operator==(util::arg_t<TYPE> lhs, util::arg_t<TYPE> rhs) { \
        return std::tuple{CMP_OPS_LHS(__VA_ARGS__)} ==                     \
               std::tuple{CMP_OPS_RHS(__VA_ARGS__)};                       \
    }

#endif
