#ifndef CERAPH_UTIL_HPP
#define CERAPH_UTIL_HPP

#include "llvm/ADT/SmallVector.h"

#include <range/v3/algorithm/lower_bound.hpp>
#include <range/v3/algorithm/sort.hpp>
#include <range/v3/algorithm/unique.hpp>

#include <algorithm>

namespace util {

template <template <typename, typename...> typename VectorLike,
          typename Func,
          typename T>
auto transform(const VectorLike<T>& v, Func f) {
    using Result = std::invoke_result_t<Func, const T&>;
    VectorLike<Result> ret;
    ret.reserve(v.size());
    std::transform(v.begin(), v.end(), std::back_inserter(ret), f);
    return ret;
}
template <unsigned N, typename Func, typename T>
auto transform(const llvm::SmallVector<T, N>& v, Func f) {
    using Result = std::invoke_result_t<Func, const T&>;
    llvm::SmallVector<Result, N> ret;
    ret.reserve(v.size());
    std::transform(v.begin(), v.end(), std::back_inserter(ret), f);
    return ret;
}

template <typename F>
struct ScopeGuard {
    ScopeGuard(F in_f) : f{std::move(in_f)} {}
    ~ScopeGuard() { std::invoke(f); }
    ScopeGuard(const ScopeGuard&) = delete;
    ScopeGuard& operator=(const ScopeGuard&) = delete;
    ScopeGuard(ScopeGuard&&) = delete;
    ScopeGuard& operator=(ScopeGuard&&) = delete;

    F f;
};
template <typename F>
ScopeGuard(F)->ScopeGuard<F>;

template <typename T, typename Proj = ranges::identity>
bool has_duplicates(std::vector<T> ts, Proj proj = {}) {
    ranges::sort(ts.begin(), ts.end(), std::less<>{}, proj);
    return ranges::unique(ts.begin(), ts.end(), std::equal_to<>{}, proj) !=
           ts.end();
}

// https://stackoverflow.com/a/446327/5379590
// souped up a bit
template <typename Iter,
          typename T,
          typename Proj = ranges::identity,
          typename Comp = std::less<>>
Iter binary_find(Iter begin,
                 Iter end,
                 const T& val,
                 Proj proj = {},
                 Comp comp = {}) {
    Iter it = ranges::lower_bound(begin, end, val, comp, proj);

    if (it != end && !comp(val, std::invoke(proj, *it)))
        return it;
    else
        return end;
}

}  // namespace util

#endif
