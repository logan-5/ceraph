#ifndef CERAPH_UTIL_HPP
#define CERAPH_UTIL_HPP

#include <range/v3/algorithm/lower_bound.hpp>
#include <range/v3/algorithm/sort.hpp>
#include <range/v3/algorithm/unique.hpp>

#include <algorithm>
#include <functional>

namespace util {

template <typename F = std::function<void()>>
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

template <typename F>
auto make_heap_scopeguard(F&& f) {
    return std::make_unique<ScopeGuard<std::remove_reference_t<F>>>(
          std::forward(f));
}

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

namespace detail {
template <typename T>
inline constexpr bool pass_by_value = std::is_trivially_copyable_v<T> &&
                                      sizeof(T) <= 2 * sizeof(void*);

template <typename T, bool = pass_by_value<T>>
struct arg_type {
    using type = const T&;
};

template <typename T>
struct arg_type<T, true> {
    using type = T;
};
}  // namespace detail

template <typename T>
using arg_t = typename detail::arg_type<T>::type;

}  // namespace util

#endif
