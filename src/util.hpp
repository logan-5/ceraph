#ifndef CERAPH_UTIL_HPP
#define CERAPH_UTIL_HPP

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
}  // namespace util

#endif
