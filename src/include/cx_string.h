#pragma once

#include <array>
#include <cstddef>
#include <cassert>
#include <string_view>

#include "cx_vector.h"

namespace cx
{


  template <std::size_t N = 32*1024>
  struct static_string
  {

    std::size_t m_size{};
    char m_data[N+1] = {}; 

    constexpr static_string(const char (&str)[N+1])
      : static_string(str, str + N) {}

    constexpr static_string(const char (&str)[N+1], std::size_t s)
      : static_string(str, str + s) {}

    constexpr static_string(const char *str, std::size_t s)
      : static_string(str, str + s) {}

    constexpr static_string() = default;

    template <std::input_iterator I, std::sentinel_for<I> S>
    constexpr void assign(I it, S stop) noexcept {
      m_size = std::distance(it, stop);
      std::copy(it, stop, m_data);
    }

    template <std::input_iterator I, std::sentinel_for<I> S>
    requires std::convertible_to<std::iter_value_t<I>, char> 
    constexpr static_string(I first, S last) {
      assign(first, last);
    }

    constexpr size_t size() const {
      return m_size;
    }

    constexpr const char *c_str() const {
      return m_data;
    }

    constexpr const char *begin() const {
      return m_data;
    }

    constexpr const char *end() const {
      return m_data + m_size;
    }

    template <size_t S>
    inline friend constexpr bool operator==(const static_string&               x,
                                      const static_string<S>& y) noexcept {
      return cx::equal(x.begin(), x.end(), y.begin(), y.end());
    }

    inline friend constexpr bool operator==(const static_string&               x,
                                      const char* y) noexcept {
      return cx::equal(x.begin(), x.end(), y, y + std::distance(x.begin(), x.end()));
    }
  };

  template<static_string str>
  constexpr auto static_shrink_to_fit() {
    return static_string<str.size()>{str.c_str(), str.size()};
  }

  template <std::size_t N>
  static_string(const char (&arr)[N])
  -> static_string<N-1>;

  template <std::size_t N>
  static_string(const char (&arr)[N], std::size_t s)
  -> static_string<N-1>;


  // note that this works because vector is implicitly null terminated with its data initializer
  template<typename CharType, size_t Size>
  struct basic_string : vector<CharType, Size>
  {
    constexpr basic_string(const static_string<Size> &s) 
      : vector<CharType, Size>(s.begin(), s.end())
    {
    }
    constexpr basic_string(const std::string_view &s)
      : vector<CharType, Size>(s.cbegin(), s.cend())
    {
    }

    constexpr basic_string() = default;

    constexpr basic_string &operator=(const static_string<Size> &s) {
      return *this = basic_string(s);
    }

    constexpr basic_string &operator=(const std::string_view &s) {
      return *this = basic_string(s);
    }

    constexpr const char *c_str() const {
      return this->data();
    }
  };

  template<typename CharType, size_t Size>
  constexpr bool operator==(const basic_string<CharType, Size> &lhs, const static_string<Size> &rhs)
  {
    return cx::equal(lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
  }

  using string = basic_string<char, 32>;


}