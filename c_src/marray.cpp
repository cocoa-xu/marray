#include <erl_nif.h>
#include <cstdint>
#include <vector>
#include <algorithm>
#include <memory>
#include <stdexcept>
#include "nif_utils.hpp"

#ifdef __GNUC__
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunused-function"
#endif

#if defined(_MSC_VER)
#include <BaseTsd.h>
typedef SSIZE_T ssize_t;
#endif

class marray {
private:
  void compute_new_size() {
    // computer new size based on [lowerbound, upperbound), and stride
    if (_stride < 0) {
      _size = 1 + (_upperbound - _lowerbound - 1) / -_stride;
    } else {
      _size = 1 + (_upperbound - _lowerbound - 1) / _stride;
    }
  }

public:
  using element_t = ERL_NIF_TERM;
  using array_t = std::vector<element_t>;
  std::shared_ptr<array_t> _data;

  // semi-open intervals
  size_t _lowerbound;
  size_t _upperbound;
  ssize_t _stride;
  size_t _size;

  marray(size_t capacity, ERL_NIF_TERM default_value) {
    this->_data = std::make_shared<array_t>(capacity, default_value);
    this->_lowerbound = 0;
    this->_upperbound = capacity;
    this->_stride = 1;
    this->_size = capacity;
  }

  marray(size_t capacity) {
    this->_data = std::make_shared<array_t>();
    this->_data->resize(capacity);
    this->_lowerbound = 0;
    this->_upperbound = capacity;
    this->_stride = 1;
    this->_size = capacity;
  }

  marray(const marray * other) {
    this->_data = other->_data;
    this->_lowerbound = other->_lowerbound;
    this->_upperbound = other->_upperbound;
    this->_stride = other->_stride;
    this->_size = other->_size;
  }

  marray * with_stride(ssize_t new_stride) {
    if (new_stride == 0) {
      throw std::runtime_error("stride should not be zero");
    }
    marray * t = new marray(this);
    t->_stride = new_stride;
    t->compute_new_size();
    return t;
  }

  auto size() const {
    return _size;
  }

  element_t& at(size_t n) {
    if (_stride < 0) {
      return _data->operator[](_upperbound - 1 + n * _stride);
    }
    return _data->operator[](_lowerbound + n * _stride);
  }

  const element_t& at(size_t n) const {
    if (_stride < 0) {
      return _data->operator[](_upperbound - 1 + n * _stride);
    }
    return _data->operator[](_lowerbound + n * _stride);
  }

  class MarrayIterator {
  public:
    using iterator_category = std::random_access_iterator_tag;
    using value_type = ERL_NIF_TERM;
    using difference_type = std::ptrdiff_t;
    using pointer = ERL_NIF_TERM*;
    using reference = ERL_NIF_TERM&;

  public:
    MarrayIterator(marray * ptr = nullptr, size_t index = 0) {
      m_ptr = ptr;
      m_index = index;
    }
    MarrayIterator(const MarrayIterator& iterator) = default;

    ~MarrayIterator(){}

    MarrayIterator& operator=(const MarrayIterator& iterator) = default;
    MarrayIterator& operator=(marray * ptr) {
      m_ptr = ptr;
      m_index = 0;
      return (*this);
    }

    operator bool() const {
      if(m_ptr)
        return true;
      else
        return false;
    }

    bool operator==(const MarrayIterator& iterator) const {
      return (m_ptr == iterator.m_ptr && m_index == iterator.m_index);
    }

    bool operator!=(const MarrayIterator& iterator) const {
      return (m_ptr != iterator.m_ptr || m_index != iterator.m_index);
    }

    MarrayIterator& operator+=(const difference_type& movement) {
      m_index += movement;
      return (*this);
    }

    MarrayIterator& operator-=(const difference_type& movement) {
      m_index -= movement;
      return (*this);
    }

    MarrayIterator& operator++() {
      ++m_index;
      return (*this);
    }

    MarrayIterator& operator--() {
      --m_index;
      return (*this);
    }

    MarrayIterator operator++(int) {
      auto temp(*this);
      ++m_index;
      return temp;
    }

    MarrayIterator operator--(int) {
      auto temp(*this);
      --m_index;
      return temp;
    }

    MarrayIterator operator+(const difference_type& movement) {
      auto old = m_index;
      m_index += movement;
      auto temp(*this);
      m_index = old;
      return temp;
    }

    MarrayIterator operator+(const int movement) {
      auto old = m_index;
      m_index += movement;
      auto temp(*this);
      m_index = old;
      return temp;
    }

    MarrayIterator operator-(const difference_type& movement) {
      auto old = m_index;
      m_index -= movement;
      auto temp(*this);
      m_index = old;
      return temp;
    }

    difference_type operator-(const MarrayIterator& iterator) {
      return std::abs((long long)iterator.m_index - (long long)this->m_index);
    }

    element_t& operator*() {
      return m_ptr->at(m_index);
    }

    marray * m_ptr;
    ssize_t m_index;
  };

  void reverse() {
    int size = this->size();
    for (size_t i = 0; i < size / 2; ++i) {
        std::swap(this->at(i), this->at(size - i - 1));
    }
  }

  typedef MarrayIterator iterator;

  iterator begin() {
    return iterator(this);
  }

  iterator end() {
    return iterator(this, _size);
  }
};

struct MarrayRes {
  marray * val;
  static ErlNifResourceType *type;
  static void destruct_resource(ErlNifEnv *env, void *args) {
    auto res = (MarrayRes *)args;
    if (res && res->val) {
      delete res->val;
      res->val = nullptr;
    }
  }
};

typedef MarrayRes marray_res;
ErlNifResourceType * marray_res::type = nullptr;

static ERL_NIF_TERM marray_new(ErlNifEnv *env, int argc,
                               const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM ret{};
  ERL_NIF_TERM error{};

  size_t capacity;
  if (!erlang::nif::get(env, argv[0], &capacity)) {
    return enif_make_badarg(env);
  }

  marray_res * res = static_cast<marray_res *>(enif_alloc_resource(marray_res::type, sizeof(marray_res)));
  if (res == nullptr) {
    error = erlang::nif::error(env, "cannot allocate Nif resource\n");
    return enif_raise_exception(env, error);
  }
  res->val = new marray(capacity, argv[1]);

  ret = enif_make_resource(env, res);
  enif_release_resource(res);
  return ret;
}

static ERL_NIF_TERM marray_from_list(ErlNifEnv *env, int argc,
                               const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM ret{};
  ERL_NIF_TERM error{};

  unsigned n = 0;
  enif_get_list_length(env, argv[0], &n);

  marray_res * res = static_cast<marray_res *>(enif_alloc_resource(marray_res::type, sizeof(marray_res)));
  if (res == nullptr) {
    error = erlang::nif::error(env, "cannot allocate Nif resource\n");
    return enif_raise_exception(env, error);
  }
  res->val = new marray((size_t)n);

  ERL_NIF_TERM head, tail, obj = argv[0];
  size_t i = 0;
  while (i < n) {
      if (enif_get_list_cell(env, obj, &head, &tail)) {
          res->val->at(i) = head;
          obj = tail;
          i++;
      } else {
        return enif_raise_exception(env,
          erlang::nif::error(env, "cannot access some elements in the list\n"));
      }
  }

  ret = enif_make_resource(env, res);
  enif_release_resource(res);
  return ret;
}

static ERL_NIF_TERM marray_to_list(ErlNifEnv *env, int argc,
                               const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM ret{};
  ERL_NIF_TERM error{};
  marray_res * array = nullptr;
  if (!enif_get_resource(env, argv[0], marray_res::type, reinterpret_cast<void **>(&array)) ||
      array == nullptr) {
    error = erlang::nif::error(env, "cannot access Nif resource");
    return enif_raise_exception(env, error);
  }

  size_t n = array->val->size();
  ERL_NIF_TERM * nif_array = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * n);
  if (nif_array == nullptr) {
    error = erlang::nif::error(env, "out of memory");
    return enif_raise_exception(env, error);
  }

  for (size_t i = 0; i < n; i++) {
    nif_array[i] = array->val->at(i);
  }
  ERL_NIF_TERM list = enif_make_list_from_array(env, nif_array, (unsigned)n);
  enif_free(nif_array);
  return list;
}

static ERL_NIF_TERM marray_set(ErlNifEnv *env, int argc,
                               const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM ret{};
  ERL_NIF_TERM error{};
  marray_res * array = nullptr;
  if (!enif_get_resource(env, argv[0], marray_res::type, reinterpret_cast<void **>(&array)) ||
      array == nullptr) {
    error = erlang::nif::error(env, "cannot access Nif resource");
    return enif_raise_exception(env, error);
  }

  size_t index;
  if (!erlang::nif::get(env, argv[1], &index)) {
    return enif_make_badarg(env);
  }
  if (index >= array->val->size()) {
    error = erlang::nif::error(env, "index out of bounds");
    return enif_raise_exception(env, error);
  }
  array->val->at(index) = argv[2];

  return erlang::nif::ok(env);
}

static ERL_NIF_TERM marray_get(ErlNifEnv *env, int argc,
                               const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM ret{};
  ERL_NIF_TERM error{};
  marray_res * array = nullptr;
  if (!enif_get_resource(env, argv[0], marray_res::type, reinterpret_cast<void **>(&array)) ||
      array == nullptr) {
    error = erlang::nif::error(env, "cannot access Nif resource");
    return enif_raise_exception(env, error);
  }

  size_t index;
  if (!erlang::nif::get(env, argv[1], &index)) {
    return enif_make_badarg(env);
  }
  if (index >= array->val->size()) {
    error = erlang::nif::error(env, "index out of bounds");
    return enif_raise_exception(env, error);
  }

  return array->val->at(index);
}

static ERL_NIF_TERM marray_swap(ErlNifEnv *env, int argc,
                               const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM ret{};
  ERL_NIF_TERM error{};
  marray_res * array = nullptr;
  if (!enif_get_resource(env, argv[0], marray_res::type, reinterpret_cast<void **>(&array)) ||
      array == nullptr) {
    error = erlang::nif::error(env, "cannot access Nif resource");
    return enif_raise_exception(env, error);
  }

  size_t index_i, index_j;
  if (!erlang::nif::get(env, argv[1], &index_i) || !erlang::nif::get(env, argv[2], &index_j)) {
    return enif_make_badarg(env);
  }
  if (index_i >= array->val->size() || index_j >= array->val->size()) {
    error = erlang::nif::error(env, "index out of bounds");
    return error;
  }

  std::swap(array->val->at(index_i), array->val->at(index_j));
  return erlang::nif::ok(env);
}

static ERL_NIF_TERM marray_size(ErlNifEnv *env, int argc,
                               const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM ret{};
  ERL_NIF_TERM error{};
  marray_res * array = nullptr;
  if (!enif_get_resource(env, argv[0], marray_res::type, reinterpret_cast<void **>(&array)) ||
      array == nullptr) {
    error = erlang::nif::error(env, "cannot access Nif resource");
    return enif_raise_exception(env, error);
  }

  return enif_make_uint64(env, array->val->size());
}

static ERL_NIF_TERM marray_sort(ErlNifEnv *env, int argc,
                               const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM ret{};
  ERL_NIF_TERM error{};
  marray_res * array = nullptr;
  if (!enif_get_resource(env, argv[0], marray_res::type, reinterpret_cast<void **>(&array)) ||
      array == nullptr) {
    error = erlang::nif::error(env, "cannot access Nif resource");
    return enif_raise_exception(env, error);
  }

  std::sort(array->val->begin(), array->val->end(), [](ERL_NIF_TERM a, ERL_NIF_TERM b) {
    return enif_compare(a, b) < 0;
  });
  return argv[0];
}

static ERL_NIF_TERM marray_reverse(ErlNifEnv *env, int argc,
                               const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM ret{};
  ERL_NIF_TERM error{};
  marray_res * array = nullptr;
  if (!enif_get_resource(env, argv[0], marray_res::type, reinterpret_cast<void **>(&array)) ||
      array == nullptr) {
    error = erlang::nif::error(env, "cannot access Nif resource");
    return enif_raise_exception(env, error);
  }

  array->val->reverse();
  return argv[0];
}

static ERL_NIF_TERM marray_stride_view(ErlNifEnv *env, int argc,
                                       const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM ret{};
  ERL_NIF_TERM error{};
  marray_res * array = nullptr;
  if (!enif_get_resource(env, argv[0], marray_res::type, reinterpret_cast<void **>(&array)) ||
      array == nullptr) {
    error = erlang::nif::error(env, "cannot access Nif resource");
    return enif_raise_exception(env, error);
  }

  int64_t new_stride = 0;
  if (!erlang::nif::get(env, argv[1], &new_stride) || new_stride == 0) {
    return enif_make_badarg(env);
  }

  marray_res * res = static_cast<marray_res *>(enif_alloc_resource(marray_res::type, sizeof(marray_res)));
  if (res == nullptr) {
    error = erlang::nif::error(env, "cannot allocate Nif resource");
    return enif_raise_exception(env, error);
  }
  res->val = array->val->with_stride(new_stride);

  ret = enif_make_resource(env, res);
  enif_release_resource(res);
  return ret;
}

static int on_load(ErlNifEnv *env, void **, ERL_NIF_TERM) {
  ErlNifResourceType *rt;
  rt = enif_open_resource_type(env, "marray_nif", "marray",
                               marray_res::destruct_resource, ERL_NIF_RT_CREATE,
                               NULL);
  if (!rt) return -1;
  marray_res::type = rt;
  return 0;
}

static int on_reload(ErlNifEnv *, void **, ERL_NIF_TERM) { return 0; }

static int on_upgrade(ErlNifEnv *, void **, void **, ERL_NIF_TERM) { return 0; }

static ErlNifFunc nif_functions[] = {
    {"marray_new", 2, marray_new, 0},
    {"marray_from_list", 1, marray_from_list, 0},
    {"marray_to_list", 1, marray_to_list, 0},
    {"marray_set", 3, marray_set, 0},
    {"marray_get", 2, marray_get, 0},
    {"marray_swap", 3, marray_swap, 0},
    {"marray_size", 1, marray_size, 0},
    {"marray_sort", 1, marray_sort, 0},
    {"marray_reverse", 1, marray_reverse, 0},
    {"marray_stride_view", 2, marray_stride_view, 0},
};

ERL_NIF_INIT(marray_nif, nif_functions, on_load, on_reload, on_upgrade, NULL);

#if defined(__GNUC__)
#pragma GCC visibility push(default)
#endif
