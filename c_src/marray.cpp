#include <erl_nif.h>
#include <cstdint>
#include <vector>
#include "nif_utils.hpp"

#ifdef __GNUC__
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunused-function"
#endif

class marray {
public:
  std::vector<ERL_NIF_TERM> _data;
  marray(size_t capacity, ERL_NIF_TERM default_value) {
    this->_data = std::vector<ERL_NIF_TERM>(capacity, default_value);
  }

  marray(size_t capacity) {
    this->_data = std::vector<ERL_NIF_TERM>();
    this->_data.resize(capacity);
  }

  auto data() {
    return _data.data();
  }

  auto size() const {
    return _data.size();
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
          res->val->_data[i] = head;
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

  return enif_make_list_from_array(env, array->val->data(), (unsigned)array->val->size());
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
  array->val->_data[index] = argv[2];

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

  return array->val->_data[index];
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

  std::swap(array->val->_data[index_i], array->val->_data[index_j]);
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

  std::sort(array->val->_data.begin(), array->val->_data.end(), [](ERL_NIF_TERM a, ERL_NIF_TERM b) {
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

  std::reverse(array->val->_data.begin(), array->val->_data.end());
  return argv[0];
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
    {"marray_reverse", 1, marray_reverse, 0}
};

ERL_NIF_INIT(marray_nif, nif_functions, on_load, on_reload, on_upgrade, NULL);

#if defined(__GNUC__)
#pragma GCC visibility push(default)
#endif
