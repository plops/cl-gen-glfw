#pragma once
struct lib_state;

extern "C" {
struct lib_api {
  struct lib_state *(*init)();
  void (*finalize)(struct lib_state *state);
  void (*reload)(struct lib_state *state);
  void (*unload)(struct lib_state *state);
  int (*step)(struct lib_state *state);
};
} // extern "C"

extern "C" {
extern const struct lib_api g_LIB_API;
} // extern "C"
