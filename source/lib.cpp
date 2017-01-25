#include "lib.h"
#include <stdlib.h>
#include <iostream>
#include "GLFW/glfw3.h"
extern "C" {
struct lib_state {
  float r;
};

static struct lib_state *lib_init() {
  (std::cout << "lib_init" << std::endl);
  {
    struct lib_state *state =
        reinterpret_cast<struct lib_state *>(malloc(sizeof(*state)));

    return state;
  }
}

static void lib_reload(struct lib_state *state) {
  (std::cout << "call of lib_reload" << std::endl);
}

static void lib_unload(struct lib_state *state) {
  (std::cout << "call of lib_unload" << std::endl);
}

static void lib_finalize(struct lib_state *state) {
  (std::cout << "call of lib_finalize" << std::endl);
}

static int lib_step(struct lib_state *state) {
  state->r += (1.f - 1);
  glColor3f((1.f + 0), (1.f + 0), (0.0f + 0));
  glBegin(GL_LINES);
  glVertex3f((0.0f + 0), (0.0f + 0), (0.0f + 0));
  glVertex3f((1.f + 0), (1.f + 0), (1.f + 0));
  glEnd();

  return 1;
}
} // extern "C"

extern "C" {
// sequence of entries in struct: init finalize reload unload step
const struct lib_api g_LIB_API = {lib_init, lib_finalize, lib_reload,
                                  lib_unload, lib_step};
} // extern "C"
