#include "lib.h"
#include "GLFW/glfw3.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <dlfcn.h>
#include <iostream>
int g_app_main_loop_running = GL_TRUE;

static void glfw_key_handler_cb(GLFWwindow *window, int key, int scancode,
                                int action, int mods) {
  if ((GLFW_PRESS != action)) {
    return;
  }

  switch (key) {
  case GLFW_KEY_ESCAPE: {
    g_app_main_loop_running = GL_FALSE;
    glfwSetWindowShouldClose(window, GL_TRUE);
    break;
  }
  case GLFW_KEY_R: {
    glfwSetWindowShouldClose(window, GL_TRUE);
    break;
  }
  }

  return;
}

struct plugin_view_lib {
  void *handle;
  ino_t id;
  struct lib_api api;
  struct lib_state *state;
};

const char *g_lib_library_filename = "./libviewlib.so";

static void plugin_view_lib_load(struct plugin_view_lib *lib) {
  {
    struct stat attr;

    if ((0 == stat(g_lib_library_filename, &attr))) {
      if ((lib->id != attr.st_ino)) {
        (std::cout << "inode has changed" << std::endl);
        if (lib->handle) {
          lib->api.unload(lib->state);
          dlclose(lib->handle);
        }

        {
          void *handle = dlopen(g_lib_library_filename, RTLD_NOW);

          if (handle) {
            (std::cout << "dlopen success" << std::endl);
            lib->handle = handle;
            lib->id = attr.st_ino;

            {
              const struct lib_api *lib_api =
                  reinterpret_cast<const struct lib_api *>(
                      dlsym(lib->handle, "g_LIB_API"));

              if ((NULL != lib_api)) {
                lib->api = *lib_api;
                if ((NULL == lib->state)) {
                  (std::cout << "will initialize lib->state" << std::endl);
                  if ((0 == lib_api->init)) {
                    return;

                  } else {
                    lib->state = lib->api.init();
                  }

                } else {
                  (std::cout << "lib->state is already defined" << std::endl);
                }

                (std::cout << "will reload lib->state" << std::endl);
                lib->api.reload(lib->state);

              } else {
                (std::cout << "dlsym lib_api fail" << std::endl);
                dlclose(lib->handle);
                lib->handle = NULL;
                lib->id = 0;
              }
            }

          } else {
            (std::cout << "dlopen fail" << std::endl);
            lib->handle = NULL;
            lib->id = 0;
          }
        }
      }

    } else {
      (std::cout << "stat of library fail" << std::endl);
    }
  }
}

static void plugin_view_lib_unload(struct plugin_view_lib *lib) {
  (std::cout << "unload" << std::endl);
  if (lib->handle) {
    lib->api.finalize(lib->state);
    lib->state = NULL;
    dlclose(lib->handle);
    lib->handle = NULL;
    lib->id = 0;
  }
}

int main(int argc, char **argv) {
  {
    struct plugin_view_lib lib = {0};

    for (; (GL_TRUE == g_app_main_loop_running);) {
      {
        GLFWwindow *main_window;

        if ((!(glfwInit()))) {
          return -1;
        }

        main_window = glfwCreateWindow(512, 512, "glfw", NULL, NULL);
        if ((!(main_window))) {
          glfwTerminate();
          return -1;
        }

        glfwMakeContextCurrent(main_window);
        glfwSetKeyCallback(main_window, glfw_key_handler_cb);
        for (; (!(glfwWindowShouldClose(main_window)));) {
          glClear(GL_COLOR_BUFFER_BIT);
          plugin_view_lib_load(&lib);
          if (lib.handle) {
            if ((!(lib.api.step(lib.state)))) {
              break;
            }
          }

          glfwSwapBuffers(main_window);
          glfwPollEvents();
        }

        glfwTerminate();
      }
    }

    plugin_view_lib_unload(&lib);
  }

  return 0;
}
