# Interactive programming with OpenGL

I use my domain specific language to emit C++ code from Common Lisp.
This code consists of an executable wrapper program and a shared
library. On startup, the wrapper opens an OpenGL window using
libGLFW. Then it loads libviewlib.so and executes its functions, most
notably lib_step, which will redraw the window.

## Installation

- open gen.lisp in emacs/Slime, compile with C-c C-k
- mkdir build; cd build
- cmake ../source
- make
- ./viewapp &
- watch make
- repeatedly edit the code of the library in gen.lisp (e.g. change arguments to glColor3f) and regenerate by pressing C-M-x. The running viewapp will see reload the recompiled library.

# Keys

- ESC .. leave wrapper
- r .. explicitly reload library