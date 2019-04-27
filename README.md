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

## Use

- watch make
- repeatedly edit the code of the library in gen.lisp (e.g. change arguments to glColor3f) and regenerate by pressing C-M-x. The running viewapp will see reload the recompiled library.

## Keys

- ESC .. leave wrapper
- r .. explicitly reload library

## References

- http://nullprogram.com/blog/2014/12/23/



```
 When getting started with a new OpenGL project, the first thing you should do is to enable the debug output [0] w/ glDebugMessageCallback. You also need to enable the debug bit when creating a GL context.

This will give you human readable error messages (as in complete sentences of what went wrong) as well as some other info to pinpoint what went wrong and where.

Additionally, it's a good idea to use Renderdoc [1] or your $GPU_VENDOR's debugging and profiling tools.

[0] https://www.khronos.org/opengl/wiki/Debug_Output [1] https://renderdoc.org/

	
	
hyperpallium 3 months ago [-]

Good to know about! For openGL ES, it looks like it wasn't implemented until version 3.2, the most recent (I don't have a device with that version).

https://developer.android.com/reference/android/opengl/GLES3... 
```

