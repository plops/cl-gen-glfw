
(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload :cl-cpp-generator))

(in-package :cl-cpp-generator)


(defmacro with-glfw-window ((win &key (w 512) (h 512) (title "glfw")) &body body)
  `(let ((,win :type GLFWwindow*))
     (if (! (funcall glfwInit))
	 (statements (return -1)))
     (setf ,win (funcall glfwCreateWindow ,w ,h (string ,title) NULL NULL))
     (if (! ,win)
	 (statements (funcall glfwTerminate)
		     (return -1)))
     (funcall glfwMakeContextCurrent ,win)
     ,@body
     (funcall glfwTerminate)))

(defmacro with-gl-primitive (prim &body body)
  `(statements
    (funcall glBegin ,prim)
    ,@body
    (funcall glEnd)))



;; http://nullprogram.com/blog/2015/06/06/
;; https://bitbucket.org/alfonse/glloadgen/wiki/Home
;; http://nullprogram.com/blog/2014/12/23/ Interactive Programming in C

(defmacro function-prefix (prefix &body body)
  `(with-compilation-unit ,@(mapcar (lambda (x) (if (eq 'function (first x))
						    (destructuring-bind (fun_ (name params &optional ret &key ctor specifier) &rest function-body) x
						      `(function (,(intern (string-upcase (format nil "~a_~a" prefix name)))
								   ,params ,ret :ctor ,ctor :specifier ,specifier)
								 ,@function-body))
						    x)) body)))

(progn
  (defparameter *lib-h-filename*  (merge-pathnames "stage/gen-glfw/source/lib.h"
						     (user-homedir-pathname)))
  (defparameter *lib-cpp-filename*  (merge-pathnames "stage/gen-glfw/source/lib.cpp"
						   (user-homedir-pathname)))
  
  (with-open-file (s *lib-h-filename*
		    :direction :output
		    :if-exists :supersede
		    :if-does-not-exist :create)
    (emit-cpp
    :str s
    :clear-env t
    :code 
    `(with-compilation-unit
	 (raw "#pragma once")
       (statements (raw "struct lib_state"))
       (struct lib_api ()
	       (function ("(*init)" () "struct lib_state*"))
	       (function ("(*finalize)" ((state :type "struct lib_state*")) void))
	       (function ("(*reload)" ((state :type "struct lib_state*")) void))
	       (function ("(*unload)" ((state :type "struct lib_state*")) void))
	       (function ("(*step)" ((state :type "struct lib_state*")) int))
	       )
       (decl ((LIB_API :type "extern const struct lib_api"))))))

  (with-open-file (s *lib-cpp-filename*
		    :direction :output
		    :if-exists :supersede
		    :if-does-not-exist :create)
   (emit-cpp
    :str s
    :clear-env t
    :code 
    `(with-compilation-unit
	 (include "lib.h")
       (include <stdlib.h>)
       (struct lib_state ()
	       (decl ((r :type float))))
       (function (lib_init () "static struct lib_state*")
		 (let ((state :type "struct lib_state*"
			      :init (cast "struct lib_state*"
					  (funcall malloc (funcall sizeof *state)))))
		   (return state)))
       (function (lib_reload ((state :type "struct lib_state*")) "static void")
		 (raw "//"))
       (function (lib_unload ((state :type "struct lib_state*")) "static void")
		 (raw "//"))

       (function (lib_finalize ((state :type "struct lib_state*")) "static void")
		 (funcall free state))
       (function (lib_step ((state :type "struct lib_state*")) "static int")
		 (return 1))
       (decl ((LIB_API :type "const struct lib_api" :init
			(list lib_init
			      lib_finalize
			      lib_reload
			      lib_unload
			      lib_step))))
       )))
  (sb-ext:run-program "/usr/bin/clang-format" (list "-i" (namestring *lib-cpp-filename*)))
  (sb-ext:run-program "/usr/bin/clang-format" (list "-i" (namestring *lib-h-filename*))))





(progn
  (defparameter *main-cpp-filename*  (merge-pathnames "stage/gen-glfw/source/main.cpp"
						   (user-homedir-pathname)))
  
  (with-open-file (s *main-cpp-filename*
		    :direction :output
		    :if-exists :supersede
		    :if-does-not-exist :create)
					;with-output-to-string (s)
   (emit-cpp
    :str s
    :clear-env t
   
    :code 
    `(with-compilation-unit
	 (include "lib.h")
	 (include "GLFW/glfw3.h")
       (include <sys/types.h>)
       (include <sys/stat.h>)
       (include <dlfcn.h>)
       (decl ((g_app_main_loop_running :type int :init GL_TRUE)))
       (struct plugin_view_lib ()
	       (decl ((handle :type void*)
		      (id :type ino_t)
		      (api :type "struct lib_api"
			   )
		      (state :type "struct lib_state*"))))
       (decl ((g_lib_library_filename :type "const char*" :init (string "./libview.so"))))
       (macroexpand (function-prefix plugin_view_lib
		      (function (load ((lib :type "struct plugin_view_lib*")) "static void")
				(let ((attr :type "struct stat"
					    ))
				  (if (&& (== 0 (funcall stat g_lib_library_filename &attr))
					  (!= (slot->value lib id) (slot-value attr st_ino)))
				      (statements
				       (if (slot->value lib handle)
					   (statements
					    (funcall lib->api.unload lib->state)
					    (funcall dlclose lib->handle)))
				       (let ((handle :type void* :init (funcall dlopen g_lib_library_filename RTLD_NOW)))
					 (if handle
					     (statements
					      (setf lib->handle handle
						    lib->id attr.st_ino)
					      (let ((lib_api :type "const struct lib_api*" :init (funcall "reinterpret_cast<struct lib_api*>" (funcall dlsym lib->handle (string "LIB_API")))
						      ))))
					     ))))))
			     ))
       
       (function (glfw_key_handler_cb ((window :type GLFWwindow*)
				       (key :type int)
				       (scancode :type int)
				       (action :type int)
				       (mods :type int))
				      "static void")
		 (if (!= GLFW_PRESS action)
		     (statements (return)))
		 (if (== GLFW_KEY_ESCAPE key)
		     (statements
		      (funcall glfwSetWindowShouldClose window GL_TRUE)
		      (setf g_app_main_loop_running GL_FALSE)))
		 (return))
       (function (main ((argc :type int)
			(argv :type char**))
		       int)
		 (macroexpand
		  (with-glfw-window (main_window)
		    (funcall glfwSetKeyCallback main_window glfw_key_handler_cb)
		    (for (() (! (funcall glfwWindowShouldClose main_window)) ())
			 (funcall glClear GL_COLOR_BUFFER_BIT)
			 (macroexpand
			  (with-gl-primitive GL_LINES
			    (funcall glVertex3f 0.0 0.0 0.0)
			    (funcall glVertex3f 1.0 1.0 1.0)))
			 (funcall glfwSwapBuffers main_window)
			 (funcall glfwPollEvents))))
		 (return 0)))))
  (sb-ext:run-program "/usr/bin/clang-format" (list "-i" (namestring *main-cpp-filename*))))





