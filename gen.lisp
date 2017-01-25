;; cmake -DCMAKE_BUILD_TYPE=Debug ../source/
;; make VERBOSE=1
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

;; export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib
;; /usr/local/lib/pahole ~/stage/gen-glfw/build/libviewlib.so



(defmacro function-prefix (prefix &body body)
  `(with-compilation-unit
       ,@(mapcar
	  (lambda (x)
	    (if (eq 'function (first x))
		(destructuring-bind (fun_ (name params &optional ret &key ctor specifier)
					  &rest function-body) x
		  `(function (,(intern (string-upcase (format nil "~a_~a" prefix name)))
			       ,params ,ret :ctor ,ctor :specifier ,specifier)
			     ,@function-body))
		x)) body)))

(defmacro e (&body body)
  `(statements (<< "std::cout" ,@(loop for e in body collect
				      (cond ((stringp e) `(string ,e))
					    (t e))) "std::endl")))

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
       (extern-c
	(struct lib_api ()
		(function ("(*init)" () "struct lib_state*"))
		(function ("(*finalize)" ((state :type "struct lib_state*")) void))
		(function ("(*reload)" ((state :type "struct lib_state*")) void))
		(function ("(*unload)" ((state :type "struct lib_state*")) void))
		(function ("(*step)" ((state :type "struct lib_state*")) int))
		))
       (extern-c
	(decl ((g_LIB_API :type "extern const struct lib_api")))))))

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
       (include <iostream>)
       (include "GLFW/glfw3.h")
       
       (extern-c
	(struct lib_state ()
		(decl ((r :type float))))
	(function (lib_init () "static struct lib_state*")
		  (<< "std::cout" (string "lib_init") "std::endl")
		  (let ((state :type "struct lib_state*"
			       :init (funcall "reinterpret_cast<struct lib_state*>"
					      (funcall malloc (funcall sizeof *state)))))
		    (return state)))
	(function (lib_reload ((state :type "struct lib_state*")) "static void")
		  (macroexpand (e "call of lib_reload")))
	(function (lib_unload ((state :type "struct lib_state*")) "static void")
		  (macroexpand (e "call of lib_unload")))

	(function (lib_finalize ((state :type "struct lib_state*")) "static void")
		  (macroexpand (e "call of lib_finalize")))
	
	
	(function (lib_step ((state :type "struct lib_state*")) "static int")
		  ;(macroexpand (e "call of lib_step"))
		  (+= state->r .1)
		  (funcall glColor3f 1.0 1.0 0.0)
		  (macroexpand
				 
				 (with-gl-primitive GL_LINES
				   (funcall glVertex3f 0.0 0.0 0.0)
				   (funcall glVertex3f 1.0 1.0 1.0)))
		  (return 1)))
       (extern-c
	(raw "// sequence of entries in struct: init finalize reload unload step")

        ;; The One Definition Rule (ODR) still applies, meaning that
	;; you can only have one definition of the global variable
	;; visible at link-time (static or dynamic linking).
	;; http://stackoverflow.com/questions/19373061/what-happens-to-global-and-static-variables-in-a-shared-library-when-it-is-dynam
	;; http://www.bnikolic.co.uk/blog/linux-ld-debug.html
	;; LD_DEBUG=all ./viewapp
	;; 22830:     symbol=LIB_API ;  lookup in file=./libviewlib.so [0]
	;; 22830:     binding file ./libviewlib.so [0] to ./libviewlib.so [0]: normal symbol LIB_API

	(decl ((g_LIB_API :type "const struct lib_api" :init
			(list lib_init
			      lib_finalize
			      lib_reload
			      lib_unload
			      lib_step))))))))
  (sb-ext:run-program "/usr/bin/clang-format" (list "-i" (namestring *lib-cpp-filename*)))
  (sb-ext:run-program "/usr/bin/clang-format" (list "-i" (namestring *lib-h-filename*))))





(progn
  (defparameter *main-cpp-filename*  (merge-pathnames "stage/gen-glfw/source/main.cpp"
						   (user-homedir-pathname)))
  
  (with-open-file (s *main-cpp-filename*
		    :direction :output
		    :if-exists :supersede
		    :if-does-not-exist :create)
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
       (include <iostream>)
       
       (decl ((g_app_main_loop_running :type int :init GL_TRUE)))
       (function (glfw_key_handler_cb ((window :type GLFWwindow*)
				       (key :type int)
				       (scancode :type int)
				       (action :type int)
				       (mods :type int))
				      "static void")
		 (if (!= GLFW_PRESS action)
		     (statements (return)))
		 (case key 
		     (GLFW_KEY_ESCAPE
		      (setf g_app_main_loop_running GL_FALSE)
		      (funcall glfwSetWindowShouldClose window GL_TRUE))
		     (GLFW_KEY_R
		      (funcall glfwSetWindowShouldClose window GL_TRUE)
		      ))
		 (return))
       (struct plugin_view_lib ()
	       (decl ((handle :type void*)
		      (id :type ino_t)
		      (api :type "struct lib_api")
		      (state :type "struct lib_state*"))))
       (decl ((g_lib_library_filename :type "const char*" :init (string "./libviewlib.so"))))
       (macroexpand (function-prefix plugin_view_lib
		      (function (load ((lib :type "struct plugin_view_lib*")) "static void")
				(let ((attr :type "struct stat"))
				  (if (== 0 (funcall stat g_lib_library_filename &attr))
				      (statements
				       ;;(<< "std::cout" (string "stat of library success") "std::endl")
				       (if (!= lib->id attr.st_ino)
					   (statements
					    (<< "std::cout" (string "inode has changed") "std::endl")
					    (if (slot->value lib handle)
						(statements
						 (funcall lib->api.unload lib->state)
						 (funcall dlclose lib->handle)))
					    (let ((handle :type void*
							  :init (funcall dlopen g_lib_library_filename RTLD_NOW)))
					      (if handle
						  (statements
						   (<< "std::cout" (string "dlopen success") "std::endl")
						   (setf lib->handle handle
							 lib->id attr.st_ino)
						   (let ((lib_api :type "const struct lib_api*"
								  :init
								  (funcall "reinterpret_cast<const struct lib_api*>"
									   (funcall dlsym lib->handle
										    (string "g_LIB_API")))))
						     (if (!= NULL lib_api)
							 (statements
							  (setf lib->api *lib_api)
							  (if (== NULL lib->state)
							      (statements
							       (<< "std::cout" (string "will initialize lib->state") "std::endl")
							       (if (== 0 lib_api->init)
								   (statements
								    (return))
								   (setf lib->state (funcall lib->api.init))))
							      (statements
							       (<< "std::cout" (string "lib->state is already defined") "std::endl")))
							  (<< "std::cout" (string "will reload lib->state") "std::endl")
							  (funcall lib->api.reload lib->state)
							  )
							 (statements
							  (<< "std::cout" (string "dlsym lib_api fail") "std::endl")
							  (funcall dlclose lib->handle)
							  (setf lib->handle NULL
								lib->id 0)))))
						  (statements
						   (<< "std::cout" (string "dlopen fail") "std::endl")
						   (setf lib->handle NULL
							 lib->id 0)))))))
				      (statements
				       (<< "std::cout" (string "stat of library fail") "std::endl")))))
		      (function (unload ((lib :type "struct plugin_view_lib*")) "static void")
				(<< "std::cout" (string "unload") "std::endl")
				(if lib->handle
				    (statements
				     (funcall lib->api.finalize lib->state)
				     (setf lib->state NULL)
				     (funcall dlclose lib->handle)
				     (setf lib->handle NULL
					   lib->id 0))))))
       
       
       (function (main ((argc :type int)
			(argv :type char**))
		       int)
		 (let ((lib :type "struct plugin_view_lib" :init (list 0)))
		   (for (() (== GL_TRUE g_app_main_loop_running
			     ) ())
			
			(macroexpand
			 (with-glfw-window (main_window)
			   (funcall glfwSetKeyCallback main_window glfw_key_handler_cb)
			   (for (() (! (funcall glfwWindowShouldClose main_window)) ())

				
				(funcall glClear GL_COLOR_BUFFER_BIT)
				
				(funcall plugin_view_lib_load &lib)
				(if lib.handle
				    (if (! (funcall lib.api.step lib.state))
					(raw "break")))
				
				
				(funcall glfwSwapBuffers main_window)
				(funcall glfwPollEvents))))
			
			)
		   (funcall plugin_view_lib_unload &lib))
		 (return 0)))))
  (sb-ext:run-program "/usr/bin/clang-format" (list "-i" (namestring *main-cpp-filename*))))





