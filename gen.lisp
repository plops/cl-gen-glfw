
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






(progn
  (defparameter *out-cpp-filename*  (merge-pathnames "stage/gen-glfw/source/main.cpp"
						   (user-homedir-pathname)))
  
  (with-open-file (s *out-cpp-filename*
		    :direction :output
		    :if-exists :supersede
		    :if-does-not-exist :create)
					;with-output-to-string (s)
   (emit-cpp
    :str s
    :clear-env t
   
    :code 
    `(with-compilation-unit
	 (include "GLFW/glfw3.h")
       (decl ((g_app_main_loop_running :type int :init GL_TRUE)))
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
		 (return 0))
       )))
  (sb-ext:run-program "/usr/bin/clang-format" (list "-i" (namestring *out-cpp-filename*))))



