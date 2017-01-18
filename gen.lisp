
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
       (function (main ((argc :type int)
			(argv :type char**))
		       int)
		 (macroexpand
		  (with-glfw-window (main_window)
		    (for (() (! (funcall glfwWindowShouldClose main_window)) ())
			 (funcall glClear GL_COLOR_BUFFER_BIT)
			 (funcall glfwSwapBuffers main_window)
			 (funcall glfwPollEvents))))
		 (return 0))
       )))
  (sb-ext:run-program "/usr/bin/clang-format" (list "-i" (namestring *out-cpp-filename*))))



