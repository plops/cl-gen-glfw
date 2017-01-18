
(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload :cl-cpp-generator))

(in-package :cl-cpp-generator)


(defmacro with-glfw-window ((win &key (w 512) (h 512) (title "glfw")) &body body)
  `(let ((,win :type GLFWwindow))
     (if (! (funcall glfwInit))
	 (statements (return -1)))
     (setf ,win (funcall glfwCreateWindow ,w ,h ,title NULL NULL))
     (if (! ,win)
	 (statements (funcall glfwTerminate)
		     (return -1)))
     (funcall glfwMakeContextCurrent ,win)
     ,@body
     (funcall glfwTerminate)))


(defmacro with-c-file ((f fn) &body body)
  `(let ((,f :type FILE :init (funcall fopen ,fn)
	   ))
     ,@body))

(with-output-to-string (s)
  (emit-cpp
   :str s
   :clear-env t
   
   :code 
   `(with-compilation-unit
	(include "GLFW/glfw3.h")
      (function (main ((argc :type int)
		       (argv :type char**))
		      int)
		(funcall printf "blb")
		
		(macroexpand
		 (with-glfw-window (w)
		   (funcall printf "blb")))
		(return 0))
      )))



