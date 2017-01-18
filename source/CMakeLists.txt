cmake_minimum_required (VERSION 2.6)
project (viewapp)
find_package(glfw3 REQUIRED)
find_package(OpenGL REQUIRED)
#target_include_directories(viewapp ${OPENGL_INCLUDE_DIR})


add_executable(viewapp main.cpp)
target_link_libraries(viewapp ${OPENGL_gl_LIBRARY})
target_link_libraries(viewapp glfw)
