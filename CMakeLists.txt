cmake_minimum_required(VERSION 2.7.8)
project(raggi)

enable_language (Fortran)

add_library(par_zig_mod par_zig_mod.f95)
add_executable(raggi raggi.f95)
target_link_libraries(raggi par_zig_mod)
