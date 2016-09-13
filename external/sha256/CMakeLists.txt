project(sha256 C)
cmake_minimum_required(VERSION 2.0)

set(SHA256_SOURCE sha256.c)
set(SHA256_HEADER sha256.h)
add_library(sha256 ${SHA256_SOURCE})

install(FILES ${SHA256_HEADER} DESTINATION include/sha256)
install(TARGETS sha256 DESTINATION lib)

