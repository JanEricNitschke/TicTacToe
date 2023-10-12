cmake_minimum_required(VERSION 3.14)
set(CMAKE_C_COMPILER "gcc")
set(CMAKE_CXX_COMPILER "g++")
project(TicTacToe-Cpp)
# GoogleTest requires at least C++14
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# The following folder will be included
include_directories("${PROJECT_SOURCE_DIR}/include")
include_directories("${PROJECT_SOURCE_DIR}/src")

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -pedantic -Wall -Wextra -Wcast-align \
-Wcast-qual -Wdisabled-optimization -Wformat=2 -Winit-self \
-Wlogical-op -Wmissing-declarations -Wmissing-include-dirs -Wnoexcept -Wold-style-cast \
-Woverloaded-virtual -Wredundant-decls -Wshadow -Wsign-promo \
-Wstrict-null-sentinel -Wstrict-overflow=5 -Werror -Wfloat-equal -Werror=narrowing")

set(SOURCES ${PROJECT_SOURCE_DIR}/src/main.cpp ${PROJECT_SOURCE_DIR}/src/tictactoe.cpp)

add_executable(
  tictactoe
  ${SOURCES}
)

include(FetchContent)
FetchContent_Declare(
  googletest
  URL https://github.com/google/googletest/archive/03597a01ee50ed33e9dfd640b249b4be3799d395.zip
)
# For Windows: Prevent overriding the parent project's compiler/linker settings
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

enable_testing()

set(TESTSOURCES ${PROJECT_SOURCE_DIR}/tests/tictactoe_test.cpp ${PROJECT_SOURCE_DIR}/src/tictactoe.cpp )
add_executable(
  tictactoe_test
  ${TESTSOURCES}
)

target_include_directories(tictactoe_test PRIVATE "${PROJECT_SOURCE_DIR}/tests")

target_link_libraries(
  tictactoe_test
  GTest::gtest_main
  GTest::gmock_main
)


include(GoogleTest)
gtest_discover_tests(tictactoe_test)
