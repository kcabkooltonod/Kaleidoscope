cmake_minimum_required(VERSION 3.10)

# 设置项目名称
project(ToyProject)

# 查找 LLVM 安装目录
find_program(LLVM_CONFIG llvm-config)

if(NOT LLVM_CONFIG)
    message(FATAL_ERROR "llvm-config not found. Please install LLVM or ensure llvm-config is in your PATH.")
endif()

# 使用 llvm-config 获取编译和链接标志
execute_process(COMMAND ${LLVM_CONFIG} --cxxflags OUTPUT_VARIABLE LLVM_CXXFLAGS OUTPUT_STRIP_TRAILING_WHITESPACE)
execute_process(COMMAND ${LLVM_CONFIG} --ldflags OUTPUT_VARIABLE LLVM_LDFLAGS OUTPUT_STRIP_TRAILING_WHITESPACE)
execute_process(COMMAND ${LLVM_CONFIG} --system-libs --libs core OUTPUT_VARIABLE LLVM_LIBS OUTPUT_STRIP_TRAILING_WHITESPACE)

# 设置编译选项
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -O3 ${LLVM_CXXFLAGS}")

# 添加源文件
add_executable(toy toy.cpp)

# 链接 LLVM 和其他系统库
target_link_libraries(toy ${LLVM_LIBS})
target_link_options(toy PRIVATE ${LLVM_LDFLAGS})