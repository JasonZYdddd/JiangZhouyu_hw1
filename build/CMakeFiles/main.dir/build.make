# CMAKE generated file: DO NOT EDIT!
# Generated by "MinGW Makefiles" Generator, CMake Version 3.29

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:

#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:

# Disable VCS-based implicit rules.
% : %,v

# Disable VCS-based implicit rules.
% : RCS/%

# Disable VCS-based implicit rules.
% : RCS/%,v

# Disable VCS-based implicit rules.
% : SCCS/s.%

# Disable VCS-based implicit rules.
% : s.%

.SUFFIXES: .hpux_make_needs_suffix_list

# Command-line flag to silence nested $(MAKE).
$(VERBOSE)MAKESILENT = -s

#Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force:
.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

SHELL = cmd.exe

# The CMake executable.
CMAKE_COMMAND = "C:\Program Files\CMake\bin\cmake.exe"

# The command to remove a file.
RM = "C:\Program Files\CMake\bin\cmake.exe" -E rm -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = C:\Users\22377\Desktop\JiangZhouyu_hw1-main

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = C:\Users\22377\Desktop\JiangZhouyu_hw1-main\build

# Include any dependencies generated for this target.
include CMakeFiles/main.dir/depend.make
# Include any dependencies generated by the compiler for this target.
include CMakeFiles/main.dir/compiler_depend.make

# Include the progress variables for this target.
include CMakeFiles/main.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/main.dir/flags.make

CMakeFiles/main.dir/src/algebra.c.obj: CMakeFiles/main.dir/flags.make
CMakeFiles/main.dir/src/algebra.c.obj: CMakeFiles/main.dir/includes_C.rsp
CMakeFiles/main.dir/src/algebra.c.obj: C:/Users/22377/Desktop/JiangZhouyu_hw1-main/src/algebra.c
CMakeFiles/main.dir/src/algebra.c.obj: CMakeFiles/main.dir/compiler_depend.ts
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green --progress-dir=C:\Users\22377\Desktop\JiangZhouyu_hw1-main\build\CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building C object CMakeFiles/main.dir/src/algebra.c.obj"
	C:\PROGRA~2\EMBARC~1\Dev-Cpp\TDM-GC~1\bin\gcc.exe $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -MD -MT CMakeFiles/main.dir/src/algebra.c.obj -MF CMakeFiles\main.dir\src\algebra.c.obj.d -o CMakeFiles\main.dir\src\algebra.c.obj -c C:\Users\22377\Desktop\JiangZhouyu_hw1-main\src\algebra.c

CMakeFiles/main.dir/src/algebra.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green "Preprocessing C source to CMakeFiles/main.dir/src/algebra.c.i"
	C:\PROGRA~2\EMBARC~1\Dev-Cpp\TDM-GC~1\bin\gcc.exe $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E C:\Users\22377\Desktop\JiangZhouyu_hw1-main\src\algebra.c > CMakeFiles\main.dir\src\algebra.c.i

CMakeFiles/main.dir/src/algebra.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green "Compiling C source to assembly CMakeFiles/main.dir/src/algebra.c.s"
	C:\PROGRA~2\EMBARC~1\Dev-Cpp\TDM-GC~1\bin\gcc.exe $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S C:\Users\22377\Desktop\JiangZhouyu_hw1-main\src\algebra.c -o CMakeFiles\main.dir\src\algebra.c.s

CMakeFiles/main.dir/src/main.c.obj: CMakeFiles/main.dir/flags.make
CMakeFiles/main.dir/src/main.c.obj: CMakeFiles/main.dir/includes_C.rsp
CMakeFiles/main.dir/src/main.c.obj: C:/Users/22377/Desktop/JiangZhouyu_hw1-main/src/main.c
CMakeFiles/main.dir/src/main.c.obj: CMakeFiles/main.dir/compiler_depend.ts
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green --progress-dir=C:\Users\22377\Desktop\JiangZhouyu_hw1-main\build\CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Building C object CMakeFiles/main.dir/src/main.c.obj"
	C:\PROGRA~2\EMBARC~1\Dev-Cpp\TDM-GC~1\bin\gcc.exe $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -MD -MT CMakeFiles/main.dir/src/main.c.obj -MF CMakeFiles\main.dir\src\main.c.obj.d -o CMakeFiles\main.dir\src\main.c.obj -c C:\Users\22377\Desktop\JiangZhouyu_hw1-main\src\main.c

CMakeFiles/main.dir/src/main.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green "Preprocessing C source to CMakeFiles/main.dir/src/main.c.i"
	C:\PROGRA~2\EMBARC~1\Dev-Cpp\TDM-GC~1\bin\gcc.exe $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E C:\Users\22377\Desktop\JiangZhouyu_hw1-main\src\main.c > CMakeFiles\main.dir\src\main.c.i

CMakeFiles/main.dir/src/main.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green "Compiling C source to assembly CMakeFiles/main.dir/src/main.c.s"
	C:\PROGRA~2\EMBARC~1\Dev-Cpp\TDM-GC~1\bin\gcc.exe $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S C:\Users\22377\Desktop\JiangZhouyu_hw1-main\src\main.c -o CMakeFiles\main.dir\src\main.c.s

# Object files for target main
main_OBJECTS = \
"CMakeFiles/main.dir/src/algebra.c.obj" \
"CMakeFiles/main.dir/src/main.c.obj"

# External object files for target main
main_EXTERNAL_OBJECTS =

C:/Users/22377/Desktop/JiangZhouyu_hw1-main/bin/main.exe: CMakeFiles/main.dir/src/algebra.c.obj
C:/Users/22377/Desktop/JiangZhouyu_hw1-main/bin/main.exe: CMakeFiles/main.dir/src/main.c.obj
C:/Users/22377/Desktop/JiangZhouyu_hw1-main/bin/main.exe: CMakeFiles/main.dir/build.make
C:/Users/22377/Desktop/JiangZhouyu_hw1-main/bin/main.exe: CMakeFiles/main.dir/linkLibs.rsp
C:/Users/22377/Desktop/JiangZhouyu_hw1-main/bin/main.exe: CMakeFiles/main.dir/objects1.rsp
C:/Users/22377/Desktop/JiangZhouyu_hw1-main/bin/main.exe: CMakeFiles/main.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green --bold --progress-dir=C:\Users\22377\Desktop\JiangZhouyu_hw1-main\build\CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Linking C executable C:\Users\22377\Desktop\JiangZhouyu_hw1-main\bin\main.exe"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles\main.dir\link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/main.dir/build: C:/Users/22377/Desktop/JiangZhouyu_hw1-main/bin/main.exe
.PHONY : CMakeFiles/main.dir/build

CMakeFiles/main.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles\main.dir\cmake_clean.cmake
.PHONY : CMakeFiles/main.dir/clean

CMakeFiles/main.dir/depend:
	$(CMAKE_COMMAND) -E cmake_depends "MinGW Makefiles" C:\Users\22377\Desktop\JiangZhouyu_hw1-main C:\Users\22377\Desktop\JiangZhouyu_hw1-main C:\Users\22377\Desktop\JiangZhouyu_hw1-main\build C:\Users\22377\Desktop\JiangZhouyu_hw1-main\build C:\Users\22377\Desktop\JiangZhouyu_hw1-main\build\CMakeFiles\main.dir\DependInfo.cmake "--color=$(COLOR)"
.PHONY : CMakeFiles/main.dir/depend

