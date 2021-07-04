##  CMakeFile for PG2PLplot
##  AF, 2010-11-12
##  
##  Copyright 2010-2015 AstroFloyd - astrofloyd.org
##   
##  This file is part of the PG2PLplot package, 
##  see: http://pg2plplot.sf.net/
##   
##  This is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published
##  by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
##  
##  This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
##  
##  You should have received a copy of the GNU General Public License along with this code.  If not, see 
##  <http://www.gnu.org/licenses/>.
##
##
##  To compile, from the directory that contains this file, do:
##    $ mkdir build; cd build
##    $ cmake ..
##    $ make
##    $ sudo make install


cmake_minimum_required( VERSION 2.6 FATAL_ERROR )

# Project name and language:
project( PG2PLplot Fortran )


# Search in the CMake/ directory for CMake modules:
list( APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/CMake )

# Various compile/optimisation options that we may want to enable:
include( SetCompileOptions )


# Place the products in their directories:
get_filename_component( Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME )
if( COMPILER_SPECIFIC_LIBS )
  set( MODULE_DIRECTORY "${CMAKE_SOURCE_DIR}/usr/include/pg2plplot/${Fortran_COMPILER_NAME}" )
else( COMPILER_SPECIFIC_LIBS )
  set( MODULE_DIRECTORY "${CMAKE_SOURCE_DIR}/usr/include/pg2plplot" )
endif( COMPILER_SPECIFIC_LIBS )

set( LIBRARY_OUTPUT_PATH "${CMAKE_SOURCE_DIR}/usr/lib${LIB_SUFFIX}" )


# Find dependencies:
find_package( PLplot REQUIRED )
set( INCLUDE_FLAGS "-I${PLplot_INCLUDES}" )  # will be transferred to CompilerFlags


# Set source files:
include( FileList )

# Set FORTRAN compiler flags:
include( CompilerFlags_Fortran )


# Create libraries:
if( CREATE_SHAREDLIB )
  add_library( "pg2plplot_shared"  SHARED  ${SRC_FILES} )
  if( COMPILER_SPECIFIC_LIBS )
    set_target_properties( pg2plplot_shared PROPERTIES OUTPUT_NAME "pg2plplot_${Fortran_COMPILER_NAME}" )
  else( COMPILER_SPECIFIC_LIBS )
    set_target_properties( pg2plplot_shared PROPERTIES OUTPUT_NAME "pg2plplot" )
  endif( COMPILER_SPECIFIC_LIBS )
  set_target_properties( pg2plplot_shared PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY} SOVERSION 0)
  execute_process(COMMAND pkg-config --libs plplot-fortran OUTPUT_VARIABLE PKG_LINK OUTPUT_STRIP_TRAILING_WHITESPACE)
  target_link_libraries( pg2plplot_shared ${PKG_LINK})
endif( CREATE_SHAREDLIB )

if( CREATE_STATICLIB )
  add_library( "pg2plplot_static"  STATIC  ${SRC_FILES} )
  if( COMPILER_SPECIFIC_LIBS )
    set_target_properties( pg2plplot_static PROPERTIES OUTPUT_NAME "pg2plplot_${Fortran_COMPILER_NAME}" )
  else( COMPILER_SPECIFIC_LIBS )
    set_target_properties( pg2plplot_static PROPERTIES OUTPUT_NAME "pg2plplot" )
  endif( COMPILER_SPECIFIC_LIBS )
  set_target_properties( pg2plplot_static PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY} )
  execute_process(COMMAND pkg-config --cflags plplot-fortran OUTPUT_VARIABLE PKG_FLAGS OUTPUT_STRIP_TRAILING_WHITESPACE)
endif( CREATE_STATICLIB )


# Install to destination:
#   use DIRECTORY rather than TARGETS to allow include/<fortrancompiler>/
install( DIRECTORY usr/ DESTINATION ${CMAKE_INSTALL_PREFIX} )
install( FILES src/rgb.txt DESTINATION ${CMAKE_INSTALL_PREFIX}/share/pg2plplot)
