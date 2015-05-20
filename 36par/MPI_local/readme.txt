MPI_Local
*********

Overview
--------
MPI_Local is a library emulating some basic MPI functions on a single machine. It is meant to ease the debugging of MPI programs. (Because debugging of truly parallel programs is not an easy task.)

Using the library
-----------------
When you want to use the MPI_Local library instead of the classical MPI library, you have to do two things: 1. Change the include path, or otherwise tell the compiler to use the mpi.h from MPI_Local instead of the standard mpi.h file. 2. Include the MPI_local.c file in the project, i.e. compile it and link it with the program.
There should be no need to change your program to use the library. If you want to use a feature of MPI that is not provided by MPI_Local, I am sorry. (You can of course add it to the library...)

Portability
-----------
The library should compile on any ANSI C compiler. The library is Win32 only. Sorry.

Warnings
--------
I created the library when I had to create a program for MPI (as a school project). As I have a single computer at home, I had to create something to help...
Because of this, the library is not meant to be precise, correct, ..., emulator of MPI. I used it and it worked for me. That is enough for me. If you find some bugs, I will appreciate if you let me know, but you will probably have to correct them yourself...
Once again I state: THIS LIBRARY IS NOT A STANDARDS-COMPLYING IMPLEMENTATION OF MPI!

Copyright and disclaimer
------------------------
MPI_Local is Copyright (C) 2002 by Petr Kadlec <mormegil@centrum.cz>.

This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option) any later version.
This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

The names of actual companies and products mentioned herein may be the trademarks of their respective owners.
