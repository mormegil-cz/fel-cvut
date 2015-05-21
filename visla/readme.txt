VISLA -- Visualization Library for Accelerators
(C) 2003-2004 Petr Kadlec <mormegil@centrum.cz>
***********************************************

This is a library that has been developed as a part of my Master Thesis at the Czech Technical University in Prague. It contains some functions for scientific visualization using some features of modern graphics hardware.
It is licensed under Lesser General Public License (LGPL). See license.txt for details.

I am afraid that the library is not in a very good shape. If you want to use/develop it, you are welcome (although it would be fine to let me know about it). But I do not have much time to maintain it, so do not expect much support from me (you may try it, though).

The library should be more or less portable (OpenGL, ANSI C++). There are some catches, like the p-buffer, which is implemented only on Windows (using WGL extension), the support for GLX is missing there.
To use the library, you will need the MGL library, which contains a wrapper for GLUT, implementing some texture loading functions, etc. At the moment, the MGL library can be found at http://www.cgg.cvut.cz/~xgayer/mgl/

The library is documented (to some extent) using the Doxygen style. There some also some basic usage examples in the "examples" directory, which should be the primary source for inspiration how should the programs using Visla be written.
