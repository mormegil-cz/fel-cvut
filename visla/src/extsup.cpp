/*  Visla -- Visualisation Library for Accelerators
 *
 *  Copyright (C) 2003  Petr Kadlec <mormegil@centrum.cz>
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */


/*!
 * \file extsup.cpp
 * \author Petr Kadlec
 * \brief GL Extensions support utilities
 *
 * This file contains some utilities for dealing with GL extensions.
 */

#ifdef _WIN32
  #ifndef WIN32_LEAN_AND_MEAN
    #define WIN32_LEAN_AND_MEAN 1
  #endif
  #include <windows.h>
  #include <GL/gl.h>
  #include <GL/wglext.h>
#else
  #include <GL/gl.h>
#endif

#include "extsup.h"

/*!
 * This utility function tests whether the specified extension is listed in the
 * list of supported extensions.
 * \param extension The name of the extension that is to be queried for support.
 * \param extlist List of supported extensions.
 * \return A bool specifying whether the extension is in the list.
 */
static bool extInExtList(const char *extension, const GLubyte *extlist)
{
    const GLubyte *start;
    GLubyte *where, *terminator;

    /* Extension names should not have spaces. */
    where = (GLubyte *) strchr(extension, ' ');
    if (where || *extension == '\0')
	throw invalid_argument("Invalid extension name");

    /* No extension list, should not happen (except some strange OpenGL error...), just for sure */
    if (!extlist) return false;

    /* It takes a bit of care to be fool-proof about parsing the
       OpenGL extensions string. Don't be fooled by sub-strings, etc. */
    start = extlist;
    while((where = (GLubyte *) strstr((const char *) start, extension)) != 0) {
	terminator = where + strlen(extension);
	if (where == start || *(where - 1) == ' ')
	    if (*terminator == ' ' || *terminator == '\0') return true;
        start = terminator;
    }
    return false;
}

/*!
 * This utility function tests whether the specified extension is supported on this system.
 * \param extension The name of the extension that is to be queried for support.
 * \return A bool specifying whether the extension is supported.
 */
bool Visla::isExtensionSupported(const char *extension)
{
    return extInExtList(extension, glGetString(GL_EXTENSIONS));
}

#ifdef _WIN32

static PFNWGLGETEXTENSIONSSTRINGARBPROC wglGetExtensionsStringARB = 0;
static bool noWGL = false;

/*!
 * This utility function tests whether the specified WGL extension is supported on this system.
 * \param extension The name of the extension that is to be queried for support.
 * \return A bool specifying whether the extension is supported.
 */
bool Visla::isWGLExtensionSupported(const char *extension)
{
    if (!wglGetExtensionsStringARB && !noWGL) {
        wglGetExtensionsStringARB = (PFNWGLGETEXTENSIONSSTRINGARBPROC)wglGetProcAddress("wglGetExtensionsStringARB");
        if (!wglGetExtensionsStringARB) noWGL = true;
    }
    return !noWGL && extInExtList(extension, (const GLubyte*)wglGetExtensionsStringARB(wglGetCurrentDC()));
}

#endif /* _WIN32 */
