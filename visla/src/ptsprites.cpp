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
 * \file ptsprites.cpp
 * \author Petr Kadlec
 * \brief Point Sprites support.
 *
 * This file contains implementation of Point Sprite support.
 */

// --- includes - OpenGL
#ifdef _WIN32
  #ifndef WIN32_LEAN_AND_MEAN
    #define WIN32_LEAN_AND_MEAN 1
  #endif
  #include <windows.h>
  #include <GL/gl.h>
#else /* GLX */
  #include <GL/gl.h>
#endif

#include <GL/glext.h>

// --- includes - Visla
#include "extsup.h"
#include "ptsprites.h"

// --- preprocessor defines

/*! \def SUPPORT_ARB
 * \brief Should we support ARB_point_sprite ?
 *
 * If \c SUPPORT_ARB is defined, this library supports the ARB_point_sprite extension.
 */
#undef SUPPORT_ARB
/*! \def SUPPORT_NV
 * \brief Should we support NV_point_sprite ?
 *
 * If \c SUPPORT_NV is defined, this library supports the NV_point_sprite extension.
 */
#define SUPPORT_NV

// --- namespace declaration
namespace Visla {
    static GLenum GL_POINT_SPRITE_xxx;
    static GLenum GL_COORD_REPLACE_xxx;

    void initPointSprites();
    bool pointSpritesSupported();
    void enablePointSprites(const int texUnit);
    void disablePointSprites();
};

using namespace Visla;

// --- implementation

#if (defined(SUPPORT_ARB) && !defined(GL_ARB_point_sprite))
#error "ARB_point_sprite is not supported by the GL/glext.h header file."\
       "Get a newer glext.h file or #undef SUPPORT_ARB."
#endif
#if (defined(SUPPORT_NV) && !defined(GL_NV_point_sprite))
#error "NV_point_sprite is not supported by the GL/glext.h header file."\
       "Get a newer glext.h file or #undef SUPPORT_NV."
#endif

bool Visla::pointSpritesAreSupported = false;

/*!
 * \note This function \b must be called before any other point sprite related
 *       function.
 * \note The function may be called more than once without problem.
 */
void Visla::initPointSprites()
{
#ifdef SUPPORT_ARB
    if (isExtensionSupported("GL_ARB_point_sprite")) {
        GL_POINT_SPRITE_xxx = GL_POINT_SPRITE_ARB;
        GL_COORD_REPLACE_xxx = GL_COORD_REPLACE_ARB;
        pointSpritesAreSupported = true;
        return;
    }
#endif
#ifdef SUPPORT_NV
    if (isExtensionSupported("GL_NV_point_sprite")) {
        GL_POINT_SPRITE_xxx = GL_POINT_SPRITE_NV;
        GL_COORD_REPLACE_xxx = GL_COORD_REPLACE_NV;
        pointSpritesAreSupported = true;
        return;
    }
#endif

    pointSpritesAreSupported = false;
}

/*!
 * \param texUnit Texture unit on which should the point sprites rendering be enabled.
 *                This parameter is ignored in this version.
 * \throws not_supported_error if no known extension for point sprites is supported on
 *                             the system. You may use pointSpritesSupported() to check
 *                             the support before calling this function.
 */
void Visla::enablePointSprites(const int texUnit)
{
    if (!pointSpritesAreSupported) {
        throw not_supported_error("No known extension for point sprites is supported");
    }
    glEnable(GL_POINT_SPRITE_xxx);
    glTexEnvi(GL_POINT_SPRITE_xxx, GL_COORD_REPLACE_xxx, GL_TRUE);
}

/*!
 * \note If point sprites are not supported, the function does nothing (i.e. it does not
 *       even signal an error).
 */
void Visla::disablePointSprites()
{
    if (pointSpritesAreSupported)
        glDisable(GL_POINT_SPRITE_xxx);
}
