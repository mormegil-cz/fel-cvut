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


#ifndef __PBUFFER_H_
#define __PBUFFER_H_

/*!
 * \file pbuffer.h
 * \author Petr Kadlec
 * \brief p-buffer support header file.
 *
 * This file contains interface to p-buffer support wrapper class.
 */

#include <exception>

#ifdef _WIN32
  #ifndef WIN32_LEAN_AND_MEAN
    #define WIN32_LEAN_AND_MEAN 1
  #endif
  #include <windows.h>
  #include <GL/gl.h>
  #include <GL/wglext.h>
#else
  #error "p-buffer support not yet written for other platforms than Win32"
#endif

namespace Visla {
    class pbuffer_error;
    class PBuffer;
};
using namespace std;

/*!
 * \brief Exception signalling that p-buffer-related operation failed.
 *
 * pbuffer_error will be thrown when a p-buffer creation or other operation failed.
 * The error message may contain further details.
 */
class Visla::pbuffer_error : public exception {
public:
    /*!
     * \brief Create the exception with the given error message.
     *
     * \param what_arg A message describing the problem.
     */
    explicit pbuffer_error(const char* what_arg) : exception(what_arg) { };
};

/*!
 * \brief A wrapper utility class for OpenGL p-buffer support.
 *
 * \todo Implementation for other platforms (using GLX/...)
 */
class Visla::PBuffer {
private:
    HDC         origDC;         //!< Original DC, saved in initialize(), restored by restoreTarget()
    HGLRC       origCtx;        //!< Original GL context, saved in initialize(), restored by restoreTarget()
    HDC         buffDC;         //!< Buffer's DC
    HGLRC       buffCtx;        //!< Buffer's GL context
    HPBUFFERARB buffer;         //!< p-buffer
    unsigned    mode;           //!< Mode for the p-buffer, combination of various GLUT_xxx flags (see PBuffer())
    int         width;          //!< Width of the p-buffer [pixels]
    int         height;         //!< Height of the p-buffer [pixels]
public:
    PBuffer(int width, int height, unsigned mode);      //!< Create the instance, the p-buffer is not yet created!
    ~PBuffer();                                         //!< Destroy the p-buffer and the instance.

    void initialize(bool shared = false);               //!< Initialize and create the p-buffer
    void release();                                     //!< Destroy the p-buffer
    void handleModeSwitch();                            //!< Check if the p-buffer is not lost and possibly recreate it.
    void makeCurrent();                                 //!< Make the p-buffer the current rendering target
    void restoreTarget();                               //!< Restore the rendering target set during initialize()

    inline unsigned getMode() const { return mode; }    //!< Get the mode flags of the p-buffer
    inline int getWidth() const { return width; }       //!< Get the width of the p-buffer
    inline int getHeight() const { return height; }     //!< Get the height of the p-buffer
};

#endif /* ifndef __PBUFFER_H_ */
