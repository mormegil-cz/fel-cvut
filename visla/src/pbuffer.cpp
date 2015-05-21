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
 * \file pbuffer.cpp
 * \author Petr Kadlec
 * \brief P-buffer support wrapper class.
 *
 * This file contains implementation of P-Buffer support wrapper class.
 */

#include <cassert>
#include <cstdlib>
#include <GL/glut.h>
#include <GL/glext.h>

#include "extsup.h"
#include "pbuffer.h"

using namespace std;
using namespace Visla;

/*!
 * \def MAX_ATTRIBS
 * \brief Maximum number of p-buffer attributes supported.
 */
#define MAX_ATTRIBS 32

/*!
 * \def MAX_PFORMATS
 * \brief Maximum number of p-buffer formats returned by the WGL system.
 */
#define MAX_PFORMATS 256

/*!
 * Entry points to the p-buffer ARB extension.
 */

static PFNWGLCHOOSEPIXELFORMATARBPROC wglChoosePixelFormatARB = 0;
static PFNWGLCREATEPBUFFERARBPROC wglCreatePbufferARB = 0;
static PFNWGLGETPBUFFERDCARBPROC wglGetPbufferDCARB = 0;
static PFNWGLQUERYPBUFFERARBPROC wglQueryPbufferARB = 0;
static PFNWGLRELEASEPBUFFERDCARBPROC wglReleasePbufferDCARB = 0;
static PFNWGLDESTROYPBUFFERARBPROC wglDestroyPbufferARB = 0;

//! Has the extensions already been checked and found?
static bool checkedExt = false;

/*!
 * Check if the required extensions are supported and retrieve
 * the entry points.
 */
inline static void initExt()
{
    if (!checkedExt) {
        bool hasExt = isWGLExtensionSupported("WGL_ARB_pbuffer");
        if (!hasExt) throw not_supported_error("WGL_ARB_pbuffer required, but not supported");

        wglChoosePixelFormatARB = (PFNWGLCHOOSEPIXELFORMATARBPROC)wglGetProcAddress("wglChoosePixelFormatARB");
        wglCreatePbufferARB = (PFNWGLCREATEPBUFFERARBPROC)wglGetProcAddress("wglCreatePbufferARB");
        wglGetPbufferDCARB = (PFNWGLGETPBUFFERDCARBPROC)wglGetProcAddress("wglGetPbufferDCARB");
        wglQueryPbufferARB = (PFNWGLQUERYPBUFFERARBPROC)wglGetProcAddress("wglQueryPbufferARB");
        wglReleasePbufferDCARB = (PFNWGLRELEASEPBUFFERDCARBPROC)wglGetProcAddress("wglReleasePbufferDCARB");
        wglDestroyPbufferARB = (PFNWGLDESTROYPBUFFERARBPROC)wglGetProcAddress("wglDestroyPbufferARB");

        if (!wglChoosePixelFormatARB || !wglCreatePbufferARB ||
            !wglGetPbufferDCARB || !wglQueryPbufferARB ||
            !wglReleasePbufferDCARB || !wglDestroyPbufferARB)
            throw not_supported_error("WGL_ARB_pbuffer functions not found");
        
        checkedExt = true;
    }
}

/*!
 * This call does not create the p-buffer. It only prepares the
 * instance.
 *
 * \param width Requested width of the p-buffer [pixels]
 * \param height Requested height of the p-buffer [pixels]
 * \param mode Mode flags for the p-buffer (bit combination of any of GLUT_INDEX,
 *             GLUT_DOUBLE, GLUT_DEPTH, GLUT_STENCIL, GLUT_ACCUM).
 * \see initialize
 */
PBuffer::PBuffer(int width, int height, unsigned mode) :
    width(width), height(height), mode(mode), origDC(0), origCtx(0),
    buffDC(0), buffCtx(0), buffer(0)
{
}

/*!
 * \warning This method may not be called when the p-buffer is set as current!
 */
PBuffer::~PBuffer()
{
    release();
}

/*!
 * Create the p-buffer. This method may be called only after a window has been created.
 * If the p-buffer is already created, this method releases it and creates a new one.
 *
 * \param shared Should the p-buffer share display-list space with the main context?
 * \warning This method may not be called when the p-buffer is set as current!
 * \throw pbuffer_error if the p-buffer can not be created
 */
void PBuffer::initialize(bool shared)
{
    initExt();

    release();

    origDC = wglGetCurrentDC();
    origCtx = wglGetCurrentContext();

    // Query for a suitable pixel format based on the specified mode.
    int   attributes[2*MAX_ATTRIBS];
    int   nattribs = 0;

    attributes[nattribs++] = WGL_DRAW_TO_PBUFFER_ARB;
    attributes[nattribs++] = true;

    if (mode & GLUT_INDEX) {
        attributes[nattribs++] = WGL_PIXEL_TYPE_ARB;
        attributes[nattribs++] = WGL_TYPE_COLORINDEX_ARB;
    } else {
        attributes[nattribs++] = WGL_PIXEL_TYPE_ARB;
        attributes[nattribs++] = WGL_TYPE_RGBA_ARB;
    }

    if (mode & GLUT_DOUBLE) {
        attributes[nattribs++] = WGL_DOUBLE_BUFFER_ARB;
        attributes[nattribs++] = true;
    }

    if (mode & GLUT_DEPTH) {
        attributes[nattribs++] = WGL_DEPTH_BITS_ARB;
        attributes[nattribs++] = 1;
    }

    if (mode & GLUT_STENCIL) {
        attributes[nattribs++] = WGL_STENCIL_BITS_ARB;
        attributes[nattribs++] = 1;
    }

    if (mode & GLUT_ACCUM) {
        attributes[nattribs++] = WGL_ACCUM_BITS_ARB;
        attributes[nattribs++] = 1;
    }

    attributes[nattribs++] = WGL_SUPPORT_OPENGL_ARB;
    attributes[nattribs++] = true;

    attributes[nattribs] = 0;

    int format;
    int pformat[MAX_PFORMATS];
    unsigned int nformats;
    float fattribs[1] = { 0 };

    if (!wglChoosePixelFormatARB(origDC, attributes, fattribs, MAX_PFORMATS, pformat, &nformats)) {
        throw pbuffer_error("p-buffer creation error (wglChoosePixelFormatARB() failed)");
    }
    if (nformats <= 0) {
        throw pbuffer_error("p-buffer creation error (no suitable pixel format found)");
    }
    format = pformat[0];

    // Create the p-buffer.
    attributes[0] = 0;
    buffer = wglCreatePbufferARB(origDC, format, width, height, attributes);
    if (!buffer) {
        throw pbuffer_error("p-buffer creation error (wglCreatePbufferARB() failed)");
    }

    // Get the device context.
    buffDC = wglGetPbufferDCARB(buffer);
    if (!buffDC) {
        throw pbuffer_error("p-buffer creation error (wglGetPbufferDCARB() failed)");
    }

    // Create a gl context for the p-buffer.
    buffCtx = wglCreateContext(buffDC);
    if (!buffCtx) {
        throw pbuffer_error("p-buffer creation error (wglCreateContext() failed)");
    }

    if (shared) {
        if (!wglShareLists(origCtx, buffCtx)) {
            throw pbuffer_error("Error sharing display lists");
        }
    }

    // Determine the actual width and height we were able to create.
    wglQueryPbufferARB(buffer, WGL_PBUFFER_WIDTH_ARB, &width);
    wglQueryPbufferARB(buffer, WGL_PBUFFER_HEIGHT_ARB, &height);
}

/*!
 * This method deletes the p-buffer, which restores the state to the state before the call
 * to initialize().
 * \warning This method may not be called when the p-buffer is set as current!
 */
void PBuffer::release()
{
    if (buffer) {
        wglDeleteContext(buffCtx);
        wglReleasePbufferDCARB(buffer, buffDC);
        wglDestroyPbufferARB(buffer);
    }
    buffCtx = 0;
    buffDC = 0;
    buffer = 0;
}

/*!
 * Check if the p-buffer is not lost. If yes, release it and recreate it.
 */
void PBuffer::handleModeSwitch()
{
    assert(buffer);

    int lost = 0;
    wglQueryPbufferARB(buffer, WGL_PBUFFER_LOST_ARB, &lost);
    if (lost) {
        release();
        initialize();
    }
}

/*!
 * Make the p-buffer the current rendering target.
 * \throw pbuffer_error in case of an error
 */
void PBuffer::makeCurrent()
{
    assert(buffDC && buffCtx && buffer);
    if (!wglMakeCurrent(buffDC, buffCtx)) {
        throw pbuffer_error("Unable to make the p-buffer current");
    }
}

/*!
 * Set the original render target (set during initialize()) current.
 * \note All errors are ignored
 */
void PBuffer::restoreTarget()
{
    if (origDC && origCtx) {
        wglMakeCurrent(origDC, origCtx);
    }
}
