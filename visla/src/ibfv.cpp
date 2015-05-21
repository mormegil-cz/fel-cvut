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
 * \file ibfv.cpp
 * \author Petr Kadlec
 * \brief Image Based Flow Visualization implementation
 *
 * This file contains implementation for Visla::IBFVHelper class.
 */

#include <cstdlib>
#include "visla.h"
#include "extsup.h"

#include <GL/glext.h>

/*!
 * \class Visla::IBFVHelper
 *
 * The IBFVHelper class is a wrapper for all IBFV-related functions from the Visla library.
 *
 * \note Based on Image Based Flow Visualization by Jarke J. van Wijk,
 *       Technische Universiteit Eindhoven. More information can be
 *       found in Proceedings ACM SIGGRAPH 2002, or at http://www.win.tu.nl/vis
 */

using namespace std;
using namespace Visla;

/*!
 * \brief Checks whether a number is a power of two.
 *
 * \param x the number to be tested
 * \returns true for x = 1, 2, 4, ..., false otherwise
 * \note The function is \b NOT optimized!
 */
inline static bool isPowerOfTwo(unsigned x)
{
    if (!x) return false; // zero, not a power of two

    for (;; x >>= 1) {
        if (x & 1) return !(x & ~1);
    }
}

/*!
 * Create IBFVHelper and set up some default configuration. The constructor
 * is private, as it should be called only via Visualiser, which is a friend
 * class.\n
 * The defaults set by the constructor:\n
 * - grid size 4x4
 * - 32 patterns, sized 64x64
 * - scale 4.0
 * - alpha 0.1
 *
 * \param vis Visualiser used to create this instance
 * \param width Width of the p-buffer. It must be a power of two unless \c NV_texture_rectangle
 *              extension is supported.
 * \param height Height of the p-buffer. It must be a power of two unless \c NV_texture_rectangle
 *              extension is supported.
 * \throw \c not_supported_error if non-power-of-two p-buffer is requested, but
 *        NV_texture_rectangle extension is not supported.
 */
IBFVHelper::IBFVHelper(Visualiser *vis, int width, int height) :
                pbufftex(0), patTextures(0), data_x(0), data_y(0),
                framenum(0), numpats(0), scale(0.0f), alpha(0.0f),
                xcount(0), ycount(0), umax(0.0f), vmax(0.0f),
                texTarget(0), texUSize(0.0f), texVSize(0.0f),
                texwidth(width), texheight(height), pbuffer(width, height, 0)
{
    // check capabilities
    if (isExtensionSupported("GL_NV_texture_rectangle")) {
        texTarget = GL_TEXTURE_RECTANGLE_NV;
        texUSize = texwidth;
        texVSize = texheight;
    } else {
        texTarget = GL_TEXTURE_2D;
        texUSize = texVSize = 1.0f;
        if (!isPowerOfTwo(width) || !isPowerOfTwo(height))
            throw not_supported_error("Non-power-of-two textures are not supported");
    }

    // get a name for the p-buffer texture
    glGenTextures(1, &pbufftex);

    // create and init the p-buffer
    pbuffer.initialize(true);
    pbuffer.makeCurrent();

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glTranslatef(-1.0, -1.0, 0.0);
    glScalef(2.0f/width, 2.0f/height, 1.0);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

    glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
    glShadeModel(GL_FLAT);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    glClear(GL_COLOR_BUFFER_BIT);

    pbuffer.restoreTarget();

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

    // setup other parameters
    setIBFVParams(64, 32, 4.0f, 0.1f);
    setGridParams(4, 4);
}

/*!
 * Destroys the IBFVHelper instance and frees all memory and resources allocated
 * by it.
 */
IBFVHelper::~IBFVHelper()
{
    delete[] data_x;
    data_x = 0;
    delete[] data_y;
    data_y = 0;

    glDeleteTextures(1, &pbufftex);
    pbufftex = 0;
    glDeleteTextures(numpats, patTextures);
    delete[] patTextures;
    patTextures = 0;
    numpats = 0;
}

/*!
 * Sets size of the vector field grid.
 *
 * \param xcount Number of grid points on the x axis.
 * \param ycount Number of grid points on the y axis.
 */
void IBFVHelper::setGridParams(int xcount, int ycount)
{
    if (this->xcount == xcount && this->ycount == ycount) return;

    delete[] data_x;
    data_x = 0;
    delete[] data_y;
    data_y = 0;

    this->xcount = xcount;
    this->ycount = ycount;

    data_len = xcount * ycount;
    data_x = new float[data_len];
    data_y = new float[data_len];
}

/*!
 * Sets visualization parameters for the IBFV method.
 *
 * \param patsize Size of the pattern texture (width=height). \b MUST be a power of two.
 * \param numpats Number of patterns used.
 * \param scale Scale parameter
 * \param alpha Alpha parameter for the pattern blending
 */
void IBFVHelper::setIBFVParams(int patsize, int numpats, float scale, float alpha)
{
    if (this->patsize == patsize && this->numpats == numpats &&
        this->scale == scale && this->alpha == alpha) return;

    assert(isPowerOfTwo(patsize) && numpats > 0 && scale > 0);

    // generate/delete texture names according to the requested number of patterns
    if (this->numpats > numpats) {
        // we are removing patterns
        GLuint *tempList = new GLuint[numpats];
        glDeleteTextures(this->numpats - numpats, patTextures + numpats);
        memcpy(tempList, patTextures, sizeof(GLuint) * numpats);
        delete[] patTextures;
        patTextures = tempList;
    } else {
        // we are adding patterns
        GLuint *tempList = new GLuint[numpats];
        if (this->numpats)
            memcpy(tempList, patTextures, sizeof(GLuint) * this->numpats);
        delete[] patTextures;
        patTextures = tempList;
        glGenTextures(numpats - this->numpats, patTextures + this->numpats);
    }

    this->scale = scale;
    this->alpha = alpha;

    this->umax = texwidth / (scale * patsize);
    this->vmax = texheight / (scale * patsize);

    // create/update the patterns
    if (this->patsize != patsize) this->numpats = 0;
    if (this->numpats == numpats) return;

    this->patsize = patsize;

    int texsize = patsize * patsize;
    unsigned char lut[256];
    unsigned char *phase = new unsigned char[texsize];
    GLubyte *pat = new GLubyte[texsize * 3];
    int i;

    //! \todo Choice of functions
    memset(lut, 0, 128);
    memset(lut + 128, 255, 128);

    unsigned char *pp = phase;
    for (i = 0; i < texsize; i++)
        *pp++ = (unsigned char)(rand());

    for (i = 0; i < numpats; i++) {
        int t = i*256/numpats;
        pp = phase;
        GLubyte *pt = pat;
        for (int j = 0; j < texsize; j++, pt += 3, pp++) {
            pt[0] = pt[1] = pt[2] = lut[(t + *pp) % 256];
        }
        glBindTexture(GL_TEXTURE_2D, patTextures[i]);
        glTexImage2D(GL_TEXTURE_2D, 0, 3, patsize, patsize, 0, GL_RGB, GL_UNSIGNED_BYTE, pat);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    }

    delete[] pat;
    delete[] phase;

    this->numpats = numpats;
}

/*!
 */
void IBFVHelper::beginGrid()
{
    rem_data = data_len;
    curr_px = data_x;
    curr_py = data_y;
}

/*!
 * \param x X component of the vector in the current grid point
 * \param y Y component of the vector in the current grid point
 */
void IBFVHelper::gridValue(float x, float y)
{
    assert(rem_data > 0);
    *curr_px++ = x;
    *curr_py++ = y;
    rem_data--;
}

/*!
 * \param doFrame Should IBFVHelper call doFrame() automatically?
 */
void IBFVHelper::endGrid(bool doFrame)
{
    assert(rem_data == 0);
    rem_data = -1;
    if (doFrame) this->doFrame();
}

/*!
 * \brief Helper function for computing differences.
 *
 * \param x Current x coordinate
 * \param y Current y coordinate
 * \param dx Value of the displacement vector at [x,y]
 * \param dy Value of the displacement vector at [x,y]
 * \param dmax Maximum displacement
 * \param px Output for displaced x coordinate
 * \param py Output for displaced y coordinate
 */
inline static void getDP(float x, float y, float dx, float dy, float dmax, float &px, float &py)
{
    float r = dx*dx + dy*dy;
    if (r > dmax*dmax) {
        float q = dmax / sqrt(r);
        dx *= q;
        dy *= q;
    }
    px = x + dx;
    py = y + dy;
}

/*!
 * Computes the current frame by moving the last frame and adding the current pattern in.
 */
void IBFVHelper::doFrame()
{
    assert(rem_data == -1);      // We are AFTER beginGrid() + endGrid() -- see endGrid()

    const float *xd1 = data_x;
    const float *xd2 = data_x + xcount;
    const float *yd1 = data_y;
    const float *yd2 = data_y + xcount;

    float cellx = (float)texwidth / (xcount - 1);
    float celly = (float)texheight / (ycount - 1);
    float du = texUSize / (xcount - 1);
    float dv = texVSize / (ycount - 1);

    framenum = (framenum + 1) % numpats;

    beginDrawToBuffer();
    
    glColor3f(1.0,1.0,1.0);

    // move the current frame
    glBindTexture(texTarget, pbufftex);
    glEnable(texTarget);
    glTexParameteri(texTarget, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(texTarget, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    for (int y = 1; y < ycount; y++) {
        float yc2 = celly * y;
        float yc1 = yc2 - celly;
        float v2 = dv * y;
        float v1 = v2 - dv;

        glBegin(GL_QUAD_STRIP);

        for (int x = 0; x < xcount; x++) {
            float xc = cellx * x;
            float u = du * x;
            float px, py;

            glTexCoord2f(u, v2);
            getDP(xc, yc2, *xd2, *yd2, scale, px, py);
            glVertex2f(px, py);

            glTexCoord2f(u, v1);
            getDP(xc, yc1, *xd1, *yd1, scale, px, py);
            glVertex2f(px, py);

            xd1++;
            yd1++;
            xd2++;
            yd2++;
        }
        glEnd();
    }
    glBindTexture(texTarget, 0);
    glDisable(texTarget);

    // blend with the noise pattern texture
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, patTextures[framenum]);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glColor4f(1.0f, 1.0f, 1.0f, alpha);
    glBegin(GL_QUAD_STRIP);
        glTexCoord2f(0.0,  vmax); glVertex2f(0,         texheight);
        glTexCoord2f(0.0,  0.0);  glVertex2f(0,         0);
        glTexCoord2f(umax, vmax); glVertex2f(texwidth,  texheight);
        glTexCoord2f(umax, 0.0);  glVertex2f(texwidth,  0);
    glEnd();
    glDisable(GL_BLEND);
    glBindTexture(GL_TEXTURE_2D, 0);

    endDrawToBuffer();
}

/*!
 * \param corners Coordinates specifying where the should be drawn
 * \param alpha Alpha value for the drawn frame (default 1.0)
 * \note Dimensions of the drawn frame (specified by \c corners) should match
 *       the p-buffer dimensions (specified during construction). If they do not,
 *       the visual quality may suffer.
 */
void IBFVHelper::draw(const TVector3 corners[2], float alpha)
{
    glBindTexture(texTarget, pbufftex);
    glEnable(texTarget);
    glColor4f(1.0f, 1.0f, 1.0f, alpha);
    glBegin(GL_QUADS);
        glTexCoord2f(0.0f,     0.0f);       glVertex2fv(corners[0]);
        glTexCoord2f(texUSize, 0.0f);       glVertex2f(corners[1].x(), corners[0].y());
        glTexCoord2f(texUSize, texVSize);   glVertex2fv(corners[1]);
        glTexCoord2f(0.0f,     texVSize);   glVertex2f(corners[0].x(), corners[1].y());
    glEnd();
    glBindTexture(texTarget, 0);
    glDisable(texTarget);
}

#ifdef SUPPORT_LEGACY_2D
/*!
 * \overload void IBFVHelper::draw(const TVector3 corners[2])
 */
void IBFVHelper::draw(const ::Tvector corners[2], float alpha)
{
    glBindTexture(texTarget, pbufftex);
    glEnable(texTarget);
    glColor4f(1.0f, 1.0f, 1.0f, alpha);
    glBegin(GL_QUADS);
        glTexCoord2f(0.0f,     0.0f);       glVertex2f(corners[0].x, corners[0].y);
        glTexCoord2f(texUSize, 0.0f);       glVertex2f(corners[1].x, corners[0].y);
        glTexCoord2f(texUSize, texVSize);   glVertex2f(corners[1].x, corners[1].y);
        glTexCoord2f(0.0f,     texVSize);   glVertex2f(corners[0].x, corners[1].y);
    glEnd();
    glBindTexture(texTarget, 0);
    glDisable(texTarget);
}
#endif // SUPPORT_LEGACY_2D

/*!
 * After this function, all OpenGL drawing proceeds into the p-buffer.
 * \warning Every call to beginDrawToBuffer() must be matched with a
 *          call to endDrawToBuffer().
 * \warning Calls to beginDrawToBuffer() / endDrawToBuffer() may not be nested.
 * \warning No IBFV methods other than endDrawToBuffer() may be called after this
 *          function.
 * \warning You have to restore OpenGL state to the original value before calling
 *          endDrawToBuffer().
 * \see endDrawToBuffer()
 */
void IBFVHelper::beginDrawToBuffer()
{
    pbuffer.makeCurrent();
}

/*!
 * After this function, all OpenGL drawing proceeds into the original target.
 * \see beginDrawToBuffer()
 */
void IBFVHelper::endDrawToBuffer()
{
    // copy current frame to the texture (for transfer to framebuffer and move in the next frame)
    glEnable(texTarget);
    glBindTexture(texTarget, pbufftex);
    glTexParameteri(texTarget, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(texTarget, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

    glCopyTexImage2D(texTarget, 0, GL_RGB, 0, 0, texwidth, texheight, 0);
    glBindTexture(texTarget, 0);
    glDisable(texTarget);

    pbuffer.restoreTarget();
}
