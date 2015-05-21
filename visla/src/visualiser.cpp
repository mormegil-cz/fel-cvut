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
 * \file visualiser.cpp
 * \author Petr Kadlec
 * \brief VISualization Library for Accelerators implementation
 *
 * This file contains implementation for Visla::Visualiser class.
 */

#include <cassert>
#include <fstream>
#include <iostream>
#include <sys/stat.h>

#include "extsup.h"
#include "visla.h"
#include "vertprog.h"
#include "fragprog.h"
#include "vertarray.h"
#include "ptsprites.h"

#include <GL/glext.h>

/*!
 * \class Visla::Visualiser
 * The Visualiser class is a wrapper for all visualising functions from the Visla library.
 */

#ifndef USE_MGL
#error "USE_MGL is not defined, although no other utility library is supported yet."
#endif

using namespace std;
using namespace Visla;

/*!
 * \see VERSION_TAG
 */
const char *Visla::VERSION = "0.1-dev-20040124";

/*!
 */
const char *Visla::BUILD_DATE = __DATE__ " " __TIME__;

/*!
 * The function determines the current system's support for OpenGL
 * extensions that are used by Visualiser and returns the according flags.
 * \return A bitwise OR of SupportFlags values specifying what features are supported.
 * \see Visualiser::SupportFlags
 */
unsigned Visualiser::whatIsSupported()
{
    const char *OGL_version = (const char*)(glGetString(GL_VERSION));
    float ver;
    if (sscanf(OGL_version, "%f", &ver) != 1 || ver < 1.1f)
        return Visualiser::supportedNothing; // OpenGL version 1.0, at least 1.1 required

    initPointSprites();

    unsigned result = Visualiser::supportedNothing;
    if (VertexProgram::isSupported()) result |= Visualiser::supportedVertexPrograms;
    if (FragmentProgram::isSupported()) result |= Visualiser::supportedFragmentPrograms;
    if (pointSpritesSupported()) result |= Visualiser::supportedPointSprites;
    if (isExtensionSupported("GL_NV_texture_rectangle")) result |= Visualiser::supportedNPOTTextures;
    //if (VertexArray::isSupported()) result |= Visualiser::supportedExtArrays;
    return result;
}

/*!
 * The function prepends the vertex program directory before the filename.
 * If the file is not found there, a filename without the directory
 * specification is tried.
 * \param filename Name of the file with the vertex or fragment program, without path
 * \throws runtime_error when the vertex or fragment program has not been found
 * \return A new fstream, has to be delete`d after use.
 */
fstream *Visualiser::make_stream_for_prog(const char* filename)
{
    struct stat statbuf;

    // 1. seek the file in the prog_path directory
    char fname[_MAX_PATH];
    strncpy(fname, prog_path, sizeof(fname));
    fname[sizeof(fname)-1] = '\0';
    strncat(fname, filename, sizeof(fname) - strlen(fname));
    fname[sizeof(fname)-1] = '\0';
    if (stat(fname, &statbuf) == 0)
        return new fstream(fname, ios_base::in | ios_base::binary);

    // 2. try the current directory
    if (*prog_path && stat(filename, &statbuf) == 0)
        return new fstream(filename, ios_base::in | ios_base::binary);

    throw runtime_error("Vertex/fragment program not found");
}

/*!
 * \param program_path Path to where vertex and fragment programs are stored. \b MUST end with
 *                     a path separator.
 *
 * The constructor. Initializes the library. It assumes that OpenGL has already been initialized.
 * The library is \b not thread-safe and the OpenGL context that was current in the time the
 * constructor was called must be current whenever any Visla function is called.\n
 * The library is configured as follows:
 *    - no point sprite texture, no value texture
 *    - point rendering mode set to pmFlat
 *    - quad rendering mode set to qmRGB
 *    - data range [0.0;1.0]
 *    - point size 1.0
 *    - particle alpha 1.0
 *    - patch division 4
 */
Visualiser::Visualiser(const char *program_path)
{
    prog_path = 0;
    valuePaletteFunc = 0;
    pointSpriteTex = valuePaletteTex = 0;
    pointsMode = pmFlat;
    quadsMode = qmRGB;
    splineMode = smVertInterp;
    range_low = 0.0f;
    range_scaler = 1.0f;
    point_size = 1.0f;
    patchPoints = 0;

    maxParticles = gridXSize = gridYSize = 0;

    pGridData = 0;

    vpPointValue = vpPointSprite = vpSplineInt = vpFragSplineInt = 0;
    fpFragSplineInt = 0;
    dlSplinePatch = 0;
    vaSplinePatch0 = vaSplinePatch1 = 0;
    vaParticles = 0;

    particleWhiteWithAlpha = mglWhite;
    quadWhiteWithAlpha = mglWhite;

    prog_path = new char[strlen(program_path) + 1];
    strcpy(prog_path, program_path);

    initPointSprites();

    fstream *f;
    vpPointValue = VertexProgram::create();
    f = make_stream_for_prog("ptvalue.vp");
    vpPointValue->compile(*f);
    delete f;
    vpPointSprite = VertexProgram::create();
    f = make_stream_for_prog("ptsprite.vp");
    vpPointSprite->compile(*f);
    delete f;
    vpSplineInt = VertexProgram::create();
    f = make_stream_for_prog("splineint.vp");
    vpSplineInt->compile(*f);
    delete f;

    // default spline division
    this->setPatchDivision(4);

    // just to be sure
    assert(sizeof(TRGBAColor) == 4*sizeof(float) && "RGBAColor must be 4 consecutive floats!");
}

/*!
 * The destructor, finishes the library use and frees any resources it allocated.
 */
Visualiser::~Visualiser()
{
    delete[] pGridData;

    delete vaParticles;
    delete vaSplinePatch1;
    delete vaSplinePatch0;
    if (dlSplinePatch) glDeleteLists(dlSplinePatch, 1);
    delete fpFragSplineInt;
    delete vpFragSplineInt;
    delete vpSplineInt;
    delete vpPointSprite;
    delete vpPointValue;

    delete[] prog_path;
}

/*!
 * The library visualises scalar data that map to color information. The range
 * of values that map to the whole range of colors is set by this function.
 * When you try to display a value outside this range, the results are undefined.
 * \param low The lowest value that is to be displayed by graphics functions
 * \param high The highest value that is to be displayed by graphics functions
 * \warning \a low must be SMALLER than \a high, otherwise the results are undefined.
 */
void Visualiser::setDataRange(value_t low, value_t high)
{
    assert(low <= high);
    range_low = low;
    if (low == high) {
        // special case: all values are supposed to be equal to the given value
        range_scaler = 0.0f;
    } else {
        range_scaler = 1.0f / (high - low);
    }
}

/*!
 * \param func The value palette function that should be used to generate the texture
 * \param size The desired size of the generated texture (pixels)
 * \param filter OpenGL filtering mode for the texture (GL_NEAREST, GL_LINEAR, etc.)
 * \return Value palette texture generated from the function. Must be completely deleted after
 *         use (via mglDeleteTexture()), or memory leak will occur.
 */
Visualiser::valueTex_t Visualiser::genValueTex(valueFunc_p func, unsigned size, GLint filter)
{
    assert(size > 0);
    // create image
    mglImage *img = mglCreateImage(size, 1, 3);
    assert(img);
    img->type = GL_RGB;
    // fill the image with values
    value_t dx = 1.0f/size;
    unsigned char *p = img->data;
    for (unsigned x = 0; x < size; x++) {
        TRGBAColor col = func(x * dx);
        *p++ = (int)(255.0f * col.r);
        *p++ = (int)(255.0f * col.g);
        *p++ = (int)(255.0f * col.b);
    }
    // create the texture from the image
    valueTex_t tex = mglMakeTexture(img, filter);
    // delete the image
    mglDeleteImage(img);
    return tex;
}

/*!
 * \param func The value palette function that should be used to generate the texture
 * \param size The desired size of the generated texture (pixels)
 * \param filter OpenGL filtering mode for the texture (GL_NEAREST, GL_LINEAR, etc.)
 * \param equidistance Distance between two neighboring isolines (in pixels of the texture)
 * \param isoColor Color of the isolines
 * \return Value palette texture generated from the function. Must be completely deleted after
 *         use (via mglDeleteTexture()), or memory leak will occur.
 */
Visualiser::valueTex_t Visualiser::genValueTexIso(valueFunc_p func, unsigned size, GLint filter, unsigned equidistance, const TRGBAColor &isoColor)
{
    assert(size > 0 && equidistance > 0);
    // create image
    mglImage *img = mglCreateImage(size, 1, 3);
    assert(img);
    img->type = GL_RGB;
    // fill the image with values
    value_t dx = 1.0f/size;
    unsigned char *p = img->data;
    for (unsigned x = 0; x < size; x++) {
        if (x % equidistance == equidistance-1) {   // make the isolines not at 0,d,2d,..., but d-1,2d-1,...
            *p++ = (int)(255.0f * isoColor.r);
            *p++ = (int)(255.0f * isoColor.g);
            *p++ = (int)(255.0f * isoColor.b);
        } else {
            TRGBAColor col = func(x * dx);
            *p++ = (int)(255.0f * col.r);
            *p++ = (int)(255.0f * col.g);
            *p++ = (int)(255.0f * col.b);
        }
    }
    // create the texture from the image
    valueTex_t tex = mglMakeTexture(img, filter);
    // delete the image
    mglDeleteImage(img);
    return tex;
}

/*!
 * Create IBFVHelper and set up some default configuration:\n
 * - grid size 4x4
 * - 32 patterns, sized 64x64
 * - scale 4.0
 * - alpha 0.1
 *
 * \param width Width of the p-buffer. It must be a power of two unless \c NV_texture_rectangle
 *              extension is supported.
 * \param height Height of the p-buffer. It must be a power of two unless \c NV_texture_rectangle
 *              extension is supported.
 * \throw not_supported_error if non-power-of-two p-buffer is requested, but
 *        \c NV_texture_rectangle extension is not supported.
 */
IBFVHelper* Visualiser::createIFBVHelper(int width, int height)
{
    return new IBFVHelper(this, width, height);
}

/*!
 * Marks the beginning of the part of the frame in that will the Visla functions be called.
 * No Visla functions may be called outside the pair of calls to beginFrame() and endFrame().
 * \see endFrame()
 */
void Visualiser::beginFrame()
{
    glPushAttrib(GL_TEXTURE_BIT | GL_POINT_BIT | GL_ENABLE_BIT | GL_CURRENT_BIT);
    glPushClientAttrib(GL_CLIENT_VERTEX_ARRAY_BIT);
}

/*!
 * Marks the end of the part of the frame began with call to beginFrame().
 * No Visla functions may be called outside the pair of calls to beginFrame() and endFrame().
 * This function restores the OpenGL state variables to their state during beginFrame().
 * \see beginFrame()
 */
void Visualiser::endFrame()
{
    // disable all vertex programs (the specific vpXX is not important here)
    vpPointValue->disable();

    // disable all fragment programs (the specific fpXX is not important here)
    if (fpFragSplineInt) {
        fpFragSplineInt->disable();
    }

    glPopClientAttrib();
    glPopAttrib();
}

/*!
 * \param mode The rendering mode for point sprite rendering
 */
void Visualiser::setPointsMode(PointMode mode)
{
    pointsMode = mode;
}

/*!
 * \param mode The rendering mode for quad rendering
 */
void Visualiser::setQuadsMode(QuadMode mode)
{
    quadsMode = mode;
}

/*!
 * \param mode The spline interpolation mode
 * \note If this is the first time when smPerFragment is set,
 *       this call loads+compiles the required vertex+fragment programs so
 *       that the call may take some time
 */
void Visualiser::setSplineMode(SplineMode mode)
{
    splineMode = mode;
    if (mode == smPerFragment) {
        if (!vpFragSplineInt) {
            vpFragSplineInt = VertexProgram::create();
            fstream *f = make_stream_for_prog("splineint_frag.vp");
            vpFragSplineInt->compile(*f);
            delete f;
        }
        if (!fpFragSplineInt) {
            fpFragSplineInt = FragmentProgram::create();
            fstream *f = make_stream_for_prog("splineint_frag.fp");
            fpFragSplineInt->compile(*f);
            delete f;
        }
    }
}

/*!
 * \param mode The function that converts a value into its corresponding color.
 */
void Visualiser::setValueFunc(TRGBAColor (VISLACALLBACK *func)(float_t))
{
    assert(func);
    valuePaletteFunc = func;
}

/*!
 * \param size The size of point sprites, in screen coordinates [0.0; 1.0].
 */
void Visualiser::setPointSize(value_t size)
{
    point_size = size;
}

/*!
 * This function sets the subdivision of spline patches, and also
 * recomputes the patch geometry. Because of that, you are advised
 * not to use it in performance sensitive parts of program.
 * \param points Number of division points on spline patch side
 * \param useDList Should display lists be used?
 */
void Visualiser::setPatchDivision(unsigned points, bool useDList)
{
    if (patchPoints == points) return;

    patchPoints = points;

    if (!dlSplinePatch) {
        // if the display list is not yet generated, generate it
        dlSplinePatch = glGenLists(1);
    }

    // (re)create the spline patch data
    unsigned side_points = 2 + points;
    unsigned vert_count = side_points*side_points;
    delete vaSplinePatch0;
    vaSplinePatch0 = 0;
    delete vaSplinePatch1;
    vaSplinePatch1 = 0;
    vaSplinePatch0 = new VertexArray_Basic(vert_count, 4, GL_FLOAT, 0);
    vaSplinePatch1 = new VertexArray_Basic(vert_count, 4, GL_FLOAT, 0);

    // fill the vertex data
    unsigned y;
    GLfloat *vp0 = (GLfloat*)(vaSplinePatch0->mapData());
    GLfloat *vp1 = (GLfloat*)(vaSplinePatch1->mapData());
    GLfloat cd = 1.0f / (1 + points);
    for (y = 0; y < side_points; y++) {
        GLfloat yc = y * cd;
        GLfloat yc2 = yc * yc;
        GLfloat yc3 = yc2 * yc;

        for (unsigned x = 0; x < side_points; x++) {
            GLfloat xc = x*cd;
            GLfloat xc2 = xc*xc;
            *vp0++ = 1.0f;
            *vp0++ = xc;
            *vp0++ = xc2;
            *vp0++ = xc2 * xc;

            *vp1++ = 1.0f;
            *vp1++ = yc;
            *vp1++ = yc2;
            *vp1++ = yc3;
        }
    }
    vaSplinePatch1->unmapData();
    vaSplinePatch0->unmapData();

    // delete the index list, if exists
    delete[] indicesVPSplineInt;
    indicesVPSplineInt = 0;

    // create the index list
    unsigned ind_count = 2 * side_points;
    indicesVPSplineInt = new GLint[ind_count];
    GLint *ip = indicesVPSplineInt;
    for (unsigned i = 0; i < side_points; i++) {
        *ip++ = i + side_points;
        *ip++ = i;
    }

    useDLists = useDList;
    if (useDList) {
        // (re)compile the display list
        glNewList(dlSplinePatch, GL_COMPILE);
        drawVPSplineInt();
        glEndList();
    }
}

/*!
 * This function sets maximal number of particles rendered between a pair of
 * beginParticles(), endParticles(). The function allocates a vertex array
 * of the required size. Because of that, you are advised
 * not to use it in performance sensitive parts of program.
 * \param dim Dimensionality of particle positions (2,3,4)
 * \param maxCount Maximal number of particles rendered between one beginParticles(), endParticles() pair.
 */
void Visualiser::setParticleParams(unsigned dim, unsigned maxCount)
{
    if (maxParticles == maxCount && dim == particleDim) return;

    delete vaParticles;
    vaParticles = 0;

    assert(maxCount > 0);
    maxParticles = maxCount;
    particleDim = dim;

    //vaParticles = VertexArray::create(4 * maxCount * (dim * sizeof(GLfloat) + 2 * sizeof(GLfloat) + sizeof(TRGBAColor)));
    vaParticles = new VertexArray_Basic(4 * maxCount * (dim * sizeof(GLfloat) + 2 * sizeof(GLfloat) + sizeof(TRGBAColor)));
}

/*!
 * This function sets parameters for grid rendering using beginGrid(), gridPoint(), endGrid()
 * functions. The function allocates a buffer for the required number of grid values.
 * \param corners Corners of the grid ([0]=minx,miny, [1]=maxx,maxy)
 * \param xcount Number of grid cells (not points) on the x axis of the grid
 * \param ycount Number of grid cells (not points) on the y axis of the grid
 * \param layers Number of grid layers that may be stored simultaneously
 */
void Visualiser::setGridParams(const TVector3 corners[2], unsigned xcount, unsigned ycount, unsigned layers)
{
    delete[] pGridData;
    pGridData = 0;

    // store params
    assert(xcount > 0 && ycount > 0 && layers > 0);
    gridXSize = xcount;
    gridYSize = ycount;
    gridLayers = layers;
    gridCorners[0] = corners[0];
    gridCorners[1] = corners[1];

    // allocate buffer
    unsigned layerSize = (xcount+3)*(ycount+3);
    pGridData = new value_t[layerSize*layers];
}

#ifdef SUPPORT_LEGACY_2D
/*!
 * \overload void Visualiser::setGridParams(const TVector3 corners[2], unsigned xcount, unsigned ycount, unsigned layers)
 */
void Visualiser::setGridParams(const ::Tvector corners[2], unsigned xcount, unsigned ycount, unsigned layers)
{
    TVector3 v3Corners[2] = { TVector3(corners[0]), TVector3(corners[1]) };
    setGridParams(v3Corners, xcount, ycount, layers);
}
#endif

/*!
 * \param tex mglTexture used as the point sprite texture
 */
void Visualiser::setPointSpriteTexture(valueTex_t tex)
{
    assert(tex);
    pointSpriteTex = tex;
}

/*!
 * \param tex mglTexture used as the value palette texture, 
 */
void Visualiser::setValueTexture(valueTex_t tex)
{
    assert(tex);
    valuePaletteTex = tex;
}

void Visualiser::drawVPSplineInt() const
{
    vpSplineInt->enable();
    vpSplineInt->bind();
    vpSplineInt->enableAttribPointer(0);
    vpSplineInt->enableAttribPointer(1);
    vaSplinePatch0->useAsVPAttrib(0, *vpSplineInt);
    vaSplinePatch1->useAsVPAttrib(1, *vpSplineInt);
    unsigned side_points = 2 + patchPoints;
    unsigned vert_count = side_points*side_points;
    unsigned ind_count = 2 * side_points;
    unsigned bi = 0;
    for (unsigned y = 0; y < 1 + patchPoints; y++) {
        VertexArray::drawIndexedArray(GL_QUAD_STRIP, 0, vert_count, indicesVPSplineInt, ind_count, bi);
        bi += side_points;
    }
    vpSplineInt->disableAttribPointer(1);
    vpSplineInt->disableAttribPointer(0);
    vpSplineInt->disable();
}

/*!
 * \param point Location of the point
 * \param value The value associated with the point
 */
void Visualiser::drawPoint(const TVector3 &point, value_t value)
{
    TRGBAColor color;

    switch (pointsMode) {
    case pmFlat:
        assert(valuePaletteTex);

        glEnable(GL_TEXTURE_2D);
        mglChangeTexture(valuePaletteTex);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
        setupVPPointValue();

        glPointSize(point_size);

        glBegin(GL_POINTS);
          glColor3f(1.0f, 1.0f, 1.0f);
          glTexCoord1f(value);
          glVertex2fv(point);
        glEnd();
        break;

    case pmPointSprite:
        assert(pointSpriteTex);
        assert(valuePaletteFunc);
        setupPointSprites();
        glEnable(GL_TEXTURE_2D);
        mglChangeTexture(pointSpriteTex);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
        if (pointSpritesSupported()) {
            glBegin(GL_POINTS);
            color = valuePaletteFunc(normalizeData(value));
            glColor4fv((GLfloat*)(&color));
            glVertex2fv(point);
            glEnd();
        } else {
            glBegin(GL_QUADS);
            color = valuePaletteFunc(normalizeData(value));
            glColor4fv((GLfloat*)(&color));
            glTexCoord2f(0.0f, 0.0f);
            glVertex2fv(point);
            glTexCoord2f(1.0f, 0.0f);
            glVertex2fv(point);
            glTexCoord2f(1.0f, 1.0f);
            glVertex2fv(point);
            glTexCoord2f(0.0f, 1.0f);
            glVertex2fv(point);
            glEnd();
        }
        break;
    }
}

/*!
 * \param corners Coordinates of the corners of the quad
 * \param values  Values associated with the corners of the quad
 */
void Visualiser::drawQuad(const TVector3 corners[4], const value_t values[4])
{
    TRGBAColor color;

    switch (quadsMode) {
    case qmRGB:
        assert(valuePaletteFunc);

        glDisable(GL_TEXTURE_2D);
        vpPointValue->disable();
        //setupVPPointValue();

        glBegin(GL_QUADS);
          color = valuePaletteFunc(normalizeData(values[0]));
          glColor4fv((GLfloat*)(&color));
          glVertex2fv(corners[0]);
          color = valuePaletteFunc(normalizeData(values[1]));
          glColor4fv((GLfloat*)(&color));
          glVertex2fv(corners[1]);
          color = valuePaletteFunc(normalizeData(values[2]));
          glColor4fv((GLfloat*)(&color));
          glVertex2fv(corners[2]);
          color = valuePaletteFunc(normalizeData(values[3]));
          glColor4fv((GLfloat*)(&color));
          glVertex2fv(corners[3]);
        glEnd();
        break;

    case qmTexture:
        assert(valuePaletteTex);

        glEnable(GL_TEXTURE_2D);
        mglChangeTexture(valuePaletteTex);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
        setupVPPointValue();

        glBegin(GL_QUADS);
          glColor3f(1.0f, 1.0f, 1.0f);
          glTexCoord1f(values[0]);
          glVertex3fv(corners[0]);
          glTexCoord1f(values[1]);
          glVertex3fv(corners[1]);
          glTexCoord1f(values[2]);
          glVertex3fv(corners[2]);
          glTexCoord1f(values[3]);
          glVertex3fv(corners[3]);
        glEnd();
        break;
   }
}

/*!
 * count Number of points to be rendered
 * points Coordinates of the points
 * values Values of the respective points
 */
void Visualiser::drawPoints(unsigned count, const TVector3 points[], const value_t values[])
{
    const TVector3 *pp = points;
    const value_t  *pv = values;
    // draw all given points in maximum-size batches via beginParticles(), particle(), endParticles()
    while (count) {
        unsigned batch = count > maxParticles ? maxParticles : count;
        count -= batch;

        beginParticles();
        while (batch--) particle(*pp++, *pv++);
        endParticles();
    }
}

/*!
 * count Number of quads to be rendered
 * corners 4*count TVector3's with coordinates of the quads' corners
 * values 4*count values of the respective quad corners
 */
void Visualiser::drawQuads(unsigned count, const TVector3 corners[], const value_t values[])
{
    // \todo drawQuads
    const TVector3 *pc = corners;
    const value_t  *pv = values;
    for (unsigned i = 0; i < count; i++) {
        drawQuad(pc, pv);
        pc += 4;
        pv += 4;
    }
}

namespace Visla {
    /*!
     * Four coordinate vector
     *
     * Used internally for transfers of parameters to vertex programs.
     */
    struct TVector4 {
        float_t x;
        float_t y;
        float_t z;
        float_t w;
    };

    /*!
     * A 4x4 matrix.
     *
     * Used internally as simple transformation matrix.
     */
    struct TMatrix4x4 {
        float_t m[4][4];        //!< Matrix values

        inline operator float_cp() const { return &(m[0][0]); } //!< Pointer to the constant matrix
        inline operator float_p() { return &(m[0][0]); }        //!< Pointer to the matrix

        inline operator const TVector4* () const { return (const TVector4*)(&(m[0][0])); }  //!< Pointer to the constant matrix as an array of vectors
        inline operator TVector4* () { return (TVector4*)(&(m[0][0])); }  //!< Pointer to the matrix as an array of vectors

        /*!
         * \brief Multiplication of a matrix with another matrix
         *
         * \param result The matrix that should hold the result of the multiplication
         * \param a The left hand operand of the product
         * \param b The right hand operand of the product
         * \return result, holding the product \a a x \a b
         */
        inline static TMatrix4x4& multiply_t(TMatrix4x4 &result, const TMatrix4x4 &a, const TMatrix4x4 &b) {
            for (int i = 0; i < 4; i++)
                for (int j = 0; j < 4; j++) {
                    float_t dot = 0;
                    for (int k = 0; k < 4; k++)
                        dot += a.m[i][k] * b.m[k][j];
                    result.m[j][i] = dot;
                }
                return result;
        }
    };

    /*!
     * \brief A utility function for spline parameter precalculation.
     *
     * This function precalculates a vector of Catmull-Rom spline parameters from the given
     * values in four points.
     * \param v Vector that should hold the resulting parameters
     * \param p1 Value of the spline at -1
     * \param p2 Value of the spline at 0
     * \param p3 Value of the spline at +1
     * \param p4 Value of the spline at +2
     */
    static inline void precalcCatmullRom(TVector4 &v, float_t p1, float_t p2, float_t p3, float_t p4)
    {
        v.x=p2;
        v.y=(p3-p1)*0.5f;
        v.z=p1-2.5f*p2+2.0f*p3-0.5f*p4;
        v.w=-0.5f*p1+1.5f*p2-1.5f*p3+0.5f*p4;
    }

    /*!
     * The basis matrix for spline parameter calculation
     */
    static const TMatrix4x4 splineBasisMatrix = {
        {
          {  0.0f,  1.0f,  0.0f,  0.0f },
          { -0.5f,  0.0f,  0.5f,  0.0f },
          {  1.0f, -2.5f,  2.0f, -0.5f },
          { -0.5f,  1.5f, -1.5f,  0.5f }
        }
    };
};

/*!
 * \param xcount Number of spline cells (not points) on the x axis of the grid
 * \param ycount Number of spline cells (not points) on the y axis of the grid
 * \param corners Corners of the grid ([0]=(\a minx, \a miny), [1]=(\a maxx, \a maxy))
 * \param values Values in points of the grid, contains (\a xcount+3)*(\a ycount+3) values,
 *               [0] is the value at \a corners[0] (that value does not get drawn!),
 *               then the values go row-by-row (row == constant y coordinate) towards
 *               corners[1].
 */
void Visualiser::drawSplineInterpolatedGrid(unsigned xcount, unsigned ycount, const TVector3 corners[2], const value_t values[])
{
    static const GLfloat quadRelPos[8] = {
        0.0f, 0.0f,
        1.0f, 0.0f,
        1.0f, 1.0f,
        0.0f, 1.0f
    };

    assert(valuePaletteTex);

    value_t quadXSize = (corners[1].x() - corners[0].x()) / xcount;
    value_t quadYSize = (corners[1].y() - corners[0].y()) / ycount;

    glEnable(GL_TEXTURE_2D);
    mglChangeTexture(valuePaletteTex);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
    setupVPSplineInt(quadXSize, quadYSize);

    const value_t *vp = values;
    GLfloat corner[4] = { corners[0].x(), corners[0].y(), corners[0].z(), 0.0f };

    switch (splineMode) {
    case smVertInterp:
        vpSplineInt->enableAttribPointer(0);
        vpSplineInt->enableAttribPointer(1);
        vaSplinePatch0->useAsVPAttrib(0, *vpSplineInt);
        vaSplinePatch1->useAsVPAttrib(1, *vpSplineInt);
        vpSplineInt->setProgramParams(12, 1, (const float*)(&quadWhiteWithAlpha));
        break;

    case smPerFragment:
        vpFragSplineInt->enableAttribPointer(0);
        vpFragSplineInt->vertexAttribPointer(0, 2, GL_FLOAT, 0, quadRelPos);
        fpFragSplineInt->setProgramParams(0, 1, (const float*)(&quadWhiteWithAlpha));
        break;
    }

    for (unsigned y = 0; y < ycount; y++) {
        corner[0] = corners[0].x();
        vp = values + (y * (xcount + 3));
        for (unsigned x = 0; x < xcount; x++) {
            TMatrix4x4 precalcMatrix, splineMatrix;

            TVector4 *pmv = precalcMatrix;
            const float_t *prow1 = vp;
            const float_t *prow2 = vp + (xcount + 3);
            const float_t *prow3 = vp + 2 * (xcount + 3);
            const float_t *prow4 = vp + 3 * (xcount + 3);

            for (int i = 4; i > 0; i--)
                precalcCatmullRom(*pmv++, *prow1++, *prow2++, *prow3++, *prow4++);
            vp++;

            TMatrix4x4::multiply_t(splineMatrix, splineBasisMatrix, precalcMatrix);

            switch (splineMode) {
            case smVertInterp:
                vpSplineInt->setProgramParams(7, 1, corner);
                vpSplineInt->setProgramParams(8, 4, splineMatrix);
                if (useDLists) glCallList(dlSplinePatch);
                else drawVPSplineInt();
                break;

            case smPerFragment:
                vpFragSplineInt->setProgramParams(4, 1, corner);
                fpFragSplineInt->setProgramParams(1, 4, splineMatrix);
                glDrawArrays(GL_QUADS, 0, 4);
                break;
            }

            corner[0] += quadXSize;
        }
        corner[1] += quadYSize;
    }

    switch (splineMode) {
    case smVertInterp:
        vpSplineInt->disableAttribPointer(1);
        vpSplineInt->disableAttribPointer(0);
        break;

    case smPerFragment:
        vpFragSplineInt->disableAttribPointer(0);
        break;
    }
}

/*!
 */
void Visualiser::beginParticles()
{
    void *vap = vaParticles->mapData();

    pParticlePositions = (GLfloat*)vap;
    pParticleColors = (mglColor*)(pParticlePositions + 4 * (particleDim * maxParticles));
    pParticleTexCoords = (GLfloat*)(pParticleColors + 4 * maxParticles);

    nParticles = 0;
}

/*!
 */
void Visualiser::endParticles()
{
    if (!vaParticles->unmapData()) return;

    switch (pointsMode) {
    case pmFlat:
        glPointSize(point_size);

        setupVPPointValue();
        glEnable(GL_TEXTURE_2D);
        assert(valuePaletteTex);
        mglChangeTexture(valuePaletteTex);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);

        glEnableClientState(GL_VERTEX_ARRAY);
        glEnableClientState(GL_COLOR_ARRAY);
        glEnableClientState(GL_TEXTURE_COORD_ARRAY);
        vaParticles->useAsVertex(particleDim, GL_FLOAT);
        vaParticles->useAsColor(4, GL_FLOAT, 0, 4 * maxParticles * (particleDim * sizeof(GLfloat)));
        vaParticles->useAsTexCoord(1, GL_FLOAT, 0, 4 * maxParticles * (particleDim * sizeof(GLfloat) + sizeof(TRGBAColor)));
        vaParticles->drawArray(GL_POINTS, 0, nParticles);
        vaParticles->finished();
        glDisableClientState(GL_TEXTURE_COORD_ARRAY);
        glDisableClientState(GL_COLOR_ARRAY);
        glDisableClientState(GL_VERTEX_ARRAY);
        break;
        
    case pmPointSprite:
        setupPointSprites();
        glEnable(GL_TEXTURE_2D);
        assert(pointSpriteTex);
        mglChangeTexture(pointSpriteTex);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);

        glEnableClientState(GL_VERTEX_ARRAY);
        glEnableClientState(GL_COLOR_ARRAY);
        vaParticles->useAsVertex(particleDim, GL_FLOAT);
        vaParticles->useAsColor(4, GL_FLOAT, 0, 4 * maxParticles * (particleDim * sizeof(GLfloat)));
        if (pointSpritesSupported()) {
            glPointSize(point_size);
            vaParticles->drawArray(GL_POINTS, 0, nParticles);
        } else {
            glEnableClientState(GL_TEXTURE_COORD_ARRAY);
            vaParticles->useAsTexCoord(2, GL_FLOAT, 0, 4 * maxParticles * (particleDim * sizeof(GLfloat) + sizeof(TRGBAColor)));
            vaParticles->drawArray(GL_QUADS, 0, nParticles);
        }
        vaParticles->finished();
        glDisableClientState(GL_TEXTURE_COORD_ARRAY);
        glDisableClientState(GL_COLOR_ARRAY);
        glDisableClientState(GL_VERTEX_ARRAY);
        disablePointSprites();
        break;
    }
}

/*!
 * \param position Coordinates of the particle (the dimensionality of the vector is given
 *                 by the \a dim parameter in call to setParticleParams()
 * \param value Value associated with this particle
 */
void Visualiser::particle(const GLfloat *position, value_t value)
{
    static const GLfloat spriteTexCoords[] = {
        0.0f, 0.0f,
        1.0f, 0.0f,
        1.0f, 1.0f,
        0.0f, 1.0f
    };

    assert(nParticles < 4*maxParticles);

    switch (pointsMode) {
    case pmFlat:
        memcpy(pParticlePositions, position, particleDim * sizeof(GLfloat));
        pParticlePositions += particleDim;
        *pParticleColors++ = particleWhiteWithAlpha;
        *pParticleTexCoords++ = value;
        nParticles++;
        break;

    case pmPointSprite:
        assert(valuePaletteFunc);
        mglColor color = valuePaletteFunc(normalizeData(value));
        color.a = particleWhiteWithAlpha.a;
        if (pointSpritesSupported()) {
            memcpy(pParticlePositions, position, particleDim * sizeof(GLfloat));
            pParticlePositions += particleDim;
            *pParticleColors++ = color;
            nParticles ++;
        } else {
            for (int i = 0; i < 4; i++) {
                memcpy(pParticlePositions, position, particleDim * sizeof(GLfloat));
                pParticlePositions += particleDim;
                *pParticleColors++ = color;
            }

            memcpy(pParticleTexCoords, spriteTexCoords, sizeof(spriteTexCoords));
            pParticleTexCoords += 8;
            nParticles += 4;
        }
        break;
    }
}

/*!
 * \param layer The layer of the grid to be filled (0-based)
 */
void Visualiser::beginGrid(unsigned layer)
{
    assert(layer < gridLayers);

    currLayer = layer;
    pCurrGridValue = pGridData + layer*(gridXSize+3)*(gridYSize+3);

    remGridValues = (gridXSize + 3) * (gridYSize + 3);

    // skip the top border + left border of the first line
    pCurrGridValue += gridXSize + 4;
    remGridValues -= gridXSize + 4;
}

/*!
 * \param drawNow Should the grid be immediately rendered?
 *
 * The grid is always drawn spline-interpolated (the current quad rendering mode is not
 * effective for beginGrid(), endGrid(), ... functionality).
 */
void Visualiser::endGrid(bool drawNow)
{
    assert(remGridValues == gridXSize + 2);  // the down border except the first point must remain

    // border linear extrapolation
    unsigned xPts = gridXSize + 3;
    unsigned yPts = gridYSize + 3;
    value_t *pLayer = pGridData + currLayer*xPts*yPts;
    unsigned lastLineOfs = xPts*(yPts-1);
    for (unsigned x = 0; x < gridXSize + 1; x++) {
        pLayer[x + 1] = 2*pLayer[x + xPts + 1] - pLayer[x + 2*xPts + 1];                                               // top border
        pLayer[lastLineOfs + x + 1] = 2*pLayer[lastLineOfs + x - xPts + 1] - 2*pLayer[lastLineOfs + x - 2*xPts + 1];   // bottom border
    }
    for (unsigned y = 0, ofs = 0; y < yPts; y++, ofs += xPts) {
        pLayer[ofs] = 2*pLayer[ofs + 1] - pLayer[ofs + 2];                          // left border
        pLayer[ofs + xPts - 1] = 2*pLayer[ofs + xPts - 2] - pLayer[ofs + xPts - 3]; // right border
    }

    if (drawNow)
        drawSplineInterpolatedGrid(gridXSize, gridYSize, gridCorners, pLayer);
}

/*!
 * \param layer The layer of the grid to be drawn (0-based)
 *
 * The grid is always drawn spline-interpolated (the current quad rendering mode is not
 * effective for beginGrid(), endGrid(), ... functionality).
 */
void Visualiser::drawStoredGrid(unsigned layer)
{
    assert(layer < gridLayers);
    drawSplineInterpolatedGrid(gridXSize, gridYSize, gridCorners, pGridData + layer*(gridXSize+3)*(gridYSize+3));
}

/*!
 * \param dest  The layer into that should the interpolated data be stored
 * \param layer The layer of the grid to be drawn (0-based), e.g. 0.5 means
 *              the layer halfway between layers 0 and 1.
 * \param mode  Mode for value interpolation
 */
void Visualiser::interpolateGridLayer(unsigned dest, float layer, LayerInterpMode mode)
{
    assert(dest >= 0 && dest < gridLayers);
    assert(layer >= 0);

    const unsigned baseLayer = (unsigned)floor(layer);
    const float interp = layer - baseLayer;
    assert(interp >= 0.0f && interp < 1.0f);

    const unsigned layerSize = (gridXSize+3)*(gridYSize+3);
    const value_t *p1 = pGridData + baseLayer*layerSize;
    value_t *pd = pGridData + dest*layerSize;

    if (interp < 0.00001) {
        // no interpolation, layer is integer -- just copy the layer
        if (baseLayer != dest)
            memcpy(pd, p1, sizeof(value_t) * layerSize);
        return;
    }
    assert(baseLayer+1 < gridLayers);
    const value_t *p2 = p1 + layerSize;

    switch(mode) {
    case limLinear:
        {
            for (unsigned rem = layerSize; rem > 0; pd++, p1++, p2++, rem--) {
                *pd = *p1 + (*p2 - *p1) * interp;
            }
            break;
        }

    case limSpline:
        {
            assert(baseLayer > 0 && baseLayer+2 < gridLayers);
            const float s = interp;
            const float s2 = s*s;
            const float s3 = s2*s;

            const value_t *p0 = p1 - layerSize;
            const value_t *p3 = p2 + layerSize;

            for (unsigned rem = layerSize; rem > 0; pd++, p0++, p1++, p2++, p3++, rem--) {
                *pd = *p0 * (s2 - 0.5*s3 - 0.5*s) +
                      *p1 * (1.5*s3 - 2.5*s2 + 1) +
                      *p2 * (2*s2 - 1.5*s3 + 0.5*s) +
                      *p3 * (0.5*s3 - 0.5*s2);
            }
            break;
        }

    default:
        assert(!"Invalid interpolation mode");
    }
}

/*!
 * \param value Value associated with the current grid point.
 */
void Visualiser::gridPoint(value_t value)
{
    assert(remGridValues);

    // per-fragment must receive already normalized data, vertex-interpolated normalizes them itself
    switch(splineMode) {
    case smVertInterp:
        *pCurrGridValue++ = value;
        break;

    case smPerFragment:
        *pCurrGridValue++ = normalizeData(value);
        break;
    }
    remGridValues--;

    if (remGridValues % (gridXSize + 3) == 1) {
        // we are at the right hand end of the visible grid => skip the right+left border
        if (remGridValues == 1) {
            // we are at the end of the grid
            remGridValues = 0;
        } else {
            assert(remGridValues >= 2);
            pCurrGridValue += 2;
            remGridValues -= 2;
        }
    }
}
