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


#ifndef __VISLA_H_
#define __VISLA_H_

/*!
 * \file visla.h
 * \author Petr Kadlec
 * \brief VISualization Library for Accelerators interface header file.
 *
 * This file contains interface part of Visla.
 */

#ifdef _MSC_VER
#pragma warning(disable: 4786)	// "long name for debug information, truncated"
#endif

#include <map>
#include <istream>
#include <ostream>
#include <cassert>

#ifdef _WIN32
  #ifndef WIN32_LEAN_AND_MEAN
    #define WIN32_LEAN_AND_MEAN 1
  #endif
  #include <windows.h>
#endif
#include <GL/gl.h>

#include <vertprog.h>
#include <fragprog.h>
#include <vertarray.h>
#include <ptsprites.h>
#include <pbuffer.h>

/*!
 * \def SUPPORT_LEGACY_2D
 * \brief Should we support some legacy 2D types?
 *
 * If SUPPORT_LEGACY_2D is defined, Visla supports some operations on some legacy
 * types (Tvector, Tintvector, ...).
 */
#define SUPPORT_LEGACY_2D

/*!
 * \def USE_MGL
 * \brief Should we use MGL functions?
 *
 * If USE_MGL is defined, Visla uses its functions and structures for working with
 * textures, etc.
 */
#define USE_MGL

#ifdef SUPPORT_LEGACY_2D
#include <vector.h>
#endif

#ifdef USE_MGL
#include <mgl.h>
#endif

/*!
 * \def VISLACALLBACK
 * \brief Calling convention for callback functions used by Visla.
 *
 * All user-defined functions called by Visla should have this calling convention.
 */
#ifdef USE_MGL
 #define VISLACALLBACK MGL_CALLBACK
#endif

namespace Visla {
    /*!
     * \brief Version tag for the library
     *
     * \see VERSION
     */
    enum { VERSION_TAG = 2 };
    /*!
     * \brief Version tag for the library
     */
    extern const char *VERSION;
    /*!
     * \brief Build date for the library
     */
    extern const char *BUILD_DATE;

    /*!
     * \brief The floating-point type that is used by the library.
     *
     * All floating-point values within the library (vector components, scalars, etc.) are
     * represented in this type.
     */
    typedef float float_t, *float_p;
    typedef const float_t *float_cp;

    /*!
     * \brief The "epsilon value" for vector equality comparisons.
     *
     * When two vectors are tested for equality, they are, in fact, compared for length of
     * their difference. When that difference is shorter than EQUALITY_EPSILON, the vectors
     * are considered equal.
     * \see TVector3::operator== ()
     */
    static float_t EQUALITY_EPSILON = 1E-6f;

    /*!
     * Type used by the library for RGBA color representation.
     */
    #ifdef USE_MGL
    typedef mglColor TRGBAColor;
    #endif

    //! This class represents a 3D vector.
    class TVector3 {
        union {
          float_t m[3];
          struct {
            float_t x;
            float_t y;
            float_t z;
          } vec;
        } data;
    public:
        // --- constructors, etc.
        TVector3();                                         //!< Create a zero vector
        TVector3(float_t x, float_t y, float_t z);          //!< Create an initialized vector
        /*
        -- these are not required (compiler does a good job in creating them)
        TVector3(const TVector3 &v);                        //!< Copy constructor
        inline TVector3 &operator = (const TVector3 &v);    //!< Assignment operator
        */

        // --- accessors
        inline float_t x() const;                           //!< Get the x component of the vector
        inline float_t y() const;                           //!< Get the y component of the vector
        inline float_t z() const;                           //!< Get the z component of the vector
        inline float_t operator[] (int component) const;    //!< Get the specified component of the vector (0=x, 1=y, 2=z)
        inline float_t &operator[] (int component);         //!< Get the specified component of the vector (0=x, 1=y, 2=z)
        inline operator float_p();                          //!< Get the component array {x,y,z}
        inline operator float_cp() const;                   //!< Get the component array {x,y,z}

        // --- unary functions
        inline float_t length() const;                      //!< Get the length of the vector
        inline float_t length_sq() const;                   //!< Get the square of the length of the vector
        inline TVector3 normalized() const;                 //!< Get a normalized copy of the vector

        // --- modifiers
        void set_x(float_t x);                       //!< Change the value of the x component
        void set_y(float_t y);                       //!< Change the value of the y component
        void set_z(float_t z);                       //!< Change the value of the z component

        // --- special modifiers
        inline void normalize();                            //!< Normalize this vector
        inline void reverse();                              //!< Reverse this vector
        inline void resize_to(float_t size);                //!< Resize the vector

        // --- operations/by overloaded operators
        inline float_t  operator () () const;                                   //!< Length of the vector
        inline TVector3 operator - () const;                                    //!< Vector reversion
        inline TVector3 operator + (const TVector3 &v) const;                   //!< Vector addition
        inline TVector3&operator += (const TVector3 &v);                        //!< Vector addition
        inline TVector3 operator - (const TVector3 &v) const;                   //!< Vector subtraction
        inline TVector3&operator -= (const TVector3 &v);                        //!< Vector subtraction
        inline float_t  operator * (const TVector3 &v) const;                   //!< DOT PRODUCT
        inline TVector3 operator * (float_t f) const;                           //!< Multiplication of vector by scalar
        inline TVector3&operator *= (float_t f);                                //!< Multiplication of vector by scalar
        inline TVector3 operator / (float_t f) const;                           //!< Multiplication of vector by 1/scalar
        inline TVector3&operator /= (float_t f);                                //!< Multiplication of vector by 1/scalar
        friend inline TVector3 operator * (float_t f, const TVector3 &v);       //!< Multiplication of vector by scalar

        // --- comparison operators
        inline bool operator == (const TVector3 &v) const;    //!< Vector equality
        inline bool operator != (const TVector3 &v) const;    //!< Vector nonequality

        // --- stream input, output
        friend std::ostream& operator << (std::ostream &s, const TVector3 &v);  //!< Output of vector to stream
        friend std::istream& operator >> (std::istream &s, TVector3 &v);        //!< Input of vector from stream

        // --- non-operator versions of binary operations (all static)
        static inline TVector3 &add(TVector3 &result, const TVector3 &a, const TVector3 &b);                //!< Vector addition
        static inline TVector3 &subtract(TVector3 &result, const TVector3 &a, const TVector3 &b);           //!< Vector subtraction
        static inline TVector3 &scalar_mul(TVector3 &result, const TVector3 &a, float_t f);                 //!< Multiplication of vector by scalar
        static inline float_t   dot_product(const TVector3 &a, const TVector3 &b);                          //!< Dot product
        static inline TVector3 &cross_product(TVector3 &result, const TVector3 &a, const TVector3 &b);      //!< Cross product
        static inline TVector3 &component_product(TVector3 &result, const TVector3 &a, const TVector3 &b);  //!< Component product
        static inline TVector3 &component_division(TVector3 &result, const TVector3 &a, const TVector3 &b); //!< Component division
        static inline TVector3 &component_min(TVector3 &result, const TVector3 &a, const TVector3 &b);      //!< Minimum of components
        static inline TVector3 &component_max(TVector3 &result, const TVector3 &a, const TVector3 &b);      //!< Maximum of components

        #ifdef SUPPORT_LEGACY_2D
        // --- Legacy 2D types support
        TVector3(const ::Tvector &v, float_t z = 0.0f);      //!< Conversion from 2D Tvector
        TVector3(const ::Tintvector &v, float_t z = 0.0f);   //!< Conversion from 2D Tintvector

        inline TVector3 &operator = (const ::Tvector &v);    //!< Conversion from 2D Tvector
        inline TVector3 &operator = (const ::Tintvector &v); //!< Conversion from 2D Tintvector

        inline operator ::Tvector() const;                   //!< Conversion to 2D Tvector
        inline operator ::Tintvector() const;                //!< Conversion to 2D Tintvector
        #endif
    }; // TVector3

    //! This class is a wrapper class for Image Based Flow Visualization
    class IBFVHelper {
    private:
        friend class Visualiser;

        PBuffer pbuffer;            //!< p-buffer in which the data is computed
        GLuint  pbufftex;           //!< Texture name for the p-buffer as texture
        GLuint  *patTextures;       //!< Texture names for the pattern textures (array)

        float *data_x;              //!< Buffer for x coordinates supplied via gridValue()
        float *data_y;              //!< Buffer for y coordinates supplied via gridValue()
        int xcount;                 //!< Number of cells of the grid in the x dimension
        int ycount;                 //!< Number of cells of the grid in the y dimension
        int data_len;               //!< Size of data_x or data_y, equal to (xcount+1)*(ycount+1)

        float *curr_px;             //!< Pointer into data_x, for the current beginGrid(), endGrid() operation
        float *curr_py;             //!< Pointer into data_y, for the current beginGrid(), endGrid() operation
        int rem_data;               //!< Number of data remaining to be supplied in the current beginGrid(), endGrid() operation

        int texwidth;               //!< Width of the p-buffer
        int texheight;              //!< Height of the p-buffer

        int numpats;                //!< Number of patterns
        float scale;                //!< Scale parameter
        float alpha;                //!< Alpha parameter for the patterns

        int patsize;                //!< Size (width=height) of the pattern texture
        float umax;                 //!< Maximum u coordinate for the patterns
        float vmax;                 //!< Maximum v coordinate for the patterns

        int framenum;               //!< Current frame number (current pattern used)

        GLenum  texTarget;          //!< Texture target used (GL_TEXTURE_2D, or some extension)
        GLfloat texUSize;           //!< Maximum u coordinate of the p-buffer texture (1.0 for GL_TEXTURE_2D)
        GLfloat texVSize;           //!< Maximum v coordinate of the p-buffer texture (1.0 for GL_TEXTURE_2D)

        IBFVHelper(Visualiser *vis, int width, int height); //!< Constructor, to be called via Visualiser function
    public:
        ~IBFVHelper();              //!< Destructor, deallocates all resources for this IBFVHelper

        void setGridParams(int xcount, int ycount);                             //!< Setup grid parameters
        void setIBFVParams(int patsize, int numpats, float scale, float alpha); //!< Setup IBFV parameters

        void beginGrid();                       //!< Begin transfer of data
        void gridValue(float x, float y);       //!< Transfer a single vector in the grid
        void endGrid(bool doFrame = true);      //!< End transfer of data
        void doFrame();                         //!< Do a single frame animation step
        void draw(const TVector3 corners[2], float alpha = 1.0f);   //!< Draw the current frame
        #ifdef SUPPORT_LEGACY_2D
        void draw(const ::Tvector corners[2], float alpha = 1.0f);  //!< Draw the current frame
        #endif

        void beginDrawToBuffer();               //!< Start drawing directly into the p-buffer
        void endDrawToBuffer();                 //!< End drawing directly into the p-buffer
    };

    //! This is the basic wrapper class, everything is rendered via an instance of Visualiser.
    class Visualiser {
    public:
        //! Bit flags for support determination (see whatIsSupported())
        enum SupportFlags {
            supportedNothing = 0x00,            //!< Special value -- nothing is supported

            supportedVertexPrograms = 0x01,     //!< Vertex programs are supported. If this flag is not present, not even an instance of Visla may be created!
            supportedFragmentPrograms = 0x02,   //!< Fragment programs are supported. This flag is required for smPerFragment spline interpolation.
            supportedPointSprites = 0x04,       //!< Point sprites are supported. Everything will work without this, but pmPointSprite points will be much slower.
            supportedNPOTTextures = 0x08        //!< \c NV_texture_rectangle is supported, which allows for non-power-of-two textures.
        };
	//! type of a single data value
	typedef float_t value_t;
        //! Point rendering mode for drawPoint(), drawPoints()
	enum PointMode { 
	    pmFlat,		//!< Flat, one color point
	    pmPointSprite	//!< Full texture ("point sprite")
	};
	//! Quad rendering mode for drawQuad(), drawQuads()
	enum QuadMode {
	    qmRGB,		//!< RGB interpolated -- the values are interpolated through RGB space
	    qmTexture		//!< Texture interpolated -- the values are interpolated in texture space
	};
        //! Spline interpolation mode
        enum SplineMode {
            smVertInterp,       //!< Evaluated in subdivision points, then linearily interpolated
            smPerFragment       //!< Evaluated at each fragment (pixel)
        };
        //! Layer interpolation mode (for interpolateGridLayer())
        enum LayerInterpMode {
            limLinear,          //!< Linear
            limSpline           //!< Cubic (spline)
        };
        //! Value palette function (conversion from value to color)
        typedef TRGBAColor (VISLACALLBACK *valueFunc_p)(float_t);
        #ifdef USE_MGL
        //! Value palette texture (in fact, a LUT for value palette function)
        typedef mglTexture *valueTex_t;
        #endif

        static bool useDLists;              //!< Use display lists for vertex-spline interpolation.
    private:
        value_t range_low;      //!< The lower bound of data values \see setDataRange
        value_t range_scaler;   //!< 1 / (\a range_upper - \a range_lower) \see setDataRange
        float   point_size;     //!< Point sprite size

        PointMode  pointsMode;  //!< Current point rendering mode \see setPointsMode
        QuadMode   quadsMode;   //!< Current quad rendering mode \see setQuadsMode
        SplineMode splineMode;  //!< Current spline interpolation mode \see setSplineMode

        unsigned  patchPoints;  //!< Number of points on spline patch side
        unsigned  maxParticles; //!< Maximum number of particles rendered between one beginParticles(), endParticles() pair
        unsigned  particleDim;  //!< Dimensionality of particle coords
        unsigned  nParticles;   //!< Current number of particles to render

        char *prog_path;        //!< Path to vertex and fragment programs

        VertexProgram *vpPointValue;        //!< Vertex program for value computing (ptvalue.vp)
        VertexProgram *vpPointSprite;       //!< Vertex program for point sprites (ptsprite.vp)
        VertexProgram *vpSplineInt;         //!< Vertex program for spline interpolation (splineint.vp)
        VertexProgram   *vpFragSplineInt;   //!< Vertex program for per-fragment spline interpolation (splineint_frag.vp)
        FragmentProgram *fpFragSplineInt;   //!< Fragment program for per-fragment spline interpolation (splineint_frag.fp)

        GLuint dlSplinePatch;               //!< Display list for the spline-interpolated patch

        VertexArray_Basic *vaSplinePatch0;  //!< Vertex array (basic) for the spline patch, attribute 0
        VertexArray_Basic *vaSplinePatch1;  //!< Vertex array (basic) for the spline patch, attribute 1
        VertexArray *vaParticles;           //!< Vertex array for particles

        valueFunc_p valuePaletteFunc;       //!< Current value palette function
        valueTex_t  pointSpriteTex;         //!< Current point sprite texture
        valueTex_t  valuePaletteTex;        //!< Current value palette texture

        mglColor particleWhiteWithAlpha;    //!< White color, but with alpha value set via setParticleAlpha()
        mglColor quadWhiteWithAlpha;        //!< White color, but with alpha value set via setQuadAlpha()
        unsigned gridXSize;                 //!< Grid x size set by setGridParams()
        unsigned gridYSize;                 //!< Grid y size set by setGridParams()
        unsigned gridLayers;                //!< Number of grid layers set by setGridParams()
        unsigned currLayer;                 //!< Currently used grid layer (set by beginGrid())
        TVector3 gridCorners[2];            //!< Grid corners set by setGridParams()
        value_t *pGridData;                 //!< Grid data buffer for beginGrid(), endGrid(), gridPoint()
        value_t *pCurrGridValue;            //!< Pointer to grid data buffer
        unsigned remGridValues;             //!< Number of remaining grid values to be transferred via gridPoint()
        GLfloat *pParticlePositions;        //!< Particle position data buffer pointer for beginParticles(), endParticles(), particle()
        GLfloat *pParticleTexCoords;        //!< Particle texcoords data buffer pointer for beginParticles(), endParticles(), particle()
        mglColor *pParticleColors;          //!< Particle color data buffer pointer for beginParticles(), endParticles(), particle()
        GLint *indicesVPSplineInt;          //!< Indices used in drawVPSplineInt() to do vertex-spline interpolation

        std::fstream *make_stream_for_prog(const char* filename);   //!< Create an input stream for a vertex or fragment program stored on disk.

        inline void setupVPPointValue() const;  //!< Set vpPointValue as current and update its associated context variables
        inline void setupPointSprites() const;  //!< Sets the GL state for point sprite rendering (either using point sprite extension, or vpPointSprite)
        inline void setupVPSplineInt(value_t quadScaleX, value_t quadScaleY) const; //!< Set vpSplineInt as current and update its associated context variables
        void drawVPSplineInt() const;           //!< Draw vertex-spline-interpolated quad

        Visualiser(const Visualiser&) { throw 0; }          //!< Instances of this class may not be copied!
        void operator = (const Visualiser&) { throw 0; }    //!< Instances of this class may not be assigned!
    public:
        // --- special query
        static unsigned whatIsSupported();                  //!< Determine what features are supported on this system

        // --- initialize/release
	Visualiser(const char *program_path = "..\\programs\\");    //!< Initialize the visualiser
	~Visualiser();	                                            //!< Close the visualiser

	// --- data values
	void setDataRange(value_t low, value_t high);	    //!< Set the range of the values
        inline value_t getDataRangeLow() const;             //!< Get the current range's lower bound
        inline value_t getDataRangeHigh() const;            //!< Get the current range's upper bound
        inline value_t normalizeData(value_t value) const;  //!< Convert the data value to [0.0f;1.0f]

        // --- value texture generation
        static valueTex_t genValueTex(valueFunc_p func, unsigned size, GLint filter);       //!< Generate value palette texture using value palette function
        static valueTex_t genValueTexIso(valueFunc_p func, unsigned size, GLint filter, unsigned equidistance, const TRGBAColor &isoColor);    //!< Generate value palette texture with isolines using value palette function

        // --- helpers
        IBFVHelper* createIFBVHelper(int width, int height);     //!< Create a new IBFV helper.

	// --- state management
	void beginFrame();	//!< Set up the OpenGL state for the library
	void endFrame();	//!< Restore the OpenGL state to state before beginFrame

	// --- rendering configuration
	void setPointsMode(PointMode mode);	        //!< Set the rendering mode for drawPoints()
	void setQuadsMode(QuadMode mode);	        //!< Set the rendering mode for drawQuads(), drawCubicInterpolatedGrid()
	void setSplineMode(SplineMode mode);	        //!< Set the spline interpolation mode
        void setValueFunc(valueFunc_p func);            //!< Set the value palette function
        void setPointSize(value_t size);                //!< Set the point size
        void setPatchDivision(unsigned points, bool useDList = true);   //!< Set the number of spline patch division points when using smVertInterp spline mode
        void setParticleParams(unsigned dim, unsigned maxCount);        //!< Set parameters of particles for beginParticles(), endParticles(), particle()
        void setGridParams(const TVector3 corners[2], unsigned xcount, unsigned ycount, unsigned layercount = 1);    //!< Set parameters of grid for beginGrid(), endGrid(), ...
        inline void setParticleAlpha(float alpha);      //!< Set the alpha value for particles
        inline void setQuadAlpha(float alpha);          //!< Set the alpha value for spline-interpolated quads
        #ifdef SUPPORT_LEGACY_2D
        void setGridParams(const ::Tvector corners[2], unsigned xcount, unsigned ycount, unsigned layercount = 1);   //!< Set parameters of grid for beginGrid(), endGrid(), ...
        #endif
        #ifdef USE_MGL
        void setPointSpriteTexture(mglTexture *tex);    //!< Set the texture for point sprites
        void setValueTexture(mglTexture *tex);          //!< Set the value palette texture
        #endif

	// --- rendering
        void drawPoint(const TVector3 &point, value_t value);                               //!< Draw a single point using the point rendering mode
	void drawPoints(unsigned count, const TVector3 points[], const value_t values[]);   //!< Draw points using the point rendering mode
	void drawQuad(const TVector3 corners[4], const value_t values[4]);                  //!< Draw a single quad using the quad rendering mode
	void drawQuads(unsigned count, const TVector3 corners[], const value_t values[]);   //!< Draw quads using the quad rendering mode
	void drawSplineInterpolatedGrid(unsigned xcount, unsigned ycount, const TVector3 corners[4], const value_t values[]);	//!< Draw a quad grid, with spline interpolation in the texture space
        void beginParticles();                                  //!< Begins transfer of particles
        void endParticles();                                    //!< Ends transfer of particles and causes the particles to be drawn
        void particle(const GLfloat *position, value_t value);  //!< Transfers a single particle
        void beginGrid(unsigned layer = 0);                     //!< Begins transfer of grid values
        void endGrid(bool drawNow = true);                      //!< Ends transfer of grid values and possibly causes the grid to be drawn
        void drawStoredGrid(unsigned layer = 0);                //!< Draws the grid stored by beginGrid(), endGrid(), ...
        void interpolateGridLayer(unsigned dest, float layer, LayerInterpMode mode);  //!< Computes (and stores) an interpolated grid layer
        void gridPoint(value_t value);                          //!< Transfers a single grid value
    }; // class Visualiser

    /*!
     * A utility class allowing the user to blindly ask for value palette textures in any order, this
     * class caches the resulting textures and cares also about their memory management.
     */
    class ValueTextureCache {
    public:
        typedef Visualiser::valueFunc_p key_t;          //!< Type of the cache key (value palette function)
        typedef Visualiser::valueTex_t tex_t;           //!< Type of the cached texture (mglTexture*)
    private:
        unsigned genTexSize;                            //!< Size of the generated textures
        GLint    genTexFilter;                          //!< Filtering mode for the generated textures
        unsigned genTexEquidistance;                    //!< Equidistance for the generated textures
        mglColor genIsolineColor;                       //!< Isoline color for the generated textures

        typedef  map<key_t, tex_t> textureCache_t;

        Visualiser *vis;                                //!< The Visualiser used with this texture cache
        textureCache_t cachedTextures;                  //!< The storage for the cached textures without isolines
        textureCache_t cachedTexturesIso;               //!< The storage for the cached textures with isolines
    public:
        ValueTextureCache(Visualiser *vis);     //!< The constructor
        ~ValueTextureCache();                   //!< The destructor
        
        tex_t getTexture(key_t key, bool withIsolines = false);         //!< Get the texture corresponding to the given value palette function
        void    activateTexture(key_t key, bool withIsolines = false);  //!< Activate the value palette function (with its corresponding texture) in the Visualiser
        void    clear();                                                //!< Remove all cached textures

        inline  void setGenTexSize(unsigned size);                      //!< Set size for newly generated textures
        inline  void setGenTexFilter(GLint filter);                     //!< Set filtering mode for newly generated textures
        inline  void setGenTexEquidistance(unsigned dist);              //!< Set isoline equidistance for newly generated textures
        inline  void setGenIsolineColor(const mglColor &color);         //!< Set isoline color for newly generated textures

        inline  unsigned getGenTexSize() const;             //!< Get the current texture size set via setGenTexSize()
        inline  GLint setGenTexFilter() const;              //!< Get the current filtering mode set via setGenTexFilter()
        inline  unsigned getGenTexEquidistance() const;     //!< Get the current isoline equidistance for newly generated textures set via setGenTexEquidistance()
        inline  const mglColor &setGenIsolineColor() const; //!< Get the current isoline color set via setGenIsolineColor()
    };
}; // namespace Visla

// ----------------------- The TVector3 inline functions -------------------------------
using namespace Visla;

/*!
 * \return The x component of this vector.
 */
inline float_t TVector3::x() const
{
    return data.vec.x;
}

/*!
 * \return The y component of this vector.
 */
inline float_t TVector3::y() const
{
    return data.vec.y;
}

/*!
 * \return The z component of this vector.
 */
inline float_t TVector3::z() const
{
    return data.vec.z;
}

/*!
 * \arg component which component is being queried (0 = \a x, 1 = \a y, 2 = \a z);
 * \return The specified component of this vector.
 */
inline float_t TVector3::operator[] (int component) const
{
    return data.m[component];
}

/*!
 * \arg component which component is being queried (0 = \a x, 1 = \a y, 2 = \a z);
 * \return Reference to the specified component of this vector.
 */
inline float_t &TVector3::operator[] (int component)
{
    return data.m[component];
}

/*!
 * \return Pointer to the component array \c float_t[3]
 */
inline TVector3::operator Visla::float_p()
{
    return data.m;
}

/*!
 * \return Constant pointer to the component array \c const \c float_t[3]
 */
inline TVector3::operator Visla::float_cp() const
{
    return data.m;
}

/*!
 * \return Length of this vector.
 * \see operator()()
 * \see length_sq()
 */
inline float_t TVector3::length() const
{
    return sqrt(
        data.vec.x * data.vec.x +
        data.vec.y * data.vec.y + 
        data.vec.z * data.vec.z
        );
}

/*!
 * \return The square of the length of this vector.
 * \see length()
 */
inline float_t TVector3::length_sq() const
{
    return
        data.vec.x * data.vec.x +
        data.vec.y * data.vec.y + 
        data.vec.z * data.vec.z;
}

/*!
 * Makes a copy of this vector that has the same direction, but unit length. This vector
 * is not modified.
 * \warning If this is a zero vector, the results are undefined.
 * \return A unit-length copy of this vector.
 * \see normalize()
 */
inline TVector3 TVector3::normalized() const
{
    TVector3 result(*this);
    result.normalize();
    return result;
}

/*!
 * Changes the value of the x coordinate to the specified. The rest is not modified.
 * \param x The new value of the x coordinate.
 */
inline void TVector3::set_x(float_t x)
{
    data.vec.x = x;
}

/*!
 * Changes the value of the y coordinate to the specified. The rest is not modified.
 * \param y The new value of the y coordinate.
 */
inline void TVector3::set_y(float_t y)
{
    data.vec.y = y;
}

/*!
 * Changes the value of the z coordinate to the specified. The rest is not modified.
 * \param z The new value of the z coordinate.
 */
inline void TVector3::set_z(float_t z)
{
    data.vec.z = z;
}

/*!
 * Changes this vector so that its direction is unchanged, but its length is now unity.
 * \warning If this is a zero vector, the results are undefined.
 * \see normalized()
 * \see resize_to()
 */
inline void TVector3::normalize()
{
    float_t inv_length = 1.0f / length();
    data.vec.x *= inv_length;
    data.vec.y *= inv_length;
    data.vec.z *= inv_length;
}

/*!
 * Changes this vector so that it has the same length, but the opposite direction.
 */
inline void TVector3::reverse()
{
    data.vec.x = -data.vec.x;
    data.vec.y = -data.vec.y;
    data.vec.z = -data.vec.z;
}
        
/*!
 * Changes this vector so that its direction is unchanged, but its length is changed to
 * the specified value.
 * \param size The requested new length of the vector.
 * \warning If this is a zero vector, the results are undefined.
 * \see normalize()
 */
inline void TVector3::resize_to(float_t size)
{
    float_t inv_length = size / length();
    data.vec.x *= inv_length;
    data.vec.y *= inv_length;
    data.vec.z *= inv_length;
}

/*!
 * \return Length of this vector.
 * \see length()
 */
inline float_t TVector3::operator () () const
{
    return length();
}

/*!
 * Computes the reverse vector to this vector, i.e. the vector with the same length, but exactly
 * opposite direction.
 * \return The reverse vector to this vector.
 * \see reverse()
 */
inline TVector3 TVector3::operator - () const
{
    TVector3 result;
    result.data.vec.x = -data.vec.x;
    result.data.vec.y = -data.vec.y;
    result.data.vec.z = -data.vec.z;
    return result;
}

/*!
 * \param v The vector that is to be added to this vector.
 * \return The sum of the two vectors
 * \see add()
 */
inline TVector3 TVector3::operator + (const TVector3 &v) const {
    TVector3 result;
    return add(result, *this, v);
}

/*!
 * \param v The vector that is to be added to this vector.
 * \return This vector, after the specified vector has been added to it.
 * \see operator+()
 * \see add()
 */
inline TVector3& TVector3::operator += (const TVector3 &v)
{
    data.vec.x += v.data.vec.x;
    data.vec.y += v.data.vec.y;
    data.vec.z += v.data.vec.z;
    return *this;
}

/*!
 * \param v The vector that is to be subtracted from this vector.
 * \return The difference of the two vectors
 * \see subtract()
 */
inline TVector3 TVector3::operator - (const TVector3 &v) const
{
    TVector3 result;
    return subtract(result, *this, v);
}

/*!
 * \param v The vector that is to be subtracted from this vector.
 * \return This vector, after the specified vector has been subtracted from it.
 * \see operator-(const TVector3&)
 * \see subtract()
 */
inline TVector3& TVector3::operator -= (const TVector3 &v)
{
    data.vec.x -= v.data.vec.x;
    data.vec.y -= v.data.vec.y;
    data.vec.z -= v.data.vec.z;
    return *this;
}

/*!
 * \param v The vector that is to be dot-multiplied by this vector.
 * \return The dot product of this vector and the specified vector.
 * \see dot_product()
 */
inline float_t TVector3::operator * (const TVector3 &v) const
{
    return dot_product(*this, v);
}

/*!
 * \param f The scalar with that should be this vector multiplied.
 * \return The product of this vector and the scalar.
 * \see scalar_mul()
 */
inline TVector3 TVector3::operator * (float_t f) const
{
    TVector3 result;
    return scalar_mul(result, *this, f);
}

/*!
 * \param f The scalar with that should be this vector multiplied.
 * \return This vector, after it has been multiplied by the scalar.
 * \see operator*(float_t)
 * \see scalar_mul()
 */
inline TVector3& TVector3::operator *= (float_t f)
{
    data.vec.x *= f;
    data.vec.y *= f;
    data.vec.z *= f;
    return *this;
}

/*!
 * \param f The scalar with that should be this vector divided.
 * \return This vector, after it has been multiplied by 1/\c f.
 * \see operator*(float_t)
 * \see scalar_mul()
 */
inline TVector3 TVector3::operator / (float_t f) const
{
    TVector3 result;
    return scalar_mul(result, *this, 1.0f / f);
}

/*!
 * \param f The scalar with that should be this vector divided.
 * \return This vector, after it has been multiplied by 1/\c f.
 * \see operator/(float_t)
 * \see scalar_mul()
 */
inline TVector3& TVector3::operator /= (float_t f)
{
    float_t rf = 1.0f / f;
    data.vec.x *= rf;
    data.vec.y *= rf;
    data.vec.z *= rf;
    return *this;
}

/*!
 * \param f The scalar.
 * \param v The vector.
 * \return The product of the vector and the scalar.
 * \see operator*(float_t)
 * \see scalar_mul()
 */
inline TVector3 operator * (float_t f, const TVector3 &v)
{
    TVector3 result;
    return TVector3::scalar_mul(result, v, f);
}

/*!
 * Checks whether the difference of the two vectors has length smaller than EQUALITY_EPSILON.
 * \param v The vector with that is this vector compared.
 * \return A \c bool specifying whether this vector is "practically equal" to \c v.
 * \see EQUALITY_EPSILON
 */
inline bool TVector3::operator == (const TVector3 &v) const
{
    return (*this - v)() < EQUALITY_EPSILON;
}

/*!
 * Checks whether the difference of the two vectors has length greater than or equal to EQUALITY_EPSILON.
 * \param v The vector with that is this vector compared.
 * \return A \c bool specifying whether this vector is "definitely not equal" to \c v.
 * \see EQUALITY_EPSILON
 */
inline bool TVector3::operator != (const TVector3 &v) const
{
    return !(*this == v);
}

/*!
 * \param result The vector into which should the result be placed.
 * \param a The left operand of addition.
 * \param b The right operand of addition.
 * \return \c result
 * \note \c result MAY be identical to either \c a, or \c b.
 * \see TVector3::operator+ ()
 */
inline TVector3 &TVector3::add(TVector3 &result, const TVector3 &a, const TVector3 &b)
{
    result.data.vec.x = a.data.vec.x + b.data.vec.x;
    result.data.vec.y = a.data.vec.y + b.data.vec.y;
    result.data.vec.z = a.data.vec.z + b.data.vec.z;
    return result;
}

/*!
 * \param result The vector into which should the result be placed.
 * \param a The left operand of subtraction.
 * \param b The right operand of subtraction.
 * \return \c result
 * \note \c result MAY be identical to either \c a, or \c b.
 * \see TVector3::operator- ()
 */
inline TVector3 &TVector3::subtract(TVector3 &result, const TVector3 &a, const TVector3 &b)
{
    result.data.vec.x = a.data.vec.x - b.data.vec.x;
    result.data.vec.y = a.data.vec.y - b.data.vec.y;
    result.data.vec.z = a.data.vec.z - b.data.vec.z;
    return result;
}

/*!
 * \param result The vector into which should the result be placed.
 * \param a The vector.
 * \param f The scalar.
 * \return \c result
 * \see TVector3::operator*(float_t)
 * \note \c result MAY be identical to \c a.
 */
inline TVector3 &TVector3::scalar_mul(TVector3 &result, const TVector3 &a, float_t f)
{
    result.data.vec.x = f * a.data.vec.x;
    result.data.vec.y = f * a.data.vec.y;
    result.data.vec.z = f * a.data.vec.z;
    return result;
}

/*!
 * \param a The left operand of dot product.
 * \param b The right operand of dot product.
 * \return The dot product of \a a and \a b
 * \see TVector3::operator*(const TVector3&)
 */
inline float_t TVector3::dot_product(const TVector3 &a, const TVector3 &b)
{
    return a.data.vec.x * b.data.vec.x + a.data.vec.y * b.data.vec.y + a.data.vec.z * b.data.vec.z;
}

/*!
 * \param result The vector into which should the result be placed.
 * \param a The left operand of cross product.
 * \param b The right operand of cross product.
 * \return \c result
 * \warning \c result <b>MAY NOT</b> be identical to neither \c a nor \c b.
 */
inline TVector3 &TVector3::cross_product(TVector3 &result, const TVector3 &a, const TVector3 &b)
{
    result.data.vec.x = a.data.vec.y * b.data.vec.z - a.data.vec.z * b.data.vec.y;
    result.data.vec.y = a.data.vec.z * b.data.vec.x - a.data.vec.x * b.data.vec.z;
    result.data.vec.z = a.data.vec.x * b.data.vec.y - a.data.vec.y * b.data.vec.x;
    return result;
}

/*!
 * This function computes the component-wise product of two vectors, that is, the vector, whose components
 * are products of the respective components of the operands.
 * \param result The vector into which should the result be placed.
 * \param a The left operand of component-wise product.
 * \param b The right operand of component-wise product.
 * \return \c result
 * \note \c result MAY be identical to either \c a, or \c b.
 */
inline TVector3 &TVector3::component_product(TVector3 &result, const TVector3 &a, const TVector3 &b)
{
    result.data.vec.x = a.data.vec.x * b.data.vec.x;
    result.data.vec.y = a.data.vec.y * b.data.vec.y;
    result.data.vec.z = a.data.vec.z * b.data.vec.z;
    return result;
}

/*!
 * This function computes the component-wise division of two vectors, that is, the vector, whose components
 * are quotients of the respective components of the operands.
 * \param result The vector into which should the result be placed.
 * \param a The left operand of component-wise division.
 * \param b The right operand of component-wise division.
 * \return \c result
 * \note \c result MAY be identical to either \c a, or \c b.
 */
inline TVector3 &TVector3::component_division(TVector3 &result, const TVector3 &a, const TVector3 &b)
{
    result.data.vec.x = a.data.vec.x / b.data.vec.x;
    result.data.vec.y = a.data.vec.y / b.data.vec.y;
    result.data.vec.z = a.data.vec.z / b.data.vec.z;
    return result;
}

/*!
 * This function computes the component-wise minimum of two vectors, that is, the vector, whose components
 * are always the smaller number from the respective components of the operands.
 * \param result The vector into which should the result be placed.
 * \param a The left operand of component-wise minimum.
 * \param b The right operand of component-wise minimum.
 * \return \c result
 * \note \c result MAY be identical to either \c a, or \c b.
 */
inline TVector3 &TVector3::component_min(TVector3 &result, const TVector3 &a, const TVector3 &b)
{
    result.data.vec.x = min(a.data.vec.x, b.data.vec.x);
    result.data.vec.y = min(a.data.vec.y, b.data.vec.y);
    result.data.vec.z = min(a.data.vec.z, b.data.vec.z);
    return result;
}

/*!
 * This function computes the component-wise maximum of two vectors, that is, the vector, whose components
 * are always the greater number from the respective components of the operands.
 * \param result The vector into which should the result be placed.
 * \param a The left operand of component-wise maximum.
 * \param b The right operand of component-wise maximum.
 * \return \c result
 * \note \c result MAY be identical to either \c a, or \c b.
 */
inline TVector3 &TVector3::component_max(TVector3 &result, const TVector3 &a, const TVector3 &b)
{
    result.data.vec.x = max(a.data.vec.x, b.data.vec.x);
    result.data.vec.y = max(a.data.vec.y, b.data.vec.y);
    result.data.vec.z = max(a.data.vec.z, b.data.vec.z);
    return result;
}

#ifdef SUPPORT_LEGACY_2D
// ------------------------------ LEGACY_2D ---------------------------------

/*!
 * Initializes the vector with the values from the legacy 2D vector. The current \a z coordinate is preserved.
 * \param v The 2D vector that is to be converted into this vector.
 */
inline TVector3 &TVector3::operator = (const ::Tvector &v)
{
    data.vec.x = v.x;
    data.vec.y = v.y;
}

/*!
 * Initializes the vector with the values from the legacy 2D vector. The current \a z coordinate is preserved.
 * \param v The 2D intvector that is to be converted into this vector.
 */
inline TVector3 &TVector3::operator = (const ::Tintvector &v)
{
    data.vec.x = v.x;
    data.vec.y = v.y;
}

/*!
 * Converts this vector to a legacy 2D vector. The \a z coordinate is discarded.
 * \return A Tvector that has the same \a x, \a y coordinates as this vector.
 * \todo Check the exact syntax of operator, possibly inline this into the class decl.
 */
inline TVector3::operator Tvector() const
{
    return Tvector(data.vec.x, data.vec.y);
}

/*!
 * Converts this vector to a legacy 2D intvector. The \a z coordinate is discarded, the
 * \a x and \a y coordinates are converted to \c int using the standard C-style cast.
 * \return A Tintvector that has the same \a x, \a y coordinates (only cast to \c int) as this vector.
 * \todo Check the exact syntax of operator, possibly inline this into the class decl.
 */
inline TVector3::operator Tintvector() const
{
    return Tintvector((int)data.vec.x, (int)data.vec.y);
}

#endif  // ifdef SUPPORT_LEGACY_2D

// ----------------------- The Visualiser inline functions -------------------------------

/*!
 * Sets the vpPointValue vertex program as current and sets all required state
 * variables.
 */
inline void Visualiser::setupVPPointValue() const
{
    float params[8] = {
        range_low, range_scaler, 0.0f, 0.0f,
        0.0f, 0.5f, 1.0f, 2.0f
    };
    assert(vpPointValue);
    vpPointValue->enable();
    vpPointValue->bind();
    vpPointValue->trackMatrix(0, VertexProgram::tmModelViewProjection);
    vpPointValue->setProgramParams(4, 2, params);
}

/*!
 * Enables point sprite rendering, either via point sprite extension, or
 * using the vpPointSprite vertex program (which is in the case set as current
 * and all required state variables are set).
 */
inline void Visualiser::setupPointSprites() const
{
    if (pointSpritesSupported()) {
        vpPointSprite->disable();
        enablePointSprites();
    } else {
        float viewport[4];
        glGetFloatv(GL_VIEWPORT, viewport);
        float params[8] = {
            point_size/viewport[2], point_size/viewport[3], 0.0f, 0.0f,
            0.0f, 0.5f, 1.0f, 2.0f
        };
        assert(vpPointSprite);
        vpPointSprite->enable();
        vpPointSprite->bind();
        vpPointSprite->trackMatrix(0, VertexProgram::tmModelViewProjection);
        vpPointSprite->setProgramParams(4, 2, params);
    }
}

/*!
 * \param quadScaleX X size of one quad
 * \param quadScaleY Y size of one quad
 *
 * Sets the vpSplineInt or vpFragSplineInt vertex program (depending on splineMode)
 * as current and sets all required state variables.
 */
inline void Visualiser::setupVPSplineInt(value_t quadScaleX, value_t quadScaleY) const
{
    float params[12] = {
        range_low, range_scaler, 0.0f, 0.0f,
        0.0f, 0.5f, 1.0f, 2.0f,
        quadScaleX, quadScaleY, 0.0f, 1.0f
    };
    switch(splineMode) {
    case smVertInterp:
        {
            assert(vpSplineInt);
            vpSplineInt->enable();
            vpSplineInt->bind();
            vpSplineInt->trackMatrix(0, VertexProgram::tmModelViewProjection);
            vpSplineInt->setProgramParams(4, 3, params);
            break;
        }

    case smPerFragment:
        {
            assert(vpFragSplineInt);
            vpFragSplineInt->enable();
            vpFragSplineInt->bind();
            vpFragSplineInt->trackMatrix(0, VertexProgram::tmModelViewProjection);
            vpFragSplineInt->setProgramParams(5, 1, params + 8);

            assert(fpFragSplineInt);
            fpFragSplineInt->enable();
            fpFragSplineInt->bind();
            break;
        }
    }
}

/*!
 * \return The lower bound of the current data range.
 * \see setDataRange
 */
inline Visualiser::value_t Visualiser::getDataRangeLow() const
{
    return range_low;
}

/*!
 * \return The upper bound of the current data range.
 * \see setDataRange
 * \note The value may not be exactly identical to the value set by setDataRange() because
 *       of rounding errors.
 */
inline Visualiser::value_t Visualiser::getDataRangeHigh() const
{
    if (range_scaler)
        return range_low + 1.0f / range_scaler;
    else
        return range_low;   // special case: range_low = range_high (see setDataRange())
}

/*!
 * \return (\a value - \a range_low) / (\a range_high - \a range_low)
 * \see setDataRange
 */
inline Visualiser::value_t Visualiser::normalizeData(value_t value) const
{
    return (value - range_low) * range_scaler;
}

/*!
 * \param alpha The alpha value of drawn particles (0 = fully transparent, 1 = fully opaque)
 *
 * \note Visla never enables alpha blending, you have to enable it when you want to
 *       see the effects of setParticleAlpha() !
 */
inline void Visualiser::setParticleAlpha(float alpha)
{
    particleWhiteWithAlpha.a = alpha;
}

/*!
 * \param alpha The alpha value of drawn spline-interpolated quads (0 = fully transparent, 1 = fully opaque)
 *
 * \note Visla never enables alpha blending, you have to enable it when you want to
 *       see the effects of setQuadAlpha() !
 */
inline void Visualiser::setQuadAlpha(float alpha)
{
    quadWhiteWithAlpha.a = alpha;
}

// --------------------- The ValueTextureCache inline functions -----------------------------

/*!
 * \param size Size of the textures generated by this cache.
 * \note This function has NO effect on already generated textures (which are in
 *       cache). To affect all textures, call clear() first.
 * \see Visualiser::genValueTex()
 * \see Visualiser::genValueTexIso()
 */
inline void ValueTextureCache::setGenTexSize(unsigned size)
{
    genTexSize = size;
}

/*!
 * \param filter Filtering mode for textures generated by this cache (GL_NEAREST, GL_LINEAR, etc.)
 * \note This function has NO effect on already generated textures (which are in
 *       cache). To affect all textures, call clear() first.
 * \see Visualiser::genValueTex()
 * \see Visualiser::genValueTexIso()
 */
inline void ValueTextureCache::setGenTexFilter(GLint filter)
{
    genTexFilter = filter;
}

/*!
 * \param dist Isoline equidistance for textures generated by this cache
 * \note This function has NO effect on already generated textures (which are in
 *       cache). To affect all textures, call clear() first.
 * \see Visualiser::genValueTexIso()
 */
inline void ValueTextureCache::setGenTexEquidistance(unsigned dist)
{
    genTexEquidistance = dist;
}

/*!
 * \param color Color of isolines for textures generated by this cache
 * \note This function has NO effect on already generated textures (which are in
 *       cache). To affect all textures, call clear() first.
 * \see Visualiser::genValueTexIso()
 */
inline void ValueTextureCache::setGenIsolineColor(const mglColor &color)
{
    genIsolineColor = color;
}

/*!
 * \return Current generated texture size set by setGenTexSize()
 */
inline unsigned ValueTextureCache::getGenTexSize() const
{
    return genTexSize; 
}

/*!
 * \return Current generated texture filter mode set by setGenTexFilter()
 */
inline GLint ValueTextureCache::setGenTexFilter() const
{
    return genTexFilter;
}

/*!
 * \return Current generated texture isoline equidistance set by setGenTexEquidistance()
 */
inline unsigned ValueTextureCache::getGenTexEquidistance() const
{
    return genTexEquidistance; 
}

/*!
 * \return Current generated textures' isoline color set by setGenIsolineColor()
 */
inline const mglColor &ValueTextureCache::setGenIsolineColor() const
{
    return genIsolineColor;
}

#endif // #ifndef __VISLA_H_
