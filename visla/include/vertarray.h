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


#ifndef __VERTARRAY_H_
#define __VERTARRAY_H_

// --- includes - OpenGL
#ifdef _WIN32
  #include <windows.h>
#endif
#include <GL/gl.h>

// --- includes - Visla
#include "vertprog.h"

// --- namespace declaration
namespace Visla {
    class VertexArray;
    class VertexArray_Basic;
};
using namespace Visla;

// --- VertexArray

/*!
 * An abstract class for dealing with vertex arrays. Its subclassing implement its functionality
 * using various OpenGL extensions.
 */
class Visla::VertexArray {
public:
    virtual ~VertexArray() { }  //!< The destructor, does nothing in this abstract class.

    /*!
     * \brief Get the name of the extension used
     *
     * This function can be used to determine the name of the OpenGL extension currently used
     * to work with vertex arrays.
     * \return OpenGL extension name specifying the currently used extension for vertex arrays
     */
    virtual const char *extension_used() const = 0;

    /*!
     * \brief Reallocate the vertex array
     *
     * \param bytes New size of the vertex array (bytes)
     * \throws bad_alloc When memory allocation fails.
     */
    virtual void realloc(unsigned size) = 0;

    /*!
     * \brief Map the vertex array into user-accessible memory, so that it can be read/modified.
     *
     * Every call to mapData must be followed by a call to unmapData() before any operation
     * on the array (including useAsVertex(), mapData(), etc.) takes place.
     * \return Pointer to the start of the mapped vertex array.
     * \see unmapData()
     */
    virtual void* mapData() = 0;
    /*!
     * \brief Unmap the vertex array mapped by a previous call to mapData()
     *
     * \return false if the vertex array got corrupted between mapData() and unmapData().
     *         This may happen, e.g. when the display mode is changed.
     * \see mapData()
     */
    virtual bool unmapData() = 0;

    /*!
     * \brief Reports that the rendering from this vertex array has been finished for this time.
     *
     * This call should take place after all rendering from the vertex array has been
     * delivered to OpenGL. This function serves the purpose of protecting the array from
     * overwriting before the rendering is completed. No rendering from this array should
     * take place between calls to finished() and mapData().
     */
    virtual void finished() {};

    /*!
     * \brief Set that this vertex array should be used as the source for vertex positions.
     *
     * This call sets this vertex array as the source for vertex positions. The call
     * does not enable vertex arrays! You must call glEnableClientState(GL_VERTEX_ARRAY) to
     * enable rendering from vertex array!
     * \param dim Number of coordinates per vertex (2, 3, or 4)
     * \param type Datatype of each coordinate (one of GL_BYTE, GL_FLOAT, GL_DOUBLE, etc. constants)
     * \param stride Data offset between consecutive vertices. Zero stride specifies tightly packed vertices.
     * \param offset Offset (bytes) of the first vertex position from the beginning of the array.
     */
    virtual void useAsVertex(GLint dim, GLenum type, GLsizei stride = 0, unsigned offset = 0) = 0;
    /*!
     * \brief Set that this vertex array should be used as the source for vertex colors.
     *
     * This call sets this vertex array as the source for vertex colors. The call
     * does not enable vertex color arrays! You must call glEnableClientState(GL_COLOR_ARRAY) to
     * enable rendering from color array!
     * \param dim Number of components per color (3 or 4)
     * \param type Datatype of each component (one of GL_BYTE, GL_FLOAT, GL_DOUBLE, etc. constants)
     * \param stride Data offset between consecutive colors. Zero stride specifies tightly packed colors.
     * \param offset Offset (bytes) of the first vertex color from the beginning of the array.
     */
    virtual void useAsColor(GLint dim, GLenum type, GLsizei stride = 0, unsigned offset = 0) = 0;
    /*!
     * \brief Set that this vertex array should be used as the source for texture coordinates.
     *
     * This call sets this vertex array as the source for texture coordinates. The call
     * does not enable texture coordinate arrays! You must call glEnableClientState(GL_TEXTURE_COORD_ARRAY) to
     * enable rendering from texture coordinate array!
     * \param dim Number of texture coordinates per vertex (1, 2, 3, or 4)
     * \param type Datatype of each coordinate (one of GL_BYTE, GL_FLOAT, GL_DOUBLE, etc. constants)
     * \param stride Data offset between consecutive vertices. Zero stride specifies tightly packed vertices.
     * \param offset Offset (bytes) of the first texture coordinate from the beginning of the array.
     */
    virtual void useAsTexCoord(GLint dim, GLenum type, GLsizei stride = 0, unsigned offset = 0) = 0;
    /*!
     * \brief Set that this vertex array should be used as the source for the specified vertex program attribute.
     *
     * This call sets this vertex array as the source for the specified vertex program attribute.
     * The call does not enable the array! You must call glEnableClientState() to
     * enable rendering from the vertex array!
     * \param index Index of the program attribute to be set
     * \param vertProg The vertex program that should be used as an interface
     * \param dim Number of components of each attribute
     * \param type Datatype of each component (one of GL_BYTE, GL_FLOAT, GL_DOUBLE, etc. constants)
     * \param stride Data offset between consecutive array elements. Zero stride specifies tightly packed elements.
     * \param offset Offset (bytes) of the first array element from the beginning of the array.
     */
    virtual void useAsVPAttrib(GLuint index, const VertexProgram &vertProg, GLint dim, GLenum type, GLsizei stride = 0, unsigned offset = 0) = 0;

    /*!
     * \brief Render the arrays
     *
     * This call causes the corresponding number of primitives to be rendered from all
     * enabled vertex arrays.
     * \param primType Type of the primitive that should be rendered (GL_POINTS, GL_LINES, etc.)
     * \param fromVertex Starting index in the enabled arrays
     * \param numVertices Number of vertices to render
     */
    static void drawArray(GLenum primType, int fromVertex, int numVertices);
    /*!
     * \brief Render the arrays
     *
     * This call causes the corresponding number of primitives to be rendered from all
     * enabled vertex arrays.
     * \param primType Type of the primitive that should be rendered (GL_POINTS, GL_LINES, etc.)
     * \param fromVertex Index of the first used element in the enabled arrays
     * \param numVertices Number of vertices used in this call
     * \param indices Indices of the elements that should be rendered
     * \param indexCount Number of indices/vertices to render
     * \param baseIndex Index offset that should be added to each indices[] element
     */
    static void drawIndexedArray(GLenum primType, int fromVertex, int numVertices, const GLint indices[], unsigned indexCount, unsigned baseIndex = 0);

    //! Create an instance of an implementation of vertex arrays
    static VertexArray* create(unsigned size_bytes);
};

// --- VertexArray_Basic

/*!
 * Quite special implementation of VertexArray, using OpenGL 1.1 vertex arrays stored
 * in system memory. (I.e. this implementation does not use any OpenGL extension, only
 * basic OpenGL 1.1 functionality.)
 */
class Visla::VertexArray_Basic : public VertexArray {
    void   *vertex_data;    //!< The vertex array data, stored in the system memory
    unsigned vertex_count;  //!< Number of vertices stored in the array
    GLsizei  vertex_size;   //!< Size of each vertex
    GLint    dim;           //!< Number of components per vertex
    GLenum   type;          //!< Datatype of each component
    GLsizei  stride;        //!< Data offset between consecutive array elements. Zero stride specifies tightly packed elements.
public:
    /*!
     * \brief Create the vertex array for the specified vertices.
     *
     * \param vertex_count Number of vertices that should be stored in the array.
     * \param dim Number of components of each attribute
     * \param type Datatype of each component (one of GL_BYTE, GL_FLOAT, GL_DOUBLE, etc. constants)
     * \param stride Data offset between consecutive array elements. Zero stride specifies tightly packed elements.
     */
    VertexArray_Basic(unsigned vertex_count, GLint dim, GLenum type, GLsizei stride = 0);
    /*!
     * \brief Create the vertex array
     *
     * \param size_bytes Size (bytes) of the array
     */
    VertexArray_Basic(unsigned size_bytes);
    //! The destructor
    ~VertexArray_Basic();

    /*!
     * \brief Get the name of the extension used
     *
     * This function can be used to determine the name of the OpenGL extension currently used
     * to work with vertex arrays.
     * \return As this class uses only basic OpenGL 1.1 functionality, this function returns
     *         empty string ("").
     */
    virtual const char *extension_used() const { return ""; }

    /*!
     * \brief Reallocate the vertex array
     *
     * \param vertex_array If the array has been created using VertexArray_Basic(unsigned, GLint, GLenum, GLsizei),
     *                     this parameter specifies new number of vertices that should be stored
     *                     in the array, otherwise, this specifies new size (bytes) of the vertex array.
     * \throws bad_alloc When memory allocation fails.
     */
    virtual void realloc(unsigned vertex_count);

    /*!
     * \brief Get the pointer to the vertex array.
     *
     * \return Pointer to the start of the mapped vertex array.
     */
    virtual void* mapData() { return vertex_data; }
    /*!
     * \brief Does nothing in this class.
     *
     * \return Always true
     * \see VertexArray::unmapData()
     */
    virtual bool unmapData() { return true; }

    //! \todo Comments
    inline void useAsVertex(unsigned firstVertex = 0);
    inline void useAsColor(unsigned firstVertex = 0);
    inline void useAsTexCoord(unsigned firstVertex = 0);
    inline void useAsVPAttrib(GLuint index, const VertexProgram &vertProg, unsigned firstVertex = 0);

    virtual void useAsVertex(GLint dim, GLenum type, GLsizei stride = 0, unsigned offset = 0);
    virtual void useAsColor(GLint dim, GLenum type, GLsizei stride = 0, unsigned offset = 0);
    virtual void useAsTexCoord(GLint dim, GLenum type, GLsizei stride = 0, unsigned offset = 0);
    virtual void useAsVPAttrib(GLuint index, const VertexProgram &vertProg, GLint dim, GLenum type, GLsizei stride = 0, unsigned offset = 0);
};

// ---------------------------

inline void VertexArray_Basic::useAsVertex(unsigned firstVertex)
{
    const GLubyte *vp = (const GLubyte*)vertex_data;
    vp += vertex_size * firstVertex;
    glVertexPointer(dim, type, stride, vp);
}

inline void VertexArray_Basic::useAsColor(unsigned firstVertex)
{
    const GLubyte *vp = (const GLubyte*)vertex_data;
    vp += vertex_size * firstVertex;
    glColorPointer(dim, type, stride, vp);
}

inline void VertexArray_Basic::useAsTexCoord(unsigned firstVertex)
{
    const GLubyte *vp = (const GLubyte*)vertex_data;
    vp += vertex_size * firstVertex;
    glTexCoordPointer(dim, type, stride, vp);
}

inline void VertexArray_Basic::useAsVPAttrib(GLuint index, const VertexProgram &vertProg, unsigned firstVertex)
{
    const GLubyte *vp = (const GLubyte*)vertex_data;
    vp += vertex_size * firstVertex;
    vertProg.vertexAttribPointer(index, dim, type, stride, vp);
}

#endif // #ifndef __VERTARRAY_H_
