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


// --- includes - OpenGL
#ifdef _WIN32
  #ifndef WIN32_LEAN_AND_MEAN
    #define WIN32_LEAN_AND_MEAN 1
  #endif
  #include <windows.h>
  #include <GL/gl.h>
  #include <GL/wglext.h>
#else /* GLX */
  #include <GL/gl.h>
  #include <GL/glx.h>
  #define wglGetProcAddress glXGetProcAddressARB
#endif
#include <GL/glext.h>

// --- includes - C++
#include <cassert>

// --- includes - Visla
#include "extsup.h"
#include "vertarray.h"

using namespace std;
using namespace Visla;

// --- preprocessor defines

/*! \def SUPPORT_ARB
 * \brief Should we support ARB_vertex_buffer_object ?
 *
 * If \c SUPPORT_ARB is defined, this library supports the ARB_vertex_buffer_object extension.
 */
#define SUPPORT_ARB
/*! \def SUPPORT_ATI
 * \brief Should we support ATI_vertex_array_object + ATI_map_object_buffer ?
 *
 * If \c SUPPORT_ATI is defined, this library supports the ATI_vertex_array_object + ATI_map_object_buffer extensions.
 */
#undef SUPPORT_ATI
/*! \def SUPPORT_NV
 * \brief Should we support NV_vertex_array_range + NV_fence ?
 *
 * If \c SUPPORT_NV is defined, this library supports the NV_vertex_array_range + NV_fence extensions.
 */
#define SUPPORT_NV


/*! \def BUFFER_OFFSET
 * \brief A simple tool to creating offsets into vertex arrays.
 */
#define BUFFER_OFFSET(i) ((char *)NULL + (i))

// --- namespace declaration
namespace Visla {
#ifdef SUPPORT_NV
    struct VAFunctionWrapper_NV;
    class VertexArray_NV;
#endif
#ifdef SUPPORT_ARB
    struct VAFunctionWrapper_ARB;
    class VertexArray_ARB;
#endif
#ifdef SUPPORT_ATI
#error "Support for ATI_vertex_array_object + ATI_map_object_buffer has not been implemented"
#endif
};

using namespace std;
using namespace Visla;

// --- concrete class implementations

#ifdef SUPPORT_NV

#ifndef GL_NV_vertex_array_range
#error "NV_vertex_array is not supported by the GL/glext.h header file."\
       "Get a newer glext.h file or #undef SUPPORT_NV."
#endif

#ifndef GL_NV_fence
#error "NV_fence is not supported by the GL/glext.h header file."\
       "Get a newer glext.h file or #undef SUPPORT_NV."
#endif

#ifdef _WIN32
  #ifndef WGL_NV_vertex_array_range
  #error "NV_vertex_array is not supported by the GL/wglext.h header file."\
         "Get a newer wglext.h file or #undef SUPPORT_NV."
  #endif

  #define ALLOC_FUNC_NAME   "wglAllocateMemoryNV"
  #define DEALLOC_FUNC_NAME "wglFreeMemoryNV"
#else /* GLX */
  #ifndef GLX_NV_vertex_array_range
  #error "NV_vertex_array is not supported by the GL/glxext.h header file."\
         "Get a newer glxext.h file or #undef SUPPORT_NV."
  #endif

  #define ALLOC_FUNC_NAME   "glXAllocateMemoryNV"
  #define DEALLOC_FUNC_NAME "glXFreeMemoryNV"
#endif

/*!
 * \brief A wrapper containing addresses of the extension functions for the NV_vertex_array_range
 * and NV_fence extensions.
 *
 * This struct contains function pointers to the functions for the extension.
 * The function pointers are initialized in the constructor. This struct is meant to be private
 * in this library.
 */
struct Visla::VAFunctionWrapper_NV {
    PFNGLVERTEXARRAYRANGENVPROC glVertexArrayRangeNV;
    
    PFNWGLALLOCATEMEMORYNVPROC wglAllocateMemoryNV;
    PFNWGLFREEMEMORYNVPROC wglFreeMemoryNV;
    
    PFNGLGENFENCESNVPROC glGenFencesNV;
    PFNGLDELETEFENCESNVPROC glDeleteFencesNV;
    PFNGLSETFENCENVPROC glSetFenceNV;
    PFNGLFINISHFENCENVPROC glFinishFenceNV;

    /*!
     * \brief Create the wrapper using pointers retrieved from the system.
     *
     * This constructor fills the function pointers by values retrieved by wglGetProcAddress()
     * from the OpenGL library.
     *
     * \throws not_supported_error if some of the functions of the extension is not found
     *                             by the wglGetProcAddress() function.
     */
    VAFunctionWrapper_NV() {
        glVertexArrayRangeNV = (PFNGLVERTEXARRAYRANGENVPROC) wglGetProcAddress("glVertexArrayRangeNV");

        wglAllocateMemoryNV = (PFNWGLALLOCATEMEMORYNVPROC) wglGetProcAddress(ALLOC_FUNC_NAME);
        wglFreeMemoryNV = (PFNWGLFREEMEMORYNVPROC) wglGetProcAddress(DEALLOC_FUNC_NAME);

        glGenFencesNV = (PFNGLGENFENCESNVPROC) wglGetProcAddress("glGenFencesNV");
        glDeleteFencesNV = (PFNGLDELETEFENCESNVPROC) wglGetProcAddress("glDeleteFencesNV");
        glSetFenceNV = (PFNGLSETFENCENVPROC) wglGetProcAddress("glSetFenceNV");
        glFinishFenceNV = (PFNGLFINISHFENCENVPROC) wglGetProcAddress("glFinishFenceNV");

        if (!glVertexArrayRangeNV) {
	    throw not_supported_error("NV_vertex_array_range extension functions not found");
	}
        if (!wglAllocateMemoryNV || !wglFreeMemoryNV) {
	    throw not_supported_error("NV_vertex_array_range WGL/GLX extension functions not found");
	}
        if (!glGenFencesNV || !glDeleteFencesNV || !glSetFenceNV || !glFinishFenceNV) {
	    throw not_supported_error("NV_fence extension functions not found");
        }
    }
};

/*!
 * \brief A concrete version of the abstract VertexArray class for the NV_vertex_array_range
 * extension.
 *
 * This class implements the vertex program functionality using the NV_vertex_array_range
 * extension.
 */
class Visla::VertexArray_NV: public VertexArray {
private:
    //! Function pointers for the NV_vertex_array_range extension
    VAFunctionWrapper_NV *funcs;

    //! Vertex array pointer
    void *buffer;

    //! Fence handle
    GLuint fence;

    //! Current size of the vertex array (bytes)
    unsigned size;

    //! Has been the fence already set?
    bool fence_set;

    //! Generate the fence handle
    inline void genfence() {
        if (fence) delfence();
        funcs->glGenFencesNV(1, &fence);
    }

    //! Delete the fence handle
    inline void delfence() {
        if (fence) {
            funcs->glDeleteFencesNV(1, &fence);
            fence = 0;
        }
    }

    //! Free the allocated memory
    inline void free() {
        if (buffer) {
            funcs->wglFreeMemoryNV(buffer);
            buffer = 0;
        }
        size = 0;
    }
public:
    /*!
     * \brief Create a vertex array wrapper, using the specified function pointers.
     *
     * Create a new vertex array, using the given function pointers.
     * \param size_bytes Size of the vertex array, in bytes
     * \param funcs A VAFunctionWrapper_NV containing the function pointers for
     *              the NV_vertex_array_range extension, or 0 for a new function wrapper.
     * \throws not_supported_error if some of the functions of the extension is not found
     *                             by the wglGetProcAddress() function.
     */
    explicit VertexArray_NV(unsigned size_bytes, const VAFunctionWrapper_NV *funcs = 0) : buffer(0), fence(0), fence_set(false) {
	if (funcs) {
	    this->funcs = new VAFunctionWrapper_NV(*funcs);
	} else {
	    this->funcs = new VAFunctionWrapper_NV();
	}
        genfence();
        realloc(size_bytes);
    };

    //! The destructor
    virtual ~VertexArray_NV() {
	free();
        delfence();
	delete funcs;
    }

    /*!
     * \brief Get the name of the extension used
     *
     * This function can be used to determine the name of the OpenGL extension currently used
     * to work with vertex arrays.
     * \return "NV_vertex_array_range"
     */
    virtual const char *extension_used() const {
	return "NV_vertex_array_range";
    }

    /*!
     * \brief Reallocate the vertex array
     *
     * \param bytes New size of the vertex array (bytes)
     * \throws bad_alloc When memory allocation fails.
     */
    virtual void realloc(unsigned bytes) {
        free();
        buffer = funcs->wglAllocateMemoryNV(bytes, 0.0f, 1.0f, 0.5f);       //! \todo user-definable usage+priority?
        if (!buffer) throw bad_alloc();
        size = bytes;
        funcs->glVertexArrayRangeNV(size, buffer);
        glEnableClientState(GL_VERTEX_ARRAY_RANGE_NV);
    }

    //! \todo Comments
    virtual void* mapData() {
        if (fence_set) {
            fence_set = false;
            funcs->glFinishFenceNV(fence);
        }
        return buffer;
    }

    virtual bool unmapData() {
        return true;
    }

    virtual void finished() {
        assert(!fence_set);
        funcs->glSetFenceNV(fence, GL_ALL_COMPLETED_NV);
        fence_set = true;
    }

    virtual void useAsVertex(GLint dim, GLenum type, GLsizei stride = 0, unsigned offset = 0) {
        const GLubyte *vp = (const GLubyte*)buffer;
        vp += offset;
        glVertexPointer(dim, type, stride, vp);
    }

    virtual void useAsColor(GLint dim, GLenum type, GLsizei stride = 0, unsigned offset = 0) {
        const GLubyte *vp = (const GLubyte*)buffer;
        vp += offset;
        glColorPointer(dim, type, stride, vp);
    }

    virtual void useAsTexCoord(GLint dim, GLenum type, GLsizei stride = 0, unsigned offset = 0) {
        const GLubyte *vp = (const GLubyte*)buffer;
        vp += offset;
        glTexCoordPointer(dim, type, stride, vp);
    }

    virtual void useAsVPAttrib(GLuint index, const VertexProgram &vertProg, GLint dim, GLenum type, GLsizei stride = 0, unsigned offset = 0) {
        const GLubyte *vp = (const GLubyte*)buffer;
        vp += offset;
        vertProg.vertexAttribPointer(index, dim, type, stride, vp);
    }
};

#endif // SUPPORT_NV

// --------------------------------------------------------------

#ifdef SUPPORT_ARB

#ifndef GL_ARB_vertex_buffer_object
#error "ARB_vertex_buffer_object is not supported by the GL/glext.h header file."\
       "Get a newer glext.h file or #undef SUPPORT_ARB."
#endif

/*!
 * \brief A wrapper containing addresses of the extension functions for the ARB_vertex_buffer_object.
 *
 * This struct contains function pointers to the functions for the extension.
 * The function pointers are initialized in the constructor. This struct is meant to be private
 * in this library.
 */
struct Visla::VAFunctionWrapper_ARB {
    PFNGLGENBUFFERSARBPROC glGenBuffersARB;
    PFNGLDELETEBUFFERSARBPROC glDeleteBuffersARB;
    PFNGLBINDBUFFERARBPROC glBindBufferARB;
    PFNGLBUFFERDATAARBPROC glBufferDataARB;
    PFNGLMAPBUFFERARBPROC glMapBufferARB;
    PFNGLUNMAPBUFFERARBPROC glUnmapBufferARB;

    /*!
     * \brief Create the wrapper using pointers retrieved from the system.
     *
     * This constructor fills the function pointers by values retrieved by wglGetProcAddress()
     * from the OpenGL library.
     *
     * \throws not_supported_error if some of the functions of the extension is not found
     *                             by the wglGetProcAddress() function.
     */
    VAFunctionWrapper_ARB() {
        glGenBuffersARB = (PFNGLGENBUFFERSARBPROC) wglGetProcAddress("glGenBuffersARB");
        glDeleteBuffersARB = (PFNGLDELETEBUFFERSARBPROC) wglGetProcAddress("glDeleteBuffersARB");
        glBindBufferARB = (PFNGLBINDBUFFERARBPROC) wglGetProcAddress("glBindBufferARB");
        glBufferDataARB = (PFNGLBUFFERDATAARBPROC) wglGetProcAddress("glBufferDataARB");
        glMapBufferARB = (PFNGLMAPBUFFERARBPROC) wglGetProcAddress("glMapBufferARB");
        glUnmapBufferARB = (PFNGLUNMAPBUFFERARBPROC) wglGetProcAddress("glUnmapBufferARB");

	if (!glGenBuffersARB || !glDeleteBuffersARB || !glBindBufferARB ||
            !glBufferDataARB || !glMapBufferARB || !glUnmapBufferARB) {
	    throw not_supported_error("ARB_vertex_buffer_object extension functions not found");
	}
    }
};

/*!
 * \brief A concrete version of the abstract VertexArray class for the ARB_vertex_buffer_object
 * extension.
 *
 * This class implements the vertex program functionality using the ARB_vertex_buffer_object
 * extension.
 */
class Visla::VertexArray_ARB: public VertexArray {
private:
    //! Function pointers for the ARB_vertex_buffer_object extension
    VAFunctionWrapper_ARB *funcs;

    //! Vertex buffer object handle
    GLuint buffid;

    //! \a buffid is valid
    bool valid;

    //! OpenGL vertex program target (\c GL_ARRAY_BUFFER_ARB, \c GL_ELEMENT_ARRAY_BUFFER_ARB)
    GLenum target;

    //! Current size of the vertex array (bytes)
    unsigned size;

    /*!
     * \brief Generate a \a buffid.
     *
     * Generate a buffer object handle into \a buffid.
     */
    void gen() {
	if (valid) del();
	funcs->glGenBuffersARB(1, &buffid);
	valid = true;
    }

    /*!
     * \brief Delete the \a buffid.
     *
     * Delete the buffer object handle in \a buffid.
     */
    void del() {
	if (valid) {
	    valid = false;
            unbind();
	    funcs->glDeleteBuffersARB(1, &buffid);
	}
    }

    void bind() {
        if (!valid) gen();
        funcs->glBindBufferARB(target, buffid);
    }

    void unbind() {
        funcs->glBindBufferARB(target, 0);
    }
public:
    /*!
     * \brief Create a vertex buffer object wrapper, using the specified function pointers.
     *
     * Create a new vertex buffer object, using the given function pointers.
     * \param target OpenGL vertex buffer target (\c GL_ARRAY_BUFFER_ARB, \c GL_ELEMENT_ARRAY_BUFFER_ARB)
     * \param size_bytes Size of the vertex buffer object, in bytes
     * \param funcs A VAFunctionWrapper_ARB containing the function pointers for
     *              the ARB_vertex_buffer_object extension, or 0 for a new function wrapper.
     * \throws not_supported_error if some of the functions of the extension is not found
     *                             by the wglGetProcAddress() function.
     */
    explicit VertexArray_ARB(unsigned size_bytes, GLenum target = GL_ARRAY_BUFFER_ARB, const VAFunctionWrapper_ARB *funcs = 0) {
	valid = false;
	this->target = target;
	if (funcs) {
	    this->funcs = new VAFunctionWrapper_ARB(*funcs);
	} else {
	    this->funcs = new VAFunctionWrapper_ARB();
	}
        realloc(size_bytes);
    };

    //! The destructor
    virtual ~VertexArray_ARB() {
	del();
	delete funcs;
    }

    /*!
     * \brief Get the name of the extension used
     *
     * This function can be used to determine the name of the OpenGL extension currently used
     * to work with vertex arrays.
     * \return "ARB_vertex_buffer_object"
     */
    virtual const char *extension_used() const {
	return "ARB_vertex_buffer_object";
    }

    /*!
     * \brief Reallocate the vertex array
     *
     * \param bytes New size of the vertex array (bytes)
     */
    virtual void realloc(unsigned bytes) {
        size = bytes;
        bind();
        funcs->glBufferDataARB(target, bytes, NULL, GL_STREAM_DRAW_ARB);    //! \todo user-definable usage hints?
        unbind();
    }

    virtual void* mapData() {
        bind();
        funcs->glBufferDataARB(target, size, NULL, GL_STREAM_DRAW_ARB);     //! \todo user-definable usage hints?
        return (void*)(funcs->glMapBufferARB(target, GL_WRITE_ONLY_ARB));   //!< \todo user-definable access?
    }

    virtual bool unmapData() {
        bind();
        bool result = (funcs->glUnmapBufferARB(target)) != GL_FALSE;
        unbind();
        return result;
    }

    virtual void useAsVertex(GLint dim, GLenum type, GLsizei stride = 0, unsigned offset = 0) {
        bind();
        glVertexPointer(dim, type, stride, BUFFER_OFFSET(offset));
        unbind();
    }

    virtual void useAsColor(GLint dim, GLenum type, GLsizei stride = 0, unsigned offset = 0) {
        bind();
        glColorPointer(dim, type, stride, BUFFER_OFFSET(offset));
        unbind();
    }

    virtual void useAsTexCoord(GLint dim, GLenum type, GLsizei stride = 0, unsigned offset = 0) {
        bind();
        glTexCoordPointer(dim, type, stride, BUFFER_OFFSET(offset));
        unbind();
    }

    virtual void useAsVPAttrib(GLuint index, const VertexProgram &vertProg, GLint dim, GLenum type, GLsizei stride = 0, unsigned offset = 0) {
        bind();
        vertProg.vertexAttribPointer(index, dim, type, stride, BUFFER_OFFSET(offset));
        unbind();
    }
};

#endif // SUPPORT_ARB

// -------------------- VertexArray_Basic -----------------------

VertexArray_Basic::VertexArray_Basic(unsigned vertex_count, GLint dim, GLenum type, GLsizei stride) : vertex_data(0), vertex_count(0), dim(dim), type(type), stride(stride)
{
    switch (type) {
    case GL_BYTE:           vertex_size = sizeof(GLbyte); break;
    case GL_SHORT:          vertex_size = sizeof(GLshort); break;
    case GL_INT:            vertex_size = sizeof(GLint); break;
    case GL_FLOAT:          vertex_size = sizeof(GLfloat); break;
    case GL_DOUBLE:         vertex_size = sizeof(GLdouble); break;
    case GL_UNSIGNED_BYTE:  vertex_size = sizeof(GLubyte); break;
    case GL_UNSIGNED_SHORT: vertex_size = sizeof(GLushort); break;
    case GL_UNSIGNED_INT:   vertex_size = sizeof(GLuint); break;
    default:                assert(!"Invalid datatype of vertex coordinates");
    };
    vertex_size *= dim;
    vertex_size += stride;
    assert(vertex_count > 0);
    realloc(vertex_count);
}

VertexArray_Basic::VertexArray_Basic(unsigned size_bytes) : vertex_data(0), vertex_count(0), dim(0), type(0), stride(0)
{
    vertex_size = 1;
    realloc(size_bytes);
}

VertexArray_Basic::~VertexArray_Basic()
{
    delete[] vertex_data;
}

void VertexArray_Basic::realloc(unsigned vertex_count)
{
    if (vertex_count != this->vertex_count) {
        delete[] vertex_data;
        vertex_data = 0;
        this->vertex_count = vertex_count;
        vertex_data = new GLubyte[vertex_size * vertex_count];
    }
}

void VertexArray_Basic::useAsVertex(GLint dim, GLenum type, GLsizei stride, unsigned offset) {
    const GLubyte *vp = (const GLubyte*)vertex_data;
    vp += offset;
    glVertexPointer(dim, type, stride, vp);
}

void VertexArray_Basic::useAsColor(GLint dim, GLenum type, GLsizei stride, unsigned offset) {
    const GLubyte *vp = (const GLubyte*)vertex_data;
    vp += offset;
    glColorPointer(dim, type, stride, vp);
}

void VertexArray_Basic::useAsTexCoord(GLint dim, GLenum type, GLsizei stride, unsigned offset) {
    const GLubyte *vp = (const GLubyte*)vertex_data;
    vp += offset;
    glTexCoordPointer(dim, type, stride, vp);
}

void VertexArray_Basic::useAsVPAttrib(GLuint index, const VertexProgram &vertProg, GLint dim, GLenum type, GLsizei stride, unsigned offset) {
    const GLubyte *vp = (const GLubyte*)vertex_data;
    vp += offset;
    vertProg.vertexAttribPointer(index, dim, type, stride, vp);
}

// ---------------------- VertexArray -------------------------

void VertexArray::drawArray(GLenum primType, int fromVertex, int numVertices)
{
    glDrawArrays(primType, fromVertex, numVertices);
}

void VertexArray::drawIndexedArray(GLenum primType, int fromVertex, int numVertices, const GLint indices[], unsigned indexCount, unsigned baseIndex)
{
    glBegin(primType);
    const GLint *pi = indices;
    for (unsigned i = 0; i < indexCount; i++) {
        //assert(*pi + (int)baseIndex >= fromVertex && *pi + (int)baseIndex < fromVertex + numVertices);
        glArrayElement(*pi++ + baseIndex);
    }
    glEnd();
}

/*!
 * Create an instance of a concrete VertexArray subclass that is supported on this system.
 * If no known extension for vertex arrays is supported on this system, an exception is thrown.
 *
 * \return Pointer to a new instance of some VertexArray subclass. You must call \c delete on it
 *         after you are finished with it.
 * \throws not_supported_error if no known extension for vertex arrays is supported on this system.
 */
VertexArray *VertexArray::create(unsigned size_bytes)
{
    static bool support_detected = false;
    #ifdef SUPPORT_ARB
    static bool ARB_supported;
    static VAFunctionWrapper_ARB *ARB_functions;
    #endif
    #ifdef SUPPORT_ATI
    static bool ATI_supported;
    static VAFunctionWrapper_ATI *ATI_functions;
    #endif
    #ifdef SUPPORT_NV
    static bool NV_supported;
    static VAFunctionWrapper_NV *NV_functions;
    #endif

    if (!support_detected) {
	// detect support
        #ifdef SUPPORT_ARB
	ARB_supported = isExtensionSupported("GL_ARB_vertex_buffer_object");
	if (ARB_supported)
	    ARB_functions = new VAFunctionWrapper_ARB;
        #endif
        #ifdef SUPPORT_ATI
	ATI_supported = isExtensionSupported("GL_ATI_vertex_array_object") &&
                        isExtensionSupported("GL_ATI_map_object_buffer");
	if (ATI_supported)
	    ATI_functions = new VAFunctionWrapper_ATI;
        #endif
        #ifdef SUPPORT_NV
	NV_supported  = isExtensionSupported("GL_NV_vertex_array_range");
	if (NV_supported)
	    NV_functions = new VAFunctionWrapper_NV;
        #endif
    }

    #ifdef SUPPORT_ARB
    if (ARB_supported)
	return new VertexArray_ARB(size_bytes, GL_ARRAY_BUFFER_ARB, ARB_functions);
    #endif
    #ifdef SUPPORT_ATI
    if (ATI_supported)
	return new VertexArray_ATI(size_bytes, ATI_functions);
    #endif
    #ifdef SUPPORT_NV
    if (NV_supported)
	return new VertexArray_NV(size_bytes, NV_functions);
    #endif
    throw not_supported_error("No known extension for vertex arrays is supported.");
}
