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
 * \file vertprog.cpp
 * \author Petr Kadlec
 * \brief Vertex Program support.
 *
 * This file contains implementation of Vertex Program support wrapper classes.
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
  #include <GL/glx.h>
  #define wglGetProcAddress glXGetProcAddressARB
#endif
#include <GL/glu.h>
#include <GL/glext.h>

// --- includes - C++
#include <exception>
#include <memory>
#include <iostream>
#include <cstring>

// --- includes - Visla
#include "extsup.h"
#include "vertprog.h"

// --- preprocessor defines

/*! \def SUPPORT_ARB
 * \brief Should we support ARB_vertex_program ?
 *
 * If \c SUPPORT_ARB is defined, this library supports the ARB_vertex_program extension.
 */
#define SUPPORT_ARB
/*! \def SUPPORT_EXT
 * \brief Should we support EXT_vertex_shader ?
 *
 * If \c SUPPORT_EXT is defined, this library supports the EXT_vertex_shader extension.
 */
#undef SUPPORT_EXT
/*! \def SUPPORT_NV
 * \brief Should we support NV_vertex_program ?
 *
 * If \c SUPPORT_NV is defined, this library supports the NV_vertex_program extension.
 */
#define SUPPORT_NV

// --- namespace declaration
namespace Visla {
#ifdef SUPPORT_NV
    struct VPFunctionWrapper_NV;
    class VertexProgram_NV;
#endif
#ifdef SUPPORT_ARB
    struct VPFunctionWrapper_ARB;
    class VertexProgram_ARB;
#endif
#ifdef SUPPORT_EXT
#error "Support for EXT_vertex_shader has not been implemented"
#endif
};

using namespace std;
using namespace Visla;

// --- concrete class implementations

#ifdef SUPPORT_NV

#ifndef GL_NV_vertex_program
#error "NV_vertex_program is not supported by the GL/glext.h header file."\
       "Get a newer glext.h file or #undef SUPPORT_NV."
#endif

/*!
 * \brief A wrapper containing addresses of the extension functions for the NV_vertex_program.
 *
 * This struct contains function pointers to the functions for the extension.
 * The function pointers are initialized in the constructor. This struct is meant to be private
 * in this library.
 */
struct Visla::VPFunctionWrapper_NV {
    PFNGLGENPROGRAMSNVPROC glGenProgramsNV;
    PFNGLBINDPROGRAMNVPROC glBindProgramNV;
    PFNGLLOADPROGRAMNVPROC glLoadProgramNV;
    PFNGLDELETEPROGRAMSNVPROC glDeleteProgramsNV;
    PFNGLVERTEXATTRIBS4FVNVPROC glVertexAttribs4fvNV;
    PFNGLVERTEXATTRIBPOINTERNVPROC glVertexAttribPointerNV;
    PFNGLPROGRAMPARAMETERS4FVNVPROC glProgramParameters4fvNV;
    PFNGLTRACKMATRIXNVPROC glTrackMatrixNV;

    /*!
     * \brief Create the wrapper using pointers retrieved from the system.
     *
     * This constructor fills the function pointers by values retrieved by wglGetProcAddress()
     * from the OpenGL library.
     *
     * \throws not_supported_error if some of the functions of the extension is not found
     *                             by the wglGetProcAddress() function.
     */
    VPFunctionWrapper_NV() {
	glGenProgramsNV = (PFNGLGENPROGRAMSNVPROC) wglGetProcAddress("glGenProgramsNV");
	glBindProgramNV = (PFNGLBINDPROGRAMNVPROC) wglGetProcAddress("glBindProgramNV");
	glLoadProgramNV = (PFNGLLOADPROGRAMNVPROC) wglGetProcAddress("glLoadProgramNV");
	glDeleteProgramsNV = (PFNGLDELETEPROGRAMSNVPROC) wglGetProcAddress("glDeleteProgramsNV");
        glVertexAttribs4fvNV = (PFNGLVERTEXATTRIBS4FVNVPROC) wglGetProcAddress("glVertexAttribs4fvNV");
        glVertexAttribPointerNV = (PFNGLVERTEXATTRIBPOINTERNVPROC) wglGetProcAddress("glVertexAttribPointerNV");
        glProgramParameters4fvNV = (PFNGLPROGRAMPARAMETERS4FVNVPROC) wglGetProcAddress("glProgramParameters4fvNV");
        glTrackMatrixNV = (PFNGLTRACKMATRIXNVPROC) wglGetProcAddress("glTrackMatrixNV");

	if (!glGenProgramsNV || !glBindProgramNV || !glLoadProgramNV || !glDeleteProgramsNV ||
            !glVertexAttribs4fvNV || !glProgramParameters4fvNV || !glTrackMatrixNV) {
	    throw not_supported_error("NV_vertex_program extension functions not found");
	}
    }
};

/*!
 * \brief A concrete version of the abstract VertexProgram class for the NV_vertex_program
 * extension.
 *
 * This class implements the vertex program functionality using the NV_vertex_program
 * extension.
 */
class Visla::VertexProgram_NV: public VertexProgram {
private:
    //! Function pointers for the NV_vertex_program extension
    VPFunctionWrapper_NV *funcs;

    //! Program object handle
    GLuint progid;

    //! \a progid is valid
    bool valid;

    //! OpenGL vertex program target (GL_VERTEX_PROGRAM_NV, GL_VERTEX_STATE_PROGRAM_NV)
    GLenum target;

    /*!
     * \brief Generate a \a progid.
     *
     * Generate a program object handle into \a progid.
     */
    void gen() {
	if (valid) del();
	funcs->glGenProgramsNV(1, &progid);
	valid = true;
    }

    /*!
     * \brief Delete the \a progid.
     *
     * Delete the program object handle in \a progid.
     */
    void del() {
	if (valid) {
	    valid = false;
            unbind();
	    funcs->glDeleteProgramsNV(1, &progid);
	}
    }
public:
    /*!
     * \brief Create a vertex program wrapper, using the specified function pointers.
     *
     * Create a new vertex program, using the given function pointers.
     * \param target OpenGL vertex program target (\c GL_VERTEX_PROGRAM_NV, \c GL_VERTEX_STATE_PROGRAM_NV)
     * \param funcs A VPFunctionWrapper_NV containing the function pointers for
     *              the NV_vertex_program extension, or 0 for a new function wrapper.
     * \throws not_supported_error if some of the functions of the extension is not found
     *                             by the wglGetProcAddress() function.
     */
    explicit VertexProgram_NV(GLenum target = GL_VERTEX_PROGRAM_NV, const VPFunctionWrapper_NV *funcs = 0) {
	valid = false;
	this->target = target;
	if (funcs) {
	    this->funcs = new VPFunctionWrapper_NV(*funcs);
	} else {
	    this->funcs = new VPFunctionWrapper_NV();
	}
    };

    //! The destructor
    virtual ~VertexProgram_NV() {
	del();
	delete funcs;
    }

    /*!
     * \brief Get the name of the extension used
     *
     * This function can be used to determine the name of the OpenGL extension currently used
     * to work with vertex programs.
     * \return OpenGL extension name specifying the currently used extension for vertex programs
     */
    virtual const char *extension_used() const {
	return "NV_vertex_program";
    }

    /*!
     * \brief Compile a vertex program from its source in memory.
     *
     * This function fills this instance with the vertex program stored as a source text
     * in memory. The program is also bind().
     * \param program Pointer to the source text of the vertex program.
     * \param size Size of the source text of the vertex program (bytes).
     * \see bind()
     */
    virtual void compile(const GLubyte *program, GLuint size) {
	GLenum errcode;

	bind();
	funcs->glLoadProgramNV(target, progid, size, program);

	if ( (errcode = glGetError()) != GL_NO_ERROR) {
	    const GLubyte *errString = gluErrorString(errcode);
	    int errpos;
	    glGetIntegerv(GL_PROGRAM_ERROR_POSITION_NV, &errpos);
	    throw compile_error((const char*)errString, errpos);
	}
    };

    /*!
     * \brief Set this program to be currently used
     *
     * Set that this vertex program should be used now.
     * \see unbind()
     */
    virtual void bind() {
	if (!valid) gen();
	funcs->glBindProgramNV(target, progid);
    }

    /*!
     * \brief Set that no program should be used now
     *
     * Set that no vertex program should be used now. If this or any other program is now
     * current (via bind() or anything else), this method unbounds it.
     * \see bind()
     */
    virtual void unbind() const {
	funcs->glBindProgramNV(target, 0);
    }

    /*!
     * \brief Enable vertex programs
     *
     * This call uses \c glEnable to enable programmable vertex processing.
     * \see disable()
     */
    virtual void enable() const {
	glEnable(GL_VERTEX_PROGRAM_NV);
    }

    /*!
     * \brief Disable vertex programs
     *
     * This call uses \c glDisable to disable programmable vertex processing and
     * switches back to conventional T&amp;L model.
     * \see enable()
     */
    virtual void disable() const {
	glDisable(GL_VERTEX_PROGRAM_NV);
    }

    /*!
     * \brief Set vertex attribute(s)
     *
     * This call sets one or more four-valued vertex attributes.
     * \param index Index of the first vertex attribute to be set. Call with \c index = 0
     *        causes the vertex program to process the vertex.
     * \param n Number of four-valued attributes to be set by this call.
     * \param values Values, 4 * \a n float values to be set.
     */
    virtual void vertexAttrib(int index, int n, const float *values) const {
        funcs->glVertexAttribs4fvNV(index, n, values);
    }

    /*!
     * \brief Set vertex attribute using an array (vertex attribute pointer)
     *
     * This call sets vertex attribute pointer
     * \param index Index of the vertex attribute pointer to be set.
     * \param size Number of values per vertex (one of 1,2,3,4)
     * \param type Data type of values (one of \c GL_SHORT, \c GL_FLOAT,
     *        \c GL_DOUBLE, or \c GL_UNSIGNED_BYTE)
     * \param stride Size of gap between consecutive attributes (0 if tight)
     * \param pointer Pointer to the vertex attribute data.
     */
    virtual void vertexAttribPointer(GLuint index, GLint size, GLenum type,
        GLsizei stride, const GLvoid *pointer) const {
        funcs->glVertexAttribPointerNV(index, size, type, stride, pointer);
    }

    /*!
     * \brief Enable vertex attribute array for the given attribute
     *
     * This call enables the vertex attribute array usage for the given attribute
     * \param index Index of the vertex attribute pointer to be enabled.
     */
    virtual void enableAttribPointer(GLuint index) const {
        glEnableClientState(GL_VERTEX_ATTRIB_ARRAY0_NV + index);
    }

    /*!
     * \brief Disable vertex attribute array for the given attribute
     *
     * This call disables the vertex attribute array usage for the given attribute
     * \param index Index of the vertex attribute pointer to be disabled.
     */
    virtual void disableAttribPointer(GLuint index) const {
        glDisableClientState(GL_VERTEX_ATTRIB_ARRAY0_NV + index);
    };

    /*!
     * \brief Set program parameter(s)
     *
     * This call sets one or more four-valued program parameters.
     * \param index The index of the first parameter to be set.
     * \param n Number of four-valued parameters to be set by this call.
     * \param values Values, 4 * \a n float values to be set.
     */
    virtual void setProgramParams(int index, int n, const float *values) const {
        funcs->glProgramParameters4fvNV(GL_VERTEX_PROGRAM_NV, index, n, values);
        /*for (int i = 0; i < n; i++, index++, values += 4)
          funcs->glProgramParameters4fvNV(GL_VERTEX_PROGRAM_NV, index, 1, values);*/
    }

    /*!
     * \brief Enable/disable matrix tracking
     *
     * This call enables/disables tracking of standard GL matrix states into
     * program parameter vectors.
     * \param address The address of the first program parameter into which should the matrix be tracked.
     * \param matrix Which matrix should be tracked? Set to tmNone to disable matrix tracking.
     * \param transform What transform should be applied to the matrix?
     */
    virtual void trackMatrix(int address, TrackedMatrix matrix, TrackingTransform transform) const {
        static const GLenum matrix_spec_nv[] = {
                0,
                GL_NONE,
                GL_MODELVIEW,
                GL_PROJECTION,
                GL_TEXTURE,
                GL_TEXTURE0_ARB,
                GL_TEXTURE1_ARB,
                GL_TEXTURE2_ARB,
                GL_TEXTURE3_ARB,
                GL_TEXTURE4_ARB,
                GL_TEXTURE5_ARB,
                GL_TEXTURE6_ARB,
                GL_TEXTURE7_ARB,
                GL_COLOR,
                GL_MODELVIEW_PROJECTION_NV
        };
        static const GLenum transform_spec_nv[] = {
                GL_IDENTITY_NV,
                GL_INVERSE_NV,
                GL_TRANSPOSE_NV,
                GL_INVERSE_TRANSPOSE_NV
        };

        if (matrix > 0) funcs->glTrackMatrixNV(GL_VERTEX_PROGRAM_NV, address, matrix_spec_nv[matrix], transform_spec_nv[transform]);
        else funcs->glTrackMatrixNV(GL_VERTEX_PROGRAM_NV, address, GL_MATRIX0_NV - matrix, transform_spec_nv[transform]);
    }
};
#endif // SUPPORT_NV

// --------------------------------------------------------------

#ifdef SUPPORT_ARB

#ifndef GL_ARB_vertex_program
#error "ARB_vertex_program is not supported by the GL/glext.h header file."\
       "Get a newer glext.h file or #undef SUPPORT_ARB."
#endif

/*!
 * \brief A wrapper containing addresses of the extension functions for the ARB_vertex_program.
 *
 * This struct contains function pointers to the functions for the extension.
 * The function pointers are initialized in the constructor. This struct is meant to be private
 * in this library.
 */
struct Visla::VPFunctionWrapper_ARB {
    PFNGLGENPROGRAMSARBPROC glGenProgramsARB;
    PFNGLBINDPROGRAMARBPROC glBindProgramARB;
    PFNGLPROGRAMSTRINGARBPROC glProgramStringARB;
    PFNGLDELETEPROGRAMSARBPROC glDeleteProgramsARB;
    PFNGLVERTEXATTRIB4FVARBPROC glVertexAttrib4fvARB;
    PFNGLVERTEXATTRIBPOINTERARBPROC glVertexAttribPointerARB;
    PFNGLENABLEVERTEXATTRIBARRAYARBPROC glEnableVertexAttribArrayARB;
    PFNGLDISABLEVERTEXATTRIBARRAYARBPROC glDisableVertexAttribArrayARB;
    PFNGLPROGRAMENVPARAMETER4FVARBPROC glProgramEnvParameter4fvARB;
    PFNGLPROGRAMLOCALPARAMETER4FVARBPROC glProgramLocalParameter4fvARB;

    /*!
     * \brief Create the wrapper using pointers retrieved from the system.
     *
     * This constructor fills the function pointers by values retrieved by wglGetProcAddress()
     * from the OpenGL library.
     *
     * \throws not_supported_error if some of the functions of the extension is not found
     *                             by the wglGetProcAddress() function.
     */
    VPFunctionWrapper_ARB() {
        glGenProgramsARB = (PFNGLGENPROGRAMSARBPROC) wglGetProcAddress("glGenProgramsARB");
        glBindProgramARB = (PFNGLBINDPROGRAMARBPROC) wglGetProcAddress("glBindProgramARB");
        glProgramStringARB = (PFNGLPROGRAMSTRINGARBPROC) wglGetProcAddress("glProgramStringARB");
        glDeleteProgramsARB = (PFNGLDELETEPROGRAMSARBPROC) wglGetProcAddress("glDeleteProgramsARB");
        glVertexAttrib4fvARB = (PFNGLVERTEXATTRIB4FVARBPROC) wglGetProcAddress("glVertexAttrib4fvARB");
        glVertexAttribPointerARB = (PFNGLVERTEXATTRIBPOINTERARBPROC) wglGetProcAddress("glVertexAttribPointerARB");
        glEnableVertexAttribArrayARB = (PFNGLENABLEVERTEXATTRIBARRAYARBPROC) wglGetProcAddress("glEnableVertexAttribArrayARB");
        glDisableVertexAttribArrayARB = (PFNGLDISABLEVERTEXATTRIBARRAYARBPROC) wglGetProcAddress("glDisableVertexAttribArrayARB");
        glProgramEnvParameter4fvARB = (PFNGLPROGRAMENVPARAMETER4FVARBPROC) wglGetProcAddress("glProgramEnvParameter4fvARB");
        glProgramLocalParameter4fvARB = (PFNGLPROGRAMLOCALPARAMETER4FVARBPROC) wglGetProcAddress("glProgramLocalParameter4fvARB");

        if (!glGenProgramsARB || !glBindProgramARB || !glProgramStringARB ||
            !glDeleteProgramsARB || !glVertexAttrib4fvARB || !glVertexAttribPointerARB ||
            !glEnableVertexAttribArrayARB || !glDisableVertexAttribArrayARB || !glProgramEnvParameter4fvARB ||
            !glProgramLocalParameter4fvARB) {
	    throw not_supported_error("ARB_vertex_program extension functions not found");
	}
    }
};

/*!
 * \brief A concrete version of the abstract VertexProgram class for the ARB_vertex_program
 * extension.
 *
 * This class implements the vertex program functionality using the ARB_vertex_program
 * extension.
 */
class Visla::VertexProgram_ARB: public VertexProgram {
private:
    //! Function pointers for the ARB_vertex_program extension
    VPFunctionWrapper_ARB *funcs;

    //! Program object handle
    GLuint progid;

    //! \a progid is valid
    bool valid;

    //! OpenGL vertex program target (GL_VERTEX_PROGRAM_NV, GL_VERTEX_STATE_PROGRAM_NV)
    GLenum target;

    /*!
     * \brief Generate a \a progid.
     *
     * Generate a program object handle into \a progid.
     */
    void gen() {
	if (valid) del();
	funcs->glGenProgramsARB(1, &progid);
	valid = true;
    }

    /*!
     * \brief Delete the \a progid.
     *
     * Delete the program object handle in \a progid.
     */
    void del() {
	if (valid) {
	    valid = false;
            unbind();
	    funcs->glDeleteProgramsARB(1, &progid);
	}
    }
public:
    /*!
     * \brief Create a vertex program wrapper, using the specified function pointers.
     *
     * Create a new vertex program, using the given function pointers.
     * \param target OpenGL vertex program target (\c GL_VERTEX_PROGRAM_ARB, \c GL_VERTEX_STATE_PROGRAM_ARB)
     * \param funcs A FPFunctionWrapper_ARB containing the function pointers for
     *              the ARB_vertex_program extension, or 0 for a new function wrapper.
     * \throws not_supported_error if some of the functions of the extension is not found
     *                             by the wglGetProcAddress() function.
     */
    explicit VertexProgram_ARB(GLenum target = GL_VERTEX_PROGRAM_ARB, const VPFunctionWrapper_ARB *funcs = 0) {
	valid = false;
	this->target = target;
	if (funcs) {
	    this->funcs = new VPFunctionWrapper_ARB(*funcs);
	} else {
	    this->funcs = new VPFunctionWrapper_ARB();
	}
    };

    //! The destructor
    virtual ~VertexProgram_ARB() {
	del();
	delete funcs;
    }

    /*!
     * \brief Get the name of the extension used
     *
     * This function can be used to determine the name of the OpenGL extension currently used
     * to work with vertex programs.
     * \return OpenGL extension name specifying the currently used extension for vertex programs
     */
    virtual const char *extension_used() const {
	return "ARB_vertex_program";
    }

    /*!
     * \brief Compile a vertex program from its source in memory.
     *
     * This function fills this instance with the vertex program stored as a source text
     * in memory. The program is also bind().
     * \param program Pointer to the source text of the vertex program.
     * \param size Size of the source text of the vertex program (bytes).
     * \see bind()
     */
    virtual void compile(const GLubyte *program, GLuint size) {
	GLenum errcode;

	bind();
	funcs->glProgramStringARB(target, GL_PROGRAM_FORMAT_ASCII_ARB, size, program);

	if ( (errcode = glGetError()) != GL_NO_ERROR) {
	    const GLubyte *errString = glGetString(GL_PROGRAM_ERROR_STRING_ARB);
	    int errpos;
	    glGetIntegerv(GL_PROGRAM_ERROR_POSITION_ARB, &errpos);
	    throw compile_error((const char*)errString, errpos);
	}
    };

    /*!
     * \brief Set this program to be currently used
     *
     * Set that this vertex program should be used now.
     * \see unbind()
     */
    virtual void bind() {
	if (!valid) gen();
	funcs->glBindProgramARB(target, progid);
    }

    /*!
     * \brief Set that no program should be used now
     *
     * Set that no vertex program should be used now. If this or any other program is now
     * current (via bind() or anything else), this method unbounds it.
     * \see bind()
     */
    virtual void unbind() const {
	funcs->glBindProgramARB(target, 0);
    }

    /*!
     * \brief Enable vertex programs
     *
     * This call uses \c glEnable to enable programmable vertex processing.
     * \see disable()
     */
    virtual void enable() const {
	glEnable(GL_VERTEX_PROGRAM_ARB);
    }

    /*!
     * \brief Disable vertex programs
     *
     * This call uses \c glDisable to disable programmable vertex processing and
     * switches back to conventional T&amp;L model.
     * \see enable()
     */
    virtual void disable() const {
	glDisable(GL_VERTEX_PROGRAM_ARB);
    }

    /*!
     * \brief Set vertex attribute(s)
     *
     * This call sets one or more four-valued vertex attributes.
     * \param index Index of the first vertex attribute to be set. Call with \c index = 0
     *        causes the vertex program to process the vertex.
     * \param n Number of four-valued attributes to be set by this call.
     * \param values Values, 4 * \a n float values to be set.
     */
    virtual void vertexAttrib(int index, int n, const float *values) const {
        for (int i = 0; i < n; i++, index += 4, values += 4)
          funcs->glVertexAttrib4fvARB(index, values);
    }

    /*!
     * \brief Set vertex attribute using an array (vertex attribute pointer)
     *
     * This call sets vertex attribute pointer
     * \param index Index of the vertex attribute pointer to be set.
     * \param size Number of values per vertex (one of 1,2,3,4)
     * \param type Data type of values (one of \c GL_SHORT, \c GL_FLOAT,
     *        \c GL_DOUBLE, or \c GL_UNSIGNED_BYTE)
     * \param stride Size of gap between consecutive attributes (0 if tight)
     * \param pointer Pointer to the vertex attribute data.
     */
    virtual void vertexAttribPointer(GLuint index, GLint size, GLenum type,
        GLsizei stride, const GLvoid *pointer) const {
        funcs->glVertexAttribPointerARB(index, size, type, GL_FALSE, stride, pointer);
    }

    /*!
     * \brief Enable vertex attribute array for the given attribute
     *
     * This call enables the vertex attribute array usage for the given attribute
     * \param index Index of the vertex attribute pointer to be enabled.
     */
    virtual void enableAttribPointer(GLuint index) const {
        funcs->glEnableVertexAttribArrayARB(index);
    }

    /*!
     * \brief Disable vertex attribute array for the given attribute
     *
     * This call disables the vertex attribute array usage for the given attribute
     * \param index Index of the vertex attribute pointer to be disabled.
     */
    virtual void disableAttribPointer(GLuint index) const {
        funcs->glDisableVertexAttribArrayARB(index);
    }

    /*!
     * \brief Set program parameter(s)
     *
     * This call sets one or more four-valued program parameters.
     * \param index The index of the first parameter to be set.
     * \param n Number of four-valued parameters to be set by this call.
     * \param values Values, 4 * \a n float values to be set.
     */
    virtual void setProgramParams(int index, int n, const float *values) const {
        for (int i = 0; i < n; i++, index++, values += 4) {
          funcs->glProgramEnvParameter4fvARB(GL_VERTEX_PROGRAM_NV, index, values);
          funcs->glProgramLocalParameter4fvARB(GL_VERTEX_PROGRAM_NV, index, values);
        }
    }

    /*!
     * \brief Update the program parameters with a standard GL matrix.
     *
     * This call sets four program parameter vectors to contain a standard GL matrix. The
     * ARB_vertex_program does not support matrix tracking, you have to update the matrix
     * every time it changes!
     * \param address The address of the first program parameter into which should the matrix be tracked.
     * \param matrix Which matrix should be tracked? (Only tmNone, tmModelView and tmProjection are supported!)
     * \param transform What transform should be applied to the matrix? (Only ttIdentity is supported!)
     */
    virtual void trackMatrix(int address, TrackedMatrix matrix, TrackingTransform transform) const {
        GLfloat m[16];

        switch (matrix) {
        case tmNone:
            return;

        case tmModelView:
            glGetFloatv(GL_MODELVIEW_MATRIX, m);
            break;

        case tmProjection:
            glGetFloatv(GL_PROJECTION_MATRIX, m);
            break;

        case tmModelViewProjection:
            glGetFloatv(GL_MODELVIEW_MATRIX, m);
            glMatrixMode(GL_PROJECTION);
            glPushMatrix();
            glMultMatrixf(m);
            glGetFloatv(GL_PROJECTION_MATRIX, m);
            glPopMatrix();
            break;

        default:
            throw not_supported_error("Matrix tracking not supported by ARB_vertex_program");
        }

        switch (transform) {
        case ttTranspose:
            // Nothing, the matrix gets transposed in the process of glGetFloatv(...) + setProgramParams(...)
            break;

        case ttIdentity:
            {
                // Because the matrix gets transposed, we must transpose it back...
                for (int i = 0; i < 4; i++) {
                    for (int j = 0; j < i; j++) {
                        float tmp = m[4*i + j];
                        m[4*i + j] = m[4*j + i];
                        m[4*j + i] = tmp;
                    }
                }
                break;
            }

        default:
            throw not_supported_error("Inverse matrix tracking not supported by ARB_vertex_program");
        }

        setProgramParams(address, 4, m);
    }
};
#endif  // SUPPORT_ARB

// ---------------------- VertexProgram -------------------------

/*!
 * This function fills this instance with the vertex program stored as a source text
 * in memory, that is zero terminated.
 * \param program Pointer to the zero-terminated source text of the vertex program.
 * \throws compile_error if the compilation fails.
 */
void VertexProgram::compile(const GLubyte *program) {
    compile(program, strlen((const char *)program));
}

/*!
 * This function fills this instance with the vertex program stored as a source text
 * in input stream. The rest of the stream from the current position is read and
 * then compiled.
 * \param stream An input stream containing the source text. The stream is required
 *               to be binary (no conversions!) and forward/backwards seekable.
 * \throws compile_error if the compilation fails.
 */
void VertexProgram::compile(istream &stream) {
    size_t oldpos, size;
    // get size and read the rest of the stream
    oldpos = stream.tellg();
    stream.seekg(0, ios_base::end);
    if (stream.fail())
	throw compile_error("Unable to read stream");
    size = 1 + stream.tellg();              // +1 for the NUL char
    stream.seekg(oldpos, ios_base::beg);
    auto_ptr<GLubyte> program(new GLubyte[size]);
    stream.get((istream::char_type*)(program.get()), size, stream.widen('\0'));
    if (stream.fail())
	throw compile_error("Unable to read stream");
    compile(program.get(), size - 1);       // +1 for the NUL char
};

#ifdef SUPPORT_ARB
static bool ARB_supported;
static VPFunctionWrapper_ARB *ARB_functions;
#endif
#ifdef SUPPORT_EXT
static bool EXT_supported;
static VPFunctionWrapper_EXT *EXT_functions;
#endif
#ifdef SUPPORT_NV
static bool NV_supported;
static VPFunctionWrapper_NV *NV_functions;
#endif

/*!
 * \brief Check supported extensions.
 *
 * This functions fills the global xxx_supported, xxx_functions variables.
 */
inline static void checkSupport()
{
    static bool support_detected = false;

    if (!support_detected) {
	// detect support
        #ifdef SUPPORT_ARB
	ARB_supported = isExtensionSupported("GL_ARB_vertex_program");
	if (ARB_supported)
	    ARB_functions = new VPFunctionWrapper_ARB;
        #endif
        #ifdef SUPPORT_EXT
	EXT_supported = isExtensionSupported("GL_EXT_vertex_shader");
	if (EXT_supported)
	    EXT_functions = new VPFunctionWrapper_EXT;
        #endif
        #ifdef SUPPORT_NV
	NV_supported  = isExtensionSupported("GL_NV_vertex_program");
	if (NV_supported)
	    NV_functions = new VPFunctionWrapper_NV;
        #endif
    }
}

/*!
 * \return true if any known extension for vertex program is supported, false otherwise
 */
bool VertexProgram::isSupported()
{
    checkSupport();
    return false
#ifdef SUPPORT_ARB
        || ARB_supported
#endif
#ifdef SUPPORT_EXT
        || EXT_supported
#endif
#ifdef SUPPORT_NV
        || NV_supported
#endif
        ;
}

/*!
 * Create an instance of a concrete VertexProgram subclass that is supported on this system.
 * If no known extension for vertex programs is supported on this system, an exception is thrown.
 *
 * \return Pointer to a new instance of some VertexProgram subclass. You must call \c delete on it
 *         after you are finished with it.
 * \throws not_supported_error if no known extension for vertex programs is supported on this system.
 */
VertexProgram *VertexProgram::create()
{
    checkSupport();

    #ifdef SUPPORT_ARB
    if (ARB_supported)
	return new VertexProgram_ARB(GL_VERTEX_PROGRAM_ARB, ARB_functions);
    #endif
    #ifdef SUPPORT_EXT
    if (EXT_supported)
	return new VertexProgram_EXT(EXT_functions);
    #endif
    #ifdef SUPPORT_NV
    if (NV_supported)
	return new VertexProgram_NV(GL_VERTEX_PROGRAM_NV, NV_functions);
    #endif
    throw not_supported_error("No known extension for vertex programs is supported.");
}
