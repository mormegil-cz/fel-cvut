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
 * \file fragprog.cpp
 * \author Petr Kadlec
 * \brief Fragment Program support.
 *
 * This file contains implementation of Fragment Program support wrapper classes.
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
#include <GL/glext.h>

// --- includes - C++
#include <exception>
#include <memory>
#include <iostream>
#include <cstring>

// --- includes - Visla
#include "extsup.h"
#include "fragprog.h"

// --- preprocessor defines

/*! \def SUPPORT_ARB
 * \brief Should we support ARB_fragment_program ?
 *
 * If \c SUPPORT_ARB is defined, this library supports the ARB_fragment_program extension.
 */
#define SUPPORT_ARB
/*! \def SUPPORT_ATI
 * \brief Should we support ATI_fragment_shader ?
 *
 * If \c SUPPORT_ATI is defined, this library supports the ATI_fragment_shader extension.
 */
#undef SUPPORT_ATI
/*! \def SUPPORT_NV
 * \brief Should we support NV_fragment_program ?
 *
 * If \c SUPPORT_NV is defined, this library supports the NV_fragment_program extension.
 */
#define SUPPORT_NV

// --- namespace declaration
namespace Visla {
#ifdef SUPPORT_ARB
    struct FPFunctionWrapper_ARB;
    class FragmentProgram_ARB;
#endif
#ifdef SUPPORT_NV
    struct FPFunctionWrapper_NV;
    class FragmentProgram_NV;
#endif
};

using namespace std;
using namespace Visla;

// --- concrete class implementations

#ifdef SUPPORT_ARB

#ifndef GL_ARB_fragment_program
#error "ARB_fragment_program is not supported by the GL/glext.h header file."\
       "Get a newer glext.h file or #undef SUPPORT_ARB."
#endif

/*!
 * \brief A wrapper containing addresses of the extension functions for the ARB_fragment_program.
 *
 * This struct contains function pointers to the functions for the extension.
 * The function pointers are initialized in the constructor. This struct is meant to be private
 * in this library.
 */
struct Visla::FPFunctionWrapper_ARB {
    PFNGLGENPROGRAMSARBPROC glGenProgramsARB;
    PFNGLBINDPROGRAMARBPROC glBindProgramARB;
    PFNGLPROGRAMSTRINGARBPROC glProgramStringARB;
    PFNGLDELETEPROGRAMSARBPROC glDeleteProgramsARB;
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
    FPFunctionWrapper_ARB() {
        glGenProgramsARB = (PFNGLGENPROGRAMSARBPROC) wglGetProcAddress("glGenProgramsARB");
        glBindProgramARB = (PFNGLBINDPROGRAMARBPROC) wglGetProcAddress("glBindProgramARB");
        glProgramStringARB = (PFNGLPROGRAMSTRINGARBPROC) wglGetProcAddress("glProgramStringARB");
        glDeleteProgramsARB = (PFNGLDELETEPROGRAMSARBPROC) wglGetProcAddress("glDeleteProgramsARB");
        glProgramLocalParameter4fvARB = (PFNGLPROGRAMLOCALPARAMETER4FVARBPROC) wglGetProcAddress("glProgramLocalParameter4fvARB");

        if (!glGenProgramsARB || !glBindProgramARB || !glProgramStringARB ||
            !glDeleteProgramsARB || !glProgramLocalParameter4fvARB) {
	    throw not_supported_error("ARB_fragment_program extension functions not found");
	}
    }
};

/*!
 * \brief A concrete version of the abstract VertexProgram class for the ARB_fragment_program
 * extension.
 *
 * This class implements the vertex program functionality using the ARB_fragment_program
 * extension.
 */
class Visla::FragmentProgram_ARB: public Visla::FragmentProgram {
private:
    //! Function pointers for the ARB_fragment_program extension
    FPFunctionWrapper_ARB *funcs;

    //! Program object handle
    GLuint progid;

    //! \a progid is valid
    bool valid;

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
            unbind();
	    funcs->glDeleteProgramsARB(1, &progid);
	    valid = false;
	}
    }
public:
    /*!
     * \brief Create a fragment program wrapper, using the specified function pointers.
     *
     * Create a new fragment program, using the given function pointers.
     * \param funcs A FPFunctionWrapper_ARB containing the function pointers for
     *              the ARB_fragment_program extension, or 0 for a new function wrapper.
     * \throws not_supported_error if some of the functions of the extension is not found
     *                             by the wglGetProcAddress() function.
     */
    explicit FragmentProgram_ARB(const FPFunctionWrapper_ARB *funcs = 0) {
	valid = false;
	if (funcs) {
	    this->funcs = new FPFunctionWrapper_ARB(*funcs);
	} else {
	    this->funcs = new FPFunctionWrapper_ARB();
	}
    };

    //! The destructor
    virtual ~FragmentProgram_ARB() {
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
	return "ARB_fragment_program";
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
	funcs->glProgramStringARB(GL_FRAGMENT_PROGRAM_ARB, GL_PROGRAM_FORMAT_ASCII_ARB, size, program);

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
	funcs->glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, progid);
    }

    /*!
     * \brief Set that no program should be used now
     *
     * Set that no vertex program should be used now. If this or any other program is now
     * current (via bind() or anything else), this method unbounds it.
     * \see bind()
     */
    virtual void unbind() const {
	funcs->glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, 0);
    }

    /*!
     * \brief Enable vertex programs
     *
     * This call uses \c glEnable to enable programmable vertex processing.
     * \see disable()
     */
    virtual void enable() const {
	glEnable(GL_FRAGMENT_PROGRAM_ARB);
    }

    /*!
     * \brief Disable vertex programs
     *
     * This call uses \c glDisable to disable programmable vertex processing and
     * switches back to conventional T&amp;L model.
     * \see enable()
     */
    virtual void disable() const {
	glDisable(GL_FRAGMENT_PROGRAM_ARB);
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
        for (int i = 0; i < n; i++, index++, values += 4)
          funcs->glProgramLocalParameter4fvARB(GL_FRAGMENT_PROGRAM_ARB, index, values);
    }
};
#endif // SUPPORT_ARB

#ifdef SUPPORT_NV

#ifndef GL_NV_fragment_program
#error "NV_fragment_program is not supported by the GL/glext.h header file."\
       "Get a newer glext.h file or #undef SUPPORT_NV."
#endif

/*!
 * \brief A wrapper containing addresses of the extension functions for the NV_fragment_program.
 *
 * This struct contains function pointers to the functions for the extension.
 * The function pointers are initialized in the constructor. This struct is meant to be private
 * in this library.
 */
struct Visla::FPFunctionWrapper_NV {
    PFNGLGENPROGRAMSNVPROC glGenProgramsNV;
    PFNGLBINDPROGRAMNVPROC glBindProgramNV;
    PFNGLLOADPROGRAMNVPROC glLoadProgramNV;
    PFNGLDELETEPROGRAMSNVPROC glDeleteProgramsNV;
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
    FPFunctionWrapper_NV() {
        glGenProgramsNV = (PFNGLGENPROGRAMSNVPROC) wglGetProcAddress("glGenProgramsNV");
        glBindProgramNV = (PFNGLBINDPROGRAMNVPROC) wglGetProcAddress("glBindProgramNV");
        glLoadProgramNV = (PFNGLLOADPROGRAMNVPROC) wglGetProcAddress("glLoadProgramNV");
        glDeleteProgramsNV = (PFNGLDELETEPROGRAMSNVPROC) wglGetProcAddress("glDeleteProgramsNV");
        glProgramLocalParameter4fvARB = (PFNGLPROGRAMLOCALPARAMETER4FVARBPROC) wglGetProcAddress("glProgramLocalParameter4fvARB");

        if (!glGenProgramsNV || !glBindProgramNV || !glLoadProgramNV ||
            !glDeleteProgramsNV || !glProgramLocalParameter4fvARB) {
	    throw not_supported_error("NV_fragment_program extension functions not found");
	}
    }
};

/*!
 * \brief A concrete version of the abstract VertexProgram class for the NV_fragment_program
 * extension.
 *
 * This class implements the vertex program functionality using the NV_fragment_program
 * extension.
 */
class Visla::FragmentProgram_NV: public Visla::FragmentProgram {
private:
    //! Function pointers for the NV_fragment_program extension
    FPFunctionWrapper_NV *funcs;

    //! Program object handle
    GLuint progid;

    //! \a progid is valid
    bool valid;

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
            unbind();
	    funcs->glDeleteProgramsNV(1, &progid);
	    valid = false;
	}
    }
public:
    /*!
     * \brief Create a fragment program wrapper, using the specified function pointers.
     *
     * Create a new fragment program, using the given function pointers.
     * \param funcs A FPFunctionWrapper_NV containing the function pointers for
     *              the NV_fragment_program extension, or 0 for a new function wrapper.
     * \throws not_supported_error if some of the functions of the extension is not found
     *                             by the wglGetProcAddress() function.
     */
    explicit FragmentProgram_NV(const FPFunctionWrapper_NV *funcs = 0) {
	valid = false;
	if (funcs) {
	    this->funcs = new FPFunctionWrapper_NV(*funcs);
	} else {
	    this->funcs = new FPFunctionWrapper_NV();
	}
    };

    //! The destructor
    virtual ~FragmentProgram_NV() {
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
	return "NV_fragment_program";
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
	funcs->glLoadProgramNV(GL_FRAGMENT_PROGRAM_NV, progid, size, program);

	if ( (errcode = glGetError()) != GL_NO_ERROR) {
	    const GLubyte *errString = glGetString(GL_PROGRAM_ERROR_STRING_NV);
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
	funcs->glBindProgramNV(GL_FRAGMENT_PROGRAM_NV, progid);
    }

    /*!
     * \brief Set that no program should be used now
     *
     * Set that no vertex program should be used now. If this or any other program is now
     * current (via bind() or anything else), this method unbounds it.
     * \see bind()
     */
    virtual void unbind() const {
	funcs->glBindProgramNV(GL_FRAGMENT_PROGRAM_NV, 0);
    }

    /*!
     * \brief Enable vertex programs
     *
     * This call uses \c glEnable to enable programmable vertex processing.
     * \see disable()
     */
    virtual void enable() const {
	glEnable(GL_FRAGMENT_PROGRAM_NV);
    }

    /*!
     * \brief Disable vertex programs
     *
     * This call uses \c glDisable to disable programmable vertex processing and
     * switches back to conventional T&amp;L model.
     * \see enable()
     */
    virtual void disable() const {
	glDisable(GL_FRAGMENT_PROGRAM_NV);
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
        for (int i = 0; i < n; i++, index++, values += 4)
          funcs->glProgramLocalParameter4fvARB(GL_FRAGMENT_PROGRAM_NV, index, values);
    }
};
#endif // SUPPORT_NV

// ---------------------- FragmentProgram -------------------------

/*!
 * This function fills this instance with the vertex program stored as a source text
 * in memory, that is zero terminated.
 * \param program Pointer to the zero-terminated source text of the vertex program.
 * \throws compile_error if the compilation fails.
 */
void FragmentProgram::compile(const GLubyte *program) {
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
void FragmentProgram::compile(istream &stream) {
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
static FPFunctionWrapper_ARB *ARB_functions;
#endif
#ifdef SUPPORT_ATI
static bool ATI_supported;
static FPFunctionWrapper_ATI *ATI_functions;
#endif
#ifdef SUPPORT_NV
static bool NV_supported;
static FPFunctionWrapper_NV *NV_functions;
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
	ARB_supported = isExtensionSupported("GL_ARB_fragment_program");
	if (ARB_supported)
	    ARB_functions = new FPFunctionWrapper_ARB;
        #endif
        #ifdef SUPPORT_ATI
	ATI_supported = isExtensionSupported("GL_ATI_fragment_shader");
	if (ATI_supported)
	    ATI_functions = new FPFunctionWrapper_ATI;
        #endif
        #ifdef SUPPORT_NV
	NV_supported  = isExtensionSupported("GL_NV_fragment_program");
	if (NV_supported)
	    NV_functions = new FPFunctionWrapper_NV;
        #endif
    }
}

/*!
 * \return true if any known extension for vertex program is supported, false otherwise
 */
bool FragmentProgram::isSupported()
{
    checkSupport();
    return false
#ifdef SUPPORT_ARB
        || ARB_supported
#endif
#ifdef SUPPORT_ATI
        || EXT_supported
#endif
#ifdef SUPPORT_NV
        || NV_supported
#endif
        ;
}

/*!
 * Create an instance of a concrete FragmentProgram subclass that is supported on this system.
 * If no known extension for fragment programs is supported on this system, an exception is thrown.
 *
 * \return Pointer to a new instance of some FragmentProgram subclass. You must call \c delete on it
 *         after you are finished with it.
 * \throws not_supported_error if no known extension for fragment programs is supported on this system.
 */
FragmentProgram *FragmentProgram::create()
{
    checkSupport();

    #ifdef SUPPORT_ARB
    if (ARB_supported)
	return new FragmentProgram_ARB(ARB_functions);
    #endif
    #ifdef SUPPORT_ATI
    if (ATI_supported)
	return new FragmentProgram_ATI(ATI_functions);
    #endif
    #ifdef SUPPORT_NV
    if (NV_supported)
	return new FragmentProgram_NV(NV_functions);
    #endif
    throw not_supported_error("No known extension for fragment programs is supported.");
}
