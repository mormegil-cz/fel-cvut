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


#ifndef __VERTPROG_H_
#define __VERTPROG_H_

/*!
 * \file vertprog.h
 * \author Petr Kadlec
 * \brief Vertex Program support header file.
 *
 * This file contains interface to Vertex Program support wrapper classes.
 */

#include <cstring>
#include <istream>

#include "bothprog.h"

// --- namespace declaration
namespace Visla {
    class VertexProgram;
};

using namespace std;

/*!
 * \brief A generic vertex program wrapper
 *
 * This is a generic wrapper for a vertex program. This is an abstract class, that does not
 * implement any vertex program functionality.
 * Its descendants implement the functionality using various extensions.
 */
class Visla::VertexProgram {
protected:
    VertexProgram() {}		    //!< the default constructor is protected, this class may not be instantiated
public:
    /*!
     * Matrix to be tracked.
     * \see trackMatrix
     */
    enum TrackedMatrix {
        tmNone = 1,                 //!< disable matrix tracking
        tmModelView,                //!< modelview matrix
        tmProjection,               //!< projection matrix
        tmTexture,                  //!< texture transformation matrix
        tmTexture0,                 //!< TEXTURE0_ARB
        tmTexture1,                 //!< TEXTURE1_ARB
        tmTexture2,                 //!< TEXTURE2_ARB
        tmTexture3,                 //!< TEXTURE3_ARB
        tmTexture4,                 //!< TEXTURE4_ARB
        tmTexture5,                 //!< TEXTURE5_ARB
        tmTexture6,                 //!< TEXTURE6_ARB
        tmTexture7,                 //!< TEXTURE7_ARB
        tmColor,                    //!< if the ARB_imaging subset is supported
        tmModelViewProjection       //!< P * M (projection * modelview)
    };

    /*!
     * Transformation of a tracked matrix.
     * \see trackMatrix
     */
    enum TrackingTransform {
        ttIdentity,                 //!< The specified matrix (no transformation)
        ttInverse,                  //!< Inverse of the specified matrix
        ttTranspose,                //!< Transpose of the specified matrix
        ttInverseTranspose          //!< Transpose of the inverse of the specified matrix
    };
    
    virtual ~VertexProgram() {};    //!< a generic destructor, does nothing

    /*!
     * \brief Copy constructor -- <b>INSTANCES OF THIS CLASS MAY NOT BE COPIED.</b>
     *
     * This copy constructor throws immediately a \c logic_error exception.
     * \warning Instances of this class or subclasses may not be assigned or copied
     *		using the copy constructor.
     * \throws logic_error immediately
     */
    explicit VertexProgram(const VertexProgram&) {
	throw logic_error("VertexPrograms may not be copied");
    }

    /*!
     * \brief Assignment operator -- <b>INSTANCES OF THIS CLASS MAY NOT BE ASSIGNED.</b>
     *
     * This assignment operator throws immediately a \c logic_error exception.
     * \warning Instances of this class or subclasses may not be assigned or copied
     *		using the copy constructor.
     * \throws logic_error immediately
     */
    VertexProgram& operator = (const VertexProgram&) {
	throw logic_error("VertexPrograms may not be copied");
    }

    // --- abstract functions - vertex program interface

    /*!
     * \brief Get the name of the extension used
     *
     * This function can be used to determine the name of the OpenGL extension currently used
     * to work with vertex programs.
     * \return OpenGL extension name specifying the currently used extension for vertex programs
     */
    virtual const char *extension_used() const = 0;

    /*!
     * \brief Compile a vertex program from its source in memory.
     *
     * This function fills this instance with the vertex program stored as a source text
     * in memory.
     * \param program Pointer to the source text of the vertex program.
     * \param size Size of the source text of the vertex program (bytes).
     * \throws compile_error if the compilation fails.
     */
    virtual void compile(const GLubyte *program, GLuint size) = 0;

    void compile(const GLubyte *program);   //!< Compile a vertex program from its source in memory.
    void compile(std::istream &stream);	    //!< compile a program from stream

    /*!
     * \brief Set this program to be currently used
     *
     * Set that this vertex program should be used now.
     * \see compile
     */
    virtual void bind() = 0;

    /*!
     * \brief Set that no program should be used now
     *
     * Set that no vertex program should be used now. If this or any other program is now
     * current (via bind() or anything else), this method unbounds it.
     * \see bind()
     */
    virtual void unbind() const = 0;

    /*!
     * \brief Enable vertex programs
     *
     * This call uses \c glEnable to enable programmable vertex processing.
     * \see disable()
     */
    virtual void enable() const = 0;

    /*!
     * \brief Disable vertex programs
     *
     * This call uses \c glDisable to disable programmable vertex processing and
     * switches back to conventional T&amp;L model.
     * \see enable()
     */
    virtual void disable() const = 0;

    // --- vertex program inputs

    /*!
     * \brief Set vertex attribute(s)
     *
     * This call sets one or more four-valued vertex attributes.
     * \param index Index of the first vertex attribute to be set. Call with \c index = 0
     *        causes the vertex program to process the vertex.
     * \param n Number of four-valued attributes to be set by this call.
     * \param values Values, 4 * \a n float values to be set.
     */
    virtual void vertexAttrib(int index, int n, const float *values) const = 0;

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
        GLsizei stride, const GLvoid *pointer) const = 0;

    /*!
     * \brief Enable vertex attribute array for the given attribute
     *
     * This call enables the vertex attribute array usage for the given attribute
     * \param index Index of the vertex attribute pointer to be enabled.
     */
    virtual void enableAttribPointer(GLuint index) const = 0;

    /*!
     * \brief Disable vertex attribute array for the given attribute
     *
     * This call disables the vertex attribute array usage for the given attribute
     * \param index Index of the vertex attribute pointer to be disabled.
     */
    virtual void disableAttribPointer(GLuint index) const = 0;

    /*!
     * \brief Set program parameter(s)
     *
     * This call sets one or more four-valued program parameters.
     * \param index The index of the first parameter to be set.
     * \param n Number of four-valued parameters to be set by this call.
     * \param values Values, 4 * \a n float values to be set.
     */
    virtual void setProgramParams(int index, int n, const float *values) const = 0;
    
    /*!
     * \brief Enable/disable matrix tracking
     *
     * This call enables/disables tracking of standard GL matrix states into
     * program parameter vectors.
     * \param address The address of the first program parameter into which should the matrix be tracked.
     * \param matrix Which matrix should be tracked? Set to tmNone to disable matrix tracking. Use values 0, -1, -2, ...
     *               integer values to specify absolute matrix indices (0, 1, 2, ...).
     * \param transform What transform should be applied to the matrix?
     */
    virtual void trackMatrix(int address, TrackedMatrix matrix, TrackingTransform transform = ttIdentity) const = 0;

    //! \todo maybe some setresident()

    // --- special functions

    static bool isSupported();                              //!< Are vertex programs supported on this system?
    static VertexProgram *create();			    //!< create a concrete instance of some subclass
};

#endif /* ifndef __VERTPROG_H_ */
