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


#ifndef __FRAGPROG_H_
#define __FRAGPROG_H_

/*!
 * \file fragprog.h
 * \author Petr Kadlec
 * \brief Fragment Program support header file.
 *
 * This file contains interface to Fragment Program support wrapper classes.
 */

#include <cstring>
#include <istream>

#include "bothprog.h"

using namespace std;

namespace Visla {
    class FragmentProgram;
};

/*!
 * \brief A generic fragment program wrapper
 *
 * This is a generic wrapper for a fragment program. This is an abstract class, that does not
 * implement any fragment program functionality.
 * Its descendants implement the functionality using various extensions.
 */
class Visla::FragmentProgram {
protected:
    FragmentProgram() {};           //!< the default constructor is protected, this class may not be instantiated

    /*!
     * \brief Copy constructor -- <b>INSTANCES OF THIS CLASS MAY NOT BE COPIED.</b>
     *
     * This copy constructor throws immediately a \c logic_error exception.
     * \warning Instances of this class or subclasses may not be assigned or copied
     *		using the copy constructor.
     * \throws logic_error immediately
     */
    explicit FragmentProgram(const FragmentProgram&) {
        throw logic_error("FragmentPrograms may not be copied");
    }
    
    /*!
     * \brief Assignment operator -- <b>INSTANCES OF THIS CLASS MAY NOT BE ASSIGNED.</b>
     *
     * This assignment operator throws immediately a \c logic_error exception.
     * \warning Instances of this class or subclasses may not be assigned or copied
     *		using the copy constructor.
     * \throws logic_error immediately
     */
    FragmentProgram& operator = (const FragmentProgram&) {
        throw logic_error("FragmentPrograms may not be copied");
    }
public:
    virtual ~FragmentProgram() {};  //!< a generic destructor, does nothing
    
    // --- abstract functions - fragment program interface
    
    /*!
     * \brief Get the name of the extension used
     *
     * This function can be used to determine the name of the OpenGL extension currently used
     * to work with fragment programs.
     * \return OpenGL extension name specifying the currently used extension for fragment programs
     */
    virtual const char *extension_used() const = 0;
    
    /*!
     * \brief Compile a fragment program from its source in memory.
     *
     * This function fills this instance with the fragment program stored as a source text
     * in memory.
     * \param program Pointer to the source text of the fragment program.
     * \param size Size of the source text of the fragment program (bytes).
     * \throws compile_error if the compilation fails.
     */
    virtual void compile(const GLubyte *program, GLuint size) = 0;

    void compile(const GLubyte *program);   //!< Compile a fragment program from its source in memory.
    void compile(std::istream &stream);	    //!< compile a program from stream

    /*!
     * \brief Set this program to be currently used
     *
     * Set that this fragment program should be used now.
     * \see compile
     */
    virtual void bind() = 0;

    /*!
     * \brief Set that no program should be used now
     *
     * Set that no fragment program should be used now. If this or any other program is now
     * current (via bind() or anything else), this method unbounds it.
     * \see bind()
     */
    virtual void unbind() const = 0;

    /*!
     * \brief Enable fragment programs
     *
     * This call uses \c glEnable to enable programmable fragment processing.
     * \see disable()
     */
    virtual void enable() const = 0;

    /*!
     * \brief Disable fragment programs
     *
     * This call uses \c glDisable to disable programmable fragment processing and
     * switches back to conventional texturing model.
     * \see enable()
     */
    virtual void disable() const = 0;

    // --- fragment program inputs

    /*!
     * \brief Set program parameter(s)
     *
     * This call sets one or more four-valued program parameters.
     * \param index The index of the first parameter to be set.
     * \param n Number of four-valued parameters to be set by this call.
     * \param values Values, 4 * \a n float values to be set.
     */
    virtual void setProgramParams(int index, int n, const float *values) const = 0;

    //! \todo maybe some setresident()

    // --- special functions

    static bool isSupported();                              //!< Are fragment programs supported on this system?
    static FragmentProgram *create();			    //!< create a concrete instance of some subclass
};

#endif /* ifndef __FRAGPROG_H_ */
