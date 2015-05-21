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


#ifndef __BOTHPROG_H_
#define __BOTHPROG_H_

/*!
 * \file bothprog.h
 * \author Petr Kadlec
 * \brief Vertex/Fragment Programs support header file.
 *
 * This file contains some utilities common to both Vertex and Fragment Programs.
 */

#include <exception>
#include <iostream>

namespace Visla {
    class compile_error;
};
using namespace std;

/*!
 * \brief Exception signalling that vertex/fragment program compilation failed.
 *
 * compile_error will be thrown when a vertex/fragment program compilation failed. The error
 * message will contain more details.
 */
class Visla::compile_error: public exception {
    /*!
     * \brief Position of the program error
     *
     * This variable contains the position of the error detected by the program compiler.
     * It is an GLubyte offset into the compiled program indicating where is the
     * first error in the program.\n
     * If the program fails to compile because of some semantic restriction that
     * cannot be determined until the program is completely scanned, the error position
     * is equal to the length of the program.\n
     * If the error position is unknown, this variable has a value of minus one.
     */
    int errpos;
public:
    /*!
     * \brief Create the exception with the given error message.
     *
     * \param what_arg A message describing the problem.
     */
    explicit compile_error(const char* what_arg, int errpos = -1) : exception(what_arg), errpos(errpos) { };

    /*!
     * \brief Retrieve the error position
     *
     * This function retrieves the \a errpos variable.
     * \return The position of the error detected by the program compiler.
     * It is an GLubyte offset into the compiled program indicating where is the
     * first error in the program.\n
     * If the program fails to compile because of some semantic restriction that
     * cannot be determined until the program is completely scanned, the error position
     * is equal to the length of the program.\n
     * If the error position is unknown, the function returns a value of minus one.
     */
    int get_errpos() const { return errpos; }
};

#endif /* ifndef __BOTHPROG_H_ */
