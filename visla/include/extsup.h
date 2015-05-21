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


#ifndef __EXTSUP_H_
#define __EXTSUP_H_

/*!
 * \file extsup.h
 * \author Petr Kadlec
 * \brief GL Extensions support utilities
 *
 * This file contains some utilities for dealing with GL extensions.
 */

#include <exception>
#include <iostream>

namespace Visla {
    class not_supported_error;

    //! Is an OpenGL extension supported on this system?
    bool isExtensionSupported(const char *extension);

#ifdef _WIN32
    //! Is a WGL extension supported on this system?
    bool isWGLExtensionSupported(const char *extension);
#endif
};
using namespace std;

/*!
 * \brief Exception signalling that some requested feature is not supported.
 *
 * not_supported_error will be thrown whenever some function require a feature
 * that is not supported by the OpenGL library on the system.
 */
class Visla::not_supported_error: public runtime_error {
public:
    /*!
     * \brief Create the exception with the given error message.
     *
     * \param what_arg A message describing the problem.
     */
    explicit not_supported_error(const string& what_arg) : runtime_error(what_arg) { };
};

#endif // #ifndef __EXTSUP_H_
