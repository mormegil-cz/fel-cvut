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
 * \file vector.cpp
 * \author Petr Kadlec
 * \brief TVector3 implementation
 *
 * This file contains implementation for operations on Visla::TVector3 class.
 */

/*!
 * \class Visla::TVector3
 * The TVector3 class represents a general 3D vector, stored in cartesian coordinates (x,y,z).
 * There are operations for all standard math (addition, subtraction, dot product, cross product,
 * etc.).\n
 * The operations are generally given in two flavors:
 *   - overloaded operators that return the result by value.
 *   - named static functions that operate on two given vectors and store the result
 *     into another \b given vector. This version is usually a bit faster, because no copying
 *     of the result is required.
 */

/* min() and max() functions, normally in <algorithm>, I have not found them in MSVC++ */
#ifdef _MSC_VER
  template <typename T> inline const T& min (const T& a, const T& b)
  {
      return b < a ? b : a;
  }
  
  template <typename T> inline const T& max (const T& a, const T& b)
  {
      return a < b ? b : a;
  }
#else
#include <algorithm>
#endif
  
#include <cmath>
#include "visla.h"

using namespace std;
using namespace Visla;

/*!
 * Initializes the vector to (0,0,0).
 */
TVector3::TVector3()
{
    data.vec.x = 0.0f;
    data.vec.y = 0.0f;
    data.vec.z = 0.0f;
}

/*!
 * Initializes the vector with the given values.
 * \param x The x component of the vector.
 * \param y The y component of the vector.
 * \param z The z component of the vector.
 */
TVector3::TVector3(float_t x, float_t y, float_t z)
{
    data.vec.x = x;
    data.vec.y = y;
    data.vec.z = z;
}

#if 0
/*!
 * Initializes this vector to a copy of another one.
 * \param v The vector that should be copied into this vector.
 */
TVector3::TVector3(const TVector3 &v)
{
    data = v.data;
}
#endif

/*!
 * Outputs a vector to an output stream in human-readable format "(\a x, \a y, \a z)".
 * \param s The output stream to which should the vector be printed.
 * \param v The vector that is to be printed.
 * \return \c s, after the vector has been printed.
 */
ostream& Visla::operator << (ostream &s, const TVector3 &v)
{
    return s << '(' << v.data.vec.x << ", " << v.data.vec.y << ", " << v.data.vec.z << ')';
}

/*!
 * Inputs a vector from an input stream in textual format (reads the \a x, \a y, \a z components).
 * \param s The input stream from which should the vector be read.
 * \param v The vector that is to be read.
 * \return \c s, after the vector has been read.
 */
istream& Visla::operator >> (istream &s, TVector3 &v)
{
    return s >> v.data.vec.x >> v.data.vec.y >> v.data.vec.z;
}

#ifdef SUPPORT_LEGACY_2D
// ------------------------------ LEGACY_2D ---------------------------------

/*!
 * Initializes the vector with the values from the legacy 2D vector and the possibly given \a z component, that
 * is missing in the 2D vector.
 * \param v The 2D vector.
 * \param z The z component for this vector.
 */
TVector3::TVector3(const ::Tvector &v, float_t z)
{
    data.vec.x = v.x;
    data.vec.y = v.y;
    data.vec.z = z;
}

/*!
 * Initializes the vector with the values from the legacy 2D vector and the possibly given \a z component, that
 * is missing in the 2D vector.
 * \param v The 2D intvector.
 * \param z The z component for this vector.
 */
TVector3::TVector3(const ::Tintvector &v, float_t z)
{
    data.vec.x = v.x;
    data.vec.y = v.y;
    data.vec.z = z;
}

#endif  // ifdef SUPPORT_LEGACY_2D
