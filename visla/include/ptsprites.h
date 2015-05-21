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


#ifndef __PTSPRITES_H_
#define __PTSPRITES_H_

/*!
 * \file ptsprites.h
 * \author Petr Kadlec
 * \brief Point Sprites support header file.
 *
 * This file contains interface to Point Sprite support.
 */

using namespace std;

// --- namespace declaration
namespace Visla {
    /*!
     * \brief Are point sprites supported? (private)
     */
    extern bool pointSpritesAreSupported;
    
    /*!
     * \brief Initialize the point sprite support
     */
    void initPointSprites();                    
    /*!
     * \brief Check whether the point sprites are supported by the system
     */
    inline bool pointSpritesSupported();        
    
    /*!
     * \brief Enable point sprite rendering
     */
    void enablePointSprites(const int texUnit = 0);
    /*!
     * \brief Disable point sprite rendering
     */
    void disablePointSprites();                     
};

// --- inline functions

/*!
 * \return true if the point sprites are supported on the system.
 */
bool Visla::pointSpritesSupported()
{
    return pointSpritesAreSupported;
}

#endif /* ifndef __PTSPRITES_H_ */
