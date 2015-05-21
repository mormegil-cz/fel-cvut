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


#include <visla.h>
#include <mgl.h>

using namespace Visla;

/*!
 * The default settings after construction: setGenTexSize(256), setGenTexFilter(GL_NEAREST),
 * setGenTexEquidistance(16), setGenIsolineColor(mglBlack)
 * \param vis The Visualiser that will be used with this cache
 */
ValueTextureCache::ValueTextureCache(Visualiser *vis) : vis(vis), cachedTextures(),
    cachedTexturesIso(), genTexSize(256), genTexFilter(GL_NEAREST), genTexEquidistance(16),
    genIsolineColor(mglBlack)
{
}

/*!
 * Deletes all textures from the cache, deallocates all memory and destroys the object.
 */
ValueTextureCache::~ValueTextureCache()
{
    clear();
}

/*!
 * If a texture corresponding to the given palette function has already been generated
 * and is in the cache, the texture is returned. Otherwise, the texture is generated
 * and stored into the cache, then returned.
 * \note The value palette function must be idempotent (i.e. key(\a x) should \b ALWAYS
 *       return the same value for a given \a x, no matter how many times, or when
 *       is it called, without respect for any global variables, etc.
 * \param key The value palette function, to which should the texture correspond.
 * \param withIsolines Should the texture have isolines?
 */
ValueTextureCache::tex_t ValueTextureCache::getTexture(key_t key, bool withIsolines)
{
    textureCache_t::iterator found = withIsolines ? cachedTexturesIso.find(key) : cachedTextures.find(key);
    textureCache_t::iterator endMarker = withIsolines ? cachedTexturesIso.end() : cachedTextures.end();
    if (found == endMarker) {
        // not yet in the cache -- generate
        if (withIsolines) {
            tex_t tex = Visualiser::genValueTexIso(key, genTexSize, genTexFilter, genTexEquidistance, genIsolineColor);
            cachedTexturesIso[key] = tex;
            return tex;
        } else {
            tex_t tex = Visualiser::genValueTex(key, genTexSize, genTexFilter);
            cachedTextures[key] = tex;
            return tex;
        }
    } else {
        // already in the cache
        return found->second;
    }
}

/*!
 * If a texture corresponding to the given palette function has already been generated
 * and is in the cache, the texture is activated. Otherwise, the texture is generated
 * and stored into the cache, then activated.
 * \note The value palette function must be idempotent (i.e. key(\a x) should \b ALWAYS
 *       return the same value for a given \a x, no matter how many times, or when
 *       is it called, without respect for any global variables, etc.
 * \param key The value palette function, to which should the texture correspond.
 */
void ValueTextureCache::activateTexture(key_t key, bool withIsolines)
{
    tex_t tex = getTexture(key, withIsolines);
    vis->setValueFunc(key);
    vis->setValueTexture(tex);
}

/*!
 * This function removes all textures that are currently in cache, frees the associated
 * memory, etc. If you call getTexture() next time, the texture gets generated again.
 */
void ValueTextureCache::clear()
{
    textureCache_t::iterator i;
    for (i = cachedTextures.begin(); i != cachedTextures.end(); i++) {
        mglDeleteTexture(i->second);
    }
    cachedTextures.erase(cachedTextures.begin(), cachedTextures.end());
    for (i = cachedTexturesIso.begin(); i != cachedTexturesIso.end(); i++) {
        mglDeleteTexture(i->second);
    }
    cachedTexturesIso.erase(cachedTexturesIso.begin(), cachedTexturesIso.end());
}
