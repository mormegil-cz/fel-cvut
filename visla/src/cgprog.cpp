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

#ifndef WIN32_LEAN_AND_MEAN
  #define WIN32_LEAN_AND_MEAN 1
#endif
#include <windows.h>
#include <cstdlib>
#include <cassert>

#include <Cg/cg.h>
#include <Cg/cgGL.h>

#include "cgprog.h"

using namespace std;
using namespace Visla;

unsigned  CgProgram::instanceCount = 0;
CGcontext CgProgram::cgContext;

void CgProgram::errorCallback()
{
    const char *msg = cgGetErrorString(cgGetError());
    //::MessageBox(0, msg, "Cg Error", MB_APPLMODAL | MB_ICONERROR | MB_OK);
    throw compile_error(msg);
}

void CgProgram::initContext()
{
    cgSetErrorCallback(errorCallback);
    if (!cgIsContext(cgContext))
        cgContext = cgCreateContext();
}

void CgProgram::freeContext()
{
    //cgSetErrorCallBack(0);
    if (cgIsContext(cgContext))
        cgDestroyContext(cgContext);
}

CGparameter CgProgram::getVarParameterByIndex(unsigned index) const
{
    if (index >= varCnt)
        throw invalid_argument("Varying parameter index out of range");
    CGparameter param = varParams[index];
    if (!param)
        throw invalid_argument("Unknown varying parameter");
    return param;
}

CGparameter CgProgram::getUniParameterByIndex(unsigned index) const
{
    if (index >= uniCnt)
        throw invalid_argument("Uniform parameter index out of range");
    CGparameter param = uniParams[index];
    if (!param)
        throw invalid_argument("Unknown uniform parameter");
    return param;
}

void CgProgram::doLoadProgram(CgProgSource source, const GLubyte *program)
{
    static CGenum progType[] = { CG_SOURCE, CG_OBJECT };

    if (cgIsProgram(cgProgram))
        throw logic_error("Program already loaded");

    cgGLSetOptimalOptions(cgProfile);
    cgProgram = cgCreateProgram(cgContext, progType[source], (const char*)program, cgProfile, NULL, NULL);
    assert(cgIsProgram(cgProgram));

    varParams = (CGparameter*)malloc(0);
    uniParams = (CGparameter*)malloc(0);
    varCnt = uniCnt = 0;

    CGparameter param = cgGetFirstLeafParameter(cgProgram, CG_PROGRAM);
    if (!param) return;

    do {
        if (cgGetParameterBaseResource(param) == CG_UNDEFINED) continue;
        
        CGenum var = cgGetParameterVariability(param);
        CGparameter **parray;
        unsigned *plen;

        switch(var) {
        case CG_VARYING:
            parray = &varParams;
            plen = &varCnt;
            break;

        case CG_UNIFORM:
            parray = &uniParams;
            plen = &uniCnt;
            break;

        case CG_CONSTANT:
            continue;

        case CG_MIXED:
        default:
            assert(!"Unexpected value of parameter variability");
            parray = 0; // just to remove the warning about non-initialization...
            plen = 0;
        }

        unsigned index = cgGetParameterResourceIndex(param);
        if (index >= *plen) {
            *parray = (CGparameter*)(realloc(*parray, sizeof(CGparameter) * (index + 1)));
            for (unsigned i = *plen; i <= index; i++) *parray[i] = CGparameter(0);
            *plen = index + 1;
        }
        *parray[index] = param;
    } while ((param = cgGetNextLeafParameter(param)) != 0);
}
