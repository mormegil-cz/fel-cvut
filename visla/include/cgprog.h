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


#ifndef __CGPROG_H_
#define __CGPROG_H_

#include <cassert>

#include "vertprog.h"
#include "fragprog.h"

#include <Cg/cg.h>
#include <Cg/cgGL.h>

// --- namespace declaration
namespace Visla {
    class CgProgram;
    class CgVertexProgram;
    class CgFragmentProgram;
};

using namespace std;
using namespace Visla;

class Visla::CgProgram {
public:
    typedef enum { cgptVertex, cgptFragment } CgProgType;
    typedef enum { cgpsSource, cgpsObject   } CgProgSource;

private:
    // ----- static -----
    static unsigned  instanceCount;
    static CGcontext cgContext;
    static void errorCallback();
    static void initContext();
    static void freeContext();
    // ------------------
    CGprofile cgProfile;
    CGprogram cgProgram;
    CGparameter *varParams;
    CGparameter *uniParams;
    unsigned varCnt;
    unsigned uniCnt;
protected:
    CgProgram(CgProgType type) {
        if (!instanceCount++) initContext();
        assert(cgIsContext(cgContext));

        switch (type) {
        case cgptVertex:
            cgProfile = cgGLGetLatestProfile(CG_GL_VERTEX);
            break;

        case cgptFragment:
            cgProfile = cgGLGetLatestProfile(CG_GL_FRAGMENT);
            break;

        default:
            assert(!"Invalid value of type");
        }
    }

    ~CgProgram() {
        if (cgIsProgram(cgProgram)) cgDestroyProgram(cgProgram);
        if (!--instanceCount) freeContext();
    }

    inline const char *profileString() const {
	return cgGetProfileString(cgProfile);
    }

    CGparameter getVarParameterByIndex(unsigned index) const;
    CGparameter getUniParameterByIndex(unsigned index) const;

    void doLoadProgram(CgProgSource source, const GLubyte *program);

    inline void doBind() const {
        cgGLBindProgram(cgProgram);
    }

    inline void doEnable() const {
        cgGLEnableProfile(cgProfile);
    }

    inline void doDisable() const {
        cgGLDisableProfile(cgProfile);
    }

public:
    CGparameter paramByName(const char *name) const {
        return cgGetNamedParameter(cgProgram, name);
    }
};

class Visla::CgVertexProgram : public CgProgram, public VertexProgram {
    static inline CGGLenum matrixEnum(TrackedMatrix matrix) {
        static CGGLenum matrixValues[] = {
            CGGLenum(-1),
            CGGLenum(-1),
            CG_GL_MODELVIEW_MATRIX,
            CG_GL_PROJECTION_MATRIX,
            CG_GL_TEXTURE_MATRIX,
            CGGLenum(-1),
            CGGLenum(-1),
            CGGLenum(-1),
            CGGLenum(-1),
            CGGLenum(-1),
            CGGLenum(-1),
            CGGLenum(-1),
            CGGLenum(-1),
            CGGLenum(-1),
            CG_GL_MODELVIEW_PROJECTION_MATRIX
        };
        if (matrix < 0 || matrixValues[matrix] == CGGLenum(-1))
            throw invalid_argument("Unsupported matrix to track");
        if (matrix >= 0) return matrixValues[matrix];
    }

    static inline CGGLenum transformEnum(TrackingTransform transform) {
        static CGGLenum transformValues[] = {
            CG_GL_MATRIX_IDENTITY,
            CG_GL_MATRIX_INVERSE,
            CG_GL_MATRIX_TRANSPOSE,
            CG_GL_MATRIX_INVERSE_TRANSPOSE
        };
        return transformValues[transform];
    }
public:
    CgVertexProgram() : CgProgram(cgptVertex) {
    };

    virtual const char *extension_used() const {
        return profileString();
    }

    virtual void compile(const GLubyte *program, GLuint size) {
        doLoadProgram(cgpsObject, program);
    }

    virtual void compileFromCg(const GLubyte *program, GLuint size) {
        doLoadProgram(cgpsSource, program);
    }

    virtual void bind() {
        doBind();
    }

    virtual void unbind() const {
        // nothing appropriate
    }

    virtual void enable() const {
        doEnable();
    }

    virtual void disable() const {
        doDisable();
    }

    virtual void vertexAttrib(int index, int n, const float *values) const {
        for (int i = 0; i < n; i++, index++, values += 4)
            cgGLSetParameter4fv(getVarParameterByIndex(index), values);
    }

    void vertexAttrib(CGparameter param, const float *values) const {
        cgGLSetParameter4fv(param, values);
    }

    virtual void vertexAttribPointer(GLuint index, GLint size, GLenum type,
        GLsizei stride, const GLvoid *pointer) const {
        cgGLSetParameterPointer(getVarParameterByIndex(index), size, type, stride, const_cast<GLvoid*>(pointer));
    }

    void vertexAttribPointer(CGparameter param, GLint size, GLenum type,
        GLsizei stride, const GLvoid *pointer) const {
        cgGLSetParameterPointer(param, size, type, stride, const_cast<GLvoid*>(pointer));
    }

    virtual void enableAttribPointer(GLuint index) const {
        cgGLEnableClientState(getVarParameterByIndex(index));
    }

    void enableAttribPointer(CGparameter param) const {
        cgGLEnableClientState(param);
    }

    virtual void disableAttribPointer(GLuint index) const {
        cgGLDisableClientState(getVarParameterByIndex(index));
    }

    void disableAttribPointer(CGparameter param) const {
        cgGLDisableClientState(param);
    }

    virtual void setProgramParams(unsigned index, int n, const float *values) const {
        for (int i = 0; i < n; i++, index++, values += 4)
            cgGLSetParameter4fv(getUniParameterByIndex(index), values);
    }

    void setProgramParam(CGparameter param, const float *values) const {
        cgGLSetParameter4fv(param, values);
    }

    virtual void trackMatrix(int address, TrackedMatrix matrix, TrackingTransform transform = ttIdentity) const {
        cgGLSetStateMatrixParameter(getUniParameterByIndex(address), matrixEnum(matrix), transformEnum(transform));
    }

    void trackMatrix(CGparameter param, TrackedMatrix matrix, TrackingTransform transform = ttIdentity) const {
        cgGLSetStateMatrixParameter(param, matrixEnum(matrix), transformEnum(transform));
    }
};

class Visla::CgFragmentProgram : public CgProgram, public FragmentProgram {
public:
    CgFragmentProgram() : CgProgram(cgptFragment) {
    };

    virtual const char *extension_used() const {
        return profileString();
    }

    virtual void compile(const GLubyte *program, GLuint size) {
        doLoadProgram(cgpsObject, program);
    }

    virtual void compileFromCg(const GLubyte *program, GLuint size) {
        doLoadProgram(cgpsSource, program);
    }

    virtual void bind() {
        doBind();
    }

    virtual void unbind() const {
        // nothing appropriate
    }

    virtual void enable() const {
        doEnable();
    }

    virtual void disable() const {
        doDisable();
    }

    virtual void setProgramParams(int index, int n, const float *values) const {
        for (int i = 0; i < n; i++, index++, values += 4)
            cgGLSetParameter4fv(getUniParameterByIndex(index), values);
    }

    void setProgramParam(CGparameter param, const float *values) const {
        cgGLSetParameter4fv(param, values);
    }
};

#endif /* ifndef __CGPROG_H_ */
