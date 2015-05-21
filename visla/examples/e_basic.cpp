/*  Example program for Visla -- Visualization Library for Accelerators
 *  Copyright (C) 2003  Petr Kadlec <mormegil@centrum.cz>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

 
#ifndef WIN32_LEAN_AND_MEAN
  #define WIN32_LEAN_AND_MEAN 1
#endif
#include <windows.h>

#include <mgl.h>
#include <visla.h>

using namespace Visla;

#define GRID_CELLS 5
#define GRID_POINTS (GRID_CELLS + 1)

Visualiser *vis;
mglTexture *texValues;

void display()
{
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    mglBegin();
    vis->beginFrame();

    vis->beginGrid();
    for (int y = 0; y < GRID_POINTS; y++) {
        for (int x = 0; x < GRID_POINTS; x++) {
            // some function that should be drawn...
            float value = 0.5f*x/(GRID_POINTS-1.0f) + 0.25f*sin(6.28f*y/(GRID_POINTS-1.0f)) + 0.25f;
            vis->gridPoint(value);
        }
    }
    vis->endGrid();

    vis->endFrame();
    mglEnd();
}

// value palette function
static mglColor VISLACALLBACK getpalfluent(float value)
{
    mglColor c;

    c.a = 1; c.r = c.g = c.b = 0;
    if (value > 0.7)
    {
        c.r = 1; c.g = 1 - (value - 0.7) / 0.3;
    }
    else if (value > 0.5)
    {
        c.g = 1; c.r = (value - 0.5) / 0.2;
    }
    else if (value > 0.3)
    {
        c.g = 1; c.b = 1 - (value - 0.3) / 0.2;
    }
    else
    {
        c.b = 1; c.g = value / 0.3;
    }

    c.r = c.g = c.b = value;
    
    return c;
}

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
    // init MGL
    mglOpenWindow("Visualiser Example", 100, 100, 256, 256);
    mglSetupView(0.0f, 10.0f, 0.0f, 10.0f);
    mglSetDisplayHandler(display);

    // create value palette texture
    texValues = Visualiser::genValueTex(getpalfluent, 256, GL_NEAREST);

    Tvector gridCorners[2] = {Tvector(0,0), Tvector(10,10)};

    // init Visualiser
    vis = new Visualiser();
    vis->setDataRange(0, 1);
    vis->setPatchDivision(10);
    vis->setValueFunc(getpalfluent);
    vis->setValueTexture(texValues);
    vis->setPointSize(1.0);
    vis->setPointsMode(Visualiser::pmFlat);
    vis->setParticleParams(2, 500);
    vis->setGridParams(gridCorners, GRID_CELLS, GRID_CELLS);

    // let's roll!
    mglRun();

    return 0;
}
