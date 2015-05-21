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
#include <ctime>

#ifndef WIN32_LEAN_AND_MEAN
  #define WIN32_LEAN_AND_MEAN 1
#endif
#include <windows.h>

#include <mgl.h>
#include <visla.h>

using namespace Visla;

#define APP_TITLE "Visualiser Example"
#define GRID_CELLS 4

Visualiser *vis;
ValueTextureCache *texCache;
Visualiser::valueFunc_p currPalette;
Visualiser::SplineMode splineMode;
bool isolines;
int  subdivision;

// -------------------------------------------------

static void updateFPSInfo()
{
    static const char *modeStr[2] = { "per-vertex", "per-pixel" };
    static LARGE_INTEGER currtime, lasttime, freq;
    static time_t lastupdate;
    char buff[200];

    QueryPerformanceCounter(&currtime);
    QueryPerformanceFrequency(&freq);
    if (lastupdate != time(0)) {    // update every second
        lastupdate = time(0);
        sprintf(buff, APP_TITLE " [mode: %s, subdivision points: %d, %.1f FPS]", modeStr[splineMode], subdivision, (float)freq.QuadPart / (currtime.QuadPart - lasttime.QuadPart));
        glutSetWindowTitle(buff);
    }
    lasttime = currtime;
}

static void display()
{
    updateFPSInfo();

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    mglBegin();
    vis->beginFrame();

    texCache->activateTexture(currPalette, isolines);
    vis->setSplineMode(splineMode);
    vis->beginGrid();
    for (int y = 0; y < GRID_CELLS + 1; y++) {
        for (int x = 0; x < GRID_CELLS + 1; x++) {
            // some function that should be drawn...
            float value = 0.5f*x/GRID_CELLS + 0.25f*sin(6.28f*y/GRID_CELLS) + 0.25f + 4.0f * exp(-15.f * (fabs((float)x/GRID_CELLS-0.2)+fabs((float)y/GRID_CELLS-0.6)));
            vis->gridPoint(value);
        }
    }
    vis->endGrid();

    vis->endFrame();
    mglEnd();
}

// ------- value palette functions -------
static mglColor VISLACALLBACK palSpectrum(float value)
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
    
    return c;
}

static mglColor VISLACALLBACK palGreyscale(float value)
{
    mglColor c;
    c.a = 1; c.r = c.g = c.b = value;
    return c;
}

static mglColor VISLACALLBACK palYellow(float value)
{
    mglColor c;
    c.a = 1; c.b = 0; c.r = c.g = value;
    return c;
}

static mglColor VISLACALLBACK palRedYellow(float value)
{
    mglColor c;
    c.a = 1; c.b = 0; c.r = 1.0; c.g = value;
    return c;
}
// ---------------------------------------

static void keyboard(int key)
{
    switch(key) {
    case 1:
        MessageBox(GetActiveWindow(), "Visualiser Example: Texture Cache\nCopyright (c) Petr Kadlec, 2003", 
            "About Visualiser", MB_OK | MB_ICONINFORMATION | MB_APPLMODAL);
        break;

    case 'i':
    case 'I':
        isolines = !isolines;
        break;

    case '1':
        currPalette = palSpectrum;
        break;

    case '2':
        currPalette = palGreyscale;
        break;

    case '3':
        currPalette = palYellow;
        break;

    case '4':
        currPalette = palRedYellow;
        break;

    case '9':
        splineMode = Visualiser::smVertInterp;
        break;

    case '0':
        splineMode = Visualiser::smPerFragment;
        break;

    case '=':
    case '+':
        subdivision++;
        if (vis)
            vis->setPatchDivision(subdivision);
        break;

    case '-':
        if (subdivision > 0) subdivision--;
        if (vis)
            vis->setPatchDivision(subdivision);
        break;
    }
}

static void menu(int entry)
{
    keyboard(entry);
}

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
  try {
    // init MGL
    mglOpenWindow("Visualiser Example", 100, 100, 250, 250);
    mglSetupView(0.0f, 10.0f, 0.0f, 10.0f);
    mglSetDisplayHandler(display);
    mglSetKeyboardHandler(keyboard);

    // --- menu
    int mode_menu = glutCreateMenu(menu);
    glutAddMenuEntry("Subdivision vertices\t9", '9');
    glutAddMenuEntry("Per-pixel\t0", '0');
    glutAddMenuEntry("---", 0);
    glutAddMenuEntry("Increase subdivision\t+", '+');
    glutAddMenuEntry("Decrease subdivision\t-", '-');

    int palette_menu = glutCreateMenu(menu);
    glutAddMenuEntry("Spectrum\t1", '1');
    glutAddMenuEntry("Greyscale\t2", '2');
    glutAddMenuEntry("Yellow\t3", '3');
    glutAddMenuEntry("Red-yellow\t4", '4');

    int main_menu = glutCreateMenu(menu);
    glutAddSubMenu("Interpolation", mode_menu);
    glutAddSubMenu("Palette", palette_menu);
    glutAddMenuEntry("Isolines on/off\tI", 'I');
    glutAddMenuEntry("About...", 1);
    glutAttachMenu(GLUT_RIGHT_BUTTON);
    // ---

    Tvector gridCorners[2] = {Tvector(0,0), Tvector(10,10)};

    currPalette = palSpectrum;
    isolines = false;
    splineMode = Visualiser::smVertInterp;
    subdivision = 5;

    // init Visualiser
    if (!(Visualiser::whatIsSupported() & Visualiser::supportedVertexPrograms)) {
        ::MessageBox(0, "This computer does not support required OpenGL functionality.", "Fatal Error", MB_ICONERROR | MB_APPLMODAL);
        return 1;
    }
    vis = new Visualiser();
    vis->setPatchDivision(subdivision);
    vis->setParticleParams(2, 500);
    vis->setGridParams(gridCorners, GRID_CELLS, GRID_CELLS);
    //vis->setQuadAlpha(0.9f);

    // init texture cache
    texCache = new ValueTextureCache(vis);
    texCache->setGenTexFilter(GL_LINEAR);
    texCache->setGenTexSize(256);
    texCache->setGenTexEquidistance(16);

    mglTexture *tex = Visualiser::genValueTexIso(palSpectrum, 1024, GL_NEAREST, 64, mglBlack);
    mglSaveImage("\\texiso.bmp", &(tex->image));

    // let's roll!
    mglRun();

    return 0;
  } catch (compile_error &e) {
    char ep[10];
    ::MessageBox(0, itoa(e.get_errpos(), ep, sizeof(ep)-1), e.what(), MB_ICONERROR | MB_APPLMODAL);
    return 2;
  } catch (exception &e) {
    ::MessageBox(0, e.what(), "Exception", MB_ICONERROR | MB_APPLMODAL);
    return 1; 
  }
}
