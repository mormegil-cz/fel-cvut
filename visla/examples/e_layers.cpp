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
#include <mmsystem.h>

#include <mgl.h>
#include <visla.h>

using namespace Visla;

#define APP_TITLE "Visualiser Example"
#define GRID_CELLS 6
#define GRID_POINTS (GRID_CELLS + 1)
#define DEST_LAYER 2

Visualiser *vis;
ValueTextureCache *texCache;
Visualiser::valueFunc_p currPalette;
Visualiser::SplineMode splineMode;
bool isolines;
int  subdivision;

bool  animating;
bool  going_up;
float curr_layer;

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
        sprintf(buff, APP_TITLE " [mode: %s, subdivision: %d, layer=%.3f, %.1f FPS]", modeStr[splineMode], subdivision, curr_layer, (float)freq.QuadPart / (currtime.QuadPart - lasttime.QuadPart));
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
    vis->interpolateGridLayer(DEST_LAYER, curr_layer, Visualiser::limLinear);
    vis->drawStoredGrid(DEST_LAYER);

    // animation
    static DWORD lasttime = 0;
    DWORD currtime = timeGetTime();
    DWORD elapsed = lasttime ? (currtime-lasttime) : 0;
    float delta = 1e-3 * elapsed;
    lasttime = currtime;
    if (animating) {
        if (going_up) {
            curr_layer += delta;
            if (curr_layer >= 1.0f) {
                curr_layer = 1.0f;
                going_up = false;
            }
        } else {
            curr_layer -= delta;
            if (curr_layer <= 0.0f) {
                curr_layer = 0.0f;
                going_up = true;
            }
        }
    }

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

static void storeData()
{
    int y;

    vis->beginGrid(0);
    for (y = 0; y < GRID_POINTS; y++) {
        for (int x = 0; x < GRID_POINTS; x++) {
            // function at layer 0
            float value = 0.5f*x/(GRID_POINTS-1.0f) + 0.25f*sin(6.28f*y/(GRID_POINTS-1.0f)) + 0.25f;
            vis->gridPoint(value);
        }
    }
    vis->endGrid(false);

    vis->beginGrid(1);
    for (y = 0; y < GRID_POINTS; y++) {
        for (int x = 0; x < GRID_POINTS; x++) {
            // function at layer 1
            float value = 0.25f*cos(6.28f*x/(GRID_POINTS-1.0f)) + 0.5f*(sqrt(y/(GRID_POINTS-1.0f))) + 0.25f;
            vis->gridPoint(value);
        }
    }
    vis->endGrid(false);
}

static void keyboard(int key)
{
    switch(key) {
    case 1:
        MessageBox(GetActiveWindow(), "Visualiser Example: Grid Layer Interpolation\nCopyright © Petr Kadlec, 2003",
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
        vis->setPatchDivision(subdivision);
        storeData();
        break;

    case '-':
        if (subdivision > 0) subdivision--;
        vis->setPatchDivision(subdivision);
        storeData();
        break;

    case '\r':
        animating = !animating;
        break;

    case ' ':
        if (!animating) {
            if (going_up) {
                curr_layer += 0.05f;
                if (curr_layer >= 1.0f) {
                    curr_layer = 1.0f;
                    going_up = false;
                }
            } else {
                curr_layer -= 0.05f;
                if (curr_layer <= 0.0f) {
                    curr_layer = 0.0f;
                    going_up = true;
                }
            }
        }
        break;

    case '\t':
        if (!animating) {
            if (curr_layer < 0.5f) curr_layer = 1.0f;
            else curr_layer = 0.0f;
        }
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
    mglOpenWindow("Visualiser Example", 100, 100, 512, 512);
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

    int layer_menu = glutCreateMenu(menu);
    glutAddMenuEntry("Animation\tENTER", '\r');
    glutAddMenuEntry("Step\tSPACE", ' ');
    glutAddMenuEntry("First/last\tTAB", '\t');

    int main_menu = glutCreateMenu(menu);
    glutAddSubMenu("Interpolation", mode_menu);
    glutAddSubMenu("Palette", palette_menu);
    glutAddSubMenu("Layers", layer_menu);
    glutAddMenuEntry("Isolines on/off\tI", 'I');
    glutAddMenuEntry("About...", 1);
    glutAttachMenu(GLUT_RIGHT_BUTTON);
    // ---

    Tvector gridCorners[2] = {Tvector(0,0), Tvector(10,10)};

    currPalette = palSpectrum;
    isolines = false;
    splineMode = Visualiser::smVertInterp;
    subdivision = 5;
    animating = false;
    curr_layer = 0;
    going_up = true;

    // init Visualiser
    vis = new Visualiser();
    vis->setDataRange(0, 1);
    vis->setPatchDivision(subdivision);
    vis->setPointSize(1.0);
    vis->setPointsMode(Visualiser::pmFlat);
    vis->setParticleParams(2, 500);
    vis->setGridParams(gridCorners, GRID_CELLS, GRID_CELLS, DEST_LAYER+1);

    // init texture cache
    texCache = new ValueTextureCache(vis);
    texCache->setGenTexFilter(GL_LINEAR);
    texCache->setGenTexSize(1024);
    texCache->setGenTexEquidistance(64);

    // store grid data
    storeData();

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
