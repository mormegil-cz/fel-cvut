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

#define APP_TITLE "IBFV Example"
#define GRID_POINTS_X 40
#define GRID_POINTS_Y 30
#define DEST_LAYER 2

#define WIDTH 256
#define HEIGHT 256

Visualiser *vis;
IBFVHelper *ibfv;

int patsize;
int numpats;
float scale;
float alpha;

bool  animating;

bool dropDye = false;
float dropX;
float dropY;
mglColor dyeColor;

// -------------------------------------------------

static void updateFPSInfo()
{
    static LARGE_INTEGER currtime, lasttime, freq;
    static time_t lastupdate;
    char buff[200];

    QueryPerformanceCounter(&currtime);
    QueryPerformanceFrequency(&freq);
    if (lastupdate != time(0)) {    // update every second
        lastupdate = time(0);
        sprintf(buff, APP_TITLE " [%d x %dx%d, a=%.2f, s=%.1f %.1f FPS]", numpats, patsize, patsize, alpha, scale, (float)freq.QuadPart / (currtime.QuadPart - lasttime.QuadPart));
        glutSetWindowTitle(buff);
    }
    lasttime = currtime;
}

static void display()
{
    updateFPSInfo();

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    static TVector3 corners[2] = { TVector3(0.0f, 0.0f, 0.0f), TVector3(WIDTH, HEIGHT, 0.0f) };

    mglBegin();
    vis->beginFrame();

    if (animating) ibfv->doFrame();

    if (dropDye) {
        ibfv->beginDrawToBuffer();
        glPushAttrib(GL_ENABLE_BIT | GL_TEXTURE_BIT | GL_POINT_BIT);
        glPointSize(30.0f);
        glDisable(GL_TEXTURE_2D);
        glEnable(GL_POINT_SMOOTH);
        glEnable(GL_BLEND);
        glBegin(GL_POINTS);
          glColor3fv((const float*)(&dyeColor));
          glVertex2f(dropX, dropY);
        glEnd();
        glPopAttrib();
        ibfv->endDrawToBuffer();
    }

    ibfv->draw(corners);

    vis->endFrame();
    mglEnd();
}

// ---------------------------------------

static void storeData()
{
    int y;

    ibfv->beginGrid();
    for (y = 0; y < GRID_POINTS_Y; y++) {
        for (int x = 0; x < GRID_POINTS_X; x++) {
            // vector field values (single vortex)
            TVector3 vec((y - 0.5f * GRID_POINTS_Y) / GRID_POINTS_Y, - (x - 0.5f * GRID_POINTS_X) / GRID_POINTS_X, 0.0f);
            vec.scalar_mul(vec, vec, 2.0f);
            float dist = vec.length();
            if (dist > 0) vec.resize_to(1.0f / dist);

            ibfv->gridValue(vec.x(), vec.y());
        }
    }
    ibfv->endGrid(true);
}

static void MGL_CALLBACK keyboard(int key)
{
    bool ibfv_changed = false;

    switch(key) {
    case 1:
        MessageBox(GetActiveWindow(), "Visualiser Example: Image Based Flow Visualization\nCopyright © Petr Kadlec, 2003",
            "About Visualiser", MB_OK | MB_ICONINFORMATION | MB_APPLMODAL);
        break;

    case '+':
        alpha += 0.01f;
        if (alpha > 1.0f) alpha = 1.0f;
        ibfv_changed = true;
        break;

    case '-':
        alpha -= 0.01f;
        if (alpha < 0) alpha = 0.0f;
        ibfv_changed = true;
        break;

    case '.':
        scale *= 2.0f;
        ibfv_changed = true;
        break;

    case ',':
        scale /= 2.0f;
        if (scale < 0.1f) scale = 0.01f;
        ibfv_changed = true;
        break;

    case '\'':
        numpats += 2;
        ibfv_changed = true;
        break;

    case ';':
        numpats -= 2;
        if (numpats < 1) numpats = 1;
        ibfv_changed = true;
        break;

    case ']':
        patsize *= 2;
        ibfv_changed = true;
        break;

    case '[':
        patsize /= 2;
        if (patsize < 2) patsize = 2;
        ibfv_changed = true;
        break;

    case '\r':
        animating = !animating;
        break;

    case ' ':
        if (!animating) ibfv->doFrame();
        break;

    }

    if (ibfv_changed)
        ibfv->setIBFVParams(patsize, numpats, scale, alpha);
}

static void MGL_CALLBACK mouse(int event)
{
    dropX = mglMouseX;
    dropY = mglMouseY;

    switch (event) {
    case MGL_LEFT_BUTTON_DOWN:
        dropDye = true;
        dyeColor = mglRed;
        break;

    case MGL_MIDDLE_BUTTON_DOWN:
        dropDye = true;
        dyeColor = mglYellow;
        break;

    case MGL_MIDDLE_BUTTON_UP:
    case MGL_LEFT_BUTTON_UP:
        dropDye = false;
        break;
    }
}

static void GLUTCALLBACK menu(int entry)
{
    keyboard(entry);
}

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
  try {
    // init MGL
    mglOpenWindow("Visualiser Example", 100, 100, WIDTH, HEIGHT);
    mglSetupView(0.0f, WIDTH, 0.0f, HEIGHT);
    mglSetDisplayHandler(display);
    mglSetKeyboardHandler(keyboard);
    mglSetMouseHandler(mouse);

    // --- menu
    int ibfv_menu = glutCreateMenu(menu);
    glutAddMenuEntry("Alpha +\t+", '+');
    glutAddMenuEntry("Alpha -\t-", '-');
    glutAddMenuEntry("-", 0);
    glutAddMenuEntry("Scale +\t.", '.');
    glutAddMenuEntry("Scale -\t,", ',');
    glutAddMenuEntry("-", 0);
    glutAddMenuEntry("# patterns +\t'", '\'');
    glutAddMenuEntry("# patterns -\t;", ';');
    glutAddMenuEntry("-", 0);
    glutAddMenuEntry("Pattern size +\t]", ']');
    glutAddMenuEntry("Pattern size -\t[", '[');

    int anim_menu = glutCreateMenu(menu);
    glutAddMenuEntry("Animation\tENTER", '\r');
    glutAddMenuEntry("Step\tSPACE", ' ');

    int main_menu = glutCreateMenu(menu);
    glutAddSubMenu("IBFV", ibfv_menu);
    glutAddSubMenu("Animation", anim_menu);
    glutAddMenuEntry("About...", 1);
    glutAttachMenu(GLUT_RIGHT_BUTTON);
    // ---

    animating = true;
    patsize = 64;
    numpats = 40;
    scale = 4.0f;
    alpha = 0.1f;

    // init Visualiser
    vis = new Visualiser();

    // init IBFV
    ibfv = vis->createIFBVHelper(WIDTH, HEIGHT);
    ibfv->setGridParams(GRID_POINTS_X, GRID_POINTS_Y);
    ibfv->setIBFVParams(patsize, numpats, scale, alpha);

    // store vector field data
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
