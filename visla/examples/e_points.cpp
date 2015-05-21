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
#include <iostream>
#include <cstdlib>
#include <cstdio>
#include <ctime>
#include <cassert>

#include <GL/glut.h>
#include <mgl.h>
#include <visla.h>

#ifndef WIN32_LEAN_AND_MEAN
  #define WIN32_LEAN_AND_MEAN 1
#endif
#include <windows.h>
#include <mmsystem.h>

// ---------------
using namespace std;
using namespace Visla;
// ---------------
#define APP_TITLE "Visualiser Example"
#define WIDTH 256
#define HEIGHT 256
// ---------------
Visualiser *vis = 0;
mglTexture *tex_valuepal = 0;
mglTexture *tex_pointsprite = 0;
// ---------------
Visualiser::PointMode pointMode = Visualiser::pmFlat;
float    pointSize = 9.0f;
unsigned particleCount = 0;
unsigned batchSize = 50000;
TVector3 *particlePos = 0;
float    particleAlpha = 1.0f;

// ---------------

void updateFpsInfo()
{
    static LARGE_INTEGER currtime, lasttime, freq;
    static time_t lastupdate;
    char buff[200];

    QueryPerformanceCounter(&currtime);
    QueryPerformanceFrequency(&freq);
    if (lastupdate != time(0)) {    // update every second
        lastupdate = time(0);
        sprintf(buff, APP_TITLE " [%d particles, batches per %d, %.1f FPS]", particleCount, batchSize, (float)freq.QuadPart / (currtime.QuadPart - lasttime.QuadPart));
        glutSetWindowTitle(buff);
    }
    lasttime = currtime;
}

void display()
{
    updateFpsInfo();

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    mglBegin();
    vis->beginFrame();

    vis->setPointSize(pointSize);
    vis->setPointsMode(pointMode);

    glEnable(GL_ALPHA_TEST);
    glAlphaFunc(GL_GREATER, 0.0f);
    glEnable(GL_BLEND);
    //glEnable(GL_POINT_SMOOTH);

    TVector3 *p = particlePos;
    unsigned remaining = particleCount;
    // draw all particles in batches of batchSize particles
    while (remaining) {
        unsigned batch = remaining > batchSize ? batchSize : remaining;
        remaining -= batch;

        // draw batch
        vis->beginParticles();
        while (batch--) {
            vis->particle(*p, p->x() / WIDTH);
            p++;
        }
        vis->endParticles();
    }

    vis->endFrame();
    mglEnd();
}

void genRandomParticles(TVector3 particles[], unsigned count)
{
    TVector3 *p = particles;
    for (unsigned i = count; i > 0; i--) {
        p->set_x(WIDTH * (float)rand()/RAND_MAX);
        p->set_y(HEIGHT * (float)rand()/RAND_MAX);
        p->set_z(0.0f);
        p++;
    }
}

void reallocParticles(unsigned newCount)
{
    if (newCount == particleCount) return;

    if (particlePos) {
        particlePos = (TVector3*)realloc(particlePos, sizeof(particlePos[0]) * newCount);
        assert(particlePos);
        if (newCount > particleCount)
            genRandomParticles(particlePos + particleCount, newCount - particleCount);
    } else {
        particlePos = (TVector3*)malloc(sizeof(particlePos[0]) * newCount);
        assert(particlePos);
        genRandomParticles(particlePos, newCount);
    }
    particleCount = newCount;
}

void kbdinput(int key)
{
    switch (key) {
    case 1:
        MessageBox(GetActiveWindow(), APP_TITLE " v0.1\nCopyright © Petr Kadlec, 2003",
            "About Visualiser", MB_OK | MB_ICONINFORMATION | MB_APPLMODAL);
        break;

    case '1':
        pointMode = Visualiser::pmFlat;
        break;

    case '2':
        pointMode = Visualiser::pmPointSprite;
        break;

    case '[':
        pointSize -= 0.5f;
        if (pointSize < 0.5f) pointSize = 0.5f;
        break;

    case ']':
        pointSize += 0.5f;
        break;

    case '+':
        reallocParticles(particleCount + 500);
        break;

    case '-':
        if (particleCount > 500) reallocParticles(particleCount - 500);
        break;

    case ';':
        batchSize -= 500;
        if (batchSize < 500) batchSize = 500;
        vis->setParticleParams(3, batchSize);
        break;

    case '\'':
        batchSize += 500;
        vis->setParticleParams(3, batchSize);
        break;

    case ',':
        particleAlpha -= 0.05f;
        if (particleAlpha < 0.0f) particleAlpha = 0.0f;
        vis->setParticleAlpha(particleAlpha);
        break;

    case '.':
        particleAlpha += 0.05f;
        if (particleAlpha > 1.0f) particleAlpha = 1.0f;
        vis->setParticleAlpha(particleAlpha);
        break;
    }
}

void menu(int entry)
{
    kbdinput(entry);
}

static TRGBAColor VISLACALLBACK getpalfluent(Visualiser::value_t value)
{
    TRGBAColor c;

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

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
    // init MGL
    mglOpenWindow(APP_TITLE, 100, 100, WIDTH, HEIGHT);
    mglSetupView(0, WIDTH, 0, HEIGHT);
    mglSetDisplayHandler(display);
    mglSetKeyboardHandler(kbdinput);
    // menu
    int settingsMenu = glutCreateMenu(menu);
    glutAddMenuEntry("Simple points\t1", '1');
    glutAddMenuEntry("Point sprites\t2", '2');
    glutAddMenuEntry("-", 0);
    glutAddMenuEntry("Decrease particle size\t[", '[');
    glutAddMenuEntry("Increase particle size\t]", ']');
    glutAddMenuEntry("Decrease number of particles\t-", '-');
    glutAddMenuEntry("Increase number of particles\t+", '+');
    glutAddMenuEntry("Decrease batch size\t;", ';');
    glutAddMenuEntry("Increase batch size\t'", '\'');
    glutAddMenuEntry("-", 0);
    glutAddMenuEntry("Decrease alpha\t,", ',');
    glutAddMenuEntry("Increase alpha\t.", '.');
    int main_menu = glutCreateMenu(menu);
    glutAddSubMenu("Settings", settingsMenu);
    glutAddMenuEntry("About...", 1);
    glutAttachMenu(GLUT_RIGHT_BUTTON);

    // prepare some demo data
    reallocParticles(batchSize);

    // init Visualiser
    vis = new Visualiser();
    vis->setDataRange(0, 1);
    vis->setValueFunc(getpalfluent);
    vis->setParticleParams(2, batchSize);
    tex_pointsprite = mglLoadTexture("..\\tex\\particle.tga", 1, GL_NEAREST);
    vis->setPointSpriteTexture(tex_pointsprite);
    tex_valuepal = Visualiser::genValueTex(getpalfluent, 256, GL_NEAREST);
    vis->setValueTexture(tex_valuepal);

    // let's roll!
    mglRun();

    // it never gets here, anyhow...
    return 0;
}
