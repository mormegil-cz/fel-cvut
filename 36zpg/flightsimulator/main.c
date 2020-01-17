/***************************************************************************
                          main.c  -  description
                             -------------------
    begin                : Pá lis  2 17:17:52 CET 2001
    copyright            : (C) 2001 by Petr Kadlec
    email                : kadlecp2@fel.cvut.cz
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <errno.h>

#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>

#include "sgi.h"

#include "linalgebra.h"

#define BUILDING_SIDES 4

#if defined(GL_VERSION_1_1) || defined(GL_VERSION_1_2)
#  define TEXTURE_OBJECT 1
#else
#error "TEXTURE_OBJECT not supported!"
#endif

typedef struct tagBuilding {
    t_vector3 basement[BUILDING_SIDES];
    t_vector2 rooftex[BUILDING_SIDES];
    float height;
    GLuint texture;
    GLuint rooftexture;
} t_building, *p_building;

int city_points;
int building_count;
GLsizei texture_count;

p_vector3 city;
p_vector2 citytex;

p_building buildings;
GLuint *textures;

//#define DLISTS enabled

unsigned char **texdata;
int  *texheight, *texwidth, *texdepth;

long int _txtdata[2][2]={{0xFFFFFFFF,0},{0,0xFFFFFFFF}};

t_vector3 camerapos, cameradir, upvec, rightvec;
float time = 0.0;   // daytime (for computing the position of the Sun)

GLuint Dlist;

int wwidth, wheight;
int mainWindow;

void fail(const char *msg)
{
  fflush(stdout);
  perror(msg);
  exit(1);
}

void draw_building(p_building building)
{
  int i;
  p_vector3 b1, b2;

  glBindTexture(GL_TEXTURE_2D, textures[building->texture]);
  switch(building->texture) {
    case 0: glColor3f(1.0, 0.0, 0.0);

            break;
    case 1: glColor3f(0.0, 0.0, 1.0);

            break;
    default: assert(0);
  }

  // steny
  for (i = 0; i < BUILDING_SIDES; i++) {
    t_vector3 normal;
    t_vector3 v1, v2;

    b1 = &(building->basement[i]);
    b2 = &(building->basement[(i+1) % BUILDING_SIDES]);

    minus(&v1, b1, &(building->basement[(i+BUILDING_SIDES-1) % BUILDING_SIDES]));
    minus(&v2, b2, &(building->basement[i]));

    crossproduct(&normal, &v1, &v2);
    normalize(&normal);

    glBegin(GL_TRIANGLE_FAN);
    glNormal3fv(b1->m);

    glTexCoord2f(0.0,0.0);
    glVertex3fv(b1->m);
    glTexCoord2f(1.0,0.0);
    glVertex3fv(b2->m);
    glTexCoord2f(1.0,1.0);
    glVertex3f(b2->x, b2->y+building->height, b2->z);
    glTexCoord2f(0.0,1.0);
    glVertex3f(b1->x, b1->y+building->height, b1->z);
    glEnd();
  }

  // strecha
  glBindTexture(GL_TEXTURE_2D, textures[building->rooftexture]);
  //glTexImage2D(GL_TEXTURE_2D, 0, texdepth[building->rooftexture], texwidth[building->rooftexture], texheight[building->rooftexture], 0, texdepth[building->rooftexture] == 4 ? GL_RGBA : GL_RGB, GL_UNSIGNED_BYTE, texdata[building->rooftexture]);

  switch(building->rooftexture) {
    case 0: glColor3f(0.8, 0.0, 0.0);

            break;
    case 1: glColor3f(0.0, 0.0, 0.8);

            break;
    default: assert(0);
  }

  glBegin(GL_TRIANGLE_FAN);
  for (i = 0; i<BUILDING_SIDES; i++) {
    t_vector3 b1;
    b1.x = building->basement[i].x;
    b1.y = building->basement[i].y + building->height;
    b1.z = building->basement[i].z;
    glTexCoord2fv(building->rooftex[i].m);
    glVertex3fv(b1.m);
  }
  glEnd();
}

void draw_city(void)
{
    int i;

    glColor3f(0.0, 0.5, 0.0);
    glBegin(GL_TRIANGLE_FAN);
    glNormal3f(0.0, 1.0, 0.0);
    for (i=0; i<city_points; i++) {
      glTexCoord2fv(citytex[i].m);
      glVertex3fv(city[i].m);
    }
    glEnd();
}

void draw_skybox(void)
{
#define SKY_SIZE 500.0

    glPushMatrix();
    glLoadIdentity();
    gluLookAt(0.0, 0.0, 0.0,
              cameradir.x, cameradir.y, cameradir.z,
              upvec.x, upvec.y, upvec.z);

    glBegin(GL_QUADS);
    glColor3f(0.0, 0.0, 0.8);
    glNormal3f(-1.0, 0.0, 0.0);
    glTexCoord2f(0.0, 0.0); glVertex3f(SKY_SIZE, -SKY_SIZE, -SKY_SIZE);
    glTexCoord2f(0.0, 1.0); glVertex3f(SKY_SIZE, -SKY_SIZE, +SKY_SIZE);
    glTexCoord2f(1.0, 1.0); glVertex3f(SKY_SIZE, +SKY_SIZE, +SKY_SIZE);
    glTexCoord2f(1.0, 0.0); glVertex3f(SKY_SIZE, +SKY_SIZE, -SKY_SIZE);
    glNormal3f(+1.0, 0.0, 0.0);
    glTexCoord2f(0.0, 0.0); glVertex3f(-SKY_SIZE, -SKY_SIZE, -SKY_SIZE);
    glTexCoord2f(1.0, 0.0); glVertex3f(-SKY_SIZE, +SKY_SIZE, -SKY_SIZE);
    glTexCoord2f(1.0, 1.0); glVertex3f(-SKY_SIZE, +SKY_SIZE, +SKY_SIZE);
    glTexCoord2f(0.0, 1.0); glVertex3f(-SKY_SIZE, -SKY_SIZE, +SKY_SIZE);
    glNormal3f(0.0, -1.0, 0.0);
    glTexCoord2f(0.0, 0.0); glVertex3f(-SKY_SIZE, +SKY_SIZE, -SKY_SIZE);
    glTexCoord2f(1.0, 0.0); glVertex3f(+SKY_SIZE, +SKY_SIZE, -SKY_SIZE);
    glTexCoord2f(1.0, 1.0); glVertex3f(+SKY_SIZE, +SKY_SIZE, +SKY_SIZE);
    glTexCoord2f(0.0, 1.0); glVertex3f(-SKY_SIZE, +SKY_SIZE, +SKY_SIZE);
    glNormal3f(0.0, 0.0, -1.0);
    glTexCoord2f(0.0, 0.0); glVertex3f(-SKY_SIZE, -SKY_SIZE, +SKY_SIZE);
    glTexCoord2f(0.0, 1.0); glVertex3f(-SKY_SIZE, +SKY_SIZE, +SKY_SIZE);
    glTexCoord2f(1.0, 1.0); glVertex3f(+SKY_SIZE, +SKY_SIZE, +SKY_SIZE);
    glTexCoord2f(1.0, 0.0); glVertex3f(+SKY_SIZE, -SKY_SIZE, +SKY_SIZE);
    glNormal3f(0.0, 0.0, +1.0);
    glTexCoord2f(0.0, 0.0); glVertex3f(-SKY_SIZE, -SKY_SIZE, -SKY_SIZE);
    glTexCoord2f(1.0, 0.0); glVertex3f(+SKY_SIZE, -SKY_SIZE, -SKY_SIZE);
    glTexCoord2f(1.0, 1.0); glVertex3f(+SKY_SIZE, +SKY_SIZE, -SKY_SIZE);
    glTexCoord2f(0.0, 1.0); glVertex3f(-SKY_SIZE, +SKY_SIZE, -SKY_SIZE);

    glColor3f(0.0, 0.0, 1.0);
    glNormal3f(0.0, +1.0, 0.0);
    glTexCoord2f(0.0, 0.0); glVertex3f(-SKY_SIZE, -10.0, -SKY_SIZE);
    glTexCoord2f(0.0, 1.0); glVertex3f(-SKY_SIZE, -10.0, +SKY_SIZE);
    glTexCoord2f(1.0, 1.0); glVertex3f(+SKY_SIZE, -10.0, +SKY_SIZE);
    glTexCoord2f(1.0, 0.0); glVertex3f(+SKY_SIZE, -10.0, -SKY_SIZE);

    glEnd();

    // FIXME! To je fsechno blbe!
    if (time < 180.0) {
    // Sun
    glLoadIdentity();
    glColor3f(1.0, 1.0, 0.0);
    /*gluLookAt(0.0, 0.0, 0.0,
            -cos(time), 0.0, -sin(time),
            0.0, 1.0, 0.0);*/
    glRotatef(time, 0.5, 0.0, 0.5);
    glTranslatef(0.0, SKY_SIZE/2, -SKY_SIZE);
    glutSolidSphere(10.0, 5, 5);
    }

    printf("%.3f\n",time);

    glPopMatrix();
}

#ifdef DLISTS
void render(void)
{
    t_vector3 center;
    float vec[4];

    add(&center, &camerapos, &cameradir);

    glPushMatrix();
    glLoadIdentity();
    gluLookAt(camerapos.x, camerapos.y, camerapos.z,
              center.x, center.y, center.z,
              upvec.x, upvec.y, upvec.z);

    memcpy( vec, camerapos.m, sizeof(camerapos.m) );
    vec[3] = 1.0;
    glLightfv(GL_LIGHT0, GL_POSITION, vec);
    memcpy( vec, cameradir.m, sizeof(cameradir.m) );
    glLightfv(GL_LIGHT0, GL_SPOT_DIRECTION, vec);

  	glCallList(Dlist);
  	glutSwapBuffers();
}

void _render(void)
#else
void render(void)
#endif
{
    int i;
    t_vector3 center;
    float vec[4];

    add(&center, &camerapos, &cameradir);

    //glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glClear(GL_DEPTH_BUFFER_BIT);
    glDisable(GL_TEXTURE_2D);
    glDepthMask(GL_FALSE);
    draw_skybox();

#ifndef DLISTS
    glPushMatrix();
    glLoadIdentity();
    gluLookAt(camerapos.x, camerapos.y, camerapos.z,
              center.x, center.y, center.z,
              upvec.x, upvec.y, upvec.z);

    memcpy( vec, camerapos.m, sizeof(camerapos.m) );
    vec[3] = 1.0;
    glLightfv(GL_LIGHT0, GL_POSITION, vec);
    memcpy( vec, cameradir.m, sizeof(cameradir.m) );
    glLightfv(GL_LIGHT0, GL_SPOT_DIRECTION, vec);
#endif

   glDepthMask(GL_TRUE);
   glEnable(GL_TEXTURE_2D);

    for (i=0; i<building_count; i++)
        draw_building((p_building)&(buildings[i]));

   glDisable(GL_TEXTURE_2D);
    draw_city();

#ifndef DLISTS
    glPopMatrix();

    glutSwapBuffers();
#endif

    time = ((int)time + 10) % 360;
}

void specialinput(int key, int x, int y)
{
   switch(key) {
     case GLUT_KEY_LEFT:  {
				                    rotate_y(&cameradir, +5.0);
														break;
													}
     case GLUT_KEY_RIGHT: {
                            rotate_y(&cameradir, -5.0);
	                          break;
					                }
     case GLUT_KEY_UP:    {
                            t_vector3 x;
														scalarmul(&x, &cameradir, +5.0);
                            x.y = 0.0;
                            add(&camerapos, &camerapos, &x);
                            break;
                          }
     case GLUT_KEY_DOWN:  {
                            t_vector3 x;
														scalarmul(&x, &cameradir, -5.0);
                            x.y = 0.0;
                            add(&camerapos, &camerapos, &x);
                            break;
                          }
   }
   /*printf("Pos = %.2f,%.2f,%.2f\n",camerapos.x, camerapos.y, camerapos.z);
   printf("Dir = %.2f,%.2f,%.2f\n",cameradir.x, cameradir.y, cameradir.z);*/
   glutPostWindowRedisplay(mainWindow);
}

void kbdinput(unsigned char key, int x, int y)
{
    switch(key) {
        case '\x1b': exit(0);
        case '+': camerapos.y += 5; break;
        case '-': camerapos.y -= 5; break;

        case '.': {
                    t_vector3 x;
				            scalarmul(&x, &rightvec, +5.0);
                    x.y = 0.0;
                    add(&camerapos, &camerapos, &x);
                    break;
                  }
        case ',': {
                    t_vector3 x;
				            scalarmul(&x, &rightvec, -5.0);
                    x.y = 0.0;
                    add(&camerapos, &camerapos, &x);
                    break;
                  }

        case 'L': glEnable(GL_LIGHT0); break;
        case 'l': glDisable(GL_LIGHT1); break;
    }
   glutPostWindowRedisplay(mainWindow);
}

void reshape(int width, int height)
{
    wwidth = width;
    wheight = height;

    glViewport(0,0,width,height);

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(90.0, wwidth/(float)wheight, 1.0, 3000.0);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    //glTranslatef(0.0, 0.0, -5.0);
}

void load_texture(char *filename, GLuint texname)
{
    /*unsigned char *txtdata;
    int width, height, depth;*/

    //texture = read_sgi(filename, &width, &height, &depth);
    texdata[texname] = read_sgi(filename, &(texwidth[texname]), &(texheight[texname]), &(texdepth[texname]));
    if (texdata[texname] == NULL) fail("Error loading texture");

    glBindTexture(GL_TEXTURE_2D, texname);
    //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
    //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    //gluBuild2DMipmaps(GL_TEXTURE_2D, depth, width, height, depth == 4 ? GL_RGBA : GL_RGB, GL_UNSIGNED_BYTE, txtdata);
    printf("loaded %dx%dx%d ", texwidth[texname], texheight[texname], texdepth[texname]);
    glTexImage2D(GL_TEXTURE_2D, 0, texdepth[texname], texwidth[texname], texheight[texname], 0, texdepth[texname] == 4 ? GL_RGBA : GL_RGB, GL_UNSIGNED_BYTE, texdata[texname]);
}

void load_data(void)
{
    FILE *fd;
    int i;
    char texname[256];
    p_vector2 p2;
    p_vector3 p3;
    p_building p;

    texture_count = 0;
    if ((fd = fopen("data/textures.lst", "r")) == NULL) fail("Cannot open textures.lst");
    if (fscanf(fd, "%d\n", &texture_count) != 1) fail("Error reading textures.lst");
    if ((textures = (GLuint *)malloc(sizeof(textures[0]) * texture_count)) == NULL) fail("Can not get memory for textures");
    if ((texdata = (unsigned char **)malloc(sizeof(texdata[0]) * texture_count)) == NULL) fail("Can not get memory for textures");
    if ((texheight = (int *)malloc(sizeof(texheight[0]) * texture_count)) == NULL) fail("Can not get memory for textures");
    if ((texwidth = (int *)malloc(sizeof(texwidth[0]) * texture_count)) == NULL) fail("Can not get memory for textures");
    if ((texdepth = (int *)malloc(sizeof(texdepth[0]) * texture_count)) == NULL) fail("Can not get memory for textures");
    glEnable(GL_TEXTURE_2D);
    printf("Loading %d textures...\n", texture_count);
    glGenTextures(texture_count, textures);

    for (i = 0; i<texture_count; i++) {
        char *pc;
        if (fgets(texname, sizeof(texname)-1, fd) == NULL) fail("Error reading textures.lst");
        pc = texname;
        while (*pc) {
            if (*pc == '\n') { *pc = '\0'; break; }
            pc++;
        }
        printf("Loading `%s' ... ", texname);
        load_texture(texname, textures[i]);
        printf("OK, bound to %d\n",textures[i]);
    }
    if (fclose(fd)) fail("Error closing textures.lst");

    printf("Loading city data...");
    if ((fd = fopen("data/city.dat", "r")) == NULL) fail("Cannot open city.dat");
    if (!fscanf(fd, "%d", &city_points)) fail("Error reading city.dat");
    if ((city = (p_vector3)malloc(sizeof(city[0]) * city_points)) == NULL) fail("Can not get memory for city coordinates");
    if ((citytex = (p_vector2)malloc(sizeof(citytex[0]) * city_points)) == NULL) fail("Can not get memory for city texture coordinates");

    for (i = 0, p3 = city, p2 = citytex; i < city_points; i++, p2++, p3++)
        if (fscanf(fd, "%f %f %f %f %f", &(p3->x), &(p3->y), &(p3->z), &(p2->x), &(p2->y)) != 5) fail("Error reading city data");

    if (fclose(fd)) fail("Error closing city.dat");

    printf("OK\nLoading building data...");
    if ((fd = fopen("data/buildings.dat", "r")) == NULL) fail("Cannot open buildings.dat");
    if (!fscanf(fd, "%d", &building_count)) fail("Error reading buildings.dat");
    if ((buildings = (p_building)malloc(sizeof(buildings[0]) * building_count)) == NULL) fail("Can not get memory for buildings");

    for (i = 0, p = buildings; i < building_count; i++, p++) {
        int j;
        for (j = 0; j < BUILDING_SIDES; j++)
          if (fscanf(fd, "%f %f %f %f %f", &(p->basement[j].x), &(p->basement[j].y), &(p->basement[j].z), &(p->rooftex[j].x), &(p->rooftex[j].y)) != 5) fail("Error reading building data");
        if (fscanf(fd, "%f %d %d", &(p->height), &(p->texture), &(p->rooftexture)) != 3) fail("Error reading building data");
    }

    if (fclose(fd)) fail("Error closing buildings.dat");
    printf("OK\nAll data read OK.\n");
}

int main(int argc, char *argv[])
{
    float color[4];

    printf("Loading OpenGL...");
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE | GLUT_DEPTH);

    printf("Initializing OpenGL...");
    mainWindow = glutCreateWindow("NYC");

    printf("OK\nLoading data...\n");
    load_data();

    glutReshapeFunc(reshape);
    glutDisplayFunc(render);
    glutKeyboardFunc(kbdinput);
    //glutIdleFunc(idle);
    //glutMouseFunc(mouseinput);
    //glutMotionFunc(mousemove);
    glutSpecialFunc(specialinput);

    glEnable(GL_TEXTURE_2D);
    glShadeModel(GL_SMOOTH);
    glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);

    //glEnable(GL_LIGHTING);

    /* global ambient term */
    color[0] = 0.5; color[2] = 0.5; color[3] = 0.5; color[4] = 0.5;
    glLightModelfv(GL_LIGHT_MODEL_AMBIENT, color);

    /* light #0 */
    color[0] = 0.0; color[2] = 0.0; color[3] = 0.0; color[4] = 1.0;
    glLightfv(GL_LIGHT0, GL_AMBIENT, color);
    color[0] = 0.1; color[2] = 0.1; color[3] = 0.1; color[4] = 0.1;
    glLightfv(GL_LIGHT0, GL_DIFFUSE, color);
    /*color[0] = -0.8; color[2] = -0.2; color[3] = -0.8; color[4] = 0.0;
    glLightfv(GL_LIGHT0, GL_POSITION, color);
    color[0] = 1.0;
    glLightfv(GL_LIGHT0, GL_CONSTANT_ATTENUATION, color);
    color[0] = 0.0;
    glLightfv(GL_LIGHT0, GL_LINEAR_ATTENUATION, color);
    color[0] = 0.0;
    glLightfv(GL_LIGHT0, GL_QUADRATIC_ATTENUATION, color);*/
    glEnable(GL_LIGHT0);

    /*color[0] = 1.0; color[2] = 1.0; color[3] = 1.0; color[4] = 1.0;
    glMaterialfv(GL_FRONT, GL_AMBIENT, color);*/
    glEnable(GL_COLOR_MATERIAL);

    /*glEnable(GL_FOG);
    glFogf(GL_FOG_MODE, GL_LINEAR);
    glFogf(GL_FOG_START, 1500.0);
    glFogf(GL_FOG_END, 2500.0);
    color[0] = 0.6; color[2] = 0.6; color[3] = 0.6; color[4] = 0.6;
    glFogfv(GL_FOG_COLOR, color);*/

    color[0] = 1.0; color[2] = 1.0; color[3] = 1.0; color[4] = 1.0;
    glMaterialfv(GL_FRONT, GL_AMBIENT, color);
    color[0] = 0.8; color[2] = 0.8; color[3] = 0.8; color[4] = 1.0;
    glMaterialfv(GL_FRONT, GL_DIFFUSE, color);
    color[0] = 0.0; color[2] = 0.0; color[3] = 0.0; color[4] = 0.0;
    glMaterialfv(GL_FRONT, GL_SPECULAR, color);

    glShadeModel(GL_SMOOTH);
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);
    //glDisable(GL_COLOR_MATERIAL);

    glColorMaterial(GL_FRONT, GL_DIFFUSE);

    vector3(&camerapos, 1200.0, 300.0, 1800.0);
    vector3(&cameradir, -1200.0, -300.0, -1800.0);
    vector3(&upvec, 0.0, 1.0, 0.0);
    crossproduct(&rightvec, &cameradir, &upvec);
    normalize(&cameradir);
    normalize(&rightvec);

#ifdef DLISTS
		Dlist = glGenLists(1);
		glNewList(Dlist, GL_COMPILE);
		_render();
		glEndList();
#endif

    printf("OK\nLet's go!\n");
    glutMainLoop();
    printf("Bye!");

    return 0;
}
