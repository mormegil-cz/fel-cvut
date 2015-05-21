
/*
 * MGL graphics library
 * Copyright (C) 1999-2000, ing. Marek Gayer

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.

 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.

 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#ifndef _MGL_H_
  #define _MGL_H_

  #ifdef __cplusplus
    extern "C" {
  #endif

  #include <stdlib.h>
  #include <GL/glut.h>

  #define MGL_VERSION 110
#define MGL_CALLBACK GLUTCALLBACK

/***********************/
/* header for window.c */
/***********************/

extern int mglOpenWindow (char *title, int x, int y, int width, int height);
extern int mglOpenFullscreen (char *title);

extern void mglSetWindowSize (int x, int y);
extern void mglSetWindowTitle (char *title);

extern int mglScreenWidth;
extern int mglScreenHeight;

extern int mglWindowWidth;
extern int mglWindowHeight;

typedef void (MGL_CALLBACK *mglHandlerType) (void);
extern void mglSetDisplayHandler (mglHandlerType);
extern void mglSetIdleHandler (mglHandlerType);
extern int mglReshaped;
extern int mglFrames;

extern void mglRun (void);

/***********************/
/* header for mgl.c    */
/***********************/

extern void mglBegin (void);
extern void mglEnd (void);

extern float mglVirtualWidth;
extern float mglVirtualHeight;
extern float mglVirtualStartX;
extern float mglVirtualStartY;
extern float mglVirtualEndX;
extern float mglVirtualEndY;
extern float mglCoordX (float x);
extern float mglCoordY (float y);
extern float mglUncoordX (float x);
extern float mglUncoordY (float y);

extern void mglSetupView (float left, float right, float bottom, float top);

extern char *mglVendor;
extern char *mglRenderer;
extern char *mglVersion;
extern char *mglExtensions;
extern char *mglGLUVersion;
extern char *mglGLUExtensions;

extern int mglMaxTextureSize;
extern int mglBitsPerPixel;

/*
extern int MGL_EXT_abgr;
extern int MGL_EXT_bgra;
extern int MGL_EXT_texture;
*/


/***********************/
/* header for draw.c   */
/***********************/

typedef struct {
  float r;
  float g;
  float b;
  float a;
} mglColor;

extern mglColor mglBlack;
extern mglColor mglRed;
extern mglColor mglGreen;
extern mglColor mglBlue;
extern mglColor mglYellow;
extern mglColor mglCyan;
extern mglColor mglMagenta;
extern mglColor mglWhite;
extern mglColor mglBlank;

extern mglColor mglPink;
extern mglColor mglCeleste;

extern mglColor *mglColorBase [];

#define MGL_COLORMODE_INVERT 1
#define MGL_COLORMODE_SHADES 2
#define MGL_COLORMODE_RED 4
#define MGL_COLORMODE_BLUE 8
#define MGL_COLORMODE_GREEN 16

extern int mglColorMode;
extern mglColor mglAdjustColor (mglColor c, int mode);

extern mglColor mglMixColors (mglColor color1, mglColor color2, float rate);

extern void mglClearScreen (mglColor clearColor);

extern void mglFullScreenOn (void);
extern void mglFullScreenOff (void);

  extern void mglAlphaOff (void);
  extern void mglAlphaOn (void);
  extern void mglLineStippleOn (void);
  extern void mglLineStippleOff (void);
  extern void mglSetLineStipple (int factor, unsigned short stipple);

  extern void mglPointSmoothOn (void);
  extern void mglPointSmoothOff (void);
  extern void mglSmoothOn (void);
  extern void mglSmoothOff (void);

  extern float mglRotationAngle;

  extern mglColor mglPointColor;
  extern float mglMaxPointSize;
  extern float mglSetPointSize (float size);
  extern void mglDrawPoint (float x, float y);

  extern void mglInterpolationOn (void);
  extern void mglInterpolationOff (void);

  extern mglColor mglLineColor [2];
  extern void mglSetLineColors (mglColor color);
  extern float mglMaxLineSize;
  extern float mglSetLineSize (float size);
  extern void mglDrawLine (float x1, float y1, float x2, float y2);

  extern mglColor mglTriangleColor [3];
  extern void mglSetTriangleColors (mglColor color);
  extern void mglDrawTriangle (float x1, float y1, float x2, float y2, float x3, float y3);
  extern void mglDrawFilledTriangle (float x1, float y1, float x2, float y2, float x3, float y3);

  extern mglColor mglRectangleColor [4];
  extern void mglSetRectangleColors (mglColor color);
  extern void mglDrawRectangle (float x, float y, float width, float height);
  extern void mglDrawFilledRectangle (float x, float y, float width, float height);
  void mglDrawFilledTriangleRectangle (float x, float y, float sx, float sy, mglColor c);


/***********************/
/* header for images.c */
/***********************/

typedef struct {
	int w;
	int h;
	int d;
  int type;
  int xrev;
  int yrev;
	unsigned char *data;
  unsigned char **mipmap;
} mglImage;

extern mglImage *mglNewImage (void);

extern mglImage *mglCreateImage (int width, int height, int depth);

extern mglImage *mglLoadImage (char* filename, int alpha);

extern mglImage *mglGetRGBImage (int x, int y, int w, int h);

extern void mglSaveImage (char* fileName, mglImage *image);

extern void mglDeleteImage (mglImage *image);


/*************************/
/* header for textures.c */
/*************************/

typedef struct {
  mglImage image;
  int name;
  int residancechecked;
} mglTexture;

extern mglTexture *mglMakeTexture (mglImage *image, int mode);
extern mglTexture *mglLoadTexture (char *fileName, int imageMode, int textureMode);

extern mglColor mglTextureColor [4];
extern void mglSetTextureColors (mglColor color);

extern float mglTextureZoomX;
extern float mglTextureZoomY;
extern void mglSetTextureZoom (float zoom);

extern void mglDrawTexture (mglTexture *texture, float x, float y);
extern void mglDrawSubTexture (mglTexture *texture, float x, float y, int fromx, int fromy,
                         int width, int height);
extern void mglDrawTexturedRectangle (mglTexture *texture, float x, float y,
                                float width, float height);

extern void mglChangeTexture (mglTexture *texture);
extern void mglDeleteTexture (mglTexture *texture);

extern int mglResidentTexturesTotal;
extern int mglResidentTexturesMemory;
extern int mglLocalTexturesTotal;
extern int mglLocalTexturesMemory;

/*************************/
/* header for memory.c   */
/*************************/

extern int _mallocated;
extern int _mtotal;
#define _mfree ((_mtotal<_mallocated) ? 0 : (_mtotal - _mallocated))
extern int _allocCount;
extern int _freeCount;
extern void *_alloc (int size);
extern void *_malloc (int size);
extern void *_calloc (int n, int size);
extern void *_realloc (void *mem, int newsize);
extern void _free (void *mem);
extern void _mexit (int requestedMemorySize);

/*************************/
/* header for fonts.c    */
/*************************/

typedef struct
{
  int x;
  int y;
  int width;
  int height;
} mglXY;

typedef struct
{
  int list;
  mglTexture *texture;
  int w;
  int h;
  int spacingX;
  int spacingY;
  mglXY chars [256];
} mglFont;

extern mglFont *mglNewFont (void);

extern void mglLoadFontDefinitionTextFile(mglFont *font, char *textFileName);

extern mglFont *mglLoadFont (char *textFileName, char *imageFileName);

extern int mglPutc (mglFont *font, char c, float x, float y);

extern int mglFontWrap;
extern int mglFontWordWrap;

extern void mglPuts (mglFont *font, char *s, float x, float y);

extern void mglDeleteFont (mglFont *font);

/************************/
/* header for input.c   */
/************************/

extern int mglTime;
extern int mglSystemTime;
#define mglHours(ms) (ms/1000/3600)
#define mglMinutes(ms) ((ms/1000/60)%60)
#define mglSecs(ms) ((ms/1000)%60)
#define mglMilisecs(ms) ((ms)%1000)
extern void mglResetTimer (void);
extern void mglSleep (int ms);
extern float mglFps;
extern void mglShowFPS (mglFont *font, float x, float y);

#define MGL_KEY_F1 (GLUT_KEY_F1+256)
#define MGL_KEY_F2 (GLUT_KEY_F2+256)
#define MGL_KEY_F3 (GLUT_KEY_F3+256)
#define MGL_KEY_F4 (GLUT_KEY_F4+256)
#define MGL_KEY_F5 (GLUT_KEY_F5+256)
#define MGL_KEY_F6 (GLUT_KEY_F6+256)
#define MGL_KEY_F7 (GLUT_KEY_F7+256)
#define MGL_KEY_F8 (GLUT_KEY_F8+256)
#define MGL_KEY_F9 (GLUT_KEY_F9+256)
#define MGL_KEY_F10 (GLUT_KEY_F10+256)
#define MGL_KEY_F11 (GLUT_KEY_F11+256)
#define MGL_KEY_F12 (GLUT_KEY_F12+256)
#define MGL_KEY_LEFT (GLUT_KEY_LEFT+256)
#define MGL_KEY_UP (GLUT_KEY_UP+256)
#define MGL_KEY_RIGHT (GLUT_KEY_RIGHT+256)
#define MGL_KEY_DOWN (GLUT_KEY_DOWN+256)
#define MGL_KEY_PAGE_UP (GLUT_KEY_PAGE_UP+256)
#define MGL_KEY_PAGE_DOWN (GLUT_KEY_PAGE_DOWN+256)
#define MGL_KEY_HOME (GLUT_KEY_HOME+256)
#define MGL_KEY_END (GLUT_KEY_END+256)
#define MGL_KEY_INSERT (GLUT_KEY_INSERT+256)
#define MGL_KEY_DELETE 127

typedef void (MGL_CALLBACK *mglKeyboardHandlerType) (int key);
extern void mglSetKeyboardHandler (mglKeyboardHandlerType handler);

extern int mglLastKey;
extern int mglShift;
extern int mglCtrl;
extern int mglAlt;

extern int mglMouseIn;
#define mglMouseX (mglCoordX (mglMX))
#define mglMouseY (mglCoordY (mglMY))
extern float mglMX;
extern float mglMY;


#define MGL_LEFT_BUTTON_DOWN 1
#define MGL_LEFT_BUTTON_UP 2
#define MGL_RIGHT_BUTTON_DOWN 3
#define MGL_RIGHT_BUTTON_UP 4
#define MGL_MIDDLE_BUTTON_DOWN 5
#define MGL_MIDDLE_BUTTON_UP 6

typedef void (MGL_CALLBACK *mglMouseHandlerType) (int event);
extern void mglSetMouseHandler (mglMouseHandlerType handler);

extern int mglMouseLeft;
extern int mglMouseRight;
extern int mglMouseMiddle;
extern int mglMouseState;

extern void mglSetMouseCursor (int cursor);

/*
#define GLUT_CURSOR_INHERIT GLUT_CURSOR_INHERIT
#define GLUT_CURSOR_NONE GLUT_CURSOR_NONE
#define GLUT_CURSOR_FULL_CROSSHAIR GLUT_CURSOR_FULL_CROSSHAIR
#define GLUT_CURSOR_RIGHT_ARROW GLUT_CURSOR_RIGHT_ARROW
#define GLUT_CURSOR_LEFT_ARROW GLUT_CURSOR_LEFT_ARROW
#define GLUT_CURSOR_INFO GLUT_CURSOR_INFO
#define GLUT_CURSOR_DESTROY GLUT_CURSOR_DESTROY
#define GLUT_CURSOR_HELP GLUT_CURSOR_HELP
#define GLUT_CURSOR_CYCLE GLUT_CURSOR_CYCLE
#define GLUT_CURSOR_SPRAY GLUT_CURSOR_SPRAY
#define GLUT_CURSOR_WAIT GLUT_CURSOR_WAIT
#define GLUT_CURSOR_TEXT GLUT_CURSOR_TEXT
#define GLUT_CURSOR_CROSSHAIR GLUT_CURSOR_CROSSHAIR
#define GLUT_CURSOR_UP_DOWN GLUT_CURSOR_UP_DOWN
#define GLUT_CURSOR_LEFT_RIGHT GLUT_CURSOR_LEFT_RIGHT
#define GLUT_CURSOR_TOP_SIDE GLUT_CURSOR_TOP_SIDE
#define GLUT_CURSOR_BOTTOM_SIDE GLUT_CURSOR_BOTTOM_SIDE
#define GLUT_CURSOR_LEFT_SIDE GLUT_CURSOR_LEFT_SIDE
#define GLUT_CURSOR_RIGHT_SIDE GLUT_CURSOR_RIGHT_SIDE
#define GLUT_CURSOR_TOP_LEFT_CORNER GLUT_CURSOR_TOP_LEFT_CORNER
#define GLUT_CURSOR_TOP_RIGHT_CORNER GLUT_CURSOR_TOP_RIGHT_CORNER
#define GLUT_CURSOR_BOTTOM_RIGHT_CORNER GLUT_CURSOR_BOTTOM_RIGHT_CORNER
#define GLUT_CURSOR_BOTTOM_LEFT_CORNER GLUT_CURSOR_BOTTOM_LEFT_CORNER
*/

/***********************/
/* header for errors.c */
/***********************/

#define MGL_ERROR_UNKNOWN 1

#define MGL_ERROR_COMPILER_OPTIONS 2

#define MGL_ERROR_NOT_ENOUGH_MEMORY 3

#define MGL_ERRORS_OPENGL_CALLS 4

#define MGL_ERROR_USER 5

#define MGL_ERROR_NO_OPENGL_DRIVERS 11
#define MGL_ERROR_NO_GLU_DRIVERS 12
#define MGL_ERROR_UNKNOWN_OPENGL_VERSION 13

#define MGL_ERROR_SUBTEXTURE_COORDINATES 21
#define MGL_ERROR_UNKNOWN_TEXTURE_MODE 22
#define MGL_ERROR_TOO_BIG_TEXTURE 23
#define MGL_ERROR_TEXTURE_SIZE 24
#define MGL_ERROR_MISSING_MIPMAP 25

#define MGL_ERROR_OPENING_FILE 41
#define MGL_ERROR_WRITING_FILE 42
#define MGL_ERROR_CREATING_FILE 43

#define MGL_ERROR_READING_IMAGE_DATA 44
#define MGL_ERROR_READING_TGA_HEADER 45
#define MGL_ERROR_READING_SGI_HEADER 46
#define MGL_ERROR_READING_BMP_HEADER 47

#define MGL_ERROR_UNSUPPORTED_IMAGE 48
#define MGL_ERROR_UNSUPPORTED_TGA_IMAGE 49
#define MGL_ERROR_UNSUPPORTED_BMP_IMAGE 50
#define MGL_ERROR_UNSUPPORTED_SGI_IMAGE 51

#define MGL_ERROR_FONT_DEFINITION_FILE 61

  extern int mglLog;

  typedef void (MGL_CALLBACK *mglErrorHandlerType) (int errorNumber, char *errorMessage);
  extern void mglSetErrorHandler (mglErrorHandlerType func);
  extern void mglDefaultErrorHandler (int errorNumber, char *errorMessage);

  extern void mglError (int errorNumber, char *errorMessage);
  extern char *mglErrorType;

  extern void mglMessage (char *title, char *message);
  extern void mglNote (char *note);

  #ifdef _DEBUG
      extern void mglTestOpenGLError (void); /* this function is undocumented */
  #else
    #define mglTestOpenGLError()
  #endif

  #ifdef __cplusplus
    }
  #endif

#endif
