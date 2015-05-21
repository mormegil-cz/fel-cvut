
#ifndef _mycrt_h_
#define _mycrt_h_

#include <math.h>
#include <stdlib.h>
#include <string.h>

#include <string>
#include <fstream>
#include <iostream>
using namespace std;

/* Some <math.h> files do not define M_PI... */
#ifndef M_PI
#define M_PI ((float)3.14159265358979323846)
#endif
#define M_PIX2 (2*(float)M_PI)
#define SQRT_2PI ((float)2.506628274631000502415765284811)

typedef float *Tflowrow;

Tflowrow *allocscalar (int sx, int sy, float value=0);
void freescalar (Tflowrow *row, int sx);

long filesize (string &str);

int file_exists (string &s);

string changefileext (const string &filename, char *ext);

char *strtrlite (char *s, char *pat, char c);

float randgauss (float s=0, float r=1);

inline float lesser (float a, float b)
{
	return a < b ? a : b;
}

inline float bigger (float a, float b)
{
	return a > b ? a : b;
}

inline float deg2angle (float deg)
{
  return (deg * M_PI) / 180;
}

inline float angle2deg (float angle)
{
  return (180 * angle) / M_PI;
}

inline int sign (float x)
{
  return x > 0 ? 1 : -1;
}

inline float magnitude (float x, float y)
{
  return (float) sqrt (x * x + y * y);
}


/*
inline float sqrtn (float x, float n)
{
  return exp (log (x/100)/n);
}
*/


inline float normangle (float angle)
{
  if (angle > 0)
    while (angle > M_PIX2)
      angle -= (float)M_PIX2;
  else
    while (angle < 0)
      angle += (float)M_PIX2;

  return angle;
}

inline float normangle2 (float angle)
{
	angle = normangle (angle);

	if (angle > M_PI)
		angle -= M_PI;

	return angle;
}

/*
inline float normangle (float angle)
{
  if (angle > 0)
    while (angle > M_PIX2)
      angle -= (float)M_PIX2;
  else
    while (angle < 0)
      angle += (float)M_PIX2;

  return angle;
}
*/


inline float spherevolume (float d)
{
	return M_PI * (d * d * d / 6);
}

inline float sphereface (float d)
{
	return M_PI * d * d / 4;
}

inline float coord (float x, float min, float max)
{
	return min + x * (max - min);
}

inline float uncoord (float x, float min, float max)
{
	return (x - min) / (max - min);
}

inline float boundto (float value, float min, float max)
{
	if (value > max) return max;
	else if (value < min) return min;
	return value;
}

inline bool bound (float value, float min, float max)
{
	return (value > min) && (value < max);
}

inline bool boundxy (float vx, float vy, float minx, float miny, float maxx, float maxy)
{
	return bound (vx, minx, maxx) && bound (vy, miny, maxy);
}

inline int absmod (int x, int mod)
{
	x = x % mod; if (x < 0) x += mod; return x;
}

inline bool empty (char *str)
{
	return !str || !str[0];
}

inline char *basename (string &str)
{
	char *s;
	char *ptr;
	
	ptr = (char *)str.c_str();
	s = ptr + str.length();

	while (s > ptr)
	{
		if (*s == '\\' || *s == '/')
		{
			++s;
			break;
		}
		s--;
	}
	return s;
}

inline string readfile (string &filename)
{
	string s="";

	ifstream in(filename.c_str());
	string line;
	while(getline(in, line))
	{
		s += line + "\n";
	}
	return s;
}

/*
inline float interpolate (float val1, float val2, float ratio)
{
	return val1 * ratio + val2 * (1 - ratio);
}
*/

inline void combine_floatarray (float *dest, int size, float *s0, float *s1, float ratio)
{
	int i;

	for (i = 0; i < size; i++)
	{
		dest [i] = coord (ratio, s0[i], s1[i]);
	}
}

inline string strip_ext (string &prj)
{
	int pos;

	return ((pos = prj.find_last_of (".")) == -1) ? prj : prj.substr (0, pos);
}

#endif
