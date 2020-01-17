/***************************************************************************
                          linalgebra.c  -  description
                             -------------------
    begin                : Fri Nov 2 2001
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
#include <assert.h>
#include <math.h>
#include "linalgebra.h"

#ifndef PI
#define PI 3.14159265358979323
#endif

p_vector3 vector3(p_vector3 result, float x, float y, float z) {
  result->x = x;
  result->y = y;
  result->z = z;
  return result;
}

p_vector3 add(p_vector3 result, p_vector3 a, p_vector3 b) {
 result->x = a->x + b->x;
 result->y = a->y + b->y;
 result->z = a->z + b->z;
 return result;
}

p_vector3 minus(p_vector3 result, p_vector3 a, p_vector3 b) {
 result->x = a->x - b->x;
 result->y = a->y - b->y;
 result->z = a->z - b->z;
 return result;
}

p_vector3 crossproduct(p_vector3 result, p_vector3 a, p_vector3 b) {
 result->x = (a->y * b->z) - (a->z * b->y);
 result->y = (a->z * b->x) - (a->x * b->z);
 result->z = (a->x * b->y) - (a->y * b->x);
 return result;
}

p_vector3 scalarmul(p_vector3 result, p_vector3 vec, float scalar) {
 result->x = vec->x * scalar;
 result->y = vec->y * scalar;
 result->z = vec->z * scalar;
 return result;
}

p_vector3 normalize(p_vector3 vector) {
  float length;
  length = sqrt( (vector->x * vector->x) + (vector->y * vector->y) + (vector->z * vector->z) );
  /*printf("%.2f %.2f %.2f\n",vector->x, vector->y, vector->z);*/
  assert( fabs(length) > ALGEBRA_EPSILON );
  vector->x /= length;
  vector->y /= length;
  vector->z /= length;
  return vector;
}

p_vector3 rotate_y(p_vector3 vector, float angle)
{
	float x = vector->x;
	float z = vector->z;

	angle = angle/180.0 * PI;

	vector->x = x * cos(angle) + z * sin(angle);
	vector->z = z * cos(angle) - x * sin(angle);

  return vector;
}
