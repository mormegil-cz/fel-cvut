/***************************************************************************
                          linalgebra.h  -  description
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
#ifndef __LINALGEBRA_H
#define __LINALGEBRA_H

#define ALGEBRA_EPSILON 0.001

typedef union tagVector2 {
	struct {
		float x, y;
	};
	float m[2];
} t_vector2, *p_vector2;

typedef union tagVector3 {
	struct {
		float x, y, z;
	};
	float m[3];
} t_vector3, *p_vector3;

p_vector3 vector3(p_vector3 result, float x, float y, float z);
p_vector3 add(p_vector3 result, p_vector3 a, p_vector3 b);
p_vector3 minus(p_vector3 result, p_vector3 a, p_vector3 b);
p_vector3 scalarmul(p_vector3 result, p_vector3 vec, float scalar);
p_vector3 crossproduct(p_vector3 result, p_vector3 a, p_vector3 b);
p_vector3 normalize(p_vector3 vector);
p_vector3 rotate_y(p_vector3 vector, float angle);

#endif // ifdef __LINALGEBRA_H
