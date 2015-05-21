
#ifndef _bounce_h_
#define _bounce_h_

#include "stdafx.h"

#define myfloat float



inline bool mc_lieson (myfloat x, myfloat x1, myfloat x2, myfloat bound=0) //.001
{
  return (((x >= x1-bound) && (x <= x2+bound)) || ((x >= x2-bound) && (x <= x1+bound)));
}

inline bool mc_liesonall (myfloat xp, myfloat yp, 
                  myfloat x1, myfloat y1, myfloat x2, myfloat y2, myfloat bound=0) // .001
{
  return (mc_lieson (xp, x1, x2, bound) && mc_lieson (yp, y1, y2, bound));
}

/*
inline bool mc_liesontotal (myfloat xp, myfloat yp, 
                  myfloat x1e, myfloat y1e, myfloat x2e, myfloat y2e, myfloat x1p, myfloat y1p, myfloat x2p, myfloat y2p)
{
	return (mc_liesonall (xp, yp, x1e, y1e, x2e, y2e) && mc_liesonall (xp, yp, x1p, y1p, x2p, y2p));
}
*/

inline myfloat mc_findpoint (myfloat x, myfloat x1, myfloat y1, myfloat x2, myfloat y2)
{
  myfloat a,c,y;

  a = (y2 - y1) / (x2 - x1);
  c = -a * x1 + y1;
  y = a * x + c;
  return y;
}

inline bool mc_findzero (myfloat x, myfloat x1, myfloat y1, myfloat x2, myfloat y2, myfloat y1p, myfloat y2p, float &y)
{
  if (mc_lieson (x, x1, x2))
  {
    y = mc_findpoint (x, x1, y1, x2, y2);
//    if (mc_lieson (y, y1, y2, 0.001))
  //    return mc_lieson (y, y1p, y2p, 0.001);

     return mc_liesonall (y, y, y1, y1p, y2, y2p);
  }
  return false;
}


inline bool mc_edgeconflictstub (myfloat x1e, myfloat y1e, myfloat x2e, myfloat y2e,
                       myfloat x1p, myfloat y1p, myfloat x2p, myfloat y2p, float &xp, float &yp)
{
  myfloat ap, cp, ae, ce, da;
  myfloat dxp, dxe;

  if (!(dxp = x2p - x1p))
  {
	  xp = x2p;
    // pripad pro kolmou drahu castice 
    // v tomto pripade se bud dotykaji na souradnici x2p nebo x1p - protoze jsou stejne, anebo uz nikde
    return mc_findzero (x2p, x1e, y1e, x2e, y2e, y1p, y2p, yp);
  }
  
  if (!(dxe = x2e - x1e))
  {
	  xp = x2e;
    // pripad pro kolmou hranu kotle
    // v tomto pripade se bud dotykaji na souradnici x2e nebo x1e - protoze jsou stejne, anebo uz nikde
    return mc_findzero (x2e, x1p, y1p, x2p, y2p, y1e, y2e, yp);
  }

  // vypocteme smernice pro obe primky
  ap = (y2p - y1p) / dxp; ae = (y2e - y1e) / dxe;

  // otestujeme, zda primky nejsou nahodou rovnobezne
  if (!(da = ap - ae))
    return false;
    
  cp = y1p - ap * x1p; ce = y1e - ae * x1e; 

  /* zkontrolujeme, zda nalezene body pruseciku primek se nalezaji
     na obou useckach
  */
  
	xp = (ce - cp) / da; 
	
	/*
	yp = xp * ae + ce;
	if (mc_liesonall (xp, yp, x1e, y1e, x2e, y2e) && mc_liesonall (xp, yp, x1p, y1p, x2p, y2p))
		return true;
	*/
	
    yp = xp * ap + cp; 
	return (mc_liesonall (xp, yp, x1e, y1e, x2e, y2e, 0.001f) && mc_liesonall (xp, yp, x1p, y1p, x2p, y2p, 0.001f));

	//return false;
}


#define mc_edgeconflict mc_edgeconflictstub
/*
// zjisti zda se hrany dotykaji. Vraci x-ovou souradnici doteku - pruseciku
inline bool mc_edgeconflict (myfloat x1e, myfloat y1e, myfloat x2e, myfloat y2e,
                       myfloat x1p, myfloat y1p, myfloat x2p, myfloat y2p, float &xp, float &yp)
{
	if (mc_edgeconflictstub (x1e, y1e, x2e, y2e, x1p, y1p, x2p, y2p, xp, yp))
		return true;
	return mc_edgeconflictstub (x1p, y1p, x2p, y2p, x1e, y1e, x2e, y2e, xp, yp);
}
*/

inline bool mc_boxconflict (myfloat ex1, myfloat ey1, myfloat ex2, myfloat ey2, myfloat x1, myfloat y1, myfloat x2, myfloat y2)
{
  float x, y;

  // prvni je hrana, druhy je box

  // pokud jsou oba body uvnitr boxu return true
  if (mc_liesonall (ex1, ey1, x1, y1, x2, y2) && mc_liesonall (ex2, ey2, x1, y1, x2, y2))
	  return true;

  if (mc_edgeconflict (ex1, ey1, ex2, ey2, x1, y1, x1, y2, x, y))
    return true;
  if (mc_edgeconflict (ex1, ey1, ex2, ey2, x1, y2, x2, y2, x, y))
    return true;
  if (mc_edgeconflict (ex1, ey1, ex2, ey2, x2, y2, x2, y1, x, y))
    return true;
  if (mc_edgeconflict (ex1, ey1, ex2, ey2, x2, y1, x1, y1, x, y))
    return true;

  return false;
}

inline bool mc_boxconflict2 (myfloat ex1, myfloat ey1, myfloat ex2, myfloat ey2, myfloat x, myfloat y, myfloat r)
{
	return mc_boxconflict (ex1, ey1, ex2, ey2, x - r, y - r, x + r, y + r);
}



#undef myfloat

#endif
