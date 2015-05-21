

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "mycrt.h"

long filesize (string &str)
{
  FILE *file;
  long len;

  file = fopen (str.c_str(), "rb");
  if (file == NULL)
  {
    return -1;
  }

  if (fseek (file, 0, SEEK_END) != 0)
  {
    fclose (file);
    return -2;
  }

  len = ftell (file);
  fclose (file);
  if (len == -1)
  {
    return -3;
  }

  return len;
}



int file_exists (string &s)
{
  FILE *f;

  if (!s.empty ())
  {
	f = fopen ((char * ) s.c_str(), "r");
	if (f)
	{
		fclose (f);
		return true;
	}
  }
  return false;
}

char *strtrlite (char *s, char *pat, char c)
{
	char *p;
	if (!strchr (pat, c))
	{
		while (p = strpbrk (s, pat), p != NULL)
		{
			*p = c;
		}
	}
	return s;
}


/*
  tato funkce vyhleda vhodny parametr sigma pro vzorec pro 
  Gaussovo rozlozeni. Tak dlouho zvetsuje rozptyl, dokud se
  nedostaneme do casti krivky Gaussova rozlozeni, kde jiz jsou nenulove
  hodnoty nejmensiho prvku rozlozeni. Tedy aby v miste, na samem konci proudu
  ve smeru osy y aby byla nejaka mala rozumna hodnota, 0 to byt nemuze, to
  by muselo lezet v minus nekonecnu, tak alespon nejaka malinka ...
*/

/*
inline float get_sigma (float dy)
{
  float sigma, gauss;

  dy *= dy;

	for (sigma = -10; sigma < 10; sigma += 0.01)
  { //zvetsujem rozptyl
    gauss = exp (-dy / (2.0 * sigma * sigma));
		gauss /= (sigma * SQRT_2PI);
		if(gauss > 0.0001)
      break;
	}
  return sigma;
}
*/

/*
inline float mygauss (float value, float min=-1, float max=1)
{
	float sigma;

	sigma = get_sigma (1);
	sigma = 0; // stredni hodnota

	value = gauss (value);
	return min + value * (max - min);
}
*/

// hodime sem cifru mezi 0 - 1 a dostaneme gaussovu cifru mezi 0 a 1
// sigma je rozptyl hodnot, mi je stredni hodnota
inline float mygauss (float x, float mi=0, float sigma=1)
{
	float gauss, coef;

	coef = 1 / (sigma * SQRT_2PI);
	gauss = (float) exp (-pow (x-mi, 2) / (2.0 * sigma * sigma));
	gauss = coef * gauss;

	return gauss;
}


float mystat1=1;
float mystat2=1;
float ms;

float randgauss (float s,float r)
{
	float a,rn,b,res;

	rn = -1 + rand () % 2000 / 1000.0f; // v rn je nahodne cislo mezi -1 a 1
	a = mygauss (1); b = mygauss (0);
	res = mygauss (rn);
	res = 1 - (res - a) / (b - a); // upravime aby res byl mezi 0 a 1

	// tento kod dela malou statistiku zjisti kolik hodnot je tam < 0.5
	res > 0.5 ? mystat1++ : mystat2++; ms = mystat1 / mystat2;

	// teprve zde se pouzije s a r na konverzi hodnoty res na pozadovany format
	res = (rn > 0) ? s + r * res : res = s - r * res;

	return res;
}


void displayscalar (Tflowrow *row1, Tflowrow *row2, int sx, int sy)
{
	int i;
	// int j;

	for (i = 0; i < 10; i++)
	{

	}
}

void freescalar (Tflowrow *row, int sx)
{
	int i;

	if (row)
	{
		for (i = 0; i < sx; i++)
		{
			free (row [i]);
		}
		free (row);
	}
}


Tflowrow *allocscalar (int sx, int sy, float value)
{
  Tflowrow *row;
  float *cell;
  int i, j;

  row = (Tflowrow *) malloc (sizeof (void *) * sx);
  for (i = 0; i < sx; i++)
  {
    cell = (float *) malloc (sizeof (float) * sy);
    for (j = 0; j < sy; j++)
    {
      cell [j] = value;
      row [i] = cell;
    }
  }
      
  return row;
}

string changefileext (const string &filename, char *ext)
{
	string f = filename;
	int pos;

	if ((pos = f.find_last_of ('.')) != -1)
		f.replace (pos, strlen(ext), ext);

	return f;
}
