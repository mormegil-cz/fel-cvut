#include <stdio.h>
#include <stdlib.h>

void fail(const char *msg)
{
  fflush(stdout);
  perror(msg);
  exit(1);
}

float rndnum(float min, float max)
{
    return 0.5 + min + (max-min)/RAND_MAX * rand();
}

int main(int argc, char *argv[])
{
    int xsize = 10, ysize = 10;
    float bx = 100.0, by = 100.0, sx = 10.0, sy = 10.0, hmin = 20.0, hmax = 400.0;
    int texmin = 0, texmax = 1, rooftexmin = 0, rooftexmax = 1;
    float blockx, blocky;

    int x, y;
    FILE *fd;

    if (argc > 1) xsize = atoi(argv[1]);
    if (argc > 2) ysize = atoi(argv[2]);
    if (argc > 3) bx = atof(argv[3]);
    if (argc > 4) by = atof(argv[4]);
    if (argc > 5) sx = atof(argv[5]);
    if (argc > 6) sy = atof(argv[6]);
    if (argc > 7) hmin = atof(argv[7]);
    if (argc > 8) hmax = atof(argv[8]);
    if (argc > 9) texmin = atoi(argv[9]);
    if (argc > 10) texmax = atoi(argv[10]);
    if (argc > 11) rooftexmin = atoi(argv[11]);
    if (argc > 12) rooftexmax = atoi(argv[12]);
    if (argc > 13) {
        printf("Usage: %s xsize ysize bx by sx sy hmin hmax texmin texmax rooftexmin rooftexmax\n", argv[0]);
        return 2;
    }
    blockx = bx + sx;
    blocky = by + sy;

    if ((fd = fopen("data/city.dat", "w")) == NULL) fail("Cannot create data/city.dat");
    fprintf(fd, "4\n");
    fprintf(fd,"0.0\t0.0\t0.0\t0.0\t0.0\n");
    fprintf(fd,"0.0\t0.0\t%.2f\t1.0\t0.0\n",ysize*blocky);
    fprintf(fd,"%.2f\t0.0\t%.2f\t1.0\t1.0\n",xsize*blockx,ysize*blocky);
    fprintf(fd,"%.2f\t0.0\t0.0\t0.0\t1.0\n",xsize*blockx);
    if (fclose(fd)) fail("Error closing data/city.dat");

    if ((fd = fopen("data/buildings.dat", "w")) == NULL) fail("Cannot create data/buildings.dat");
    fprintf(fd, "%d\n", xsize*ysize);
    for (x = 0; x < xsize; x++) {
     for (y = 0; y < ysize; y++) {
        float xp = x*blockx;
        float yp = y*blocky;
        fprintf(fd, "%.2f\t0.0\t%.2f\t0.0\t0.0\n",xp,yp);
        fprintf(fd, "%.2f\t0.0\t%.2f\t0.0\t1.0\n",xp,yp+by);
        fprintf(fd, "%.2f\t0.0\t%.2f\t1.0\t1.0\n",xp+bx,yp+by);
        fprintf(fd, "%.2f\t0.0\t%.2f\t1.0\t0.0\n",xp+bx,yp);
        fprintf(fd, "%.2f\t%d\t%d\n\n", rndnum(hmin, hmax), (int)rndnum(texmin, texmax), (int)rndnum(rooftexmin, rooftexmax));
     }
    }
    if (fclose(fd)) fail("Error closing data/buildings.dat");
}
