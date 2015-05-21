
#ifndef __VECTOR_H__
#define __VECTOR_H__


#include "stdafx.h"
#include "bounce.h"

#define PI_2_MULT 6.2831853
#define PI_2_DIV 1.57079633

#define DEG_TO_RAD (M_PI/180)

/**
Trida reprezentujici vektor v prostoru
umoznuje nastaveni, scitani a prevod z/na uhly
*/


class Tintvector
{
public:
    int x, y;
    inline Tintvector (int nx, int ny)
    {
        x = nx; y = ny;
    }
    inline Tintvector (void)
    {
        x = y = 0;
    }
};

class Tvector
{
    // souradnice x y
public:
    float x, y;
    /**
    Defoltni konstruktor
    vytvori vektor s parametry (0, 0)
    */
    
    Tvector(void) { 
        x = y = 0.0;
    }

    /**
    Kopirujici konstruktor
    vytvori vektor odpovidajico vektoru v
    @param v vektor
    */
    
    Tvector(const Tvector &v) {
        x = v.x; y = v.y;
    }
    
    Tvector(float nx, float ny) {
        x = nx; y = ny;
    }
    
    Tvector(float xy) {
        x = y = xy;
    }
    
    Tvector (Tintvector a)
    {
        x = a.x; y = a.y;
    }
    
    
    /**
    Konstruktor s pocatecnim nastavenim
    vytvori vektor a nastavi podle parametru
    @param x x-ova souradnice
    @param y y-ova souradnice
    */
    ////////////////////////////////////////////////// pristupy
    
    inline void set(float x, float y) {
        this->x = x; this->y = y;
    }
    
    inline void set(const Tvector &p) {
        x = p.x; y = p.y;
    }
    
    inline float volume (void)
    {
        return x * y;
    }
    
    inline float X() {
        return x;
    }
    
    inline float Y() {
        return y;
    }
    
    /*
    inline void bounce (Tvector b)
    {
    while (limit (b))
		  x /= 2, y /= 2;
                  }
    */
    
    inline void boundcheck (Tvector b1, Tvector b2)
    {
        if (x < b1.x) x = b1.x;
        if (y < b1.y) y = b1.y;
        if (x > b2.x) x = b2.x;
        if (y > b2.y) y = b2.y;
    }
    
    inline void zeroboundcheck (Tvector b)
    {
        boundcheck (0, b);
    }
    
    
    /**
    Nastavi vektor podle uhlu
    @param angle uhel v radianech
    @param magnitude
    */
    inline void setAngle(float angle, float magnitude ) {
        x = (float) cos(angle) * magnitude;
        y = (float) sin(angle) * magnitude;
    }
    
    /**
    Nastavi vektor podle uhlu
    @param angle uhel ve stupnich
    @param magnitude
    */
    inline void setAngleDeg(float angle, float magnitude ) {
        x = (float) cos(angle*DEG_TO_RAD) * magnitude;
        y = (float) sin(angle*DEG_TO_RAD) * magnitude;
    }
    
    
    /**
    Vrati uhel vektor
    uhel je v radianech 
    */
    inline float angle() {
        return (float) atan2(y, x);
    }
    
    inline float vecangle (Tvector v)
    {
        float dx, dy, tan;
        
        dx = v.x - x; dy = v.y - y;
        
        if (dx == 0)
            return dy > 0 ? M_PI / 2 : - M_PI / 2;
        
        tan = dy /dx;
        tan = atan (tan);
        if (dx < 0)	
            tan += (float)M_PI;
        
        return tan;
    }
    
    inline float zeroangle (void)
    {
        return Tvector(0,0).vecangle (Tvector (x,y));
    }
    
    /**
    *  Vrati velikost vektoru
    * @return velikost vektoru
    */
    
    inline Tvector abs () {
        Tvector a;
        a.x = fabs (x); a.y = fabs (y);
        return a;
    }
    
    inline float magnitude() {
        return (float) sqrt(x * x + y * y);
    }
    
    operator float () {
        return magnitude();
    }
    
    operator Tintvector () {
        return Tintvector (x, y);
    }
    
    /**
    * Vrati vzdalenost bodu od primky z bodu a b
    * (nejedna se presne o vzdalenost, je to hranova fukce)
    * @return vzdalenost
    */
    inline float destination(
        const Tvector &a, const Tvector &b) {
        return (x - b.x)*(b.y - a.y) - (y - b.y)*(b.x - a.x);
    }
    
    // float dist(const Tvector &a, const Tvector &b);
    
    /**
    * Vypise hodnotu vektoru na stderr
    */
    inline void print() {
        fprintf( stderr,"(%f, %f)\n", x, y);
    }
    
    
    /**
    * Normalizuje vektor na velikost 1
    */
    
    inline Tvector normalize() {
        float r = magnitude();
        if (r != 0.0)
            x /= r; y /= r;
        return *this;
    }
    
    /////////////////// PRIRAZOVACI OPERATORY ////////////////////////////// 
    
    ///
    inline Tvector operator=(const Tvector &a) {
        x = a.x;
        y = a.y;
        return *this;
    }
    
    
    inline Tvector operator-(void) {
        x = -x;
        y = -y;
        return *this;
    }
    
    /*
    inline Tvector operator=(const Tvector &a) {
    return *this;
    }
    */
    
    
    /**
    Prirazovaci operator plus
    @param a vektor
    @return a + this
    */
    inline Tvector operator+=(const Tvector &a) {
        x += a.x;
        y += a.y;
        return *this;
    }
    
    /**
    Prirazovaci operator minus
    @param a vektor
    @return a - this
    */
    Tvector operator-=(const Tvector &a) {
        x -= a.x;
        y -= a.y;
        return *this;
    }
    /**
    Prirazovaci operator nasobeni
    @param a vektor
    @return a * this
    */
    Tvector operator*=(const Tvector &a) {
        x *= a.x;
        y *= a.y;
        return *this;
    }
    
    Tvector operator*=(int a) {
        x *= a;
        y *= a;
        return *this;
    }
    
    /**
    Prirazovaci operator deleni
    @param a vektor
    @return a / this
    */
    Tvector operator/=(const Tvector &a) {
        x /= a.x;
        y /= a.y;
        return *this;
    }
    
    
    Tvector operator/=(float a) {
        x /= a;
        y /= a;
        return *this;
    }
    
    Tvector operator/=(int a) {
        x /= a;
        y /= a;
        return *this;
    }
    
    /////////////////// BINARNI OPERATORY ////////////////////////////// 
    
    /**
    Binarni operator plus
    @param a vektor
    @param b vektor
    @return a + b
    */
    static inline Tvector operator+(const Tvector &a, const Tvector &b) {
        return Tvector(a.x + b.x, a.y + b.y);
    }
    
    /**
    Binarni operator krat
    @param a vektor
    @param b vektor
    @return a * b
    */
    friend inline Tvector operator*(const Tvector &a, const Tvector &b) {
        return Tvector(a.x * b.x, a.y * b.y);
    }
    
    /**
    Binarni operator krat
    @param a float
    @param b vektor
    @return a * b
    */
    friend inline Tvector operator*(float a, const Tvector &b) {
        return Tvector(a * b.x, a * b.y);
    }
    
    
    /**
    Binarni operator krat
    @param a vektor
    @param b float
    @return a * b
    */
    friend inline Tvector operator*(const Tvector &a, float b) {
        return Tvector(a.x * b, a.y * b);
    }
    
    /**
    Binarni operator minus
    @param a vektor
    @param b vektor
    @return a - b
    */
    static inline Tvector operator - (const Tvector &a, const Tvector &b) {
        return Tvector(a.x - b.x, a.y - b.y);
    }
    
    /**
    Binarni operator plus
    @param a vektor
    @param b vektor
    @return a + b
    */
    friend inline Tvector operator/(const Tvector &a, const Tvector &b) {
        return Tvector(a.x / b.x, a.y / b.y);
    }
    
    friend inline Tvector operator/(const Tvector &a, int b) {
        return Tvector(a.x / b, a.y / b);
    }
    
    friend inline Tvector operator/(const Tvector &a, float b) {
        return Tvector(a.x / b, a.y / b);
    }
    
    friend inline Tvector operator*(const Tvector &a, int b) {
        return Tvector(a.x * b, a.y * b);
    }
    
    inline bool insideof (float x1, float y1, float x2, float y2)
    {
        return ((x >= x1) && (x <= x2) && (y >= y1) && (y <= y2));
    }
    
    inline bool insideof (Tvector p1, Tvector p2)
    {
        return ((x >= p1.x) && (x <= p2.x) && (y >= p1.y) && (y <= p2.y));		
    }
    
    inline bool insideofme (Tvector p, float r)
    {
        return insideof (p - Tvector(r), p + Tvector(r));
    }
    
    /**
    * Otoci bod a kolem bodu uhel pAngle
    * nevraci spet do bodu otaceni,
    * otaci po smeru hodinovy rucicek
    * @param a - bod ktery se otoci
    * @param pAngle - uhel o ktery se bod otoci
    * @return bod po otoceni
    */
    Tvector turn(const Tvector &a, float pAngle)
    {
        float rx, ry;
        
#if defined(DEBUG_VECTOR)
        fprintf( stderr,
            "Tvector::turn(angle: %f)\n",
            pAngle);
        Tvector(a).print();
        print();
#endif
        rx = (a.x - x) * (float) cos(pAngle) +
            (a.y - y) * (float) sin(pAngle);
        ry = (a.y - y) * (float) cos(pAngle) -
            (a.x - x) * (float) sin(pAngle);
        return Tvector(rx, ry);
    }
    
    /**
    * Otoci bod a kolem bodu uhel pAngle
    * vraci spet do bodu otaceni,
    * otaci po smeru hodinovy rucicek
    * @param a - bod ktery se otoci
    * @param pAngle - uhel o ktery se bod otoci
    * @return bod po otoceni
    */
    
    
    inline Tvector turnRight(const Tvector &a, float pAngle)
    {
        float rx, ry;
        
#if defined(DEBUG_VECTOR)
        fprintf( stderr,
            "Tvector::turnRight(angle: %f)\n",
            pAngle);
        Tvector(a).print();
        print();
#endif
        rx = (a.x - x) * (float) cos(pAngle) + (a.y - y) * (float) sin(pAngle) + x;
        ry = (a.y - y) * (float) cos(pAngle) - (a.x - x) * (float) sin(pAngle) + y;
        return Tvector(rx, ry);
    }
    
    
};

inline Tvector vecedgeconflict (Tvector e1, Tvector e2, Tvector p1, Tvector p2)
{
    Tvector a;
    
    if (!mc_edgeconflict (e1.x, e1.y, e2.x, e2.y, p1.x, p1.y, p2.x, p2.y, a.x, a.y))
        a.x = a.y = 0;
    return a;
}

#endif

