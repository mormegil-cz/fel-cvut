#ifndef MyAssertH
#define MyAssertH

#include <cstdlib>
#include <iostream>

#ifdef NDEBUG
#define assert(x) {((void)0);}
#else
inline void assert(bool test)
{
        if (!test) {
                std::cerr << "ASSERTION FAILED!" << std::endl;
                std::system("PAUSE");
                std::exit(0);
        }
}
#endif

#endif

