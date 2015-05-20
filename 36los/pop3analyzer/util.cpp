#include "util.h"

void print_ip_addr(std::ostream f, u_int ip_addr)
{
    f << ((ip_addr >> 24) & 0xff) << '.' << ((ip_addr >> 16) & 0xff) << '.' << ((ip_addr >> 8) & 0xff) << '.' << (ip_addr & 0xff);
}
