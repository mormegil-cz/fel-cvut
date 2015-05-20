#ifndef __ULOHA1_UTIL_H_
#define __ULOHA1_UTIL_H_

#include <pcap.h>
#include <iostream>

#define REPAIR_SHORT(x) ((x) = ntohs(x))
#define REPAIR_LONG(x)  ((x) = ntohl(x))

void print_ip_addr(std::ostream f, u_int ip_addr);

#endif // ifndef __ULOHA1_UTIL_H_
