#include "debug.h"
#include <iostream>

using namespace std;

void debug_output(int severity, const char *msg)
{
	cerr << msg;
}

void print_ip_addr(std::ostream f, const u_char ip_addr[4])
{
	f << int(ip_addr[0]) << '.' << int(ip_addr[1]) << '.' << int(ip_addr[2]) << '.' << int(ip_addr[3]);
}
