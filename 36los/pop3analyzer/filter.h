#ifndef __FILTER_H_
#define __FILTER_H_

#include <pcap.h>

#include "process_IP.h"
#include "process_TCP.h"

class PacketFilter {
    u_int client, server;
    u_short client_port, server_port;
    bool filter_off;
public:
    PacketFilter() : filter_off(true) { };

    PacketFilter(u_int client, u_int server, u_short client_port, u_short server_port) : filter_off(true) {
	setup_filter(client, server, client_port, server_port);
    }

    bool filter_enabled() const { return !filter_off; };
    u_int get_client() const { return client; };
    u_int get_server() const { return server; };

    void setup_filter(u_int client, u_int server, u_short client_port, u_short server_port);
    void disable_filter();

    bool pass(IPHeader iph, TCPHeader tcph) const;
};

extern PacketFilter packet_filter;

#endif // ifndef __FILTER_H_
