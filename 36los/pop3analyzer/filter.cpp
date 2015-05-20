#include "filter.h"

PacketFilter packet_filter;

void PacketFilter::setup_filter(u_int client, u_int server, u_short client_port, u_short server_port) {
    this->client = client;
    this->server = server;
    this->client_port = client_port;
    this->server_port = server_port;
    filter_off = false;
}

void PacketFilter::disable_filter() {
    filter_off = true;
}

bool PacketFilter::pass(IPHeader iph, TCPHeader tcph) const {
    return
	filter_off ||
	((iph.source() == client && iph.dest() == server && tcph.source_port() == client_port && tcph.dest_port() == server_port) ||
	 (iph.source() == server && iph.dest() == client && tcph.source_port() == server_port && tcph.dest_port() == client_port));
}
