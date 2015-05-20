#include <iostream>

#include "packets.h"
#include "debug.h"

using namespace std;

void process_echo_request(pcap_t *fp, const request_info &req)
{
    debug_output(MSG_TRACE, "ECHO\n");

    if (!send_UDP_packet(fp, req.my_mac, req.my_ip, SERVICE_ECHO,
                             req.eh->src, req.iph->source, ntohs(req.udph->sourceport),
                             req.request_data, req.request_length)) {
        cerr << "Unable to send packet" << endl;
        exit(1);
    }
}

void process_discard_request(pcap_t *fp, const request_info &req)
{
    debug_output(MSG_TRACE, "DISCARD\n");
    // this is rather SIMPLE...
    #define PROCESS_DISCARD(Packet) { TrashCan <- Packet; }  // :-)
}

void process_chargen_request(pcap_t *fp, const request_info &req)
{
    static int curr_start = 0;
    char output_buff[74];

    debug_output(MSG_TRACE, "CHARGEN\n");

    for (char c = curr_start, *p = output_buff; p < output_buff + sizeof(output_buff);)
        *p++ = c++ % 95 + ' ';

    output_buff[sizeof(output_buff) - 2] = '\x0d';
    output_buff[sizeof(output_buff) - 1] = '\x0a';

    curr_start = (curr_start + 1) % 95;

    if (!send_UDP_packet(fp, req.my_mac, req.my_ip, SERVICE_CHARGEN,
                             req.eh->src, req.iph->source, ntohs(req.udph->sourceport),
                             (u_char *)output_buff, sizeof(output_buff))) {
        cerr << "Unable to send packet" << endl;
        exit(1);
    }
}

void process_time_request(pcap_t *fp, const request_info &req)
{
    u_long currtime = time(NULL) + 2208988800L;

    debug_output(MSG_TRACE, "TIME\n");

    if (!send_UDP_packet(fp, req.my_mac, req.my_ip, SERVICE_TIME,
                             req.eh->src, req.iph->source, ntohs(req.udph->sourceport),
                             (u_char *)&currtime, sizeof(currtime))) {
        cerr << "Unable to send packet" << endl;
        exit(1);
    }
}
