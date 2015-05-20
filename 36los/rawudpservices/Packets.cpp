#include <cstdlib>
#include <iostream>

#include "debug.h"
#include "packets.h"

#include <pcap.h>
#include <pcap-int.h>
#include <Packet32.h>
#include <ntddndis.h>
#include <winsock.h>

using namespace std;

#define ETH_FRAME_IP 0x0800

#define TTL 30

/* --------------------- IP, UDP --------------------- */

u_short compute_checksum(const u_short *data, unsigned size)
{
    u_long sum = 0;
    const u_short *p = data;
    
    for (int i = 0; i < size; i++) sum += (u_long)ntohs(~*p++);
    
    return ((u_short)sum) + ((u_short)(sum >> 16));
}

u_short compute_iph_checksum(const ip_header &iph)
{
    return compute_checksum((const u_short *)&iph, sizeof(iph) / 2);
}

u_short compute_udp_checksum(const ip_header &iph, const udp_header &udph, const u_char *data, unsigned datasize)
{
    return 0;	// no UDP checksum provided
}

bool send_UDP_packet(pcap_t *fp,
                     const u_char src_mac[ETH_ALEN], const u_char src_ip[4], u_short src_port,
                     const u_char dst_mac[ETH_ALEN], const u_char dst_ip[4], u_short dst_port,
                     const u_char *data, unsigned datasize)
{
    udp_packet *packet;
    packet = (udp_packet *)calloc(sizeof(*packet) - sizeof(packet->data) + datasize, 1);
    
    memcpy(packet->eh.src, src_mac, ETH_ALEN);
    memcpy(packet->eh.dst, dst_mac, ETH_ALEN);
    packet->eh.frame_type = htons(ETH_FRAME_IP);
    
    packet->iph.version_ihl = 0x45;
    packet->iph.tos = 0;
    packet->iph.total_length = htons(sizeof(packet->iph) + sizeof(packet->udph) + datasize);
    packet->iph.identification = htonl(rand());
    packet->iph.frag_info = htons(0x0000);
    packet->iph.ttl = TTL;
    packet->iph.protocol = IPPROTO_UDP;
    memcpy(&packet->iph.source, src_ip, 4);
    memcpy(&packet->iph.dest, dst_ip, 4);
    packet->iph.checksum = htons(compute_iph_checksum(packet->iph));
    
    packet->udph.sourceport = htons(src_port);
    packet->udph.destport = htons(dst_port);
    packet->udph.length = htons(sizeof(packet->udph) + datasize);
    
    memcpy(&packet->data, data, datasize);
    
    packet->udph.checksum = htons(compute_udp_checksum(packet->iph, packet->udph, (const u_char *)(&packet->data), datasize));
    
    bool success = pcap_sendpacket(fp, (u_char *)packet, sizeof(*packet) - sizeof(packet->data) + datasize) >= 0;
    free(packet);
    return success;
}

/* ---------------------- MAC ---------------------- */

bool get_MAC_addr(const pcap_t *fp, u_char mac[ETH_ALEN])
{
    struct {
        _PACKET_OID_DATA base;
        u_char buff[512];
    } req;
    req.base.Oid = OID_802_3_CURRENT_ADDRESS;
    req.base.Length = 6;
    if (!PacketRequest(fp->adapter, 0, &req.base)) return false;
    memcpy(mac, req.base.Data, ETH_ALEN);
    return true;
}

/* --------------------- BOOTP --------------------- */

#pragma pack(push, 1)
struct bootp_packet {
    u_char	op;
    u_char	htype;
    u_char	hlen;
    u_char	hops;
    u_long	xid;
    u_short	secs;
    u_short     unused;
    u_char	ciaddr[4];
    u_char	yiaddr[4];
    u_char	siaddr[4];
    u_char	giaddr[4];
    u_char	chaddr[16];
    u_char	sname[64];
    u_char	file[128];
    u_char	vend[64];
};

struct bootp_packet_with_headers {
    eth_header eh;
    ip_header iph;
    udp_header udph;
    bootp_packet bootp;
};
#pragma pack(pop)

bool received_BOOTP_reply;
u_char *buffer_for_ip;

void receive_BOOTP_reply(u_char *user, const struct pcap_pkthdr *pkt_header, const u_char *pkt_data)
{
    if (received_BOOTP_reply) return;
    
    bootp_packet *request = (bootp_packet *)user;
    bootp_packet_with_headers *reply_packet = (bootp_packet_with_headers *)pkt_data;
    bootp_packet *reply = &(reply_packet->bootp);
    if (pkt_header->caplen < sizeof(bootp_packet_with_headers)) {
        debug_output(MSG_TRACE, ".");
        return;
    }
    if (reply->op == 2 && reply->htype == 1 && reply->hlen == 6 && reply->xid == request->xid) {
        debug_output(MSG_TRACE, "BOOTP reply received\n");
        memcpy(buffer_for_ip, reply->yiaddr, 4);
        received_BOOTP_reply = true;
        return;
    }
    debug_output(MSG_TRACE, ".");
}

bool retrieve_IP_addr_via_BOOTP(pcap_t *fp, const u_char mac[ETH_ALEN], u_char ip[4])
{
    bootp_packet request;
    u_char bcast[6] = {0xff, 0xff, 0xff, 0xff, 0xff, 0xff}; // MAC and IP broadcast
    u_char unknown_ip[4] = {0,0,0,0};
    
    memset(&request, 0, sizeof(request));
    
    request.op = 1;
    request.htype = 1;
    request.hlen = 6;
    request.hops = 0;
    request.xid = htonl(rand());
    request.secs = htons(0);
    memcpy(request.chaddr, mac, ETH_ALEN);
    
    int retransmissions = 0;
    
    received_BOOTP_reply = false;
    buffer_for_ip = ip;
    do {
        if (++retransmissions > 20) {
            cerr << "No BOOTP reply, giving up" << endl;
            return false;
        }
        
        debug_output(MSG_TRACE, "Sending BOOTP request...\n");
        if (!send_UDP_packet(fp, mac, unknown_ip, SERVICE_BOOTPC, bcast, bcast, SERVICE_BOOTPS, (u_char *)&request, sizeof(request))) {
            cerr << "Failed to send packet" << endl;
            return false;
        }
        
        if (pcap_dispatch(fp, 20, receive_BOOTP_reply, (u_char *)&request) < 0) {
            cerr << "Error in pcap_dispatch()" << endl;
            return false;
        }
        if (!received_BOOTP_reply) Sleep(250);
    } while (!received_BOOTP_reply);
    
    return true;
}

/* --------------------- server dispatching --------------------- */

void process_received_packet(pcap_t *fp, const struct pcap_pkthdr *pkt_header, const u_char *pkt_data, const u_char my_mac[ETH_ALEN], const u_char my_ip[4])
{
    const eth_header *eh = (const eth_header *)pkt_data;
    const ip_header *iph = (const ip_header *)(pkt_data + sizeof(eth_header));
    
    // check if UDP packet
    if (pkt_header->caplen < sizeof(eth_header) + sizeof(ip_header) + sizeof(udp_header))
        return;
    if (ntohs(eh->frame_type) != ETH_FRAME_IP)
        return;
    if ((iph->version_ihl & 0xf0) != 0x40)
        return;
    if (iph->protocol != IPPROTO_UDP)
        return;
    
    const udp_header *udph = (const udp_header *)((u_char *)iph + ((iph->version_ihl & 0x0f) << 2));
    const u_char *data = (u_char *)udph + sizeof(udp_header);
    
    debug_output(MSG_TRACE, ".");
    
    request_info req;
    req.packet_length = pkt_header->caplen;
    req.packet_data = pkt_data;
    req.eh = eh;
    req.iph = iph;
    req.udph = udph;
    req.request_data = data;
    req.request_length = ntohs(iph->total_length) - ((u_char *)data - (u_char *)iph);
    req.my_mac = my_mac;
    req.my_ip = my_ip;
    
    switch(ntohs(udph->destport)) {
    case SERVICE_ECHO:
        process_echo_request(fp, req);
        break;
        
    case SERVICE_DISCARD:
        process_discard_request(fp, req);
        break;
        
    case SERVICE_CHARGEN:
        process_chargen_request(fp, req);
        break;
        
    case SERVICE_TIME:
        process_time_request(fp, req);
        break;
    }
}
