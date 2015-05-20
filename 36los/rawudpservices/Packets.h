#ifndef _PACKETS_H_
#define _PACKETS_H_

#include <pcap.h>

#ifndef ETH_ALEN
#define ETH_ALEN 6                  // Ethernet address length
#endif

// some well-known port numbers
#define SERVICE_ECHO     7          // RFC862/STD20
#define SERVICE_DISCARD  9          // RFC863/STD21
#define SERVICE_CHARGEN 19          // RFC864/STD22
#define SERVICE_TIME    37          // RFC868/STD26
#define SERVICE_BOOTPS  67          // RFC951 (server)
#define SERVICE_BOOTPC  68          //        (client)

// header structures
#pragma pack(push, 1)
struct eth_header {
    u_char dst[ETH_ALEN];
    u_char src[ETH_ALEN];
    u_short frame_type;
};

struct ip_header {
    u_char  version_ihl;
    u_char  tos;
    u_short total_length;
    u_short identification;
    u_short frag_info;
    u_char  ttl;
    u_char  protocol;
    u_short checksum;
    u_char  source[4];
    u_char  dest[4];
};

struct udp_header {
    u_short sourceport;
    u_short destport;
    u_short length;
    u_short checksum;
};

struct udp_packet {
    eth_header eh;
    ip_header  iph;
    udp_header udph;
    struct {}  data;
};
#pragma pack(pop)

bool get_MAC_addr(const pcap_t *fp, u_char mac[ETH_ALEN]);
bool retrieve_IP_addr_via_BOOTP(pcap_t *fp, const u_char mac[ETH_ALEN], u_char ip[4]);

void process_received_packet(pcap_t *fp, const struct pcap_pkthdr *pkt_header, const u_char *pkt_data, const u_char my_mac[ETH_ALEN], const u_char my_ip[4]);

bool send_UDP_packet(pcap_t *fp,
                     const u_char src_mac[ETH_ALEN], const u_char src_ip[4], u_short src_port,
                     const u_char dst_mac[ETH_ALEN], const u_char dst_ip[4], u_short dst_port,
                     const u_char *data, unsigned datasize);

/* servers */

struct request_info {
    unsigned long     packet_length;        // total packet length [bytes]
    const u_char     *packet_data;          // all packet data (with all headers)
    const eth_header *eh;
    const ip_header  *iph;
    const udp_header *udph;
    const u_char     *request_data;         // UDP payload (the request packet contents)
    unsigned long     request_length;       // length of UDP payload [bytes]

    const u_char     *my_mac;               // my (server) MAC address (ETH_ALEN bytes)
    const u_char     *my_ip;                // my (server) IP address (4 bytes)
};

extern void process_echo_request(pcap_t *fp, const request_info &req);
extern void process_discard_request(pcap_t *fp, const request_info &req);
extern void process_chargen_request(pcap_t *fp, const request_info &req);
extern void process_time_request(pcap_t *fp, const request_info &req);

#endif // ifndef _PACKETS_H_
