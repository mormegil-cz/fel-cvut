#ifndef __PROCESS_IP_H_
#define __PROCESS_IP_H_

#include <pcap.h>

void process_IP_fragment(const u_char *data, size_t length);

#define IP_HEADER_SIZE 20

// IP packet header
class IPHeader {
private:
    #pragma pack(push, 1)
    struct {
	u_char  version_ihl;
	u_char  tos;
	u_short total_length;
	u_short identification;
	u_short frag_info;
	u_char  ttl;
	u_char  protocol;
	u_short checksum;
	u_int   source;
	u_int   dest;
	/* u_int options:? */
    } data;
    #pragma pack(pop)

    inline void repair_endians();
public:
    IPHeader() { } // the default constructor

    IPHeader(const u_char *from_data) {	// the basic constructor
	memcpy(&data, from_data, sizeof(data));
	repair_endians();
    }

    // compares only the inter-fragment persistent fields
    bool operator == (const IPHeader &with) const {
	return
	    data.tos == with.data.tos &&
	    data.total_length == with.data.total_length &&
	    data.identification == with.data.identification &&
	    data.protocol == with.data.protocol &&
	    data.source == with.data.source &&
	    data.dest == with.data.dest;
    }

    bool operator != (const IPHeader &with) const {
	return !(*this == with);
    }

    void dump() const;

    u_char  version() const { return data.version_ihl >> 4; };
    u_char  ihl() const { return data.version_ihl & 0x0f; };
    u_char  tos() const { return data.tos; };
    u_short total_length() const { return data.total_length; };
    u_short identification() const { return data.identification; };
    u_char  flags() const { return data.frag_info >> 13; };
    u_short frag_offset() const { return data.frag_info & 0x1fff; };
    u_char  ttl() const { return data.ttl; };
    u_char  protocol() const { return data.protocol; };
    u_short checksum() const { return data.checksum; };
    u_int   source() const { return data.source; };
    u_int   dest() const { return data.dest; };
    /*u_short compute_checksum() const {
	const u_short *p = (const u_short *)(&data);
	int hlen = ihl() << 1;
	u_short chksum = 0;
	for (int i = 0; i < hlen; i++)
	    chksum += *p++;
	return ~(chksum - checksum());
    }*/
};

/*
 0                   1                   2                   3   
    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |Version|  IHL  |Type of Service|          Total Length         |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |         Identification        |Flags|      Fragment Offset    |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |  Time to Live |    Protocol   |         Header Checksum       |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                       Source Address                          |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                    Destination Address                        |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                    Options                    |    Padding    |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
*/

#endif // ifndef  __PROCESS_IP_H_
