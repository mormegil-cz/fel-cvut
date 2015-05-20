#include <pcap.h>
#include <assert.h>

#include <stdexcept>
#include <map>

#include "util.h"
#include "stats.h"
#include "process_IP.h"
#include "process_TCP.h"

using namespace std;

// IP datagram identification [source|destination|protocol|identification]
class IPDGramID {
    u_int   source;
    u_int   destination;
    u_short identification;
    u_char  protocol;

public:
    IPDGramID(IPHeader *h) {
	source = h->source();
	destination = h->dest();
	protocol = h->protocol();
	identification = h->identification();
    }

    bool operator < (const IPDGramID &to) const {
        return
            (source < to.source) ||
              ((source == to.source) &&
                ((destination < to.destination) ||
                  ((destination == to.destination) &&
                    ((protocol < to.protocol) ||
                      ((protocol == to.protocol) && (identification == to.identification))
              ) ) ) );
    }

    bool operator == (const IPDGramID &to) const {
	return
	    source == to.source && 
	    destination == to.destination &&
	    protocol == to.protocol &&
	    identification == to.identification;
    }
};

// IP datagram
class IPDGram {
    IPHeader header;	// datagram header
    u_char *datagram;	// datagram data
    u_int   dgram_size;	// data size
    u_int   remaining;  // remaining to receive
public:
    IPDGram() {
	datagram = 0;
	dgram_size = 0;
	remaining = ~0;
    }

    IPDGram(const IPDGram &src) : datagram(0), dgram_size(0) {
	// copy-constructor
	assert(src.datagram);
	dgram_size = src.dgram_size;
	remaining  = src.remaining;
	datagram = new u_char[dgram_size];
	memcpy(datagram, src.datagram, dgram_size);
    }

    ~IPDGram() {
	delete[] datagram;
	datagram = 0;
	dgram_size = 0;
	remaining = ~0;
    }

    IPDGram& operator = (const IPDGram &src) {
	// assignment
	if (this == &src) return *this;
	assert(src.datagram);

	delete[] datagram;
	dgram_size = src.dgram_size;
	remaining  = src.remaining;
	datagram = new u_char[dgram_size];
	memcpy(datagram, src.datagram, dgram_size);
	return *this;
    }

    /*
     * h        ... fragment header
     * fragment ... pointer to the data portion of the fragment
     * size     ... size of the data portion of the fragment
     */
    void add_fragment(const IPHeader &h, const u_char *fragment, size_t size) {
	if (!datagram) {
	    // first fragment
	    dgram_size = remaining = h.total_length() - (h.ihl() << 2);
	    datagram = new u_char[dgram_size];
	    header = h;
	}

	if (h != header) throw logic_error("Packet header mismatch");

	if (size > remaining) {
	    //cout << "WARNING: Invalid fragment size, packet truncated" << endl;
	    size = remaining;
	}
	if ((h.frag_offset() << 3) + size > dgram_size) {
	    //cout << "WARNING: Fragment buffer overflow, packet truncated" << endl;
	    size = dgram_size - (h.frag_offset() << 3);
	}
	memcpy(datagram + (h.frag_offset() << 3), fragment, size);
	remaining -= size;
    }

    const u_char *get_datagram() const { return datagram; }
    u_int get_dgram_size() const { return dgram_size; }
    u_int get_remaining() const { return remaining; }

    bool  is_complete() const { return !remaining; }

    void process() {
	total_IP_packets++;

	switch(header.protocol()) {
	case IPPROTO_TCP:
	    process_TCP_packet(header, datagram, dgram_size);
	    break;

#ifndef NDEBUG
	default:
	    //throw invalid_argument("Unknown IP protocol");
	    cerr << "Unknown IP protocol, packet ignored" << endl;
#endif
	}
    }
};

// **************** IPHeader ****************
void IPHeader::repair_endians()
{
    REPAIR_SHORT(data.total_length);
    REPAIR_SHORT(data.identification);
    data.frag_info = (data.frag_info & 0xe000) | ntohs(data.frag_info & 0x1fff);
    REPAIR_SHORT(data.checksum);
    REPAIR_LONG(data.source);
    REPAIR_LONG(data.dest);
}

void IPHeader::dump() const {
    cout << "IP Header:" << endl;
    cout << "\tLength: " << total_length() - (ihl() << 2) << "B data + " << (ihl() << 2) << "B IP header" << endl;
    //cout << "\tType Of Service: " << hex << u_int(tos()) << dec << endl;
    //cout << "\tIdentification: " << identification() << endl;
    //cout << "\tFragmentation flags: " << hex << u_int(flags()) << dec << endl;
    //cout << "\tProtocol: " << int(protocol()) << endl;
    /*cout << "\tChecksum: " << hex << checksum() << ", should be " << compute_checksum() << dec << endl;*/
    cout << "\tSource address: ";
    print_ip_addr(cout, source());
    cout << endl;
    cout << "\tDestination address: ";
    print_ip_addr(cout, dest());
    cout << endl;
}

// ******** main processing function ********
void process_IP_fragment(const u_char *data, size_t length)
{
    static map<IPDGramID, IPDGram> datagrams; // currently processed datagrams

    IPHeader iph(data);
    const u_char *p = data;
    size_t iphlen;

    assert(length >= IP_HEADER_SIZE);  // every fragment must contain at least the IP header

    //cout << "process_IP_fragment(data, " << length << ")" << endl;

    iphlen = iph.ihl() << 2;
    assert(iph.version() == 4 && iphlen >= 20 && length >= iphlen); // check some basic validity constraints
    p += iphlen;
    length -= iphlen;

    total_IP_fragments++;
    //iph.dump();
    //cout << "\tLength without IP header: " << length << endl;

    IPDGramID id(&iph);

    if (datagrams.count(id)) {
	// we already have some fragments of this datagram
	IPDGram &dgram = datagrams[id];
	dgram.add_fragment(iph, p, length);
	if (dgram.is_complete()) {
	    dgram.process();
	    datagrams.erase(id);
	}
    } else {
	// new datagram
	IPDGram dgram;
	dgram.add_fragment(iph, p, length);
	if (dgram.is_complete()) {
	    dgram.process();
	} else {
	    datagrams[id] = dgram;
	}
    }
}
