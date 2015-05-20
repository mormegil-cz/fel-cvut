#include "process_POP3.h"
#include <stdio.h>


void process_incoming_POP3_data(const u_char *data, size_t size){
	fwrite( data,sizeof(u_char),size,tmp );

};
void process_outgoing_POP3_data(const u_char *data, size_t size){
	fwrite( data,sizeof(u_char),size,tmp );
};


