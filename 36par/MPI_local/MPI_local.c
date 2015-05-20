/*  MPI_Local -- MPI emulator for testing MPI programs on a single machine
 *  Copyright (C) 2002  Petr Kadlec <mormegil@centrum.cz>
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

 /***************************************************
  * Check readme.txt file before using the library! *
  ***************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <windows.h>

#include "mpi.h"

#define SHARED_MEM_FILENAME   "MPI_shared_mem"
#define BARRIER_EVENT_NAME    "MPI_barrier_event"
#define CRITICAL_SECTION_NAME "MPI_critical_section"

#define MAX_MSG_SIZE 1024
#define MAX_MSGS_IN_BUFFER 128

typedef struct msg {
           int    source;
           int    tag;
           char   data[MAX_MSG_SIZE];
           char   processed;
        } tmsg, *pmsg;

typedef struct shared_memory_t {
           /*pmsg   towrite;
           size_t remwrite;*/
           tmsg   mem_start;
        } *pheader;

#define MAX_COMM_MEM_SIZE (MAX_MSGS_IN_BUFFER * sizeof(tmsg))
#define SHARED_MEMORY_SIZE (MAX_COMM_MEM_SIZE + sizeof(struct shared_memory_t))

static int processes_count = 1;
static int my_rank         = 0;

static HANDLE  *shared_mem_file = NULL;
static pheader *shared_memory   = NULL;

static HANDLE *barrier_semaphores = NULL;
static HANDLE write_msg_section = 0;      /* FIXME: Each process' message queue should have a separate mutex. */

int (*MPI_ErrorHandler)(int) = MPI_ERROR_HANDLER_FAULT;

int MPI_ERROR_HANDLER_FAULT(int errcode) {
  fprintf(stderr, "MPI_ERROR_HANDLER_FAULT: %d\n", errcode);
  getchar();
  exit(50);

  return errcode;
}

int MPI_ERROR_HANDLER_RETURN(int errcode) {
  return errcode;
}

int create_shared_memory(HANDLE *handle, pheader *mem, size_t size, char *filename)
{
  if ((*handle = CreateFileMapping((HANDLE)0xFFFFFFFFL, NULL, PAGE_READWRITE, 0, size + 15, filename)) == 0) {
    fprintf(stderr, "CreateFileMapping failed: %x\n", GetLastError());
    return MPI_ErrorHandler(E_MPI_WIN32FAIL);
  }

  if ((*mem = (pheader)MapViewOfFile(*handle, FILE_MAP_ALL_ACCESS, 0, 0, 0)) == NULL) {
    fprintf(stderr, "MapViewOfFile failed: %x\n", GetLastError());
    CloseHandle(*handle);
    return MPI_ErrorHandler(E_MPI_WIN32FAIL);
  }

  return E_MPI_OK;
}

/* returns pointer to the "processed" field */
char *write_msg(pmsg msg, pheader destmem)
{
  char *result;
  int i = MAX_MSGS_IN_BUFFER;
  pmsg towrite = &destmem->mem_start;

  if (WaitForSingleObject(write_msg_section, INFINITE) != WAIT_OBJECT_0) {
    fprintf(stderr, "WaitForSingleObject failed: %d\a\n", GetLastError());
    exit(50);
  }

  /* Error: It writes messages without preserving order, but mostly LIFO !!!! */

  msg->processed = 0;
  msg->source = my_rank;

  while (i > 0 && !(towrite->processed)) {
    towrite++;
    i--;
  }

  if (i == 0) {
    fprintf(stderr, "BUFFER OVERFLOW!\a\n");
    getchar();
    ReleaseMutex(write_msg_section);
    exit(50);
  }

  memcpy(towrite, msg, sizeof(*msg));

  ReleaseMutex(write_msg_section);
  
  return &(towrite->processed);
}

void read_msg(pmsg dest, pheader srcmem, pmsg src)
{
  memcpy(dest, src, sizeof(*src));
  src->processed = 1;
}

pmsg find_msg(pheader mem, int source, int tag)
{
  int i;
  pmsg msg;
  for (i = 0, msg = &mem->mem_start; i < MAX_MSGS_IN_BUFFER; i++, msg++)
    if (!msg->processed &&
        (source == MPI_ANY_SOURCE || source == msg->source) &&
        (tag == MPI_ANY_TAG || tag == msg->tag)) return msg;

  return NULL;
}

char *strrplac(char *str, char *whatchars, char with)
{
  char *p = str;
  while (*p) {
    if (strchr(whatchars, *p) != NULL) *p = with;
    p++;
  }
  return str;
}

int MPI_Init(int *argc, char ***argv)
{
  char *progname;
  char filename[MAX_PATH];
  int i;

  if (*argc < 3) {
    return MPI_ErrorHandler(E_MPI_BADARGS);
  }

  progname = (*argv)[0];

  processes_count = atoi((*argv)[1]);
  (*argv)++;
  (*argc)--;
  my_rank = atoi((*argv)[1]);
  (*argv)++;
  (*argc)--;

  (*argv)[0] = progname;

  shared_mem_file = (HANDLE *)malloc(sizeof(HANDLE) * processes_count);
  shared_memory = (pheader *)malloc(sizeof(void *) * processes_count);
  barrier_semaphores = (HANDLE *)malloc(sizeof(HANDLE) * processes_count);

  for (i = 0; i < processes_count; i++) {
    int res;
    sprintf(filename, "%s_%s_%d", SHARED_MEM_FILENAME, progname, i);
    strrplac(filename, "\\/:", '_');

    res = create_shared_memory(&shared_mem_file[i], &shared_memory[i], SHARED_MEMORY_SIZE, filename);
    if (res) return res;

    sprintf(filename, "%s_%s_%d", BARRIER_EVENT_NAME, progname, i);
    strrplac(filename, "\\/:", '_');

    barrier_semaphores[i] = CreateEvent(NULL, TRUE, FALSE, filename);
  }

  sprintf(filename, "%s_%s_wmsg", CRITICAL_SECTION_NAME, progname, i);
  strrplac(filename, "\\/:", '_');
  write_msg_section = CreateMutex(NULL, FALSE, filename);

  /*
  sprintf(filename, "%s_%s_B", SHARED_MEM_FILENAME, progname);
  strrplac(filename, "\\/:", '_');
  create_shared_memory(&shared_mem_file[processes_count], &shared_memory[processes_count], processes_count, filename);
  */
  /*barrier_semaphore = CreateSemaphore(NULL, -processes_count, 0, filename);
  if (barrier_semaphore) {
    fprintf(stderr, "CreateSemaphore failed: %x\n", GetLastError());
    MPI_ERROR_HANDLER_FAULT(E_MPI_WIN32FAIL);
  }*/

  memset(shared_memory[my_rank], 1, SHARED_MEMORY_SIZE);

  /*shared_memory[my_rank]->towrite = &(shared_memory[my_rank]->mem_start);
  shared_memory[my_rank]->remwrite = MAX_MSGS_IN_BUFFER;*/

  //((char *)(shared_memory[processes_count]))[my_rank] = 0;

  return E_MPI_OK;
}

int MPI_Finalize(void) {
  int i;
  for (i = 0; i <= processes_count; i++) {
    if (shared_memory[i] != NULL) {
      UnmapViewOfFile(shared_memory[i]);
      shared_memory[i] = NULL;
    }
    if (shared_mem_file[i] != 0) {
      CloseHandle(shared_mem_file[i]);
      shared_mem_file[i] = 0;
    }
    CloseHandle(barrier_semaphores[i]);
  }

  if (write_msg_section != 0) {
    CloseHandle(write_msg_section);
    write_msg_section = 0;
  }

  if (barrier_semaphores != NULL) {
    free(barrier_semaphores);
    barrier_semaphores = NULL;
  }
  if (shared_memory != NULL) {
    free(shared_memory);
    shared_memory = NULL;
  }
  if (shared_mem_file != NULL) {
    free(shared_mem_file);
    shared_mem_file = NULL;
  }

  return E_MPI_OK;
}

int MPI_Comm_rank(int comm, int *comm_rank) {
  *comm_rank = my_rank;
  return E_MPI_OK;
}

int MPI_Comm_size(int comm, int *comm_size) {
  *comm_size = processes_count;
  return E_MPI_OK;
}

int MPI_Send(void *buff, size_t count, size_t type, int dest, int tag, int comm) {
  MPI_Request req;
  int result;

  result = MPI_Isend(buff, count, type, dest, tag, comm, &req);
  if (result) return result;

  while (!*req.processed) Sleep(0);

  return E_MPI_OK;
}

int MPI_Isend(void *buff, size_t count, size_t type, int dest, int tag, int comm, MPI_Request *request) {
  size_t sz = count * type;
  tmsg msg;
  char *processed;

  if (sz > MAX_MSG_SIZE)
    return MPI_ErrorHandler(E_MPI_TOO_BIG);

  msg.tag = tag;
  memcpy(msg.data, buff, sz);
  request->processed = write_msg(&msg, shared_memory[dest]);

  return E_MPI_OK;
}

int MPI_Recv(void *buff, size_t count, size_t type, int from, int tag, int comm, MPI_Status *status) {
  pmsg msg;
  tmsg received;
  size_t sz = count * type;

  if (sz > MAX_MSG_SIZE)
    return MPI_ErrorHandler(E_MPI_TOO_BIG);

  while ((msg = find_msg(shared_memory[my_rank], from, tag)) == NULL) Sleep(0);

  read_msg(&received, shared_memory[my_rank], msg);
  status->MPI_SOURCE = received.source;
  status->MPI_TAG = received.tag;
  memcpy(buff, &received.data, sz);

  return E_MPI_OK;
}

int MPI_Probe(int source, int tag, int comm, MPI_Status *status) {
  pmsg msg;

  while ((msg = find_msg(shared_memory[my_rank], source, tag)) == NULL) Sleep(0);

  status->MPI_SOURCE = msg->source;
  status->MPI_TAG = msg->tag;

  return E_MPI_OK;
}

int MPI_Iprobe(int source, int tag, int comm, int *waiting, MPI_Status *status) {
  pmsg msg;

  msg = find_msg(shared_memory[my_rank], source, tag);

  if (msg == NULL) {
    *waiting = 0;
  } else {
    *waiting = 1;
    status->MPI_SOURCE = msg->source;
    status->MPI_TAG = msg->tag;
  }

  return E_MPI_OK;
}

int MPI_Bcast(void *buff, size_t count, size_t type, int broadcaster, int comm) {
  int res;

  if (processes_count == 0) return E_MPI_OK;

  res = MPI_Barrier(comm);
  if (res) return res;

  if (my_rank == broadcaster) {
    int i;
    for (i = 0; i < processes_count; i++)
      if (i != my_rank) MPI_Send(buff, count, type, i, MPI_TAG_BROADCAST, comm);
  } else {
    MPI_Status status;
    MPI_Recv(buff, count, type, broadcaster, MPI_TAG_BROADCAST, comm, &status);
  }

  return E_MPI_OK;
}

int MPI_Barrier(int comm) {
  /*char *barrier_mem = (char *)shared_memory[processes_count];

  barrier_mem[my_rank] = 1;

  while(1) {
    char *p;
    int i;
    int remaining = processes_count;
    for (i = 0, p = barrier_mem; i < processes_count; i++, p++)
      if (*p) remaining--;

    if (!remaining) break;
  }

  barrier_mem[my_rank] = 0;*/

  SetEvent(barrier_semaphores[my_rank]);
  if (WaitForMultipleObjects(processes_count, barrier_semaphores, TRUE, INFINITE) != WAIT_OBJECT_0)
    return MPI_ErrorHandler(E_MPI_WIN32FAIL);

  /* THIS IS JUST A HACK, but because of lack of SignalObjectAndWait, etc. functions,
        I know of no other way */
  Sleep(100);

  ResetEvent(barrier_semaphores[my_rank]);

  return E_MPI_OK;
}

double MPI_Wtime(void) {
  LARGE_INTEGER cntr, freq;
  QueryPerformanceCounter(&cntr);
  QueryPerformanceFrequency(&freq);
  return (double)(cntr.LowPart) / freq.LowPart;
}

