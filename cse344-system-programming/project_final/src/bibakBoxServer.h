#ifndef BIBAK_BOX_SERVER_H
#define BIBAK_BOX_SERVER_H

#include "util.h"

#define LOG_FNAME "sv.log"
#define INIT_QUE_SIZE 10
#define SERVER_BACKLOG 1000

struct client {
    int sockfd;
    int id;
};

void handle_client(int cfd, const char *dirpath);

void sync_sv(int sockfd, const char *rootpath);

void sighandler();

void *worker_thread_func(void *args);

pthread_t *create_thread_pool(int size, void *thread_func(void *), void *args);

void destroy_thread_pool(pthread_t threads[], int pool_size);

int handle_args(int argc, char *argv[], int *pool_size, int *portnumber);

void usage_err(const char *pname);

#endif