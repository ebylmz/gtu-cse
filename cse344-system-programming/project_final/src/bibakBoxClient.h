#ifndef BIBAK_BOX_CLIENT_H
#define BIBAK_BOX_CLIENT_H

#include "util.h"

void sighandler();

void sync_cli(int sockfd, const char *rootpath);

int handle_args(int argc, char *argv[], int *portnumber);

void usage_err(const char *pname);

#endif