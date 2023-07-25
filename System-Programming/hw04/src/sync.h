#ifndef SYNC_H
#define SYNC_H

#include <stdio.h>
#include <errno.h>
#include <dirent.h>
#include <pthread.h>
#include "common.h"

#define FNAME_LEN 128

struct safe_file {
    char *fname;
    int AR;  /* # of active readers */ 
    int AW;  /* # of active writers */
    int WR;  /* # of waiting readers */
    int WW;  /* # of waiting writers */
    pthread_cond_t ok_to_read;
    pthread_cond_t ok_to_write;
    pthread_mutex_t mutex;
};

int reader_enter_region(struct safe_file *sfile);

int reader_exit_region(struct safe_file *sfile);

int writer_enter_region(struct safe_file *sfile); 

int writer_exit_region(struct safe_file *sfile);

struct safe_file *sfile_create(const char *fname);

struct safe_file *sfile_add(struct list *li, const char *fname);

struct safe_file *sfile_get(const struct list *li, const char *fname);

void sfile_list_destroy(struct list *li);

#endif