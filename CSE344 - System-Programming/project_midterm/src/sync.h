#ifndef SYNC_H
#define SYNC_H

#include <stdio.h>
#include <errno.h>
#include <dirent.h>
#include <semaphore.h>

#define FNAME_LEN 128
#define NUM_OF_DIR_FILE 128

struct safe_file {
    char fname[FNAME_LEN];
    int reader_count; 
    int writer_count;
    sem_t read_try;
    sem_t rmutex;
    sem_t wmutex;
    sem_t rsc;
};

struct safe_dir {
    struct safe_file files[NUM_OF_DIR_FILE];
    int size;
    int capa;
};

int reader_enter_region(struct safe_file *sfile);

int reader_exit_region(struct safe_file *sfile);

int writer_enter_region(struct safe_file *sfile); 

int writer_exit_region(struct safe_file *sfile);

int init_sdir(DIR *server_dir, struct safe_dir *sdir);

int close_sdir(struct safe_dir *sdir);

struct safe_file *add_sfile(struct safe_dir *sdir, const char *fname);

int init_sfile(struct safe_file *sfile, const char *fname);

struct safe_file *get_sfile(struct safe_dir *sdir, const char *file);

#endif