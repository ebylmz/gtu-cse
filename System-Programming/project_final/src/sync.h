#ifndef SYNC_H
#define SYNC_H

#define MAX_PATH_LENGTH 1024
#define BUFF_SIZE 1024
#define ETX 3 
#define LOG_MAX_LEN 128

#include <sys/stat.h>
#include <dirent.h>
#include <pthread.h>
#include "util.h"

enum sync_event {ADD, REM, MOD, DONE, CLOSE}; 

enum response {GET, OK};

/* Structure for representing a file entry */
struct fnode {
    char *name;
    struct stat filestat; 
    struct fnode *child;
    struct fnode *sibling;
};

struct safefile {
    char *name;
    pthread_mutex_t mutex;
    int fd;
};

struct header {
    enum sync_event event;
    size_t pathlen; 
    struct stat st;
};

struct safefile *safefile_list_find(struct circular_buffer *li, const char *name);

struct safefile *safefile_create(const char *name, int fd);

void safefile_free(struct safefile *sf);

int send_header_event(int sockfd, enum sync_event event);

int recv_header(int sockfd, struct header *head, char *path);

int send_header(int sockfd, const struct header *head, const char *path);

void set_header(struct header *head, struct stat *fstat, enum sync_event event, size_t pathlen);

int sync_sendfilep(int sockfd, const char *srcpath);

int sync_sendfile(int sockfd, int filefd);

int sync_recvfile(int sockfd, int filefd);

ssize_t sendfile(int sockfd, int filefd);

ssize_t recvfile(int sockfd, int filefd);

int sendbuff(int sockfd, char *buff, int size, int etx);

int recvbuff(int sockfd, char *buff, int size, int *etx);

ssize_t transferfilep(const char *srcpath, int dstfd);

ssize_t transferfile(int srcfd, int dstfd);

struct fnode *fnode_create(const char *name, const struct stat *fileStat);

void fnode_free(struct fnode *node);

int fnode_addchild(struct fnode *parent, struct fnode *child);

int handle_event(int sockfd, struct fnode *root, struct stat *st, const char *path, enum sync_event event, struct safefile *logfile);

int removedir(const char *dirpath, struct safefile *logfile);

int update_dirtree(struct fnode *root, const char *entrypath, enum sync_event event);

struct fnode *find_entry(struct fnode *root, const char *entrypath, int *firstchild);

void printmargin(int n);

void print_dirtree(const struct fnode *root);

void print_dirtree_rec(const struct fnode *parent, int s);

struct fnode *build_dirtree(struct fnode *parent, const char *dirpath);

void free_dirtree(struct fnode *root);

int compare_dirtree(struct fnode *root, const char *dirpath, int sock_fd, int skiplen, struct safefile *logfile);

int sync_writer_rec(int sockfd, struct fnode *root, const char *rootpath);

int sync_writer(int sockfd, struct fnode *root, const char *path);

int sync_reader(int sockfd, struct fnode *root, struct safefile *logfile);

int read_entry(int sockfd, int *pathlen, struct stat *st, char *path);

int write_entry(int sockfd, int pathlen, struct stat *st, const char *path);

int writelog(struct safefile *logfile, const char *path, enum sync_event event);

const char *get_lastfname(const char *path);

void conv_event(char *str, enum sync_event event);

#endif