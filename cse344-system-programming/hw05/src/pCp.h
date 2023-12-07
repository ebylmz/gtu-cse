#ifndef PCP_H
#define PCP_H

#define PATH_LEN 1024
#define BUFF_LEN 4096

struct cp_job {
    int src_fd;
    int dst_fd;
    char *src_path;
    char *dst_path;
};

/**
 * The seven standard Unix file types are regular, directory, symbolic link,
 * FIFO special, block special, character special, and socket as defined by POSIX
*/
struct cp_stats {
    long unsigned int num_cons;
    long unsigned int buff_size;
    long unsigned int num_dir;
    long unsigned int num_regfile;
    long unsigned int num_slink;
    long unsigned int num_fifo; 
    long unsigned int num_unsupported;
    long unsigned int totalbytes;
    double elapsed_time;
};

void *producer_thread_func(void *args);

void *consumer_thread_func(void *);

void sighandler();

pthread_t *create_thread_pool(int size, void *thread_func(void *), void *args);

void destroy_thread_pool(pthread_t threads[], int pool_size);

int copy_dir(const char *src_dirpath, const char *dst_dirpath);

long copy_file(int src_fd, int dst_fd);

const char *get_dirname(const char *path);

struct cp_job *cp_job_create(int src_fd, int dst_fd, const char *src_path, const char *dst_path);

void print_cp_status(const char *src_path, const char *dst_path, int status);

void cp_job_destroy(struct cp_job *job);

void cp_stats_print(const struct cp_stats *stats);

void usage_err(const char *pname, int errcode);

int handle_args(int argc, char *argv[], unsigned long int *buff_size, unsigned long int *num_cons);

#endif