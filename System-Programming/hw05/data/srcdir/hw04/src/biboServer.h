#ifndef BIBO_SERVER_H
#define BIBO_SERVER_H

#include <pthread.h>
#include "common.h"
#include "sync.h"

#define ERR_MSG_LEN 128

#define LOG_FILE_TEMPLATE "mybibobox.%d.log"

#define LOG_FILE_LEN (sizeof(LOG_FILE_TEMPLATE) + 20)

struct client_ref {
    int fd;
    unsigned int cid;
    struct client_info info;
};

struct thread_job {
    struct request_header header;
    char *data; 
};

struct thread_workspace {
    int log_fd;
    DIR *server_dir;
    char *server_dir_path;
};

pthread_t *create_thread_pool(int size, void* thread_func(void *), void *args);

void destroy_thread_pool(pthread_t threads[], int size);

void *thread_function(void *args);

int server_request(struct thread_workspace *workspace, struct thread_job *job);

int find_serving_client(struct list *li, pid_t pid);

int cmd_help(int client_fd, enum req_cmd cmd);

void help_usage(enum req_cmd cmd, char *str);

int cmd_list(int client_fd, DIR *server_dir);

int cmd_copy(int client_fd, const char *src_dir_path, const char *src_file, const char* dest_dir_path, int sv_write, char *err_msg);

long copy_file(int src_fd, int dest_fd, struct safe_file *sfile, int sv_write);

int cmd_readF(int client_fd, const char *server_dir_path, const char *src_file, int line_no, char *err_msg);

int seek_line(int fd, int line_no);

char *read_next_line(int fd);

int cmd_writeT(int client_fd,  const char *server_dir_path, const char *src_file, const char *str, int line_no, char *err_msg);

int open_dir_file(const char *dir_path, const char *file, int flags);

void sigint_handler();

int establish_connection(struct list *serving_list, struct client_ref *cli_ref);

int read_request(int server_fd, struct request_header *header, void**data);

int write_response(int client_fd, struct response_header *header, const void *data);

int log_request(int log_fd, pid_t client_pid, const char *cmd);

int log_response(int log_fd, pid_t client_pid, const char *cmd, int status);

int log_info(int log_fd, pid_t client_pid, const char *info);

int write_log(int log_fd, const char *log);

char *get_time();

struct thread_job *create_thread_job(const struct request_header *header, char *data);

struct client_ref *create_client_ref(int cid, int fd, const struct client_info *info);

int check_cmd_argc(enum req_cmd cmd, int argc);

int check_args(int argc, char *argv[], int *client_max, int *pool_size);

int str_to_int(const char *str, int *status);

void usage_error(char *pname);

#endif