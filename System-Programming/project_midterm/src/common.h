#ifndef COMMON_H
#define COMMON_H

#include <unistd.h>

#define REG_SERVER_FIFO_TEMPLATE "/tmp/mybibobox_reg_sv.%d"

#define REQ_SERVER_FIFO_TEMPLATE "/tmp/mybibobox_req_sv.%d"

#define CLIENT_FIFO_TEMPLATE "/tmp/mybibobox_cl.%d"

#define REG_SERVER_FIFO_NAME_LEN (sizeof(REG_SERVER_FIFO_TEMPLATE) + 20)

#define REQ_SERVER_FIFO_NAME_LEN (sizeof(REQ_SERVER_FIFO_TEMPLATE) + 20)

#define CLIENT_FIFO_NAME_LEN (sizeof(CLIENT_FIFO_TEMPLATE) + 20)

#define CMD_LEN 64

#define CMD_ARG_MAX 32

#define BUFF_SIZE 1024

#define CWD_SIZE 256

#define LOG_LEN 512

enum req_cmd {HELP, LIST, READ_F, WRITE_T, UPLOAD, DOWNLOAD, QUIT, KILL_SERVER};

enum resp_status {RESP_OK, RESP_ERROR, RESP_PART, RESP_CONT, RESP_APPROVE, RESP_CONNECT, RESP_DISCONNECT};

struct request_header {
    pid_t pid;              //! not necessary needed
    size_t data_size; 
};

struct response_header {
    enum resp_status stat; 
    size_t data_size; 
};

struct client_info {
    pid_t pid;
    int wait;
    char cwd[CWD_SIZE];
};

struct queue {
    int front;
    int rear;
    int capacity;
    int size;
    struct client_info **elements;
};

struct pid_list {
    int size;
    int capacity;
    pid_t *pids;
};

int enqueue(struct queue *que, struct client_info *item);

struct client_info *dequeue(struct queue *que);

int resize_queue(struct queue *que);

int init_queue(struct queue *que); 

void destroy_queue(struct queue *que); 

int init_pid_list(struct pid_list *plist, int capacity);

int add_pid_list(struct pid_list * plist, pid_t pid);

int remove_pid_list(struct pid_list *plist, pid_t pid);

void destroy_pid_list(struct pid_list *plist);

int find_pid_list(const struct pid_list *plist, pid_t pid);

struct client_info *create_client_info(pid_t pid, int wait, const char *cwd);

int parse_command(char *cmd, char *cmd_argv[]);

enum req_cmd convert_req_cmd(const char *cmd);

void err_exit(const char *err);

#endif