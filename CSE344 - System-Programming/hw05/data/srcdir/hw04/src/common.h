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

enum request_type {REQ_CONNECT, REQ_DISCONNECT, REQ_GET,REQ_KILL};

struct request_header {
    enum request_type type;
    pid_t pid;
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

struct node {
    void *data;
    struct node *next;
};

struct client_list {
    int size;
    int capa;
    struct client_info **clients;
};

struct list {
    int size;
    int capa;
    void **items;
};

int read_response(int client_fd, struct response_header *head, void **data);

int write_request(int client_fd, struct request_header *header, const void *data);

void resp_head_set(struct response_header *head, enum resp_status stat, size_t data_size);

void req_head_set(struct request_header *head, pid_t pid, enum request_type type, size_t data_size);

void *enqueue(struct node **head, void *data);

void *dequeue(struct node **head);

int queue_size(const struct node *head);

void queue_destroy(struct node *head);

int list_init(struct list *li, int capa);

void *list_add(struct list *li, void *item);

void *list_remove(struct list *li, int i);

int list_resize(struct list *li);

void list_destroy(struct list *li);

enum req_cmd convert_req_cmd(const char *cmd);

char *strip_ext(char *fname);

void fname_version(char *fname, char *fnamev, int v);

int parse_command(char *cmd, char *cmd_argv[]);

int str_to_int(const char *str, int *status);

void err_exit(const char *err);

#endif