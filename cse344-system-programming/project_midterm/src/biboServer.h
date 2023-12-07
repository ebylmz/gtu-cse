#ifndef BIBO_SERVER_H
#define BIBO_SERVER_H

#include "common.h"
#include "sync.h"

#define ERR_MSG_LEN 128

#define LOG_FILE_TEMPLATE "mybibobox.%d.log"

#define LOG_FILE_LEN (sizeof(LOG_FILE_TEMPLATE) + 20)

#define SHM_SEM "/safe_dir"

void server_request(DIR *server_dir, const char *server_dir_path, struct client_info *info, int client_id, int log_fd);

int spawn_child_server(DIR *server_dir, const char *server_dir_path, struct client_info *info, int *client_id, int log_fd);

int cmd_help(int client_fd, enum req_cmd cmd);

void help_usage(enum req_cmd cmd, char *str);

int cmd_list(int client_fd, DIR *server_dir);

int cmd_copy(int server_fd, int client_fd, const char *src_dir_path, const char *src_file, const char* dest_dir_path, int sv_write);

long copy_file(int src_fd, int dest_fd, struct safe_file *sfile, int sv_write);

int cmd_readF(int client_fd,  const char *server_dir_path, const char *src_file, int line_no);

int seek_line(int fd, int line_no);

char *read_next_line(int fd);

int cmd_writeT(int client_fd,  const char *server_dir_path, const char *src_file, const char *str, int line_no);

int open_dir_file(const char *dir_path, const char *file, int flags);

int cmd_kill_server(int client_fd, int client_id);

void sigint_handler();

void sigchld_handler();

void remove_child();

void kill_childs();

int cmd_quit(int client_fd);

int write_response(int client_fd, struct response_header *header, const char *data);

int log_request(int log_fd, pid_t client_pid, const char *cmd);

int log_response(int log_fd, pid_t client_pid, const char *cmd, enum resp_status status, const char *extra);

int log_info(int log_fd, pid_t client_pid, const char *info);

int write_log(int log_fd, const char *log);

char *get_time();

int check_cmd_argc(enum req_cmd cmd, int argc);

int check_args(int argc, char *argv[], int *client_max);

int str_to_int(const char *str, int *status);

void usage_error(char *pname);

#endif