#ifndef BIBO_CLIENT_H
#define BIBO_CLIENT_H

#define CLIENT_CONNECT "connect"
#define CLIENT_CONNECT_NON_BLOCKING "tryConnect"

int lower(char *str);

int read_command_line(char buff[], int buff_size);

int is_empty_cmd(const char *cmd);

int just_cmd(const char *cmd, char *cmd_name);

int check_args(int argc, char *argv[], int *wait, pid_t *server_pid);

void sig_handler();

void usage_error(char *pname);

#endif