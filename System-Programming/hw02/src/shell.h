#ifndef SHELL_H
#define SHELL_H

void sig_handler(int signum);

void exec_piping(char *cmds[], int num_cmd, int log_fd);

void spawn_exec(int in, int out, char *cmd, int log_fd);

void io_redirection(int in, int out, char *cmd, char *argv[]);

void redirect_std(int fd, int std_fd);

void run_command(char *argv[]);

void trim(char *src);

int tokenize(char *stream, char *delim, char *result[]);

int read_command_line(char buff[], int size);

void prompt();

void print_log(int fd, char *command);

void get_time(char *t);

char *create_log_fname();

void usage_error(char *pname);

void err_exit(const char *err);

#endif