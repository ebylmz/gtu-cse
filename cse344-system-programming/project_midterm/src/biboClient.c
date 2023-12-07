#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <errno.h>
#include <signal.h>
#include "biboClient.h"
#include "common.h"

volatile sig_atomic_t connected = 1;

int main(int argc, char *argv[]) 
{
    int server_fd, client_fd;
    int num_read, client_turn, approve, skip_cmd_scan;
    pid_t server_pid;
    char client_fifo[CLIENT_FIFO_NAME_LEN], server_fifo[REG_SERVER_FIFO_NAME_LEN];
    char cmd[CMD_LEN];
    struct request_header req;
    struct response_header resp;
    char *resp_data;
    struct client_info cli_info;
    struct sigaction sa_action;

    if (check_args(argc, argv, &cli_info.wait, &server_pid) == -1) {
        usage_error(argv[0]);
        exit(EXIT_FAILURE);
    }

    cli_info.pid = getpid();
    if (getcwd(cli_info.cwd, CWD_SIZE) == NULL)
        err_exit("getcwd");

    setbuf(stdout, NULL);

    snprintf(server_fifo, REG_SERVER_FIFO_NAME_LEN, REG_SERVER_FIFO_TEMPLATE, server_pid);
    snprintf(client_fifo, CLIENT_FIFO_NAME_LEN, CLIENT_FIFO_TEMPLATE, cli_info.pid);

    /* Create client FIFO for reading server responses (open it after registering server) */
    if (mkfifo(client_fifo, S_IRUSR | S_IWUSR | S_IWGRP) == -1 && errno != EEXIST)
        err_exit("mkfifo");

    /* Open server FIFO */
    if ((server_fd = open(server_fifo, O_WRONLY)) == -1)
        err_exit("open server_fifo");

    /* Make a connection request to the server */
    if (write(server_fd, &cli_info, sizeof(struct client_info)) < (long) sizeof(struct client_info)) {
        fprintf(stderr, "Error writing registeration request header to FIFO %s\n", server_fifo);
        exit(EXIT_FAILURE); 
    }

    /* Open client FIFO */
    if ((client_fd = open(client_fifo, O_RDONLY)) == -1) 
        err_exit("client_fifo");

    /* Wait for the server turn */
    if (read(client_fd, &client_turn, sizeof(int)) != (long) sizeof(int)) {
        fprintf(stderr, "Error reading registeration turn; discarding\n");
        exit(EXIT_FAILURE); 
    }

    /* Close the registeration server */
    if ((close(server_fd) == -1))
        err_exit("close reg server_fd");

    printf("Client turn: %d\n", client_turn); 
   
    if (client_turn > 0 && !cli_info.wait) {
        printf("exit\n");
        return 0; 
    }
    printf("Waiting for Que...\n");

    snprintf(server_fifo, REQ_SERVER_FIFO_NAME_LEN, REQ_SERVER_FIFO_TEMPLATE, cli_info.pid);

    /* Create new server FIFO for server send the the responses */
    if (mkfifo(server_fifo, S_IRUSR | S_IWUSR | S_IWGRP) == -1 && errno != EEXIST)
        err_exit("mkfifo");

    /* open() blocks until the server client opens the other end of the server FIFO for writing. */
    if ((server_fd = open(server_fifo, O_WRONLY)) == -1) 
        err_exit("open req server_fd");

    //TODO: send quit signal if SIGINT / SIGTERM catched

    /* Handler for SIGINT signal */
    sa_action.sa_flags = SA_SIGINFO | SA_RESTART;
    sa_action.sa_handler = sig_handler;
    if (sigemptyset(&sa_action.sa_mask) == -1 ||
        sigaction(SIGTERM, &sa_action, NULL) == -1 ||
        sigaction(SIGINT, &sa_action, NULL) == -1)
        perror("sa_action");

    fflush(stdin);
    printf("Connection established");

    skip_cmd_scan = 0;
    while (connected) {
        printf("\n\n");

        if (skip_cmd_scan == 1)
            skip_cmd_scan = 0;
        else {
            printf("MyBiboBox@sv%d$ ", server_pid);
    
            if (read_command_line(cmd, CMD_LEN) == -1) {
                fprintf(stderr, "Error reading user command %s\n", server_fifo);
                continue;
            }
            else if (cmd[0] == '\n')
                continue;
            
            /* SIGINT (CTRL-C is pressed) or SIGTERM signal catchted */
            if (connected == 0) {
                lower(cmd);
                if (strcmp(cmd, "no") == 0)
                    connected = 1;
                else if (strcmp(cmd, "yes") != 0) {
                    connected = 1;
                    printf("Exit ignored\n");
                }
                continue;
            }

            req.data_size = strlen(cmd) + 1;
            
            /* Write the header */
            if (write(server_fd, &req, sizeof(struct request_header)) < (long) sizeof(struct request_header)) {
                fprintf(stderr, "Error writing request header to FIFO %s\n", server_fifo);
                continue;
            }
            /* Write the command */
            if (write(server_fd, cmd, req.data_size) < (long) req.data_size) {
                fprintf(stderr, "Error writing request cmd to FIFO %s\n", server_fifo);
            }
        }

        do {
            /* Wait for the response */
            num_read = read(client_fd, &resp, sizeof(struct response_header));
            if (num_read == 0) {
                printf("Server fifo is closed\n");
                break;
            }
            else if (num_read < (long) sizeof(struct response_header)) {
                fprintf(stderr, "Error reading response header; discarding\n");
                break;
            }

            printf("\n");

            if (resp.stat == RESP_DISCONNECT) {
                connected = 0;
            }
            else if (resp.data_size > 0) {
                if ((resp_data = malloc(resp.data_size + 1)) == NULL) {
                    break;
                }
                if (read(client_fd, resp_data, resp.data_size) < (long) resp.data_size) {
                    fprintf(stderr, "Error reading response data; discarding\n");
                    free(resp_data);
                    break;
                }
                /* For safety put null charachter at the end of each server respond */
                resp_data[resp.data_size + 1] = '\0';

                printf("%s", resp_data);
                free(resp_data);
            } 
        } while (resp.stat == RESP_CONT);

        if (resp.stat == RESP_APPROVE) {
            /* Not overwrite is the default behaviour */
            if ((skip_cmd_scan = approve = get_approve()) == -1)
                approve = 0;
            if (write(server_fd, &approve, sizeof(int)) < (long) sizeof(int)) {
                fprintf(stderr, "Error writing approve response to FIFO %s\n", server_fifo);
                break;
            }
        }
    }

    printf("\nClosing resources...\n");

    if (close(server_fd) == -1) 
        perror("close server_fd");

    if (close(client_fd) == -1) 
        perror("close client_fd");

    if (unlink(client_fifo) == -1)
        perror("unlink client_fifo");
        
    if (unlink(server_fifo) == -1)
        perror("unlink server_fifo");

    printf("exit\n");
}

int get_approve()
{
    char resp[BUFF_SIZE];
    int len;
    while (1) {
        if (fgets(resp, BUFF_SIZE, stdin) == NULL)
            return -1;
        len = lower(resp);
        resp[len - 1] = '\0';
        if (strcmp(resp, "yes") == 0)
            return 1;
        else if (strcmp(resp, "no") == 0)
            return 0;
        printf("Please write only yes or no: ");
    }
    fflush(stdin);
}

int lower(char *str)
{
    int i;
    for (i = 0; str[i] != '\0'; ++i)
        if ('A' <= str[i] && str[i] <= 'Z')
            str[i] = 'a' + str[i] - 'A';
    return i;
}

int read_command_line(char buff[], int buff_size) 
{
    int len;

    if (fgets(buff, buff_size, stdin) == NULL)
        return -1;

    len = strlen(buff);

    /* in windows line ends with '\r\n' */
    if (len > 1 && (buff[len - 2] == '\r' || buff[len - 2] == '\n'))
        len -= 2;        
    else if (len > 1 && (buff[len - 1] == '\n' || buff[len - 1] == '\r'))
        len -= 1;
    
    buff[len] = '\0';
    return len;
}

void sig_handler()
{
    printf("\nAre you sure want to quit (yes/no)? ");
    connected = 0;
}

int check_args(int argc, char *argv[], int *wait, pid_t *server_pid) 
{
    char *endptr;

    if (argc != 3)
        return -1;

    /* Blocking/Non-blocking server connection */
    if (strcmp(argv[1], CLIENT_CONNECT) == 0)
        *wait = 1;
    else if (strcmp(argv[1], CLIENT_CONNECT_NON_BLOCKING) == 0)
        *wait = 0;
    else 
        return -1;

    /* Server pid */
    errno = 0;
    *server_pid = strtol(argv[2], &endptr, 10);
    if (*endptr != '\0'  || errno != 0)
        return -1;
    return 0;
}

void usage_error(char *pname) 
{
    fprintf(stderr, "Right usage: %s <connect/tryConnect> ServerPID\n", pname);
}