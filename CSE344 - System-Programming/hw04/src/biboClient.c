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

volatile sig_atomic_t sigint_flag = 0;

int main(int argc, char *argv[]) 
{
    int server_fd, client_fd;
    int len, read_status, connected;
    pid_t server_pid;
    void *resp_data;
    char client_fifo[CLIENT_FIFO_NAME_LEN], server_fifo[REG_SERVER_FIFO_NAME_LEN];
    char cmd[CMD_LEN], cmd_name[CMD_LEN];
    struct request_header req_head;
    struct response_header resp_head;
    struct client_info cli_info;
    struct sigaction sa_action;

    if (check_args(argc, argv, &cli_info.wait, &server_pid) == -1) {
        usage_error(argv[0]);
        exit(EXIT_FAILURE);
    }

    cli_info.pid = getpid();
    if (getcwd(cli_info.cwd, CWD_SIZE) == NULL)
        err_exit("getcwd");

    snprintf(server_fifo, REG_SERVER_FIFO_NAME_LEN, REG_SERVER_FIFO_TEMPLATE, server_pid);
    snprintf(client_fifo, CLIENT_FIFO_NAME_LEN, CLIENT_FIFO_TEMPLATE, cli_info.pid);

    /* Create client FIFO for reading server responses (open it after registering server) */
    if (mkfifo(client_fifo, S_IRUSR | S_IWUSR | S_IWGRP) == -1 && errno != EEXIST)
        err_exit("mkfifo");

    /* Open server FIFO */
    if ((server_fd = open(server_fifo, O_WRONLY)) == -1)
        err_exit("open server_fifo");

    req_head_set(&req_head, cli_info.pid, REQ_CONNECT, sizeof(struct client_info));

    /* Make a connection request to the server */
    if (write_request(server_fd, &req_head, &cli_info) == -1) {
        fprintf(stderr, "Error writing connection request\n");
        exit(EXIT_FAILURE);
    }

    setbuf(stdout, NULL);

    printf("Waiting for Que...\n");

    /* Open the client FIFO */
    if ((client_fd = open(client_fifo, O_RDONLY)) == -1) 
        err_exit("client_fifo");

    /* Wait for the server turn */
    if (read_response(client_fd, &resp_head, &resp_data) != 0) {
        fprintf(stderr, "Error reading connection response\n");
        exit(EXIT_FAILURE);
    }

    connected = 0;
    if (resp_head.data_size > 0) {
        /* Check the server response to learn whether the server is currenly available or not */
        if (*((int *)resp_data) == 0) /* No wait, instant connection */
            connected = 1;
        else {
            if (cli_info.wait) {
                /* Wait for the server connection */
                if (read_response(client_fd, &resp_head, &resp_data) != 0) {
                    fprintf(stderr, "Error reading connection response\n");
                    exit(EXIT_FAILURE);
                }

                if (resp_head.stat == RESP_OK)
                    connected = 1;
                
                if (resp_head.data_size > 0) {
                    printf("\n%s\n", (char *) resp_data);
                    free(resp_data);
                }
            }   
        }
    }

    if (connected)
        printf("Connection established\n");
    else
        printf("Connection could not establish\n");

    /* Handler for SIGINT signal */
    sa_action.sa_flags = SA_SIGINFO | SA_RESTART;
    sa_action.sa_handler = sig_handler;
    if (sigemptyset(&sa_action.sa_mask) == -1 ||
        sigaction(SIGTERM, &sa_action, NULL) == -1 ||
        sigaction(SIGINT, &sa_action, NULL) == -1)
        perror("sa_action");

    fflush(stdin);

    req_head.type = REQ_GET;
    while (connected) {
        if (sigint_flag) {
            printf("\nSIGINT signal recieved\n");
            break;
        }

        printf("MyBiboBox@sv%d$ ", server_pid);
            
        if (read_command_line(cmd, CMD_LEN) == -1) {
            fprintf(stderr, "Error reading user command %s\n", server_fifo);
            continue;
        }

        len = just_cmd(cmd, cmd_name);

        if (len == 0) /* empty cmd */
            continue;
        else if (strcmp(cmd_name, "quit") == 0) { 
            req_head.type = REQ_DISCONNECT;
            req_head.data_size = 0;
        }
        else if (strcmp(cmd_name, "killServer") == 0) {
            req_head.type = REQ_KILL;
            req_head.data_size = 0;
        }
        else
            req_head.data_size = strlen(cmd) + 1;
        
        if (write_request(server_fd, &req_head, cmd) == -1)
            fprintf(stderr, "Error, writing request\n");
        
        do {
            read_status = read_response(client_fd, &resp_head, &resp_data); 
            if (read_status == 0) {
                /* Connection is gone */
                if (resp_head.stat == RESP_DISCONNECT)
                    connected = 0;
                if (resp_head.data_size > 0) {
                    /* For safety put null charachter at the end of each server response */
                    ((char *) resp_data)[resp_head.data_size + 1] = '\0';
                    printf("\n%s\n", (char *) resp_data);
                    free(resp_data);
                }
            }
        } while (resp_head.stat == RESP_CONT);
    }

    /* If the command prompt interrupted with signals such as SIGINT, make sure to close the connection */
    if (connected) {
        /* Disconnect from the server */
        req_head_set(&req_head, cli_info.pid, REQ_DISCONNECT, 0);

        if (write_request(server_fd, &req_head, NULL) == -1) {
            fprintf(stderr, "Error writing disconnection request\n");
            exit(EXIT_FAILURE);
        }

        /* Wait for the server response for disconnection request */
        if (read_response(client_fd, &resp_head, &resp_data) != 0) {
            fprintf(stderr, "Error reading disconnection response\n");
            exit(EXIT_FAILURE);
        }

        if (resp_head.data_size > 0) {
            printf("\n%s\n", (char *) resp_data);
            free(resp_data);
        }
    }

    printf("\nClosing resources...\n");

    if (close(server_fd) == -1) 
        perror("close server_fd");

    if (close(client_fd) == -1) 
        perror("close client_fd");

    if (unlink(client_fifo) == -1)
        perror("unlink client_fifo");

    printf("exit\n");
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

int is_empty_cmd(const char *cmd)
{
    int i;
    for (i = 0; cmd[i] != '\0' && cmd[i] == ' '; ++i) ;
    return cmd[i] == '\0' || cmd[i] == '\n';
}

int just_cmd(const char *cmd, char *cmd_name)
{
    int i, j;
    /* Skip the spaces */
    for (i = 0; cmd[i] == ' '; ++i) ;
    
    for (j = 0; cmd[i] != '\0' && cmd[i] != '\n' && cmd[i] != ' '; ++i, ++j) 
        cmd_name[j] = cmd[i];
    cmd_name[j] = '\0';
    return j; /* Length of the first command */
}

void sig_handler()
{
    sigint_flag = 1;
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