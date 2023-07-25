#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include "client.h"
#include "bibo_box.h"

int main(int argc, char *argv[]) {
    int server_fd, client_fd, client_fifo_active, blocking;
    long client_pid, server_pid;
    char client_fifo[CLIENT_FIFO_NAME_LEN], server_fifo[SERVER_FIFO_NAME_LEN];
    char cmd[CMD_LEN];
    struct request_header req;
    struct response_header resp;
    char *resp_data;

    if (check_args(argc, argv, &blocking, &server_pid) == -1) {
        usage_error(argv[0]);
        exit(EXIT_FAILURE);
    }

    setbuf(stdout, NULL);

    // umask(0);
    client_pid = getpid();

    snprintf(server_fifo, SERVER_FIFO_NAME_LEN, SERVER_FIFO_TEMPLATE, server_pid);
    snprintf(client_fifo, CLIENT_FIFO_NAME_LEN, CLIENT_FIFO_TEMPLATE, client_pid);

    if (mkfifo(client_fifo, S_IRUSR | S_IWUSR | S_IWGRP) == -1 && errno != EEXIST)
NO_PROBLEMM_exit("mkfifo");

    if (atexit(remove_fifo) != 0)
        err_exit("atexit");


    /* Open server FIFO */
    //! how to enable blocking non blocking, the counter in the server side should be mutually exlusive
    if ((server_fd = open(server_fifo, O_WRONLY)) == -1) {
        //! Is the server file not found or another error
        err_exit("open");
    }

    printf("Waiting for Que...\n");
    printf("Connection established\n\n");

    client_fifo_active = 0;
    req.pid = client_pid;
    if (getcwd(req.cwd, CWD_SIZE) == NULL)
        err_exit("getcwd");

    while (1) {
        printf("Enter command: ");

        if (read_command_line(cmd, CMD_LEN) == -1) {
            fprintf(stderr, "Error reading user command %s\n", server_fifo);
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

        /* Open client FIFO */
        if (client_fifo_active == 0 && (client_fd = open(client_fifo, O_RDONLY)) == -1) 
            err_exit("client_fifo");

        /* wait for the response */
        if (read(client_fd, &resp, sizeof(struct response_header)) < (long) sizeof(struct response_header)) {
            fprintf(stderr, "Error reading response header; discarding\n");
            //! read the whole fifo content
            continue;
        }

        if (resp.data_size > 0) {
            resp_data = malloc(resp.data_size);
            if (read(client_fd, resp_data, resp.data_size) < (long) resp.data_size) {
                fprintf(stderr, "Error reading response data; discarding\n");
                free(resp_data);
                //! fflush the fifo
                continue;
            }
            printf("%s\n", resp_data);
            free(resp_data);
        } 
    }
}

void remove_fifo() {
    printf("remove_fifo call\n");
    // unlink(client_fifo);
}

int read_command_line(char buff[], int buff_size) {
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

int check_args(int argc, char *argv[], int *blocking, long *server_pid) {
    char *endptr;

    if (argc != 3)
        return -1;

    /* Blocking/Non-blocking server connection */
    if (strcmp(argv[1], CLIENT_CONNECT) == 0)
        *blocking = 1;
    else if (strcmp(argv[1], CLIENT_CONNECT_NON_BLOCKING) == 0)
        *blocking = 0;
    else 
        return -1;

    /* Server pid */
    errno = 0;
    *server_pid = strtol(argv[2], &endptr, 10);
    if (*endptr != '\0'  || errno != 0)
        return -1;
    return 0;
}

void usage_error(char *pname) {
    fprintf(stderr, "Right usage: %s <connect/tryConnect> ServerPID\n", pname);
}change_on_fileyeee