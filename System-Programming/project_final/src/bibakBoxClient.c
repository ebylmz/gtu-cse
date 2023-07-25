#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <arpa/inet.h>
#include "sync.h"
#include "bibakBoxClient.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

int client_run = 1;

int main(int argc, char *argv[])
{
    int cli_sock, portnumber;
    size_t len;
    struct sockaddr_in server_addr;
    struct sigaction sa_action;
    char *rootpath, *serverIP;

    if (handle_args(argc, argv, &portnumber) == -1) {
        usage_err(argv[0]);
        exit(EXIT_FAILURE);
    }

    /* Handler for SIGINT and SIGTERM signals */
    sa_action.sa_flags = SA_SIGINFO | SA_RESTART;
    sa_action.sa_handler = sighandler;
    if (sigemptyset(&sa_action.sa_mask) == -1 ||
        sigaction(SIGINT, &sa_action, NULL) == -1 ||
        sigaction(SIGTERM, &sa_action, NULL) == -1)
        perror("sa_action");

    rootpath = argv[1];

    cli_sock = socket(AF_INET, SOCK_STREAM, 0);
    if (cli_sock == -1) {
        perror("Failed to create socket");
        exit(EXIT_FAILURE);
    }

    /* Connection address: if no address is given, set the localhost as default */
    serverIP = (argc == 4) ? argv[3] : "127.0.0.1";

    /* Set up the server address */
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(portnumber);
    if (inet_pton(AF_INET, serverIP, &(server_addr.sin_addr)) <= 0) {
        fprintf(stderr, "%s: Invalid address or address not supported", serverIP);
        exit(EXIT_FAILURE);
    }

    /* Connect to the server */
    if (connect(cli_sock, (struct sockaddr *) &server_addr, sizeof(server_addr)) == -1) {
        perror(serverIP);
        fprintf(stderr, "Failed to connect to the server\n");
        exit(EXIT_FAILURE);
    }

    printf("Connected to the server.\n");

    len = strlen(rootpath) + 1;
    /* send the directory name for synchronization */
    if (write(cli_sock, rootpath, len) < (ssize_t) len) {
        perror("write");
        close(cli_sock);
        exit(EXIT_FAILURE);
    }
    
    sync_cli(cli_sock, rootpath);

    close(cli_sock);

    return 0;
}

void sighandler()
{
    client_run = 0;
}

void sync_cli(int sockfd, const char *rootpath)
{
    struct fnode *root;
    struct header head;
    int skiplen;
    char sv_path[MAX_PATH_LENGTH];
    char *cli_path;
    
    root = build_dirtree(NULL, rootpath);
    if (root == NULL) {
        fprintf(stderr, "Failed to build directory tree\n");
        exit(EXIT_FAILURE);
    }

    skiplen = snprintf(sv_path, MAX_PATH_LENGTH, "%s/", rootpath);
    cli_path = sv_path + skiplen;

    /* First synchronization between server and client */
    printf("Being synced %s...\n", rootpath);
    check_exit(sync_reader(sockfd, root, NULL), "sync_reader");
    check_exit(sync_writer(sockfd, root, rootpath), "sync_writer");
    printf("Your directory synchronized\n");

    // print_dirtree(root);

    while (client_run) { 
        /* Check if there is any change on client directory */
        compare_dirtree(root, rootpath, sockfd, skiplen, NULL);
        if (send_header_event(sockfd, DONE) == -1) {
            perror("send_header_event");
            continue;
        }

        /* Check if there is any change on server directory */
        while (recv_header(sockfd, &head, cli_path) == 0 && head.event != DONE && head.event != CLOSE)
            handle_event(sockfd, root, &(head.st), sv_path, head.event, NULL); 

        if (head.event == CLOSE) {
            /* Server notifies client for termination */
            printf("Server connection is closing...\n");
            break;
        }
        // sleep(1);
    }

    /* If termination due to SIGINT or SIGTERM signals on client side, notify the server */
    if (client_run == 0 && send_header_event(sockfd, CLOSE) == -1)
        perror("send_header_event");

    free_dirtree(root);

    printf("Exiting...\n");
}

int handle_args(int argc, char *argv[], int *portnumber)
{
    int status;
    long val;

    /* bibakBoxClient [dirName] [portnumber] */
    if (argc != 3 && argc != 4)
        return -1;
   
    val = str_to_long(argv[2], &status);
    if (status == -1 || val < 0)
        return -1;

    *portnumber = (int) val; 

    return 0;
}

void usage_err(const char *pname)
{
    fprintf(stderr, "Usage: %s [dirName] [portnumber] [server_address]\n", pname);
    fprintf(stderr, "[server_address] is an optional parameter for the IP address. \
        If this parameter is provided, the client will attempt to connect \
        to the server using the provided IP address. However, if the \
        parameter is not provided, the client will try to connect to a \
        server running on the local machine.");
}