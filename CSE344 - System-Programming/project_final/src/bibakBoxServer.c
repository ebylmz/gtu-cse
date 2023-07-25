#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <fcntl.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <errno.h>
#include <pthread.h>
#include "sync.h"
#include "bibakBoxServer.h"

int server_run = 1;
pthread_mutex_t mutex_clients;
pthread_mutex_t mutex_stdout;
pthread_cond_t cond_full;

struct circular_buffer waiting_clients;
struct circular_buffer directories;
struct circular_buffer logfiles;

int main(int argc, char *argv[])
{
    int maxfd, sv_sock, cli_sock, portnumber, pool_size, next_id;
    struct client *cli;
    pthread_t *threads;
    struct sockaddr_in server_addr, client_addr;
    socklen_t client_len;
    struct sigaction sa_action;
    char *sv_dirpath;
    fd_set sockfds, readfds;

    if (handle_args(argc, argv, &pool_size, &portnumber) == -1) {
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

    /* Create the server folder if it doesn't exist */
    sv_dirpath = argv[1];
    if (mkdir(sv_dirpath, S_IRWXU | S_IWUSR | S_IRUSR | S_IXUSR | S_IWGRP | S_IRGRP) == -1 && errno != EEXIST)
        err_exit("mkdir");

    /* Initialize wating clients buffer which used as queue */
    check_exit(buff_init(&waiting_clients, INIT_QUE_SIZE), "buff_init"); 

    /* Initialize directory buffer */
    check_exit(buff_init(&directories, 2), "buff_init"); 

    /* Initialize directory buffer */
    check_exit(buff_init(&logfiles, 2), "buff_init"); 

    /* İnitialize a mutex and condition variables for clients queue */
    pthread_check_exit(pthread_mutex_init(&mutex_clients, NULL), "pthread_mutex_init");
    pthread_check_exit(pthread_mutex_init(&mutex_stdout, NULL), "pthread_mutex_init");
    pthread_check_exit(pthread_cond_init(&cond_full, NULL), "pthread_cond_init");

    /* Create the server socket */
    sv_sock = socket(AF_INET, SOCK_STREAM, 0);
    check_exit(sv_sock, "socket");

    /* Set up the server address */
    server_addr.sin_family = AF_INET;
    server_addr.sin_addr.s_addr = INADDR_ANY;
    server_addr.sin_port = htons(portnumber);

    /* Bind the socket to the specified address and port */
    if (bind(sv_sock, (struct sockaddr *) &server_addr, sizeof(struct sockaddr_in)) == -1) {
        perror("Failed to bind");
        close(sv_sock);
        exit(EXIT_FAILURE);
    }

    if (listen(sv_sock, SERVER_BACKLOG) == -1) {
        perror("Failed to listen");
        close(sv_sock);
        exit(EXIT_FAILURE);
    }

    /* Create the worker thread pool */
    threads = create_thread_pool(pool_size, worker_thread_func, (void *) sv_dirpath);
    if (threads == NULL) {
        perror("Failed to create thread pool");
        close(sv_sock);
        exit(EXIT_FAILURE);
    }

    FD_ZERO(&sockfds);
    FD_SET(sv_sock, &sockfds);
    maxfd = sv_sock + 1;
    next_id = 0;

    printf("Server started. Listening on port %d...\n", portnumber);

    while (server_run) {
        readfds = sockfds;
        if (select(maxfd, &readfds, NULL, NULL, NULL) == -1) {
            if (errno == EINTR)
                continue;
            perror("select");
            break;
        }

        /* Accept incoming connection */
        if (FD_ISSET(sv_sock, &readfds)) {
            /* Client len contains the length of the cleint_addr */
            client_len = sizeof(client_addr); 
            cli_sock = accept(sv_sock, (struct sockaddr *) &client_addr, &client_len);
            if (cli_sock == -1) {
                perror("Failed to accept new connection");
                continue;
            }

            cli = (struct client *) malloc(sizeof(struct client));
            if (cli == NULL) {
                close(cli_sock);
                perror("malloc");
                continue;
            }

            cli->sockfd = cli_sock;
            cli->id = next_id;
            ++next_id;

            if (pthread_mutex_lock(&mutex_clients) != 0) {
                fprintf(stderr, "Failed to lock the mutex\n");
                break;
            }

            /* Put the client information into the queue */
            if (buff_push(&waiting_clients, cli) == -1)
                perror("buff_push");

            if (waiting_clients.size == 1) {
                if (pthread_cond_broadcast(&cond_full) != 0) {
                    fprintf(stderr, "Failed to conditional broadcast\n");
                    break;
                }
            }

            printf("New connection request from %s as Client%d\n", inet_ntoa(client_addr.sin_addr), next_id);

            if (pthread_mutex_unlock(&mutex_clients) != 0) {
                fprintf(stderr, "Failed to unlock the mutex\n");
                break;
            }
        }
    }

    if (server_run == 0)
        printf("\n");
    printf("Shutting down...");

    /* Make a conditional broadcast to terminate all the threads */
    pthread_check_exit(pthread_cond_broadcast(&cond_full), "pthread_cond_broadcast");

    /* Close/Release all the resources */
    destroy_thread_pool(threads, pool_size);

    buff_destroy(&waiting_clients, NULL);
    
    buff_destroy(&logfiles, (void (*)(void *)) safefile_free);
    
    buff_destroy(&directories, (void (*)(void *)) safefile_free);

    pthread_mutex_destroy(&mutex_clients);
    
    pthread_mutex_destroy(&mutex_stdout);

    pthread_cond_destroy(&cond_full);

    close(sv_sock);
}

void *worker_thread_func(void *args)
{
    char *sv_dirpath;
    struct client *cli;

    sv_dirpath = (char *) args;

    pthread_check_exit(pthread_mutex_lock(&mutex_clients), "pthread_mutex_lock");
    while (server_run) {
        cli = buff_pop(&waiting_clients);
        if (cli == NULL)
            pthread_check_exit(pthread_cond_wait(&cond_full, &mutex_clients), "pthread_cond_wait");
        else {
            pthread_check_exit(pthread_mutex_unlock(&mutex_clients), "pthread_mutex_unlock");

            print_threadsafe(STDOUT_FILENO, &mutex_stdout, "Client%d is connected\n", cli->id);
            handle_client(cli->sockfd, sv_dirpath);
            close(cli->sockfd);
            print_threadsafe(STDOUT_FILENO, &mutex_stdout, "Client%d is disconnected\n", cli->id); 
            free(cli);
            
            pthread_check_exit(pthread_mutex_lock(&mutex_clients), "pthread_mutex_lock");
        }
    }
    pthread_check_exit(pthread_mutex_unlock(&mutex_clients), "pthread_mutex_unlock");

    return NULL;
}

void sighandler()
{
    server_run = 0;
}

void handle_client(int cli_sock, const char *sv_dirpath) 
{
    int status, pathlen;
    char cli_dirpath[MAX_PATH_LENGTH];
    char *ptr;

    /* read the name of the directory from the socket */
    pathlen = snprintf(cli_dirpath, MAX_PATH_LENGTH, "%s/", sv_dirpath);

    for (ptr = cli_dirpath + pathlen; (status = read(cli_sock, ptr, 1)) == 1 && *ptr != '\0'; ++ptr)  ;

    /* Create the client remote directory if it doesn't exist */
    if (mkdir(cli_dirpath, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH) == -1 && errno != EEXIST)
        err_exit("mkdir");

    sync_sv(cli_sock, cli_dirpath); 
}

void sync_sv(int sockfd, const char *rootpath)
{
    struct fnode *root;
    struct header head;
    struct safefile *sdir, *logfile;
    int skiplen, logfd;
    char sv_path[MAX_PATH_LENGTH], logfile_path[MAX_PATH_LENGTH]; 
    char *cli_path;

    root = build_dirtree(NULL, rootpath);
    if (root == NULL) {
        fprintf(stderr, "Failed to build directory tree\n");
        exit(EXIT_FAILURE);
    }

    skiplen = snprintf(sv_path, MAX_PATH_LENGTH, "%s/", rootpath);
    cli_path = sv_path + skiplen;

    /* First get the safe directory object */
    sdir = safefile_list_find(&directories, cli_path); 
    if (sdir == NULL) {
        sdir = safefile_create(cli_path, -1);
        if (sdir == NULL) {
            fprintf(stderr, "Failed to create new safe directory\n");
            pthread_exit(NULL);
        }
        if (buff_push(&directories, sdir) == -1) {
            perror("buff_push");
            pthread_exit(NULL);
        }
    }

    /* Construct path for logfile */
    sprintf(logfile_path, "%s/%s", rootpath, LOG_FNAME);

    logfile = safefile_list_find(&logfiles, logfile_path);
    if (logfile == NULL) {
        logfd = open(logfile_path, O_RDWR | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
        if (logfd == -1) {
            perror(logfile_path);
            pthread_exit(NULL);
        }
        logfile = safefile_create(logfile_path, logfd);
        if (logfile == NULL) {
            fprintf(stderr, "Failed to create new safe file\n");
            pthread_exit(NULL);
        }
        if (buff_push(&logfiles, logfile) == -1) {
            perror("buff_push");
            pthread_exit(NULL);
        }
    }

    /* Remove the log file entry from the directory tree for not syncing */
    update_dirtree(root, logfile_path, REM);

    print_threadsafe(STDOUT_FILENO, &mutex_stdout, "Being synced %s...\n", rootpath);

    /* First synchronization between server and client */
    check_exit(sync_writer(sockfd, root, rootpath), "sync_writer");
    check_exit(sync_reader(sockfd, root, logfile), "sync_reader");

    print_threadsafe(STDOUT_FILENO, &mutex_stdout, "Directory %s is synchronized\n", rootpath);

    // print_dirtree(root);

    while (server_run) {
        /* Get the lock before making operation on the directory */
        pthread_check_exit(pthread_mutex_lock(&(sdir->mutex)), "pthread_mutex_lock");

        /* Check if there is any change on server directory */
        while (recv_header(sockfd, &head, cli_path) == 0 && head.event != DONE && head.event != CLOSE)
            handle_event(sockfd, root, &(head.st), sv_path, head.event, logfile); 

        if (head.event == CLOSE) {
            /* Client requests for termination */
            print_threadsafe(STDOUT_FILENO, &mutex_stdout, "Client request for termination\n");
            pthread_check_exit(pthread_mutex_unlock(&(sdir->mutex)) , "pthread_mutex_unlock");
            break;
        }

        /* Check if there is any change on client directory */
        compare_dirtree(root, rootpath, sockfd, skiplen, logfile);
        if (send_header_event(sockfd, DONE) == -1)
            perror("send_header_event");

        pthread_check_exit(pthread_mutex_unlock(&(sdir->mutex)) , "pthread_mutex_unlock");
    }

    /* If termination due to SIGINT or SIGTERM signals on server side, notify the server */
    if (server_run == 0 && send_header_event(sockfd, CLOSE) == -1)
        perror("send_header_event");

    free_dirtree(root);
}

pthread_t *create_thread_pool(int size, void *thread_func(void *), void *args)
{
    int i, status;
    pthread_t *threads;

    threads = (pthread_t *) malloc(sizeof(pthread_t) * size);
    if (threads != NULL) {
        for (i = 0, status = 0; i < size && status == 0; ++i)
            status = pthread_create(threads + i, NULL, thread_func, args);

        if (status != 0) {
            free(threads); 
            threads = NULL;
        }
    }

    return threads;
}

void destroy_thread_pool(pthread_t threads[], int pool_size) 
{
    int i;
    void *ret;

    for (i = 0; i < pool_size; ++i) {
        if (pthread_join(threads[i], &ret) != 0)
            fprintf(stderr, "Failed to join thread %ld\n", threads[i]);
    }
    free(threads);
}

int handle_args(int argc, char *argv[], int *pool_size, int *portnumber)
{
    int status;
    long val;

    /* bibakBoxServer [directory] [threadPoolSize] [portnumber] */
    if (argc != 4)
        return -1;

    val = str_to_long(argv[2], &status);
    if (status == -1 || val < 1)
        return -1;

    *pool_size = (int) val; 
    
    val = str_to_long(argv[3], &status);
    if (status == -1 || val < 0)
        return -1;

    *portnumber = (int) val; 

    return 0;
}

void usage_err(const char *pname)
{
    fprintf(stderr, "Usage: %s [directory] [threadPoolSize] [portnumber]\n", pname);
}
