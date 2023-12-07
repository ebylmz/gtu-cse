#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/mman.h>
#include <semaphore.h>
#include <signal.h>
#include <string.h>
#include <errno.h>
#include <dirent.h>
#include <pthread.h>
#include "biboServer.h"
#include "common.h"
#include "sync.h"

pthread_t *thread_pool;

pthread_mutex_t mutex_log;
pthread_mutex_t mutex_job_que;
pthread_cond_t cond_job_que;

struct list serving_list;
struct list dir_files;

struct node *job_que = NULL;
struct node *wait_que = NULL;

volatile sig_atomic_t sigint_flag = 0;
volatile sig_atomic_t server_shut_down = 0;

int main(int argc, char *argv[])
{
    int server_fd, dummy_fd, client_fd, log_fd;
    int i, status, server_kill, max_clients, pool_size, client_turn, next_client_id;
    char server_fifo[REG_SERVER_FIFO_NAME_LEN], client_fifo[CLIENT_FIFO_NAME_LEN], log_file[LOG_FILE_LEN];
    char msg[BUFF_SIZE];
    void *req_data;
    char *server_dir_path;
    DIR *server_dir;
    size_t num_read;
    struct sigaction sa_action;
    struct request_header req_head;
    struct response_header resp_head;
    struct thread_job *job;
    struct client_ref *cli_ref;
    struct client_info *cli_info;
    struct thread_workspace workspace;

    /* Handle arguments */
    if (check_args(argc, argv, &max_clients, &pool_size) == -1) {
        usage_error(argv[0]);
        exit(EXIT_FAILURE);
    }

    /* Handler for SIGINT signal */
    sa_action.sa_flags = SA_SIGINFO | SA_RESTART;
    sa_action.sa_handler = sigint_handler;
    if (sigemptyset(&sa_action.sa_mask) == -1 ||
        sigaction(SIGINT, &sa_action, NULL) == -1)
        perror("sa_action");

    setbuf(stdout, NULL);

    /* Initialize size child_pids with the number of max clients */
    status = list_init(&serving_list, max_clients);
    if (status == -1) {
        fprintf(stderr, "Error creating the serving list");
        exit(EXIT_FAILURE);
    }

    /* Create the server folder if it doesn't exist */
    server_dir_path = argv[1];
    if ((status = mkdir(server_dir_path, S_IRWXU | S_IWUSR | S_IRUSR | S_IXUSR | S_IWGRP | S_IRGRP)) == -1 && errno != EEXIST)
        err_exit("mkdir");

    /* Open server folder */
    if ((server_dir = opendir(argv[1])) == NULL)
        err_exit("opendir");


    /* initialize directory files list */
    status = list_init(&dir_files, 1);
    if (status == -1) {
        fprintf(stderr, "Error creating the directory list");
        exit(EXIT_FAILURE);
    }

    /* Create the log file */
    snprintf(log_file, LOG_FILE_LEN, LOG_FILE_TEMPLATE, getpid());

    log_fd = open(log_file, O_RDWR | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
    if (log_fd == -1)
        err_exit(log_file);

    /* To prevent race condition use mutex during the access of log file */
    if (pthread_mutex_init(&mutex_log, NULL) != 0 || 
        pthread_mutex_init(&mutex_job_que, NULL) != 0) {
        fprintf(stderr, "Error, initializing mutex\n");
        exit(EXIT_FAILURE);
    }

    if (pthread_cond_init(&cond_job_que, NULL) != 0) {
        fprintf(stderr, "Error, initializing condition variable\n");
        exit(EXIT_FAILURE);
    }

    /* Create well-known FIFO, and open it for reading */
    snprintf(server_fifo, REG_SERVER_FIFO_NAME_LEN, REG_SERVER_FIFO_TEMPLATE, getpid());

    if (mkfifo(server_fifo, S_IRUSR | S_IWUSR | S_IWGRP) == -1 && errno != EEXIST)
        err_exit("mkfifo");

    printf("Server Started PID %d\n", getpid());
    printf("Waiting for clients...\n");
    log_info(log_fd, 0, "started");

    /* open() blocks until the first client opens the other end of the server FIFO for writing */
    /* Use O_NONBLOCK flag not to be blocked while reading the server FIFO */
    if ((server_fd = open(server_fifo, O_RDONLY | O_NONBLOCK)) == -1)
        err_exit("open server_fifo");

    /* Open an extra write descriptor, so that we never see EOF */
    if ((dummy_fd = open(server_fifo, O_WRONLY)) == -1)
        err_exit("open server_fifo for EOF");

    /* Prepare the thread workspace */    
    workspace.log_fd = log_fd;
    workspace.server_dir = server_dir;
    workspace.server_dir_path = server_dir_path;

    /* Create the thread pool */
    thread_pool = create_thread_pool(pool_size, thread_function, (void *) &workspace);
    if (thread_pool == NULL) {
        fprintf(stderr, "Error creating the thread pool\n");
        exit(EXIT_FAILURE);
    }

    setbuf(stdout, NULL);

    next_client_id = 0;
    server_kill = 0;
    while (!sigint_flag && !server_kill) {
        /* Get the new connection request */
        /* read never returns EOF (0) since there is at least one writer (dummy), so num_read never becomes 0 */
        num_read = read(server_fd, &req_head, sizeof(struct request_header)); 
        if ((long) num_read != -1) {
            if (num_read < sizeof(struct request_header)) {
                fprintf(stderr, "Error, reading request; discarding\n");
                continue;
            }
            
            if (req_head.data_size > 0) {
                req_data = malloc(req_head.data_size);
                if (req_data == NULL) {
                    fprintf(stderr, "Error, cannot allocate space; discarding\n");
                    continue; 
                }

                while ((long) (num_read = read(server_fd, req_data, req_head.data_size)) ==  -1);
                
                if (num_read < req_head.data_size) {
                    perror("req_data read");
                    free(req_data);
                    continue;
                }
            }

            switch (req_head.type) {
                case REQ_CONNECT:
                    cli_info = (struct client_info *) req_data;

                    snprintf(client_fifo, CLIENT_FIFO_NAME_LEN, CLIENT_FIFO_TEMPLATE, cli_info->pid);

                    /* Open client FIFO */
                    if ((client_fd = open(client_fifo, O_WRONLY)) == -1) {
                        fprintf(stderr, "Error, opening client FIFO %s\n", client_fifo);
                        continue;
                    }

                    client_turn = (serving_list.size < serving_list.capa ? 0 : 1) + queue_size(wait_que); 
                    /* Check the availability of the system */
                    if (client_turn == 0) {
                        cli_ref = create_client_ref(next_client_id, client_fd, cli_info);
                        if (establish_connection(&serving_list, cli_ref) == -1) {
                            fprintf(stderr, "Error, cannot establish connection with the client\n");
                            continue;
                        }
                        printf("Client PID %d connected as 'client%d'\n", cli_info->pid, cli_ref->cid);
                        ++next_client_id;
                    }
                    else {
                        printf("Connection request PID %d... Que FULL\n", cli_info->pid);
                        /* Send a response to indicate server is currently not available */
                        resp_head_set(&resp_head, RESP_OK, sizeof(int));
                        if ((status = write_response(client_fd, &resp_head, &client_turn)) == -1) {
                            fprintf(stderr, "Error, cannot response for the connection request\n");
                            continue;
                        }

                        if (cli_info->wait == 1) {
                            /* Put the client into the wait queue */
                            cli_ref = create_client_ref(next_client_id, client_fd, cli_info);
                            if (enqueue(&wait_que, cli_ref) == NULL) {
                                fprintf(stderr, "Error, cannot add the client to the wait queue\n");
                                continue;
                            }
                            ++next_client_id; 
                        }
                        else {
                            printf("Connection request PID %d left without waiting\n", cli_info->pid);
                            /* Close the client_fd since we are not using it */
                            close(client_fd);
                        } 
                    }
                    free(req_data);
                    if (log_request(log_fd, req_head.pid, "connect") == -1)
                        fprintf(stderr, "Error, logging the client request\n");
                    break;
                case REQ_DISCONNECT:
                    /* Remove the client from the serving_list */
                    i = find_serving_client(&serving_list, req_head.pid);
                    if (i == -1) {
                        fprintf(stderr, "Error, either client PID %d has no connection, or something went wrong\n", req_head.pid);
                        continue;
                    }
                    cli_ref = list_remove(&serving_list, i);
                    if (cli_ref == NULL) {
                        fprintf(stderr, "Error, cannot remove the client from the serving list\n");
                        continue;
                    }

                    resp_head_set(&resp_head, RESP_DISCONNECT, 0);
                    if ((status = write_response(cli_ref->fd, &resp_head, &client_turn)) == -1) {
                        fprintf(stderr, "Error, cannot response for the disconnection request\n");
                        continue;
                    }

                    printf("client%d disconnected..\n", cli_ref->cid);

                    close(cli_ref->fd);
                    free(cli_ref);

                    /* Take a waiting client for serving */
                    if (serving_list.size < serving_list.capa && wait_que != NULL) {
                        /* Take the client for serving */
                        cli_ref = dequeue(&wait_que);
                        if (establish_connection(&serving_list, cli_ref) == -1) {
                            fprintf(stderr, "Error, cannot establish connection with the client\n");
                            continue;
                        }
                        printf("Client PID %d connected as 'client%d'\n", cli_ref->info.pid, cli_ref->cid);
                    }
                    if (log_request(log_fd, req_head.pid, "quit") == -1)
                        fprintf(stderr, "Error, logging the client request\n");
                    break;
                case REQ_GET:
                    /* Create a new job and add into jobs queue */
                    job = create_thread_job(&req_head, (char *) req_data);
                    if (job == NULL) {
                        fprintf(stderr, "Error, cannot create the request job\n");
                        continue;
                    }                

                    if (pthread_mutex_lock(&mutex_job_que) != 0) {
                        fprintf(stderr, "Error, mutex lock\n"); 
                        exit(EXIT_FAILURE);
                    } 

                    if (enqueue(&job_que, job) == NULL)
                        fprintf(stderr, "Error, cannot add the job to the queue\n");
                    else if (pthread_cond_signal(&cond_job_que) != 0)
                        fprintf(stderr, "Error, cannot make conditional signal\n");
                    
                    if (pthread_mutex_unlock(&mutex_job_que) != 0) {
                        fprintf(stderr, "Error, mutex unlock\n"); 
                        exit(EXIT_FAILURE);
                    } 
                    break;
                case REQ_KILL:
                    i = find_serving_client(&serving_list, req_head.pid);
                    if (i == -1) {
                        fprintf(stderr, "Error, either client PID %d has no connection, or something went wrong\n", req_head.pid);
                        continue;
                    }
                    printf("Kill signal from client%d. Terminating...\n", ((struct client_ref **) serving_list.items)[i]->cid);
                    /* After this point no more request is gonna be accepted */
                    server_kill = 1;

                    if (log_request(log_fd, req_head.pid, "killServer") == -1)
                        fprintf(stderr, "Error, logging the client request\n");
                    break;
                default:
                    /* Ignore the request */
                    break;
            }
        }
    }

    log_info(log_fd, 0, "shut down");

    if (pthread_mutex_lock(&mutex_job_que) != 0) {
        fprintf(stderr, "Error, mutex lock\n"); 
        exit(EXIT_FAILURE);
    } 

    server_shut_down = 1;

    if (pthread_cond_broadcast(&cond_job_que) != 0) {
        fprintf(stderr, "Error, condtional broadcast\n"); 
        exit(EXIT_FAILURE);
    }

    if (pthread_mutex_unlock(&mutex_job_que) != 0) {
        fprintf(stderr, "Error, mutex unlock\n"); 
        exit(EXIT_FAILURE);
    }

    snprintf(msg, BUFF_SIZE, "Server %d shut down", getpid());
    resp_head_set(&resp_head, RESP_DISCONNECT, strlen(msg) + 1);
    
    /* Notify all the clients that server is shutting down */
    while ((cli_ref = dequeue(&wait_que)) != NULL) {
        write_response(cli_ref->fd, &resp_head, msg);
        free(cli_ref);
    }

    while (serving_list.size > 0) {
        cli_ref = list_remove(&serving_list, serving_list.size - 1);
        write_response(cli_ref->fd, &resp_head, msg);
        free(cli_ref);
    }

    printf("Closing resources...\n");

    /* Destroy the thread pool */ 
    destroy_thread_pool(thread_pool, pool_size);

    if (pthread_mutex_destroy(&mutex_log) != 0)
        fprintf(stderr, "Error, destroying the mutex_log\n");
    if (pthread_mutex_destroy(&mutex_job_que) != 0)
        fprintf(stderr, "Error, destroying the mutex_job_que\n");
    if (pthread_cond_destroy(&cond_job_que) != 0)
        fprintf(stderr, "Error, destroying the cond_job_que\n");

    /* Close server directory */
    if (closedir(server_dir) == -1)
        perror("closedir");

    if (close(log_fd) == -1)
        perror(log_file);

    /* Close the server FIFO */  
    if (close(server_fd) == -1)
        perror("close server_fd");

    if (unlink(server_fifo) == -1)
        perror("unlink server_fifo");

    if (close(dummy_fd) == -1)
        perror("close dummy_fd");
    
    queue_destroy(wait_que);

    queue_destroy(job_que);

    list_destroy(&serving_list);

    sfile_list_destroy(&dir_files);

    printf("Exit\n");
    return 0;
}

pthread_t *create_thread_pool(int size, void *thread_func(void *), void *args)
{
    int i, status;
    pthread_t *threads;

    threads = malloc(sizeof(pthread_t) * size);
    if (threads != NULL) {
        for (i = 0, status = 0; i < size && status == 0; ++i) {
            status = pthread_create(threads + i, NULL, thread_func, args);
        }

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
        pthread_join(threads[i], &ret);
        printf("Thread TID %ld is terminated with return value %ld\n", threads[i], (long) ret);
    }
    free(threads);
}

void *thread_function(void *args)
{
    struct thread_workspace *workspace;
    struct thread_job *job;

    workspace = (struct thread_workspace *) args;
    
    pthread_mutex_lock(&mutex_job_que);
    while (1) {
        if ((job = dequeue(&job_que)) == NULL) {
            if (server_shut_down) {
                pthread_mutex_unlock(&mutex_job_que);
                break;
            }
            pthread_cond_wait(&cond_job_que, &mutex_job_que);
        }
        else {
            pthread_mutex_unlock(&mutex_job_que);
            server_request(workspace, job);
            free(job->data); 
            free(job);
            pthread_mutex_lock(&mutex_job_que);
        }
    }
    return 0;
}

void sigint_handler()
{
    sigint_flag = 1;
}

int server_request(struct thread_workspace *workspace, struct thread_job *job)
{
    int i, n, cmd_argc, status;
    char *cmd_argv[CMD_ARG_MAX];
    char err_msg[ERR_MSG_LEN]; 
    enum req_cmd rcmd;
    struct response_header resp_head;
    struct client_ref *cli_ref;
    struct client_info *cli_info;
    const char *default_err_msg = "Error, something went wrong";

    err_msg[0] = '\0';

    /* Get the necessary informations about the client */
    i = find_serving_client(&serving_list, job->header.pid);
    if (i == -1) {
        fprintf(stderr, "Error, client PID %d reference informations cannot found\n", job->header.pid);
        return -1; 
    }

    cli_ref = (struct client_ref *) serving_list.items[i];
    cli_info = &cli_ref->info;

    cmd_argc = parse_command(job->data, cmd_argv); 

    rcmd = convert_req_cmd(cmd_argv[0]);

    /* Log the request */
    log_request(workspace->log_fd, cli_info->pid, cmd_argv[0]);

    if ((int) rcmd == -1) {
        status = -1;
        snprintf(err_msg, ERR_MSG_LEN, "Command '%s' not found", cmd_argv[0]);
    }
    else if (check_cmd_argc(rcmd, cmd_argc) == 0) {
        snprintf(err_msg, ERR_MSG_LEN, "Wrong number of arguments");
        status = -1;
    }
    else {    
        status = 0;
        switch (rcmd) {
            case HELP:
                if (cmd_argc == 1) {
                    status = cmd_help(cli_ref->fd, -1);
                }
                else if (cmd_argc == 2) {
                    if ((status = n = convert_req_cmd(cmd_argv[1])) == -1)
                        snprintf(err_msg, ERR_MSG_LEN, "Command '%s' not found", cmd_argv[1]);
                    else
                        status = cmd_help(cli_ref->fd, n);
                }
                break;
            case LIST:
                status = cmd_list(cli_ref->fd, workspace->server_dir);
                break;
            case READ_F:
                if (cmd_argc == 2) {
                    status = cmd_readF(cli_ref->fd, workspace->server_dir_path, cmd_argv[1], -1, err_msg);
                }
                else if (cmd_argc == 3) {
                    n = str_to_int(cmd_argv[2], &status);
                    if (status == -1 || n < 1) {
                        status = -1;
                        snprintf(err_msg, ERR_MSG_LEN, "Please provide a positive integer for line #");
                    }
                    else {
                        status = cmd_readF(cli_ref->fd, workspace->server_dir_path, cmd_argv[1], n, err_msg);
                    }
                }
                break;
            case WRITE_T:
                if (cmd_argc == 3) {
                    status = cmd_writeT(cli_ref->fd, workspace->server_dir_path, cmd_argv[1], cmd_argv[2], -1, err_msg);
                }
                else if (cmd_argc == 4) {
                    n = str_to_int(cmd_argv[2], &status);
                    if (status == -1 || n < 1) {
                        status = -1;
                        snprintf(err_msg, ERR_MSG_LEN, "Please provide a positive integer for line #");
                    }
                    else {
                        status = cmd_writeT(cli_ref->fd, workspace->server_dir_path, cmd_argv[1], cmd_argv[3], n, err_msg);
                    }
                }
                break;
            case UPLOAD:
                /* Pass sv_write as 1 to have write access on the server file */
                status = cmd_copy(cli_ref->fd, cli_info->cwd, cmd_argv[1], workspace->server_dir_path, 1, err_msg);
                break;
            case DOWNLOAD:
                /* Pass sv_write as 0 to have read access on the server file */
                status = cmd_copy(cli_ref->fd, workspace->server_dir_path, cmd_argv[1], cli_info->cwd, 0, err_msg);
                break;
            default:
                break;
        }
   }

    /* If there is something wrong with the request, write an error message */
    if (status == -1) {
        resp_head_set(&resp_head, RESP_ERROR, strlen(err_msg) + 1);
        if (write_response(cli_ref->fd, &resp_head, err_msg[0] == '\0' ? default_err_msg : err_msg) == -1)
            fprintf(stderr, "Error writing to FIFO for RESP_ERROR\n");
    }

    /* Log the response */
    log_response(workspace->log_fd, cli_info->pid, cmd_argv[0], status);
    
    return 0;
}

int find_serving_client(struct list *li, pid_t pid)
{
    int i;
    struct client_ref **clients;
    
    clients = (struct client_ref **) li->items; 

    for (i = 0; i < li->size; ++i)
        if (clients[i]->info.pid == pid)
            return i;
    return -1;
}


int cmd_help(int client_fd, enum req_cmd cmd)
{
    struct response_header head;
    char data[BUFF_SIZE];

    help_usage(cmd, data);
    resp_head_set(&head, RESP_OK, strlen(data) + 1);
    return write_response(client_fd, &head, data);
}

void help_usage(enum req_cmd cmd, char *str) 
{
    switch (cmd) {
        case HELP:
            strcpy(str, "help <request>\n"
                "\texplain the given request if there is no specific request\n"
                "\tprovided then display the list of possible client requests");
            break;
        case LIST:
            strcpy(str, "list\n"
                "\tdisplay the list of files in Servers directory");
            break;
        case READ_F:
            strcpy(str, "readF <file> <line #>\n"
                "\tdisplay the # line of the <file>, if no line number is given, whole\n"
                "\tcontents of the file is requested (and displayed on the client side)");
            break;
        case WRITE_T:
            strcpy(str, "writeT <file> <line #> <string>\n"
                "\trequest to write the content of 'string' to the #th line the <file>, if\n"
                "\tthe line # is not given writes to the end of file. If the file does not\n"
                "\texists in Servers directory, creates and edits the file at the same time");
            break;
        case UPLOAD:
            strcpy(str, "upload <file>\n"
                "\tuploads the file from the current working directory of client to the Servers directory");
            break;
        case DOWNLOAD:
            strcpy(str, "download <file>\n"
                "\trequest to receive <file> from Servers directory to client side");
            break;
        case QUIT:
            strcpy(str, "quit\n"
                "\tSend write request to Server side log file and quits");
            break;
        case KILL_SERVER:
            strcpy(str, "killServer\n"
                "\tSends a kill request to the Server");
            break;
        default:
            strcpy(str, "Available comments are:\n"
                "\thelp, list, readF, writeT, upload, download, quit, killServer");
            break;
    }
}

int cmd_list(int client_fd, DIR *server_dir)
{
    struct response_header head;
    char data[512], *tmp; 
    struct dirent *dentry;

    /* Reset the position of the directory stream server_dir to the beginning of the directory */
    rewinddir(server_dir);

    /* Take guard for the empty directory */
    data[0] = '\0';
    tmp = data;
    while ((dentry = readdir(server_dir)) != NULL) {
        /* Skip the hidden files */
        if (strcmp(dentry->d_name, ".") != 0 && strcmp(dentry->d_name, "..") != 0) {
            sprintf(tmp, "%s\n", dentry->d_name);
            tmp += strlen(tmp);
        }
    }
    resp_head_set(&head, RESP_OK, strlen(data) + 1);

    /* Remove the last new line */
    data[head.data_size - 1] = '\0';

    return write_response(client_fd, &head, data);
}

int cmd_copy(int client_fd, const char *src_dir_path, const char *src_file, const char* dest_dir_path, int sv_write, char *err_msg)
{
    struct response_header head;
    char dest_path[BUFF_SIZE], data[BUFF_SIZE], *fname_copy;
    int src_fd, dest_fd; 
    int i, open_flags;
    long bytes_transferred;
    mode_t file_perms;
    struct safe_file *sfile;

    if ((src_fd = open_dir_file(src_dir_path, src_file, O_RDONLY)) == -1) {
        snprintf(err_msg, ERR_MSG_LEN, "Error, no such file %s", src_file);
        return -1;
    }

    snprintf(dest_path, BUFF_SIZE, "%s/%s", dest_dir_path, src_file);

    open_flags = O_CREAT | O_WRONLY | O_EXCL;
    file_perms = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP |
                 S_IROTH | S_IWOTH; /* rw-rw-rw- */

    if ((dest_fd = open(dest_path, open_flags, file_perms)) == -1 ) {
        if (errno != EEXIST) {
            snprintf(err_msg, ERR_MSG_LEN, "Error, cannot open the file %s", src_file);
            return -1;
        }
            
        fname_copy = malloc(strlen(src_file) + 1);
        if (fname_copy == NULL)
            return -1;
        i = 2;
        while ((dest_fd = open(dest_path, open_flags, file_perms)) == -1) {
            strcpy(fname_copy, src_file);
        
            /* change the file name */
            fname_version(fname_copy, data, i);
            snprintf(dest_path, 2 * BUFF_SIZE, "%s/%s", dest_dir_path, data);
            ++i;
        }
    } 

    if ((sfile = sfile_get(&dir_files, src_file)) == NULL && (sfile = sfile_add(&dir_files, src_file)) == NULL)
        return -1; 

    if ((bytes_transferred = copy_file(src_fd, dest_fd, sfile, sv_write)) == -1)
        return -1;

    snprintf(data, BUFF_SIZE, "%ld bytes transferred", bytes_transferred);

    resp_head_set(&head, RESP_OK, strlen(data) + 1);

    /* Write success response to the server */
    if (write_response(client_fd, &head, data) == -1) {
        fprintf(stderr, "Error writing cmd_copy response to FIFO\n");
        return -1;
    }

    if (close(src_fd) == -1) 
        perror("src_fd close");

    if (close(dest_fd) == -1) 
        perror("dest_fd close");

    return 0;
}

long copy_file(int src_fd, int dest_fd, struct safe_file *sfile, int sv_write)
{
    int num_read, num_write, status;
    long bytes_transferred;
    char buff[BUFF_SIZE];

    if ((sv_write ? writer_enter_region(sfile) : reader_enter_region(sfile)) != 0) 
        return -1;

    status = 0;
    bytes_transferred = 0;
    /* Transfer data until we encounter end of input or an error */
    while ((num_read = read(src_fd, buff, BUFF_SIZE)) > 0) {
        if ((num_write = write(dest_fd, buff, num_read)) != num_read) {
            fprintf(stderr, "Couldn't write whole buffer");
            status = -1;
            break;
        }
        bytes_transferred += num_write;
    }

    if ((sv_write ? writer_exit_region(sfile) : reader_exit_region(sfile)) != 0) 
        return -1;

    return status == -1 ? -1 : bytes_transferred;
}

int cmd_readF(int client_fd, const char *server_dir_path, const char *src_file, int line_no, char *err_msg)
{
    int src_fd, status, num_read, cur_line;
    struct response_header head;
    char buff[BUFF_SIZE];
    char *line;
    struct stat file_stat;
    struct safe_file *sfile;

    if ((src_fd = open_dir_file(server_dir_path, src_file, O_RDONLY)) == -1) {
        snprintf(err_msg, ERR_MSG_LEN, "Error, no such file %s", src_file);
        return -1;
    }

    if (fstat(src_fd, &file_stat) == -1)
        return -1;

    if ((sfile = sfile_get(&dir_files, src_file)) == NULL && (sfile = sfile_add(&dir_files, src_file)) == NULL)
        return -1; 

    if (reader_enter_region(sfile) != 0)
        return -1;

    /* Read operations */
    if (line_no > 0) {
        if ((cur_line = seek_line(src_fd, line_no)) < line_no) {
            if (close(src_fd) == -1)
                perror("cmd_readF close");
            snprintf(err_msg, ERR_MSG_LEN, "Total number of line %d was exceed", cur_line);
            status = -1;
        }
        else if ((line = read_next_line(src_fd)) == NULL) {
            if (close(src_fd) == -1)
                perror("cmd_readF close");
            snprintf(err_msg, ERR_MSG_LEN, "Total number of line %d was exceed", line_no);
            status = -1;
        }
        else {
            head.stat = RESP_OK;
            head.data_size = strlen(line) + 1;
            status = write_response(client_fd, &head, line);
            free(line);
        }
    }
    else {
        /* Transfer data until we encounter end of input or an error */
        num_read = 0;
        while ((head.data_size = read(src_fd, buff, BUFF_SIZE)) > 0) {
            num_read += head.data_size;
            head.stat = (num_read < file_stat.st_size) ?  RESP_CONT : RESP_OK;
            if (write_response(client_fd, &head, buff) == -1) {
                status = -1;
                break;
            }
            else if (head.stat == RESP_OK)
                break;
        }
    }

    if (reader_exit_region(sfile) != 0)
        return -1;

    if (close(src_fd) == -1)
        perror("cmd_readF close");

    return status;
}

int seek_line(int fd, int line_no)
{
    int status;
    int line_count;
    char c;

    if (line_no < 0)
        return -1;

    lseek(fd, 0, SEEK_SET);
    line_count = 1;

    if (line_no == 1)
        return 1;
    
    while ((status = read(fd, &c, 1)) == 1) {
        if (c == '\n') {
            ++line_count;
            if (line_count == line_no)
                break;
        }
    }

    return line_count;
}

char *read_next_line(int fd)
{
    int line_size, num_read, status;
    char *line;

    line_size = num_read = 0;
    line = NULL; /* Realloc works same as malloc when the given ptr is NULL */
    do {
        if (num_read == line_size) {
            line_size = (line_size == 0 ? BUFF_SIZE : line_size * 2);
            if ((line = realloc(line, line_size)) == NULL)
                return NULL;
        }

        status = read(fd, line + num_read, 1);
        if (status == -1) {
            free(line);
            return NULL;
        }
        else if (status == 0) /* EOF */
            break;
        ++num_read;
    } while (line[num_read - 1] != '\n'); 

    line[num_read] = '\0';
    return line;
}

int cmd_writeT(int client_fd,  const char *server_dir_path, const char *src_file, const char *str, int line_no, char *err_msg)
{
    int src_fd, flags, status, cur_line, i, n;
    long bytes_transferred;
    struct response_header head;
    struct safe_file *sfile;
    char data[BUFF_SIZE];

    /* Open the file with read and write permission so that the desired position on the file can be modified */
    flags = (line_no > 0) ? (O_RDWR) : (O_RDWR | O_APPEND);

    if ((src_fd = open_dir_file(server_dir_path, src_file, flags)) == -1) {
        snprintf(err_msg, ERR_MSG_LEN, "Error, no such file %s", src_file);
        return -1;
    }

    if ((sfile = sfile_get(&dir_files, src_file)) == NULL && (sfile = sfile_add(&dir_files, src_file)) == NULL)
        return -1; 

    if (writer_enter_region(sfile) != 0)
        return -1;
    if (line_no > 0) {
        cur_line = seek_line(src_fd, line_no);      
        n = line_no - cur_line;

        /* If the total number of line is less that line_no, then append the necessary lines */
        if (n > 0) {
            for (i = 0; i < n; ++i)
                data[i] = '\n';
            if ((bytes_transferred = write(src_fd, data, n)) == -1) {
                status = -1;
            }
        }       
    }

    if ((bytes_transferred = write(src_fd, str, strlen(str))) == -1)
        status = -1;

    if (writer_exit_region(sfile) != 0)
        return -1;

    snprintf(data, BUFF_SIZE, "%ld byte(s) written to file %s", bytes_transferred, src_file);
    resp_head_set(&head, RESP_OK, strlen(data) + 1);
    if (write_response(client_fd, &head, data))
        status = -1;

    status = 0;

    if (close(src_fd) == -1)
        perror("cmd_writeT close");

    return status;
}

int open_dir_file(const char *dir_path, const char *file, int flags)
{
    char file_path[BUFF_SIZE];

    snprintf(file_path, BUFF_SIZE, "%s/%s", dir_path, file);
    return open(file_path, flags);
}

int establish_connection(struct list *serving_list, struct client_ref *cli_ref)
{
    struct response_header head;
    int turn;
    
    /* Put client into serving list */
    if (list_add(serving_list, cli_ref) == NULL)
        return -1;

    turn = 0;
    /* Notify client that the connection is established */
    head.stat = RESP_OK;
    head.data_size = sizeof(int);
    if (write_response(cli_ref->fd, &head, &turn) == -1)
        return -1;

    return 0;
}

int write_response(int client_fd, struct response_header *header, const void *data)
{
    /* Write the header */
    if (write(client_fd, header, sizeof(struct response_header)) != (long) sizeof(struct response_header)) {
        fprintf(stderr, "Error writing response header to FIFO\n");
        return -1;
    }

    if (data != NULL) {
        /* Write the body */
        if (write(client_fd, data, header->data_size) != (long) header->data_size) {
            fprintf(stderr, "Error writing response body to FIFO\n");
            return -1;
        }
    }
    return 0;
}

int log_request(int log_fd, pid_t client_pid, const char *cmd)
{
    char log[LOG_LEN];

    snprintf(log, LOG_LEN, "[%-20s] : %-8s : %-5d : %-10s\n", get_time(), "REQUEST", client_pid, cmd);
    return write_log(log_fd, log);
}

int log_response(int log_fd, pid_t client_pid, const char *cmd, int status)
{
    char log[LOG_LEN];

    snprintf(log, LOG_LEN, "[%-20s] : %-8s : %-5d : %-10s : %-5s\n",
        get_time(), "RESPONSE", client_pid, cmd, (status == -1) ? "ERROR" : "OK");
    return write_log(log_fd, log);
}

int log_info(int log_fd, pid_t client_pid, const char *info)
{
    char log[LOG_LEN];

    snprintf(log, LOG_LEN, "[%-20s] : %-8s : %-5d : %s\n", get_time(), "INFO", client_pid, info);
    return write_log(log_fd, log);
}

int write_log(int log_fd, const char *log)
{
    int status;
    
    if (pthread_mutex_lock(&mutex_job_que) != 0)
        return -1;
    status = write(log_fd, log, strlen(log));
    if (pthread_mutex_unlock(&mutex_job_que) != 0)
        return -1;
    return status;
}

char *get_time() 
{
    time_t t;
    char *str_t;
    time(&t);
    str_t = ctime(&t);
    /* Trim the new line charachter */
    str_t[strlen(str_t) - 1] = '\0';
    return str_t;
}

struct thread_job *create_thread_job(const struct request_header *header, char *data)
{
    struct thread_job *job;

    job = (struct thread_job *) malloc(sizeof(struct thread_job));
    if (job != NULL) {
        job->header = *header;
        job->data = data;
    }

    return job;
}

struct client_ref *create_client_ref(int cid, int fd, const struct client_info *info)
{
    struct client_ref *client;

    client = (struct client_ref *) malloc(sizeof(struct client_ref));
    if (client != NULL) {
        client->cid = cid;
        client->fd = fd;
        client->info = *info;
    }
    return client;
}

int check_cmd_argc(enum req_cmd cmd, int argc)
{
    switch (cmd) {
        case HELP:
            return argc == 1 || argc == 2;  
        case LIST:
            return argc == 1;  
        case READ_F:
            return argc == 2 || argc == 3;  
        case WRITE_T:
            return argc == 3 || argc == 4;  
        case UPLOAD:
            return argc == 2;  
        case DOWNLOAD:
            return argc == 2;  
        case QUIT:
            return argc == 1;  
        case KILL_SERVER:
            return argc == 1;  
        default:
            return argc == -1;  
    }
}

int check_args(int argc, char *argv[], int *max_clients, int *pool_size)
{
    int status;
    if (argc != 4)
        return -1;

    /* Max # of clients */
    *max_clients = str_to_int(argv[2], &status);

    /* Thread pool size */
    if (status == 0)
        *pool_size = str_to_int(argv[3], &status);

    return status;
}

void usage_error(char *pname)
{
    fprintf(stderr, "Right usage: %s <dirname> <max. #ofClients> <poolSize>\n", pname);
}