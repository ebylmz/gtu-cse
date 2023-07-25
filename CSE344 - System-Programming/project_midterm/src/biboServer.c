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
#include "biboServer.h"
#include "common.h"
#include "sync.h"

sem_t mutex_log;

struct pid_list child_pids;

struct safe_dir *sdir;

char err_msg[ERR_MSG_LEN];

volatile sig_atomic_t sigchld_flag = 0;
volatile sig_atomic_t sigint_flag = 0;

int main(int argc, char *argv[])
{
    int server_fd, dummy_fd, client_fd, shm_fd, log_fd;
    int shm_size, stat, client_max, client_id;
    int num_read, client_turn;
    char server_fifo[REG_SERVER_FIFO_NAME_LEN], client_fifo[CLIENT_FIFO_NAME_LEN], log_file[LOG_FILE_LEN];
    char *server_dir_path;
    DIR *server_dir;
    struct sigaction sa_action;
    struct queue wait_que;
    struct client_info new_client, *cli_info;

    /* Handle arguments */
    if (check_args(argc, argv, &client_max) == -1) {
        usage_error(argv[0]);
        exit(EXIT_FAILURE);
    }

    /* Handler for SIGINT signal */
    sa_action.sa_flags = SA_SIGINFO | SA_RESTART;
    sa_action.sa_handler = sigint_handler;
    if (sigemptyset(&sa_action.sa_mask) == -1 ||
        sigaction(SIGINT, &sa_action, NULL) == -1)
        perror("sa_action");

    sa_action.sa_handler = sigchld_handler;
    if (sigaction(SIGCHLD, &sa_action, NULL) == -1)
        perror("sa_action");

    /* Initialize size child_pids with the number of max clients */
    if (init_pid_list(&child_pids, client_max) == -1) 
        err_exit("init_pid_list");

    /* Initialize the clients wait queue */
    init_queue(&wait_que);

    /* Create the server folder if it doesn't exist */
    server_dir_path = argv[1];
    if ((stat = mkdir(server_dir_path, S_IRWXU | S_IWUSR | S_IRUSR | S_IXUSR | S_IWGRP | S_IRGRP)) == -1 && errno != EEXIST)
        err_exit("mkdir");

    /* Open server folder */
    if ((server_dir = opendir(argv[1])) == NULL)
        err_exit("opendir");

    /* Create a shared memory segment */
    if ((shm_fd = shm_open(SHM_SEM, O_CREAT | O_RDWR, 0666)) == -1)
        err_exit("Error creating shared memory segment");

    /* Set the size of the shared memory segment */
    shm_size = sizeof(int) * 2 + sizeof(struct safe_file) * NUM_OF_DIR_FILE;

    if (ftruncate(shm_fd, shm_size) == -1)
        err_exit("Error setting size of shared memory segment");

    /* Map the shared memory segment to the process's address space */
    if ((sdir = mmap(NULL, shm_size, PROT_READ | PROT_WRITE, MAP_SHARED, shm_fd, 0)) == MAP_FAILED)
        err_exit("Error mapping shared memory segment");

    if (init_sdir(server_dir, sdir) == -1)
        exit(EXIT_FAILURE);

    /* Create the log file */
    snprintf(log_file, LOG_FILE_LEN, LOG_FILE_TEMPLATE, getpid());

    log_fd = open(log_file, O_RDWR | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
    if (log_fd == -1)
        err_exit(log_file);

    /* To prevent race condition use mutex during the access of log file */
    if (sem_init(&mutex_log, 1, 1) == -1)
        err_exit("sem_init mutex_log");

    /* Create well-known FIFO, and open it for reading */
    snprintf(server_fifo, REG_SERVER_FIFO_NAME_LEN, REG_SERVER_FIFO_TEMPLATE, getpid());

    if (mkfifo(server_fifo, S_IRUSR | S_IWUSR | S_IWGRP) == -1 && errno != EEXIST)
        err_exit("mkfifo");

    printf("Server Started PID %d\n", getpid());
    printf("Waiting for clients...\n");

    /* open() blocks until the first client opens the other end of the server FIFO for writing */
    /* Use O_NONBLOCK flag not to be blocked while reading the server FIFO */
    if ((server_fd = open(server_fifo, O_RDONLY | O_NONBLOCK)) == -1)
        err_exit("open server_fifo");

    /* Open an extra write descriptor, so that we never see EOF */
    if ((dummy_fd = open(server_fifo, O_WRONLY)) == -1)
        err_exit("open server_fifo for EOF");

    client_id = 0;
    while (1) {
        /* Get the new connection request */
        num_read = read(server_fd, &new_client, sizeof(struct client_info)); 
        
        /* read never returns EOF (0) since there is at least one writer (dummy), so num_read never becomes 0 */
        if (num_read != -1) {
            if (num_read < (long) sizeof(struct client_info)) {
                fprintf(stderr, "Error reading client info; discarding\n");
                continue;
            }
            else {
                snprintf(client_fifo, CLIENT_FIFO_NAME_LEN, CLIENT_FIFO_TEMPLATE, new_client.pid);

                /* Open client FIFO (previouly created by client) */
                if ((client_fd = open(client_fifo, O_WRONLY)) == -1)
                    err_exit("open client_fifo");

                /* Check if there is enough resource for serving the incoming child immediately */
                client_turn = (child_pids.size < child_pids.capacity && wait_que.size == 0) ? 0 : child_pids.capacity + wait_que.size; 

                /* Inform client whether it's need to wait or not */
                if (write(client_fd, &client_turn, sizeof(int)) != (long) sizeof(int))
                    err_exit("open client_fifo");

                if (client_turn == 0) {
                    if ((cli_info = create_client_info(new_client.pid, new_client.wait, new_client.cwd)) == NULL)
                        err_exit("create_client_info");
                    if (spawn_child_server(server_dir, server_dir_path, cli_info, &client_id, log_fd))
                        err_exit("spawn_child_server");
                }
                else {
                    /* Server cannot give the resource to client currently */
                    printf("Connection request PID %d. Queue is FULL.\n", new_client.pid);
                    if (new_client.wait) {
                        /* Put client into wait queue if it connects with wait option */
                        if (enqueue(&wait_que, &new_client) == -1)
                            perror("enqueue incoming client");
                        printf("Client PID %d waits...\n", new_client.pid);
                        log_info(log_fd, new_client.pid, "Connection request. Queue is FULL. Client waits");
                    }
                    else {
                        printf("Client PID %d does not wait...\n", new_client.pid);
                        log_info(log_fd, new_client.pid, "Connection request. Queue is FULL. Client gives up");
                    }
                } 
            }
        }

        if (sigint_flag) {
            kill_childs();
            log_info(log_fd, 0, "exit");
            break;
        }

        if (sigchld_flag) {
            sigchld_flag = 0; //! might be dangerous for synch
            remove_child();
            /* check if there is a wating client */
            if (wait_que.size > 0) {
                /* Get the client from the queue */
                if ((cli_info = dequeue(&wait_que)) == NULL)
                    err_exit("Cannot dequeue client");
                printf("Client PID %d taken from the queue.\n", cli_info->pid);
                if (spawn_child_server(server_dir, server_dir_path, cli_info, &client_id, log_fd))
                    err_exit("spawn_child_server");
            }   
        }
    }

    printf("\nClosing resources...\n");

    /* Close server directory */
    if (closedir(server_dir) == -1)
        err_exit("closedir");

    /* Remove the shared memory */
    if (shm_unlink(SHM_SEM) == -1)
        perror("shm_unlink");

    if (close(log_fd) == -1)
        perror(log_file);

    /* Close the server FIFO */  
    if (close(server_fd) == -1)
        perror("close server_fd");

    if (unlink(server_fifo) == -1)
        perror("unlink server_fifo");

    if (close(dummy_fd) == -1)
        perror("close dummy_fd");
    
    
    destroy_queue(&wait_que);

    destroy_pid_list(&child_pids);

    printf("Exit\n");
    return 0;
}

int spawn_child_server(DIR *server_dir, const char *server_dir_path, struct client_info *cli_info, int *client_id, int log_fd)
{
    pid_t pid;
    struct sigaction sa_sigint;

    pid = fork();
    if (pid == -1) {
        return -1;
    }
    else if (pid == 0) {
        /* Ignore SIGINT in the child process */
        sa_sigint.sa_flags = SA_SIGINFO | SA_RESTART;
        sa_sigint.sa_handler = SIG_IGN;
        if (sigaction(SIGINT, &sa_sigint, NULL) == -1)
            err_exit("sa_default");

        log_info(log_fd, cli_info->pid, "Client connected");
        printf("Client PID %d connected as 'client%d'\n", cli_info->pid, *client_id);

        server_request(server_dir, server_dir_path, cli_info, *client_id, log_fd);

        printf("client%d disconnected\n", *client_id);
        log_info(log_fd, cli_info->pid, "Client disconnected");
        
        free(cli_info);
        _exit(EXIT_SUCCESS);
    }

    /* Parent loops to receive next client request */
    add_pid_list(&child_pids, pid);
    *client_id += 1;
    return 0;
}

void sigint_handler()
{
    sigint_flag = 1;
}

void sigchld_handler()
{
    sigchld_flag = 1;
}

void remove_child()
{
    pid_t pid;
    int saved_errno;
    saved_errno = errno;

    //! our server shouldn't block
    while ((pid = waitpid(-1, NULL, WNOHANG)) > 0) {
        if (remove_pid_list(&child_pids, pid) == -1)
            printf("Child PID %d cannot be removed\n", pid);
        else
            printf("Child PID %d removed\n", pid);
    }
    errno = saved_errno;
}

void kill_childs()
{
    int i, status;
    pid_t pid;

    /*  Send SIGTERM signal to all child processes */
    for (i = 0; i < child_pids.size; ++i)
        kill(child_pids.pids[i], SIGTERM);

    printf("\n");
    /*  Wait for all child processes to exit */
    while ((pid = wait(&status)) > 0) {
        printf("Child process with PID %d exited with status %d\n", pid, status);
    }
}

void server_request(DIR *server_dir, const char *server_dir_path, struct client_info *cli_info, int client_id, int log_fd)
{
    int server_fd, client_fd;
    int cmd_argc, status, alive, n, num_read;
    char *cmd_argv[CMD_ARG_MAX];
    char server_fifo[REQ_SERVER_FIFO_NAME_LEN], client_fifo[CLIENT_FIFO_NAME_LEN];
    char cmd[CMD_LEN];
    enum req_cmd rcmd;
    struct request_header req;
    struct response_header resp;

    snprintf(server_fifo, REQ_SERVER_FIFO_NAME_LEN, REQ_SERVER_FIFO_TEMPLATE, cli_info->pid);
    snprintf(client_fifo, CLIENT_FIFO_NAME_LEN, CLIENT_FIFO_TEMPLATE, cli_info->pid);

    /* Open client FIFO (previouly created by client) */
    if ((client_fd = open(client_fifo, O_WRONLY)) == -1)
        err_exit("open client_fifo");

    /* Open server FIFO (previouly created by client) */
    if ((server_fd = open(server_fifo, O_RDONLY)) == -1)
        err_exit("open server_fifo");

    alive = 1;
    do {
        /* Read the request header */
        num_read = read(server_fd, &req, sizeof(struct request_header));

        if (num_read == 0) {
            printf("Server-Client fifo closed\n");
            break;
        }
        else if (num_read < (long) sizeof(struct request_header)) {
            fprintf(stderr, "Error reading request header; discarding\n");
            continue;
        }

        /* Read the request body */
        if (read(server_fd, cmd, req.data_size) < (long) req.data_size) {
            fprintf(stderr, "Error reading request cmd; discarding\n");
            continue;
        }

        cmd_argc = parse_command(cmd, cmd_argv); //! be careful with the "string quotations"

        /* Log the incoming request */
        log_request(log_fd, cli_info->pid, cmd);

        rcmd = convert_req_cmd(cmd_argv[0]);

        if ((int) rcmd == -1) {
            status = -1;
            sprintf(err_msg, "Command '%s' not found", cmd_argv[0]);
        }
        else if (check_cmd_argc(rcmd, cmd_argc) == 0) {
            status = -1;
            sprintf(err_msg, "Wrong number of arguments");
        }
        else {
            status = 0;
            switch (rcmd) {
                case HELP:
                    if (cmd_argc == 1) {
                        status = cmd_help(client_fd, -1);
                    }
                    else if (cmd_argc == 2) {
                        if ((status = n = convert_req_cmd(cmd_argv[1])) == -1)
                            sprintf(err_msg, "Command '%s' not found", cmd_argv[1]);
                        else
                            status = cmd_help(client_fd, n);
                    }
                    break;
                case LIST:
                    //! do we have race condition, when a file is loading you are trying to read
                    if ((status = cmd_list(client_fd, server_dir)) == -1)
                        sprintf(err_msg, "Something went wrong");
                    break;
                case READ_F:
                    if (cmd_argc == 2) {
                        status = cmd_readF(client_fd, server_dir_path, cmd_argv[1], -1);
                    }
                    else if (cmd_argc == 3) {
                        n = str_to_int(cmd_argv[2], &status);
                        if (status == -1 || n < 1) {
                            status = -1;
                            sprintf(err_msg, "Please provide a positive integer for line #");
                        }
                        else {
                            status = cmd_readF(client_fd, server_dir_path, cmd_argv[1], n);
                        }
                    }
                    break;
                case WRITE_T:
                    if (cmd_argc == 3) {
                        status = cmd_writeT(client_fd, server_dir_path, cmd_argv[1], cmd_argv[2], -1);
                    }
                    else if (cmd_argc == 4) {
                        n = str_to_int(cmd_argv[2], &status);
                        if (status == -1 || n < 1) {
                            status = -1;
                            sprintf(err_msg, "Please provide a positive integer for line #");
                        }
                        else {
                            status = cmd_writeT(client_fd, server_dir_path, cmd_argv[1], cmd_argv[3], n);
                        }
                    }
                    break;
                case UPLOAD:
                    /* Pass sv_write as 1 to have write access on the server file */
                    status = cmd_copy(server_fd, client_fd, cli_info->cwd, cmd_argv[1], server_dir_path, 1);
                    break;
                case DOWNLOAD:
                    /* Pass sv_write as 0 to have read access on the server file */
                    status = cmd_copy(server_fd, client_fd, server_dir_path, cmd_argv[1], cli_info->cwd, 0);
                    break;
                case QUIT:
                    if ((status = cmd_quit(client_fd)) == -1) {
                        sprintf(err_msg, "Something went wrong");
                        alive = 1;
                    }
                    else
                        alive = 0;
                    break;
                case KILL_SERVER:
                    if ((status = cmd_kill_server(client_fd, client_id)) == -1) {
                        sprintf(err_msg, "Something went wrong");
                        alive = 1;
                    }
                    else
                        alive = 0;
                    break;
            }
        }

        /* If there is something wrong with the request, send error response to the client */
        if (status == -1) {
            // fprintf(stderr, "%s\n", err_msg); 
            resp.stat = RESP_ERROR;
            resp.data_size = strlen(err_msg) + 1;
            if (write_response(client_fd, &resp, err_msg) == -1)
                fprintf(stderr, "Error writing to FIFO for RESP_ERROR\n");
        }
        /* Log the response */
        log_response(log_fd, cli_info->pid, cmd_argv[0], status, NULL);
    } while (alive);

    if (close(server_fd) == -1)
        perror("close server_fd");

    if (close(client_fd) == -1)
        perror("close client_fd");
}

int cmd_help(int client_fd, enum req_cmd cmd)
{
    struct response_header resp;
    char data[BUFF_SIZE];

    help_usage(cmd, data);

    resp.stat = RESP_OK;
    resp.data_size = strlen(data) + 1;

    /* Write the header */
    if (write(client_fd, &resp, sizeof(struct response_header)) != (long) sizeof(struct response_header)) {
        fprintf(stderr, "Error writing cmd_help header to FIFO\n");
        return -1;
    }
    /* Write the body */
    if (write(client_fd, data, resp.data_size) != (long) resp.data_size) {
        fprintf(stderr, "Error writing cmd_help body to FIFO\n");
        return -1;
    }
    return 0;
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
    struct response_header resp;
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

    resp.stat = RESP_OK;
    resp.data_size = strlen(data);

    /* Remove the last new line */
    data[resp.data_size - 1] = '\0';

    /* Write the header */
    if (write(client_fd, &resp, sizeof(struct response_header)) != (long) sizeof(struct response_header)) {
        fprintf(stderr, "Error writing cmd_list header to FIFO\n");
        return -1;
    }
    /* Write the body */
    if (write(client_fd, data, resp.data_size) != (long) resp.data_size) {
        fprintf(stderr, "Error writing cmd_list body to FIFO\n");
        return -1;
    }
    return 0;
}

int cmd_copy(int server_fd, int client_fd, const char *src_dir_path, const char *src_file, const char* dest_dir_path, int sv_write)
{
    struct response_header header;
    char dest_path[BUFF_SIZE], data[BUFF_SIZE];
    int src_fd, dest_fd; 
    int overwrite, open_flags;
    long bytes_transferred;
    mode_t file_perms;
    struct safe_file *sfile;

    if ((src_fd = open_dir_file(src_dir_path, src_file, O_RDONLY)) == -1)
        return -1;

    snprintf(dest_path, BUFF_SIZE, "%s/%s", dest_dir_path, src_file);

    open_flags = O_CREAT | O_WRONLY | O_EXCL;
    file_perms = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP |
                 S_IROTH | S_IWOTH; /* rw-rw-rw- */

    if ((dest_fd = open(dest_path, open_flags, file_perms)) == -1) {
        perror(dest_path); //! check the error
        /* Ask client whether or not she wants to overwrite the destination file */
        sprintf(err_msg, "The file %s is already exist in the destination directory. Do you want to overwrite (yes/no)? ", src_file);
        header.stat = RESP_APPROVE;
        header.data_size = strlen(err_msg) + 1;
        if (write_response(client_fd, &header, err_msg) == -1)
            return -1;

        /* 1 for yes, 0 for no */
        if (read(server_fd, &overwrite, sizeof(int)) != (long) sizeof(int))
            return -1;

        if (overwrite == 0)
            return 0;
        open_flags += O_TRUNC;
        open_flags -= O_EXCL;
        
        if ((dest_fd = open(dest_path, open_flags, file_perms)) == -1) {
            perror(dest_path); 
            return -1;
        }
    }

    if ((sfile = get_sfile(sdir, src_file)) == NULL && (sfile = add_sfile(sdir, src_file)) == NULL)
        return -1; 

    if ((bytes_transferred = copy_file(src_fd, dest_fd, sfile, sv_write)) == -1)
        return -1;

    sprintf(data, "%ld bytes transferred", bytes_transferred);

    /* Write success response to the server */
    header.stat = RESP_OK;
    header.data_size = strlen(data) + 1;
    if (write_response(client_fd, &header, data) == -1) {
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

    if ((sv_write ? writer_enter_region(sfile) : reader_enter_region(sfile)) == -1) 
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

    if ((sv_write ? writer_exit_region(sfile) : reader_exit_region(sfile)) == -1) 
        return -1;

    return status == -1 ? -1 : bytes_transferred;
}

int cmd_readF(int client_fd,  const char *server_dir_path, const char *src_file, int line_no)
{
    int src_fd, status, num_read;
    struct response_header header;
    char buff[BUFF_SIZE];
    char *line;
    struct stat file_stat;
    struct safe_file *sfile;

    if ((src_fd = open_dir_file(server_dir_path, src_file, O_RDONLY)) == -1)
        return -1;

    if (fstat(src_fd, &file_stat) == -1) {
        perror("fstat");
        return -1;
    }

    if ((sfile = get_sfile(sdir, src_file)) == NULL && (sfile = add_sfile(sdir, src_file)) == NULL)
        return -1; 

    if (reader_enter_region(sfile) == -1)
        return -1;

    /* Read operations */
    if (line_no > 0) {
        if (seek_line(src_fd, line_no) == -1) {
            if (close(src_fd) == -1)
                perror("cmd_readF close");
            
            status = -1;
        }
        else if ((line = read_next_line(src_fd)) == NULL) {
            if (close(src_fd) == -1)
                perror("cmd_readF close");
            //! this may not required
            sprintf(err_msg, "Total number of line %d was exceed", line_no); 
            status = -1;
        }
        else {
            header.stat = RESP_OK;
            header.data_size = strlen(line) + 1;
            status = write_response(client_fd, &header, line);
            free(line);
        }
    }
    else {
        /* Transfer data until we encounter end of input or an error */
        num_read = 0;
        while ((header.data_size = read(src_fd, buff, BUFF_SIZE)) > 0) {
            num_read += header.data_size;
            header.stat = (num_read < file_stat.st_size) ?  RESP_CONT : RESP_OK;
            if (write_response(client_fd, &header, buff) == -1) {
                status = -1;
                break;
            }
            else if (header.stat == RESP_OK)
                break;
        }
    }

    if (reader_exit_region(sfile) == -1)
        return -1;

    if (close(src_fd) == -1)
        perror("cmd_readF close");

    return status;
}

int seek_line(int fd, int line_no)
{
    int num_read, status;
    int line_count;
    char c;

    if (line_no == 1)
        return 0;
    else if (line_no < 0)
        return -1;

    lseek(fd, 0, SEEK_SET);

    line_count = 1;
    for (num_read = 0; (status = read(fd, &c, 1)) == 1; ++num_read) {
        if (c == '\n') {
            ++line_count;
            if (line_count == line_no)
                break;
        }
    }

    if (status == 0) {
        sprintf(err_msg, "Total number of line %d was exceed", line_count);
        return -1;
    }
    else if (status == -1) {
        sprintf(err_msg, "Something went wrong");
        return -1;
    }
    return num_read;
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
            if ((line = realloc(line, line_size)) == NULL) {
                perror("realloc read_next_line");
                return NULL;
            }
        }

        status = read(fd, line + num_read, 1);
        if (status == -1) {
            sprintf(err_msg, "Something went wrong");
            perror("read read_next_line");
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

int cmd_writeT(int client_fd,  const char *server_dir_path, const char *src_file, const char *str, int line_no)
{
    int src_fd, flags, status;
    long bytes_transferred;
    struct response_header header;
    struct safe_file *sfile;
    char data[BUFF_SIZE];

    /* Open the file with read and write permission so that the desired position on the file can be modified */
    flags = (line_no > 0) ? (O_RDWR) : (O_RDWR | O_APPEND);

    if ((src_fd = open_dir_file(server_dir_path, src_file, flags)) == -1)
        return -1;

    if ((sfile = get_sfile(sdir, src_file)) == NULL && (sfile = add_sfile(sdir, src_file)) == NULL)
        return -1; 

    writer_enter_region(sfile);

    //TODO: user should be able to write any line so append line never return erpro
    if (line_no > 0 && seek_line(src_fd, line_no) == -1) {
        status = -1;
    }
    else {
        if ((bytes_transferred = write(src_fd, str, strlen(str))) == -1)
            status = -1;
        else {
            sprintf(data, "%ld byte(s) written to file %s", bytes_transferred, src_file);
            header.stat = RESP_OK;
            header.data_size = strlen(data) + 1;
            if (write_response(client_fd, &header, data))
                status = -1;
        }
        status = 0;
    }

    writer_exit_region(sfile);

    if (close(src_fd) == -1)
        perror("cmd_writeT close");

    return status;
}

int open_file(const char *file_path, int flags)
{
    int fd;

    if ((fd = open(file_path, flags)) == -1) {
        perror(file_path);
        sprintf(err_msg, "%s: No such file or directory", file_path);
    }
    return fd;
}

int open_dir_file(const char *dir_path, const char *file, int flags)
{
    char file_path[BUFF_SIZE];

    snprintf(file_path, BUFF_SIZE, "%s/%s", dir_path, file);
    return open_file(file_path, flags);
}

int cmd_kill_server(int client_fd, int client_id)
{
    struct response_header resp;

    resp.stat = RESP_DISCONNECT;
    resp.data_size = 0;

    printf("Kill signal from client%d. Terminating...\n", client_id);

    /* Write the header */
    if (write(client_fd, &resp, sizeof(struct response_header)) != (long) sizeof(struct response_header)) {
        fprintf(stderr, "Error writing cmd_list header to FIFO\n");
        return -1;
    }

    kill(getppid(), SIGINT);

    return 0;
}

int cmd_quit(int client_fd)
{
    struct response_header resp;

    resp.stat = RESP_DISCONNECT;
    resp.data_size = 0;

    /* Write the header */
    if (write(client_fd, &resp, sizeof(struct response_header)) != (long) sizeof(struct response_header)) {
        fprintf(stderr, "Error writing cmd_list header to FIFO\n");
        return -1;
    }

    return 0;
}

int write_response(int client_fd, struct response_header *header, const char *data)
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

    sprintf(log, "[%-20s] : %-8s : %-5d : %-10s\n", get_time(), "REQUEST", client_pid, cmd);
    return write_log(log_fd, log);
}

int log_response(int log_fd, pid_t client_pid, const char *cmd, enum resp_status status, const char *extra)
{
    char log[LOG_LEN];

    sprintf(log, "[%-20s] : %-8s : %-5d : %-10s : %s : %s\n",
        get_time(), "RESPONSE", client_pid, cmd, (status == RESP_ERROR) ? "ERROR" : "SUCCESS", (extra == NULL) ? "" : extra);
    return write_log(log_fd, log);
}

int log_info(int log_fd, pid_t client_pid, const char *info)
{
    char log[LOG_LEN];

    sprintf(log, "[%-20s] : %-8s : %-5d : %s\n", get_time(), "INFO", client_pid, info);
    return write_log(log_fd, log);
}

int write_log(int log_fd, const char *log)
{
    if (sem_wait(&mutex_log) == -1)
        return -1;
    if (write(log_fd, log, strlen(log)) == -1)
        return -1;
    if (sem_post(&mutex_log) == -1)
        return -1;
    return 0;
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

int check_args(int argc, char *argv[], int *client_max)
{
    int status;
    if (argc != 3)
        return -1;

    /* Max # of clients */
    *client_max = str_to_int(argv[2], &status);
    return status;
}

int str_to_int(const char *str, int *status) 
{
    int num;
    char *endptr;

    num = 0;
    errno = 0;
    num = strtol(str, &endptr, 10);
    *status = (*endptr != '\0' || errno != 0) ? -1 : num;
    return num;
}

void usage_error(char *pname)
{
    fprintf(stderr, "Right usage: %s <dirname> <max. #ofClients>\n", pname);
}