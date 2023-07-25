#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include "shell.h"

#define CWD_LEN 200
#define BUFF_SIZE 1024
#define NUM_PIPING 20
#define EXIT_COMMAND ":q"
#define BATCH_MODE "-b"

int main(int argc, char *argv[]) {
    int num_read, num_cmd, log_fd, prompt_flag; 
    char *log_fname;
    char buff[BUFF_SIZE];
    char *cmds[256];
    pid_t child_pid;
    struct sigaction sa_ignore, sa_handle;

    if (argc > 2 || (argc == 2 && strcmp(argv[1], BATCH_MODE) != 0)) {
        usage_error(argv[0]);
        return 1;
    }

    prompt_flag = (argc == 1);

    /* ignore SIGQUIT in the shell process to kill child when it's necessary */
    sa_ignore.sa_flags = SA_SIGINFO | SA_RESTART;
    sa_ignore.sa_handler = SIG_IGN;
    if (sigemptyset(&sa_ignore.sa_mask) == -1 || 
        sigaction(SIGQUIT, &sa_ignore, NULL) == -1)
        perror("sa_ignore");

    /* handle SIGINT and SIGTERM */
    sa_handle.sa_flags = SA_SIGINFO | SA_RESTART;
    sa_handle.sa_handler = &sig_handler;
    if (sigemptyset(&sa_handle.sa_mask) == -1 || 
        sigaction(SIGINT, &sa_handle, NULL) == -1 || 
        sigaction(SIGTERM, &sa_handle, NULL) == -1)
        perror("sa_handle");
        
    while (1) {
        if (prompt_flag)
            prompt();      

        num_read = read_command_line(buff, BUFF_SIZE);
        
        /* EOF or exit command */
        if (num_read == -1 || strcmp(buff, EXIT_COMMAND) == 0)
            break;          
        else if (buff[0] == '\n')
            continue;

        /* make sure to create unique log file for each execution */
        while (1) {
            log_fname = create_log_fname();
            log_fd = open(log_fname, O_RDWR | O_EXCL | O_CREAT , S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
            if (log_fd != -1)
                break;
            else if (errno != EEXIST)
                perror(log_fname);
        }

        switch (child_pid = fork()) {
            case -1:
                err_exit("fork spawn_exec");
                break;
            case 0:
                num_cmd = tokenize(buff, "|", cmds);
                exec_piping(cmds, num_cmd, log_fd);
                break;
            default:
                /* parent waits for the child to terminate (dont caring the termination status) */
                while (waitpid(child_pid, NULL, 0) == -1 && errno == EINTR) ;
                break;
        }

        if (close(log_fd) == -1)
            perror(log_fname);
    }
}

void sig_handler(int signum) {
    int pid;

    /*  Using system calls is not recommended in signal handler but for sake of example it's preferred that way */
    switch (signum) {
        case SIGTERM:
            fprintf(stderr, "\nHandling SIGTERM\n");    
            break;
        case SIGQUIT:
            fprintf(stderr, "\nHandling SIGQUIT\n");    
            break;
        case SIGINT:
            fprintf(stderr, "\nHandling SIGINT\n");    
                break;
        default:
            fprintf(stderr, "\nHandling SIGNUM: %d\n", signum);    
                break;
    }

    /* Send the SIGQUIT signal to all processes so that all the processes except shell process are terminated */
    kill(0, SIGQUIT);

    /* Wait for all child processes to termiate */
    while ((pid = waitpid(-1, NULL, 0)) != -1 || errno != ECHILD) {
        /* Print a message indicating that the child processes have been cleaned up */
        printf("(Child PID: %d) killed\n", pid);    
    }
}

/**
 * Takes the parsed commands based on pipe symbol, executes them sequentially
 * by handling required I/O redirections and print the executed commands into the given file fd
*/
void exec_piping(char *cmds[], int num_cmd, int log_fd) {
    int i, in, out;
    int fds[2];                     /* file descriptors */
    char  *argv[NUM_PIPING];        /* at most 20 command in pipiline execution */
    char *last_cmd; 

    in = STDIN_FILENO;
    out = STDOUT_FILENO;

    for (i = 0; i < num_cmd - 1; ++i) {
        /* create a pipe */
        if (pipe(fds) == -1)
            err_exit("pipe main");
        /* spawn a child process and execute next command with the created pipe */
        spawn_exec(in, fds[1], cmds[i], log_fd);

        /* write end of the pipe is unused */
        if (close(fds[1]) == -1)
            err_exit("close fds[1]");
        /* keep the read end of the pipe, so that next child process will read from there */
        in = fds[0];
    }

    last_cmd = calloc(strlen(cmds[num_cmd - 1]) + 1, sizeof(char));
    if (last_cmd == NULL)
        err_exit("calloc");
    strcpy(last_cmd, cmds[num_cmd - 1]);

    io_redirection(in, out, cmds[num_cmd - 1], argv);

    /* execute the last stage with the current process */
    print_log(log_fd, last_cmd);
    free(last_cmd);
    run_command(argv);    
}

/**
 * Creates a child process and executes the given command with the child process
*/
void spawn_exec(int in, int out, char *cmd, int log_fd) {
    pid_t child_pid;
    char *orig_cmd; 
    char *argv[NUM_PIPING]; 
    int status;

    switch (child_pid = fork()) {
        case -1:
            err_exit("fork spawn_exec");
            break;
        case 0:
            /* take a copy of the orignal command, because io_redirection parse the command to argv */
            orig_cmd = calloc(strlen(cmd) + 1, sizeof(char));
            if (orig_cmd == NULL)
                err_exit("calloc");
            strcpy(orig_cmd, cmd);

            /* handle the file redirection operation(s) if any */
            io_redirection(in, out, cmd, argv);

            print_log(log_fd, orig_cmd);
            free(orig_cmd);
            run_command(argv); 
            break;
        default:
            /* if the child fails, all the piping process should be failed */
            if (waitpid(child_pid, &status, 0) == -1 && errno != EINTR)
                exit(EXIT_FAILURE);

            if (!WIFEXITED(status))
                exit(EXIT_FAILURE);
            break;
    }   
}

/**
 * if the command contains I/O redirection
 * reads from the last argument of input redirection and
 * writes to the last argument of output redirection 
*/
void io_redirection(int in, int out, char *cmd, char *argv[]) {
    int i, n, fd, seen, argc;
    char redirect;
    char *in_fname, *out_fname;

    argc = 1;
    seen = 0;
    redirect = '\0';
    out_fname = in_fname = NULL;
    
    /* parse the command into arguments */
    n = tokenize(cmd, " ", argv);

    /* piping without program casues invalid argument error */
    if (argv[0][0] == '<' || argv[0][0] == '>') {
        errno = EINVAL;
        exit(EXIT_FAILURE);
    }

    for (i = 1; i < n; ++i) {
        /* identification of indirection type */
        if (argv[i][0] == '<' || argv[i][0] == '>' ) {
            /* if invalid indirection symbol used or successive indirection symbols */
            if (strlen(argv[i]) > 1 || (redirect != '\0' && seen == 0)) {
                errno = EINVAL;
                perror(argv[i]);
            }
            redirect = argv[i][0];
            seen = 0;
        }
        else {
            /* program argument or file name */
            if (redirect == '<') {
                in_fname = argv[i];
                ++seen;
            }
            else if (redirect == '>') {
                out_fname = argv[i];
                ++seen;
            }
            else {
                ++argc;
            }
        }
    }

    if (in_fname != NULL) {
        if ((fd = open(in_fname, O_RDONLY, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)) == -1)
            err_exit(in_fname); 
        in = fd;
    }

    if (out_fname != NULL) {
        if ((fd = open(out_fname, O_WRONLY | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)) == -1)
            err_exit(out_fname); 
        out = fd;
    }

    redirect_std(in, STDIN_FILENO);
    redirect_std(out, STDOUT_FILENO);

    /* null terminated list for program name and its arguments */
    argv[argc] = NULL;
}

/**
 * duplicates fd and to STDOUT or STDIN according to std_fd
*/
void redirect_std(int fd, int std_fd) {
    if (fd != std_fd) {
        if (dup2(fd, std_fd) == -1)                     
            err_exit("redirect dup2");
        /* close unused file descriptor */
        if (close(fd) == -1)                    
            err_exit("redirect close");
    }
}

void run_command(char *argv[]) {
    execvp(*argv, argv);
    /* executes these only if execvp fails */
    fprintf(stderr, "command not found: %s\n", argv[0]);
    exit(EXIT_FAILURE);
}

/**
 * removes all the unnecessary spaces, and keep only the ones for seperating the arguments
*/
void trim(char *src) {
    int i, j;
    char *tmp;

    tmp = calloc(strlen(src) + 1, sizeof(char)); 
    strcpy(tmp, src);

    for (i = j = 0; tmp[i] != '\0'; ++i) {
        if (tmp[i] != ' ' || (i > 0 && src[j - 1] != ' ')) {
            src[j] = tmp[i];
            ++j;
        }
    }
    free(tmp);

    if (j > 0 && src[j - 1] == ' ')
        src[j - 1] = '\0';
    else
        src[j] = '\0';
}

/**
 * Parses the given stream based on the given deliminator delim 
 * and put them into the result array
*/
int tokenize(char *stream, char *delim, char *result[]) {
    int n = 1;

    /* split the stream into pieces based on the deliminator */
    result[0] = strtok(stream, delim);
    trim(result[0]);
    while ((result[n] = strtok(NULL, delim)) != NULL) {
        trim(result[n]); 
        ++n;
    }
    return n;
}

int read_command_line(char buff[], int size) {
    int len;

    if (fgets(buff, size, stdin) == NULL)
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

void prompt() {
    char prompt[BUFF_SIZE];
    char cwd[CWD_LEN];
    if (getcwd(cwd, sizeof(cwd)) != NULL) {
        strcpy(prompt, "EBY:");
        strcat(prompt, cwd);
        strcat(prompt, "$ ");
        printf("%s", prompt);
    }
    else 
        perror("getcwd");
}

void print_log(int fd, char *command) {
    time_t t;    
    char log[256];
    char *curr_time;    
    
    time(&t);
    curr_time = ctime(&t);
    curr_time[strlen(curr_time) - 1] = '\0';
    sprintf(log, "%s PID: %-3d, CMD: %s\n", curr_time, getpid(), command);
    write(fd, log, strlen(log));
}

void get_time(char *t) {
    time_t raw_time;
    struct tm *time_info;

    time(&raw_time);
    time_info = localtime(&raw_time);

    sprintf(t, "%d %d %d %d %d %d", 
        time_info->tm_mday, time_info->tm_mon + 1, time_info->tm_year + 1900,
        time_info->tm_hour, time_info->tm_min, time_info->tm_sec);
}

char *create_log_fname() {
    int i;
    time_t raw_time;
    char *curr_time;

    time(&raw_time);
    curr_time = ctime(&raw_time);
    for (i = 0; curr_time[i] != '\0'; ++i)
        if (curr_time[i] == ':')
            curr_time[i] = ' ';

    /* remove the new line charachter */
    curr_time[i - 1] = '\0';
    return strcat(curr_time, ".log");
}

void usage_error(char *pname) {
    fprintf(stderr, "Right usage: %s filename or %s filename %s\n", pname, pname, BATCH_MODE);
}

void err_exit(const char *err) {
    perror(err);
    exit(EXIT_FAILURE);
}
