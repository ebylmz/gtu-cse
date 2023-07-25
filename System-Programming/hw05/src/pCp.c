#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <semaphore.h>
#include <unistd.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <fcntl.h>
#include <errno.h>
#include <pthread.h>
#include "pCp.h"
#include "util.h"

struct circular_buffer jobs_buff;
struct cp_stats stats;

/* Flag to indicate producer thread finishes all its job */
int finished = 0;  

/* Flag to indicate stop any copy job and exit gracefully due to incoming SIGINT/SIGTERM signal */
int sig_finished = 0; 

pthread_t *thread_pool;
pthread_mutex_t mutex_stats, mutex_jobs_buff, mutex_stderr, mutex_stdout;
pthread_cond_t cond_full, cond_empty;

sem_t sem_fd;

int main(int argc, char *argv[])
{
    int status;
    void *ret;
    double elapsed_seconds, elapsed_microseconds;
    pthread_t producer_thread;
    struct timeval start_time, end_time;
    struct sigaction sa_action;
    struct rlimit rlim;

    /* Initialize the copy statistics */
    memset(&stats, 0, sizeof(struct cp_stats));

    status = handle_args(argc, argv, &stats.buff_size, &stats.num_cons);
    if (status != 0) {
        usage_err(argv[0], status);
        exit(EXIT_FAILURE);
    }

    /* Handler for SIGINT and SIGTERM signals */
    sa_action.sa_flags = SA_SIGINFO | SA_RESTART;
    sa_action.sa_handler = sighandler;
    if (sigemptyset(&sa_action.sa_mask) == -1 ||
        sigaction(SIGINT, &sa_action, NULL) == -1 ||
        sigaction(SIGTERM, &sa_action, NULL) == -1)
        perror("sa_action");

    /* Disable buffering for standart output */
    setbuf(stdout, NULL);

    /* Initialize jobs buffer */
    if (buff_init(&jobs_buff, stats.buff_size) == -1) {
        fprintf(stderr, "Failed to initalize the jobs buffer\n");
        exit(EXIT_FAILURE);
    }

    /* Initialize mutexes */
    if (pthread_mutex_init(&mutex_stdout, NULL) != 0 || 
        pthread_mutex_init(&mutex_stderr, NULL) != 0 || 
        pthread_mutex_init(&mutex_stats, NULL) != 0 ||
        pthread_mutex_init(&mutex_jobs_buff, NULL) != 0) {
        fprintf(stderr, "Failed to initialize mutexes\n");
        exit(EXIT_FAILURE);
    }

    /* Get the current resource limits for the maximum number of open file descriptors */
    if (getrlimit(RLIMIT_NOFILE, &rlim) != 0) {
        fprintf(stderr, "Failed to get resource limits\n");
        exit(EXIT_FAILURE);
    }

    /* Incerease the current open file descriptor limit accordint to buffer size */
    if (stats.buff_size > rlim.rlim_cur) {
        /* For safety, minus 1 */
        rlim.rlim_cur = rlim.rlim_max - 1;
        if (setrlimit(RLIMIT_NOFILE, &rlim) == -1) {
            fprintf(stderr, "Failed to set resource limits\n");
            exit(EXIT_FAILURE);
        }
        //TODO: if the buffer size is larger than rlim_max, buffer size can be decreased
    }
    
    /* initialize the semaphore with the max limit minus 3 (stdin, stdout and stderr) */
    if (sem_init(&sem_fd, 0, rlim.rlim_cur - 3 ) == -1) {
        fprintf(stderr, "Failed to initialize semaphore\n");
        return -1;
    }

    /* Initialize condition variables */
    if (pthread_cond_init(&cond_full, NULL) != 0 ||
        pthread_cond_init(&cond_empty, NULL) != 0) {
        fprintf(stderr, "Failed to initialize condition variables\n");
        exit(EXIT_FAILURE);
    }

    gettimeofday(&start_time, NULL); 

    /* argv[3] is the src directory names and the rest of them dst directory names (argv[argc] is NULL) */
    if (pthread_create(&producer_thread, NULL, producer_thread_func, argv + 3) != 0) {
        fprintf(stderr, "Failed to create producer thread\n");
        exit(EXIT_FAILURE);
    }

    /* Create the thread pool  */   
    thread_pool = create_thread_pool(stats.num_cons, consumer_thread_func, NULL);
    if (thread_pool == NULL) {
        fprintf(stderr, "Failed to succesfully create consumer threads pool for size %ld\n", stats.num_cons);
        exit(EXIT_FAILURE);
    }

    /* Wait for the threads (when all the files/subdirectories are copied) */
    if (pthread_join(producer_thread, &ret) != 0)
        print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to join procuder thread\n");
    // else print_threadsafe(STDERR_FILENO, &mutex_stderr, "Producer thread TID %ld is terminated with return value %ld\n", producer_thread, (long) ret);

    /* Waits for the thread to finish their execution and destroy the pool */
    destroy_thread_pool(thread_pool, stats.num_cons);

    gettimeofday(&end_time, NULL);

    elapsed_seconds = end_time.tv_sec - start_time.tv_sec;
    elapsed_microseconds = end_time.tv_usec - start_time.tv_usec;

    if (elapsed_microseconds < 0) {
        elapsed_seconds--;
        elapsed_microseconds += 1000000;
    }

    stats.elapsed_time = elapsed_seconds + elapsed_microseconds / 1000000.0;

    /* Print the copy statistics */
    cp_stats_print(&stats);

    /* Release the resources */
    pthread_mutex_destroy(&mutex_stdout);
    pthread_mutex_destroy(&mutex_stderr);
    pthread_mutex_destroy(&mutex_stats);
    pthread_mutex_destroy(&mutex_jobs_buff);

    sem_destroy(&sem_fd);
    
    pthread_cond_destroy(&cond_empty);
    pthread_cond_destroy(&cond_full);

    buff_destroy(&jobs_buff, (void (*)(void *)) cp_job_destroy);
}

void *producer_thread_func(void *args)
{
    int i, n;
    char **dirnames;

    dirnames = (char **) args;

    /* Count the number of directories (dirnames[n - 1] is the destination directory) */
    for (n = 0; dirnames[n] != NULL; ++n)   ;

    /* Producer takes an array of at least 2 entries (for the pathnames of the two directories) */
    for (i = 0; i < n - 1 && !sig_finished; ++i)
        copy_dir(dirnames[i], dirnames[n - 1]);

    if (pthread_mutex_lock(&mutex_jobs_buff) != 0) {
        fprintf(stderr, "Failed to lock the mutex\n"); 
        exit(EXIT_FAILURE);
    } 

    /* Set the flag finished to inform the consumer thread there will be no more new job */
    finished = 1;

    /* This informs the consumer that if there is a job in the buffer, first finish it then terminate */
    if (pthread_cond_broadcast(&cond_full) != 0) {
        fprintf(stderr, "Failed to conditional signal\n"); 
        exit(EXIT_FAILURE);
    } 

    if (pthread_mutex_unlock(&mutex_jobs_buff) != 0) {
        fprintf(stderr, "Failed to unlock the mutex\n"); 
        exit(EXIT_FAILURE);
    } 

    return 0;
}

void *consumer_thread_func(void *)
{
    struct cp_job *job;
    struct stat st;
    int status, bytes_transferred, totalbytes, num_copied_file;
            

    if (pthread_mutex_lock(&mutex_jobs_buff) != 0) {
        print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to lock the mutex\n"); 
        exit(EXIT_FAILURE);
    } 

    totalbytes = 0;
    num_copied_file = 0;
    while (1) {
        if (sig_finished == 1) {
            if (finished == 1) {
                print_threadsafe(STDOUT_FILENO, &mutex_stdout, "\nSIGNAL catched\n");
                finished = 0;
                /* Wake up all the sleeping threads for termination (producer sleeping cannot send) */
                if (pthread_cond_broadcast(&cond_full) != 0) {
                    print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to conditional signal\n"); 
                    exit(EXIT_FAILURE);
                } 
            }
            if (pthread_mutex_unlock(&mutex_jobs_buff) != 0)
                print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to unlock the mutex\n"); 
            break;
        }
        
        if (jobs_buff.size == 0 && finished == 1) {
            if (pthread_mutex_unlock(&mutex_jobs_buff) != 0)
                print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to unlock the mutex\n"); 
            break;
        }
        else if ((job = buff_pop(&jobs_buff)) == NULL) {
            if (pthread_cond_wait(&cond_full, &mutex_jobs_buff) != 0)
                print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to cond wait\n"); 
        } 
        else {
            if ((status = pthread_mutex_unlock(&mutex_jobs_buff)) != 0) {
                print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to unlock the mutex\n"); 
                break;
            } 
            
            if ((status = pthread_cond_signal(&cond_empty)) != 0) {
                print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to make conditional signal\n"); 
                break;
            } 

            bytes_transferred = copy_file(job->src_fd, job->dst_fd);
            if (bytes_transferred != -1) {
                totalbytes += bytes_transferred;
                /* Get the source file information */
                if (fstat(job->src_fd, &st) == -1) {
                    print_threadsafe(STDOUT_FILENO, &mutex_stdout, "Failed to get access file stats: %s\n", job->src_path);
                }
                else if (fchmod(job->dst_fd, st.st_mode) == -1) {
                    /* Change the file mode same as with the source file */
                    print_threadsafe(STDOUT_FILENO, &mutex_stdout, "Failed to set permissions: %s\n", job->src_path);
                }
            }
            
            ++num_copied_file;

            /* Inform the user either copying process is succed or not */
            print_cp_status(job->src_path, job->dst_path, bytes_transferred);

            /* Release the memory allocated by producer */
            cp_job_destroy(job);

            /* Up the semaphore for source and destination directory */
            if (sem_post(&sem_fd) == -1 || sem_post(&sem_fd) == -1)
                print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to post counting semaphore\n");

            if ((status = pthread_mutex_lock(&mutex_jobs_buff)) != 0) {
                print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to lock the mutex\n"); 
                break;
            } 
        }
    }

    /* Update the global copy stat */
    if (pthread_mutex_lock(&mutex_stats) != 0) {
        print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to lock the mutex\n"); 
        pthread_exit(NULL); 
    } 

    /* Update the copy statistics */
    stats.num_regfile += num_copied_file;
    stats.totalbytes += totalbytes;

    if (pthread_mutex_unlock(&mutex_stats) != 0) {
        print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to unlock the mutex\n"); 
        pthread_exit(NULL); 
    }

    return status == 0 ? NULL : (void *) 1;
}

void sighandler() 
{
    sig_finished = 1; 
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
            // TODO: detacht the created threads
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
        // else printf("Thread TID %ld is terminated with return value %ld\n", threads[i], (long) ret);
    }
    free(threads);
}

int copy_dir(const char *src_dirpath, const char *dst_dirpath)
{
    DIR *src_dir, *dst_dir;
    struct dirent *dentry;
    struct stat st;
    int status, link_len, src_fd, dst_fd;
    char src_path[PATH_LEN], dst_path[PATH_LEN], target_link[PATH_LEN];
    char *changed_dirpath;
    const char *dirname;
    struct cp_job *job;
    struct cp_stats prod_stats;

    
    /* Down the semaphore for source directory fd */
    if (sem_wait(&sem_fd) == -1) {
        print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to wait counting semaphore\n");
        return -1; 
    }

    /* Open the source directory */
    src_dir = opendir(src_dirpath);
    if (src_dir == NULL) {
        print_threadsafe(STDOUT_FILENO, &mutex_stdout, "Failed to open source directory %s\n", src_dirpath);
        return -1;
    }

    /* Execute permission is necessary for getting file status using fstat or stat */
    changed_dirpath = NULL;
    if ((status = mkdir(dst_dirpath, S_IRWXU | S_IRWXG | S_IRWXO)) == -1) {
        if (errno != EEXIST) {
            print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to create/open destination directory: %s\n", dst_dirpath);
        }
        else {
            dirname = get_dirname(src_dirpath);
            changed_dirpath = (char *) malloc(strlen(dst_dirpath) + strlen(dirname) + 2);
            if (changed_dirpath == NULL)
                print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed memory allocation\n");
            else {
                sprintf(changed_dirpath, "%s/%s", dst_dirpath, dirname);
                dst_dirpath = changed_dirpath;
                if (mkdir(dst_dirpath, S_IRWXU | S_IRWXG) == 0 || errno == EEXIST)
                    status = 0;
            }
        }

        if (status == -1) {
            closedir(src_dir);
            /* Up the semaphore for source directory */
            if (sem_post(&sem_fd) == -1)
                print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to post counting semaphore\n");
            return status;
        }
    }

    /* Down the semaphore for destination directory fd */
    if (sem_wait(&sem_fd) == -1) {
        print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to wait counting semaphore\n");
        return -1; 
    }

    dst_dir = opendir(dst_dirpath);
    if (dst_dir == NULL) {
        print_threadsafe(STDOUT_FILENO, &mutex_stdout, "Failed to open destination directory %s\n", dst_dirpath);
        closedir(src_dir);
        return -1;
    }

    memset(&prod_stats, 0, sizeof(struct cp_stats));

    while ((dentry = readdir(src_dir)) != NULL) {      
        if (sig_finished) {
            print_threadsafe(STDOUT_FILENO, &mutex_stdout, "\nSIGNAL catched during copy '%s' -> '%s'\n", src_dirpath, dst_dirpath);
            /* Wake up all the sleeping threads for termination */
            if (pthread_cond_broadcast(&cond_full) != 0) {
                print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to conditional signal\n"); 
                exit(EXIT_FAILURE);
            } 
            break;
        }

        if (strcmp(dentry->d_name, ".") == 0 || strcmp(dentry->d_name, "..") == 0)
            continue;

        sprintf(src_path, "%s/%s", src_dirpath, dentry->d_name);
        sprintf(dst_path, "%s/%s", dst_dirpath, dentry->d_name);

        /* Use lstat for handling links by getting information about the link itself */
        if (lstat(src_path, &st) == -1) {
            print_threadsafe(STDOUT_FILENO, &mutex_stdout, "Cannot get the informations of %s\n", src_path);
            return -1;
        }

        if (S_ISDIR(st.st_mode)) {
            ++prod_stats.num_dir; 
            /* Recursively copy the subdirectory */
            //TODO: If the previos directory copy was fail, should we continue or stop
            copy_dir(src_path, dst_path);
        }
        else if (S_ISFIFO(st.st_mode)) {
            /* Handle FIFO files */
            if (mkfifo(dst_path, st.st_mode) == -1 && errno != EEXIST) {
                print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to create FIFO: %s\n", dst_path);
                continue;
            }

            if (chmod(dst_path, st.st_mode) == -1) 
                print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to set permissions: %s\n", dst_path);
            
            ++prod_stats.num_fifo;
            print_cp_status(src_path, dst_path, 0);
        }
        else if (S_ISLNK(st.st_mode)) {
            /* Handle soft/symbolic links */          
            /* Use readlink to get the target link (returns the length of the link and not null terminated) */
            link_len = readlink(src_path, target_link, 128 - 1);
            target_link[link_len] = '\0';
            if (link_len == -1) {
                print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to read target link: %s\n", src_path);
            }
            /* use symlink() to create the new symlink at the destination */
            else if ((status = symlink(target_link, dst_path)) == -1) {
                if (errno == EEXIST) {
                    /* If a symlink exists, remove it */
                    if ((status = unlink(dst_path)) == -1)
                        print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to unlink existing symlink: %s\n", dst_path);
                    else if ((status = symlink(target_link, dst_path)) == -1)
                        print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to link the symlink: %s\n", dst_path);
                }
            }
            ++prod_stats.num_slink;
            print_cp_status(src_path, dst_path, link_len);
        }
        else if (S_ISREG(st.st_mode)) {
            /* Down the semaphore for source file fd */
            if (sem_wait(&sem_fd) == -1) {
                print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to wait counting semaphore\n");
                continue; 
            }

            /* Handle reguler files */
            src_fd = open(src_path, O_RDONLY);
            if (src_fd == -1) {
                print_threadsafe(STDOUT_FILENO, &mutex_stdout, "Failed to open source file: %s\n", src_path);
                continue; 
            }

            /* Down the semaphore for destination file fd */
            if (sem_wait(&sem_fd) == -1) {
                print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to wait counting semaphore\n");
                close(src_fd);
                continue; 
            }

            dst_fd = open(dst_path, O_WRONLY | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);  
            if (dst_fd == -1) {
                print_threadsafe(STDOUT_FILENO, &mutex_stdout, "Failed to open/create destination file: %s\n", dst_path);
                close(src_fd);
                continue; 
            }

            /* Put the file descriptors and file names into the structure */
            job = cp_job_create(src_fd, dst_fd, src_path, dst_path);
            if (job == NULL) {
                close(src_fd);
                close(dst_fd);
                print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to create the copy job");
                continue;
            }

            if (pthread_mutex_lock(&mutex_jobs_buff) != 0) {
                print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to lock the mutex\n"); 
                exit(EXIT_FAILURE);
            } 

            /* Add the copy structure to the producer-consumer buffer */
            while (buff_push(&jobs_buff, job) == -1) {
                if (pthread_cond_wait(&cond_empty, &mutex_jobs_buff) != 0) {
                    print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to make condtional wait\n"); 
                    exit(EXIT_FAILURE);
                }
            }

            if (pthread_cond_signal(&cond_full) != 0) {
                print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to make conditional signal\n"); 
                exit(EXIT_FAILURE);
            } 

            if (pthread_mutex_unlock(&mutex_jobs_buff) != 0) {
                print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to unlock the mutex\n"); 
                exit(EXIT_FAILURE);
            } 
        }
        else {
            ++prod_stats.num_unsupported;
            print_threadsafe(STDOUT_FILENO, &mutex_stdout, "Unsupported file type: %s\n", src_path);
        }
    }

    /* Update the global copy stat */
    if (pthread_mutex_lock(&mutex_stats) != 0) {
        print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to lock the mutex\n"); 
        exit(EXIT_FAILURE);
    }

    /* Update the copy statistics */
    stats.num_dir += prod_stats.num_dir;
    stats.num_fifo += prod_stats.num_fifo;
    stats.num_slink += prod_stats.num_slink;
    stats.num_unsupported += prod_stats.num_unsupported;

    if (pthread_mutex_unlock(&mutex_stats) != 0) {
        print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to unlock the mutex\n"); 
        exit(EXIT_FAILURE);
    }

    //TODO: Directory chmod should be done after all the files copied 
    /* Get file/directory information */
    if (stat(src_dirpath, &st) == -1) {
        perror(src_dirpath);
        return -1;
    }

    /* Set permissions of the destination directory */
    if (chmod(dst_dirpath, st.st_mode) == -1) {
        print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to set permissions: %s\n", dst_dirpath);
    }

    if (changed_dirpath != NULL)
        free(changed_dirpath);

    closedir(src_dir);
    /* Up the semaphore for source directory */
    if (sem_post(&sem_fd) == -1)
        print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to post counting semaphore\n");

    closedir(dst_dir);
    /* Up the semaphore for destination directory */
    if (sem_post(&sem_fd) == -1)
        print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to post counting semaphore\n");
    
    return 0;
}

long copy_file(int src_fd, int dst_fd)
{
    int num_read, num_write;
    long bytes_transferred;
    char buff[BUFF_LEN];
    char *ptr;

    bytes_transferred = 0;
    /* Transfer data until encountering EOF or an error */
    while ((num_read = read(src_fd, buff, BUFF_LEN)) > 0) {
        ptr = buff;
        do {
            num_write = write(dst_fd, ptr, num_read);
            if (num_write == -1) {
                print_threadsafe(STDERR_FILENO, &mutex_stderr, "Failed to write buffer content");
                return -1;
            }
            num_read -= num_write;
            ptr += num_write;
            bytes_transferred += num_write;
        } while (num_read > 0);
    }

    return bytes_transferred;
}

const char *get_dirname(const char *path)
{
    const char *trav;
    
    for (trav = path; *trav != '\0'; ++trav)
        if (*trav == '/')
            path = trav + 1;

    return path;
}

struct cp_job *cp_job_create(int src_fd, int dst_fd, const char *src_path, const char *dst_path)
{
    struct cp_job *job;

    /* Put the file descriptors and file names into the structure */
    job = (struct cp_job *) malloc(sizeof(struct cp_job));
    if (job == NULL)
        return NULL; 

    job->src_path = (char *) malloc(strlen(src_path) + 1);
    if (job->src_path == NULL) {
        free(job);
        return NULL;
    }

    job->dst_path = (char *) malloc(strlen(dst_path) + 1);
    if (job->dst_path == NULL) {
        free(job);
        free(job->src_path);
        return NULL;
    } 

    job->src_fd = src_fd;
    job->dst_fd = dst_fd;

    strcpy(job->src_path, src_path); 
    strcpy(job->dst_path, dst_path); 
    
    return job;
}

void print_cp_status(const char *src_path, const char *dst_path, int status)
{
    print_threadsafe(STDOUT_FILENO, &mutex_stdout, "%s'%s' -> '%s'\n", 
        status == -1 ? "Failed: " : "", src_path, dst_path);
}

void cp_job_destroy(struct cp_job *job)
{
    if (job != NULL) {
        if (job->src_path != NULL)
            free(job->src_path);
        if (job->dst_path != NULL)
            free(job->dst_path);
        free(job);
    }
}

void cp_stats_print(const struct cp_stats *stats)
{
    printf("\npCp STATISTICS\n");
    printf("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
    printf("USED SYSTEM RESOURCE\n");
    printf("Buffer size             : %ld\n", stats->buff_size);
    printf("Worker thread pool size : %ld\n\n", stats->num_cons);
    printf("# OF COPIED FILES\n");
    printf("Directory        : %ld\n", stats->num_dir);
    printf("Regular file     : %ld\n", stats->num_regfile);
    printf("Symbolic link    : %ld\n", stats->num_slink);
    printf("FIFO file        : %ld\n", stats->num_fifo);
    printf("Unsupported file : %ld\n\n", stats->num_unsupported);
    printf("Total transferred byte(s) : %ld\n", stats->totalbytes);
    printf("Elapsed time              : %.6f seconds\n", stats->elapsed_time);
}

void usage_err(const char *pname, int errcode) 
{
    switch (errcode) {
        case 1:
            fprintf(stderr, "Copy source to destination, or multiple sources to destination\n\n");
            fprintf(stderr, "Usage: %s <buffer size> <#of consumers> <source dirname> <dst dirname>\n", pname);
            fprintf(stderr, "Or   : %s <buffer size> <#of consumers> <source dirname1> <source dirname2> <dst dirname>\n", pname);
            break;
        case 2:
            fprintf(stderr, "Error, missing number of arguments\n");
            break;
        case 3:
            fprintf(stderr, "Error, <buffer size> should be a positive integer\n");
            break;
        case 4:
            fprintf(stderr, "Error, <#of consumers> should be a positive integer\n");
            break;
        default:
            break;
    }

    if (errcode != 1)
        fprintf(stderr, "try '%s --help' for more information\n", pname);
}

int handle_args(int argc, char *argv[], long unsigned int *buff_size, long unsigned int *num_cons)
{
    int status;
    long val;

    /* pCp <buffer size> <#of consumers> <source dirname> <dest dirname> */
    /* pCp <buffer size> <#of consumers> <source1 dirname> <source2 dirname> <dest dirname> */

    if (argc == 1)
        return 2;
    else if (strcmp(argv[1], "--help") == 0)
        return 1;

    if (argc < 5)
        return 2;

    val = str_to_long(argv[1], &status);
    if (status == -1 || val < 1)
        return 3;

    *buff_size = (unsigned long int) val; 
    
    val = str_to_long(argv[2], &status);
    if (status == -1 || val < 1)
        return 4;

    *num_cons = (unsigned long int) val; 

    return 0;
}