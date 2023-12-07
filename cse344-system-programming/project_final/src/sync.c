#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <fcntl.h>
#include <pthread.h>
#include <errno.h>
#include <time.h>
#include <unistd.h>
#include "util.h"
#include "sync.h"

struct safefile *safefile_list_find(struct circular_buffer *li, const char *name)
{
    int i;
    struct safefile **sfiles;

    sfiles = (struct safefile **)li->items;

    for (i = li->front; i != li->rear; ++i) {
        if (i == li->capa)
            i = 0;
        if (strcmp(sfiles[i]->name, name) == 0)
            return sfiles[i];
    }
    return NULL;
}

struct safefile *safefile_create(const char *name, int fd)
{
    struct safefile *sf;
    int err;

    sf = (struct safefile *)malloc(sizeof(struct safefile));
    if (sf == NULL)
        return NULL;
    sf->fd = fd;
    sf->name = (char *) malloc(strlen(name) + 1);
    if (sf->name == NULL) {
        free(sf);
        return NULL;
    }
    strcpy(sf->name, name);
    if ((err = pthread_mutex_init(&(sf->mutex), NULL)) != 0) {
        errno = err;
        free(sf->name);
        free(sf);
        return NULL;
    }
    return sf;
}

void safefile_free(struct safefile *sf)
{
    if (sf != NULL) {
        if (sf->fd > 0)
            close(sf->fd);
        pthread_mutex_destroy(&(sf->mutex));
        free(sf->name);
        free(sf);
    }
}

int send_header_event(int sockfd, enum sync_event event)
{
    struct header head;
    set_header(&head, NULL, event, 0);
    return send_header(sockfd, &head, NULL);
}

int recv_header(int sockfd, struct header *head, char *path)
{
    size_t numread;
    numread = recv(sockfd, head, sizeof(struct header), 0);
    if (numread < sizeof(struct header))
        return -1;

    if (head->pathlen > 0) {
        numread = recv(sockfd, path, head->pathlen, 0);
        if (numread < head->pathlen)
            return -1;
    }

    // make sure to place null sign
    // path[head->pathlen - 1] = '\0';

    return 0;
}

int send_header(int sockfd, const struct header *head, const char *path)
{
    size_t numwrite;
    numwrite = send(sockfd, head, sizeof(struct header), 0);
    if (numwrite < sizeof(struct header))
        return -1;

    if (head->pathlen > 0) {
        numwrite = send(sockfd, path, head->pathlen, 0);
        if (numwrite < head->pathlen)
            return -1;
    }
    return 0;
}

void set_header(struct header *head, struct stat *st, enum sync_event event, size_t pathlen)
{
    if (st == NULL)
        memset(&(head->st), 0, sizeof(struct stat));
    else
        memcpy(&(head->st), st, sizeof(struct stat));
    head->event = event;
    head->pathlen = pathlen;
}

int sync_sendfilep(int sockfd, const char *srcpath)
{
    int filefd, rv, len, sendflag;
    ssize_t bytes_transferred;
    enum response resp;
    struct stat st;
    char target_link[MAX_PATH_LENGTH];

    /* First, establish the file type */
    if (lstat(srcpath, &st) == -1)
        return -1;
    sendflag = 0;

    if (S_ISLNK(st.st_mode)) {
        /* Use readlink to get the target link (returns the length of the not null terminated link) */
        rv = len = readlink(srcpath, target_link, MAX_PATH_LENGTH - 1);
        if (rv >= 0) {
            target_link[len] = '\0'; 
            rv = sendbuff(sockfd, target_link, len + 1, 1);
        }
        sendflag = 1;
    }
    else if (S_ISREG(st.st_mode)) {
        filefd = open(srcpath, O_RDWR);
        if (filefd == -1)
            return -1;

        bytes_transferred = sendfile(sockfd, filefd);

        close(filefd);
        sendflag = 1;
    }
    else {
        /* FIFO, directory, and the rest of the file */
        rv = 0;
    }

    /* Wait receiver response for synchronization */
    if (sendflag == 1 && recv(sockfd, &resp, sizeof(enum response), 0) < (ssize_t)sizeof(enum response))
        return -1;
    return bytes_transferred;
}

int sync_sendfile(int sockfd, int filefd)
{
    ssize_t bytes_transferred;
    enum response resp;

    bytes_transferred = sendfile(sockfd, filefd);

    /* Wait receiver response for synchronization */
    if (recv(sockfd, &resp, sizeof(enum response), 0) < (ssize_t)sizeof(enum response))
        return -1;
    return bytes_transferred;
}

int sync_recvfile(int sockfd, int filefd)
{
    enum response resp;
    ssize_t bytes_transferred;

    bytes_transferred = recvfile(sockfd, filefd);

    /* Wait receiver response for synchronization */
    resp = OK;
    if (send(sockfd, &resp, sizeof(enum response), 0) < (ssize_t)sizeof(enum response))
        return -1;

    return bytes_transferred;
}

ssize_t sendfile(int sockfd, int filefd)
{
    ssize_t num_read, num_write;
    ssize_t bytes_transferred;
    char buff[BUFF_SIZE];
    char *ptr;

    bytes_transferred = 0;
    /* Transfer data until encountering EOF or an error */
    while ((num_read = read(filefd, buff, BUFF_SIZE)) > 0) {
        ptr = buff;

        num_write = sendbuff(sockfd, ptr, num_read, 0);
        if (num_write == -1)
            return -1;
        else
            bytes_transferred += num_write;
    }

    /* Place end of file delimiter */
    if (sendbuff(sockfd, NULL, 0, 1) == -1)
        return -1;

    return bytes_transferred;
}

int sendbuff(int sockfd, char *buff, int size, int etx)
{
    int num_write, bytes_transferred;
    char *ptr;
    const char etx_char = ETX;

    ptr = buff;
    bytes_transferred = 0;
    while (size > 0) {
        num_write = send(sockfd, ptr, size, 0);
        if (num_write == -1)
            return -1;
        size -= num_write;
        ptr += num_write;
        bytes_transferred += num_write;
    }

    /* Place end of file delimiter */
    if (etx == 1 && send(sockfd, &etx_char, sizeof(char), 0) < (ssize_t)sizeof(char))
        return -1;

    return bytes_transferred;
}

ssize_t recvfile(int sockfd, int filefd)
{
    ssize_t num_read, num_write;
    ssize_t bytes_transferred;
    char buff[BUFF_SIZE];
    char *ptr;
    int etx;

    bytes_transferred = 0;
    etx = 0;
    /* Transfer data until encountering EOF or an error */
    while (!etx && (num_read = recvbuff(sockfd, buff, BUFF_SIZE, &etx)) > 0) {
        ptr = buff;

        while (num_read > 0) {
            num_write = write(filefd, ptr, num_read);
            if (num_write == -1)
                return -1;
            num_read -= num_write;
            ptr += num_write;
            bytes_transferred += num_write;
        }
    }

    return bytes_transferred;
}

int recvbuff(int sockfd, char *buff, int size, int *etx)
{
    ssize_t num_read;

    if ((num_read = recv(sockfd, buff, size, 0)) > 0) {
        if ((int)buff[num_read - 1] == ETX) {
            --num_read;
            *etx = 1;
        }
        else
            *etx = 0;
    }
    return num_read;
}

struct fnode *fnode_create(const char *name, const struct stat *filestat)
{
    struct fnode *node;

    node = (struct fnode *)malloc(sizeof(struct fnode));
    if (node == NULL)
        return NULL;

    node->name = (char *) malloc(strlen(name) + 1);
    if (node->name == NULL) {
        free(node);
        return NULL;
    }

    strcpy(node->name, name);
    memcpy(&(node->filestat), filestat, sizeof(struct stat));
    node->child = NULL;
    node->sibling = NULL;

    return node;
}

void fnode_free(struct fnode *node)
{
    if (node != NULL) {
        free(node->name);
        free(node);
    }
}

int fnode_addchild(struct fnode *parent, struct fnode *child)
{
    struct fnode *last;

    if (parent == NULL)
        return -1;

    if (parent->child == NULL)
        parent->child = child;
    else {
        last = parent->child;
        while (last->sibling != NULL)
            last = last->sibling;
        last->sibling = child;
    }
    return 0;
}

int handle_event(int sockfd, struct fnode *root, struct stat *st, const char *path, enum sync_event event, struct safefile *logfile)
{
    int filefd, rv, isdir, nolog;
    char buff[BUFF_SIZE];
    int etx;

    rv = 0; 

    /* removedir recursivly removes and logs the files */
    isdir = S_ISDIR(st->st_mode);
    nolog = logfile == NULL || (isdir && event == REM);  

    if (event == REM) 
        rv = isdir ? removedir(path, logfile) : remove(path);
    else if (isdir) {
        if (event == ADD) {
            // rv = mkdir(path, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);
            rv = mkdir(path, st->st_mode);
            if (rv == -1 && errno == EEXIST)
                rv = 0;
        }
        else {
            /* Maybe changing file permissions  */
        }
    }
    else if (S_ISFIFO(st->st_mode)) {
        if (event == ADD) {
            rv = mkfifo(path, st->st_mode);
        }
        else {
            /* Maybe changing file permissions  */
        }
    }
    else if (S_ISLNK(st->st_mode)) {
        /* read the link target from the socket */
        rv = recvbuff(sockfd, buff, BUFF_SIZE, &etx);
        if (rv >= 0) {
            if (event == ADD) {
                rv = symlink(buff, path);
            }
            else { 
                /* remove the symbolic link and recreate it */
                rv = unlink(path);
                if (rv != -1)
                    rv = symlink(buff, path);
            }
        }
    }
    else if (S_ISREG(st->st_mode)) {
        /* Same operations for ADD/MOD, truncuate if the file exist */
        rv = filefd = open(path, O_RDWR | O_CREAT | O_TRUNC, st->st_mode);
        if (filefd != -1) {
            rv = sync_recvfile(sockfd, filefd);
            fcntl(filefd, F_SETFD, st->st_mode); 
            close(filefd);
        }
        else
            perror(path);
    }
    else {
        /* Unsupported file type */
        rv = -1;
    }

    if (!nolog &&  writelog(logfile, path, event) == -1)
        perror("writelog");

    if (rv != -1)
        update_dirtree(root, path, event);

    return rv;
}

int removedir(const char *dirpath, struct safefile *logfile)
{
    DIR *dir;
    struct dirent *entry;
    struct stat st;
    char entry_path[MAX_PATH_LENGTH];

    dir = opendir(dirpath);
    if (dir == NULL) {
        perror(dirpath);
        return -1;
    }

    while ((entry = readdir(dir)) != NULL) {
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0)
            continue;

        snprintf(entry_path, MAX_PATH_LENGTH, "%s/%s", dirpath, entry->d_name);

        if (lstat(entry_path, &st) == -1) {
            perror("lstat");
            continue;
        }

        /* First remove the directory content */
        if (S_ISDIR(st.st_mode)) {
            if (removedir(entry_path, logfile) == -1)
                break;
        }
        /* Remove file */
        else if (remove(entry_path) == -1)
            perror("remove");
        /* Write log */
        if (logfile != NULL && writelog(logfile, entry_path, REM) == -1)
            perror("writelog");
    }
    remove(dirpath);
    closedir(dir);
    return 0;
}

int update_dirtree(struct fnode *root, const char *entrypath, enum sync_event event)
{
    int firstchild;
    struct fnode *prev, *newchild, *old;
    struct fnode **prevnext;
    struct stat st;

    prev = find_entry(root, entrypath, &firstchild);
    if (prev == NULL)
        return -1;

    prevnext = firstchild ? &(prev->child) : &(prev->sibling);

    switch (event) {
        case ADD:
            /* file needs to be created before that */
            if (lstat(entrypath, &st) == -1)
                return -1;
            newchild = fnode_create(get_lastfname(entrypath), &st);
            if (newchild == NULL)
                return -1;
            *prevnext = newchild; /* Inserting at the end */
            break;
        case REM:
            old = *prevnext;
            *prevnext = old->sibling;
            if (old->child != NULL)
                free_dirtree(old->child);
            fnode_free(old);
            break;
        case MOD:
            if (lstat(entrypath, &st) == -1)
                return -1;
            (*prevnext)->filestat = st;
            break;
        default:
            break;
    }

    return 0;
}

struct fnode *find_entry(struct fnode *root, const char *entrypath, int *firstchild)
{
    int incomplete, rootlen;
    struct fnode *parent, *child, *prev;
    char *token, *saveptr;
    char tokenpath[MAX_PATH_LENGTH];

    /* There is no child */
    if (root->child == NULL)
        return NULL;

    rootlen = strlen(root->name) + 1; /* +1 for '/' */

    strncpy(tokenpath, entrypath + rootlen, MAX_PATH_LENGTH);

    prev = NULL;
    parent = root;
    child = root->child;

    incomplete = 0;
    token = strtok_r(tokenpath, "/", &saveptr);
    while (token != NULL) {
        /* search in current directory */
        while (child != NULL && strcmp(child->name, token) != 0) {
            prev = child;
            child = child->sibling;
        }

        token = strtok_r(NULL, "/", &saveptr);
        if (token != NULL)
        {
            if (child == NULL)
                incomplete = 1;
            else
            {
                prev = NULL;
                parent = child;
                child = parent->child;
            }
        }
    }

    if (incomplete)
        return NULL;

    *firstchild = prev == NULL;
    return *firstchild ? parent : prev;
}

void printmargin(int n)
{
    int i;
    for (i = 0; i < n; ++i)
        printf("\t");
}

void print_dirtree(const struct fnode *root)
{
    printf("Directory Tree:\n");
    printf("==================================\n");
    print_dirtree_rec(root, 0);
    printf("==================================\n");
}

void print_dirtree_rec(const struct fnode *parent, int s)
{
    if (parent != NULL) {
        printmargin(s);
        printf("%s\n", parent->name);
        if (parent->child != NULL)
            print_dirtree_rec(parent->child, s + 1);
        if (parent->sibling != NULL)
            print_dirtree_rec(parent->sibling, s);
    }
}

struct fnode *build_dirtree(struct fnode *parent, const char *dirpath)
{
    DIR *dir;
    struct dirent *entry;
    struct stat st;
    char entry_path[MAX_PATH_LENGTH];
    struct fnode *child;

    dir = opendir(dirpath);
    if (dir == NULL) {
        perror(dirpath);
        return NULL;
    }

    if (parent == NULL) {
        if (lstat(dirpath, &st) == -1) {
            perror("lstat");
            return NULL;
        }
        parent = fnode_create(dirpath, &st);
        if (parent == NULL) {
            perror("fnode_create");
            return NULL;
        }
    }

    while ((entry = readdir(dir)) != NULL) {
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0)
            continue;

        snprintf(entry_path, MAX_PATH_LENGTH, "%s/%s", dirpath, entry->d_name);

        if (lstat(entry_path, &st) == -1) {
            perror("lstat");
            continue;
        }

        /* Create a new directory entry and add it as a child */
        child = fnode_create(entry->d_name, &st);
        if (child == NULL) {
            perror("fnode_create");
            continue;
        }

        fnode_addchild(parent, child);

        /* Recursively build the subdirectory */
        if (S_ISDIR(st.st_mode))
            build_dirtree(child, entry_path);
    }
    closedir(dir);

    return parent;
}

void free_dirtree(struct fnode *root)
{
    if (root != NULL) {
        free_dirtree(root->child);
        free_dirtree(root->sibling);
        fnode_free(root);
    }
}

int compare_dirtree(struct fnode *root, const char *dirpath, int sockfd, int skiplen, struct safefile *logfile)
{
    DIR *dir;
    struct dirent *entry;
    struct stat st;
    struct fnode *child, *prev;
    struct header head;
    char entry_path[MAX_PATH_LENGTH];
    char *logfname;
    time_t currtime;
    enum sync_event event;

    dir = opendir(dirpath);
    if (dir == NULL) {
        perror("opendir");
        return -1;
    }

    logfname = logfile != NULL ? (char *)get_lastfname(logfile->name) : NULL;

    while ((entry = readdir(dir)) != NULL) {
        /* Ignore the log file */
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0 ||
            (logfname != NULL && strcmp(entry->d_name, logfname) == 0))
            continue;

        snprintf(entry_path, MAX_PATH_LENGTH, "%s/%s", dirpath, entry->d_name);
        event = DONE;

        /* Try to find the matching child node in the directory tree */
        prev = NULL;
        child = root->child;

        while (child != NULL && strcmp(child->name, entry->d_name) != 0) {
            prev = child;
            child = child->sibling;
        }

        if (lstat(entry_path, &st) == -1) {
            perror("lstat");
            continue;
        }
        currtime = time(NULL);

        if (child == NULL) {
            /* Entry is not found in the tree */
            event = ADD;

            printf("'%s' has been added!\n", entry_path);
            // no need to continue recursivly
            child = fnode_create(entry->d_name, &st);
            if (child == NULL) {
                perror("fnode_create");
                continue;
            }
            /* Add newchild as first/last child */
            if (prev == NULL)
                root->child = child;
            else
                prev->sibling = child;
        }
        else {
            /* If the size of files are different, or the last modification times are different */
            // if (child->filestat.st_size != st.st_size || child->filestat.st_mtime != st.st_mtime) {
            if (child->filestat.st_size != st.st_size || (child->filestat.st_mtime != st.st_mtime && currtime - st.st_mtime > 1)) {
                printf("'%s' has been modified!\n", entry_path);
                event = MOD;
                child->filestat = st;
            }
        }

        if (event != DONE) {
            set_header(&head, &st, event, strlen(entry_path + skiplen) + 1);

            if (logfile != NULL && writelog(logfile, entry_path, event) == -1)
                perror("writelog");

            if (send_header(sockfd, &head, entry_path + skiplen) == -1)
                perror("send_header");

            /* Handle reguler files */
            if (sync_sendfilep(sockfd, entry_path) == -1)
                perror("sync_sendfilep");
        }
        if (S_ISDIR(st.st_mode))
            compare_dirtree(child, entry_path, sockfd, skiplen, logfile);
    }

    /* Check if any child node has been removed */
    prev = NULL;
    child = root->child;

    while (child != NULL) {
        snprintf(entry_path, MAX_PATH_LENGTH, "%s/%s", dirpath, child->name);
        if (lstat(entry_path, &st) == -1 && errno == ENOENT) {
            if (prev == NULL)
                root->child = child->sibling;
            else
                prev->sibling = child->sibling;

            if (logfile != NULL && writelog(logfile, entry_path, REM) == -1)
                perror("writelog");

            printf("'%s' has been removed!\n", entry_path + skiplen);

            set_header(&head, &(child->filestat), REM, strlen(entry_path + skiplen) + 1);
            if (send_header(sockfd, &head, entry_path + skiplen) == -1)
                perror("send_header");

            if (child->child != NULL)
                free_dirtree(child->child);
            fnode_free(child);
            child = NULL;
        }

        /* If the child is removed */
        if (child == NULL) {
            /* prev does not change */
            child = (prev == NULL) ? root->child : prev->sibling;
        } 
        else {
            prev = child;
            child = child->sibling;
        }
    }

    closedir(dir);

    return 0;
}

int sync_writer_rec(int sockfd, struct fnode *root, const char *rootpath)
{
    struct fnode *trav;
    char entrypath[MAX_PATH_LENGTH];
    char *ptr;
    int skiplen, pathlen;
    enum response resp;

    if (root == NULL)
        return 0;

    skiplen = snprintf(entrypath, MAX_PATH_LENGTH, "%s/", rootpath);
    ptr = entrypath + skiplen;

    for (trav = root->child; trav != NULL; trav = trav->sibling) {
        /* Write the file */
        pathlen = skiplen + 1; /* +1 for null char */
        pathlen += snprintf(ptr, MAX_PATH_LENGTH - skiplen, "%s", trav->name);

        if (write_entry(sockfd, pathlen, &(trav->filestat), entrypath + skiplen) == -1)
            return -1;

        /* Read the sync_reader response */
        if (recv(sockfd, &resp, sizeof(enum response), 0) < (ssize_t)sizeof(enum response))
            return -1;

        if (resp == GET) {
            /* Send the file */
            sync_sendfilep(sockfd, entrypath);
        }

        /* Continue recursively for the directories */
        if (trav->child != NULL)
            sync_writer_rec(sockfd, trav, entrypath);
    }
    return 0;
}

int sync_writer(int sockfd, struct fnode *root, const char *rootpath)
{
    int rv;

    rv = sync_writer_rec(sockfd, root, rootpath);
    if (rv == 0) {
        /* Indicate the synchronization process is done */
        if (write_entry(sockfd, 0, NULL, NULL) == -1) {
            perror("write_entry");
            rv = -1;
        }
    }

    return rv;
}

int sync_reader(int sockfd, struct fnode *root, struct safefile *logfile)
{
    struct stat read_st, curr_st;
    int status, pathlen, skiplen;
    char path[MAX_PATH_LENGTH];
    char *ptr;
    enum response resp;
    enum sync_event event;

    /* Read the filename from the point ptr shows, so that we don't need to reconstruct the path everytime */
    skiplen = snprintf(path, MAX_PATH_LENGTH, "%s/", root->name);
    ptr = path + skiplen;

    while ((status = read_entry(sockfd, &pathlen, &read_st, ptr)) != -1 && pathlen > 0) {
        /* Check if the file/directory is exist in local directory */
        if (lstat(path, &curr_st) == -1) {
            if (errno == ENOENT) {
                /* File does not exist */
                resp = GET;
                event = ADD;
            }
            else {
                perror(path);
                pthread_exit(NULL);
            }
        }
        else if (curr_st.st_mtime < read_st.st_mtime) {
            /* File is not updated */
            resp = GET;
            event = MOD;
        }
        else
            resp = OK;

        /* Write the response either GET or OK */
        if (send(sockfd, &resp, sizeof(enum response), 0) < (ssize_t)sizeof(enum response))
            return -1;

        /* Update the file and its node in directory tree */
        if (resp == GET)
            handle_event(sockfd, root, &read_st, path, event, logfile); 
    }

    return status;
}

int read_entry(int sockfd, int *pathlen, struct stat *st, char *path)
{
    /* Read the path length */
    if (recv(sockfd, pathlen, sizeof(int), 0) < (ssize_t)sizeof(int))
        return -1;
    if (*pathlen > 0) {
        /* non-empty entry is written */
        if (recv(sockfd, path, *pathlen, 0) < (ssize_t)*pathlen ||
            recv(sockfd, st, sizeof(struct stat), 0) < (ssize_t)sizeof(struct stat))
            return -1;
    }
    return 0;
}

int write_entry(int sockfd, int pathlen, struct stat *st, const char *path)
{
    if (send(sockfd, &pathlen, sizeof(int), 0) < (ssize_t)sizeof(int))
        return -1;
    if (pathlen > 0) {
        /* non-empty entry is written */
        if (send(sockfd, path, pathlen, 0) < pathlen ||
            send(sockfd, st, sizeof(struct stat), 0) < (ssize_t)sizeof(struct stat))
            return -1;
    }
    return 0;
}

int writelog(struct safefile *logfile, const char *path, enum sync_event event)
{
    char log[LOG_MAX_LEN];
    char str_event[32];
    int num_write;

    conv_event(str_event, event);
    sprintf(log, "[%-20s] : %-8s : %s\n", gettime(), str_event, path);

    pthread_check_exit(pthread_mutex_lock(&(logfile->mutex)), "pthread_mutex_lock");
    num_write = write(logfile->fd, log, strlen(log));
    pthread_check_exit(pthread_mutex_unlock(&(logfile->mutex)), "pthread_mutex_unlock");
    return num_write < 0 ? -1 : 0;
}

const char *get_lastfname(const char *path)
{
    const char *trav;

    for (trav = path; *trav != '\0'; ++trav)
        if (*trav == '/')
            path = trav + 1;

    return path;
}

void conv_event(char *str, enum sync_event event)
{
    switch (event) {
        case ADD:
            strcpy(str, "ADD");
            break;
        case REM:
            strcpy(str, "REMOVE");
            break;
        case MOD:
            strcpy(str, "MODIFY");
            break;
        default:
            *str = '\0';
            break;
    }
}