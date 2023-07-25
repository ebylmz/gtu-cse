#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <semaphore.h>
#include <errno.h>
#include "common.h"

int read_response(int client_fd, struct response_header *head, void **data)
{
    int num_read;

    /* Wait for the response */
    num_read = read(client_fd, head, sizeof(struct response_header));
    
    if (num_read == -1) /* Nothing to read */
        return -1;
    if (num_read == 0) {
        fprintf(stderr, "Server fifo is closed\n"); 
        return 1;
    }
    else if (num_read < (long) sizeof(struct response_header)) {
        fprintf(stderr, "Error, reading response header\n");
        return 1;
    }

    if (head->data_size > 0) {
        /* Allocate extra 1 byte for safety of the strings (null char at the end) */
        if ((*data = malloc(head->data_size + 1)) == NULL) {
            fprintf(stderr, "Error, cannot allocate memory for the response\n");
            return 1;
        }
        if (read(client_fd, *data, head->data_size) < (long) head->data_size) {
            fprintf(stderr, "Error reading response data\n");
            free(*data);
            return 1;
        }
    } 

    return 0;
}


int write_request(int client_fd, struct request_header *header, const void *data)
{
    /* Write the header */
    if (write(client_fd, header, sizeof(struct request_header)) != (long) sizeof(struct request_header)) {
        fprintf(stderr, "Error writing request header to FIFO\n");
        return -1;
    }

    if (data != NULL && header->data_size > 0) {
        /* Write the body */
        if (write(client_fd, data, header->data_size) != (long) header->data_size) {
            fprintf(stderr, "Error writing request data to FIFO\n");
            return -1;
        }
    }
    return 0;
}

void resp_head_set(struct response_header *head, enum resp_status stat, size_t data_size)
{
    head->data_size = data_size;
    head->stat = stat;
}

void req_head_set(struct request_header *head, pid_t pid, enum request_type type, size_t data_size)
{
    head->pid = pid;
    head->type = type;
    head->data_size = data_size;
}

void *enqueue(struct node **head, void *data)
{
    struct node *headnode;

    headnode = *head;
    if (headnode == NULL) {
        headnode = (struct node *) malloc(sizeof(struct node)); 
        if (headnode == NULL)
            return NULL;
        headnode->data = data;
        headnode->next = NULL;
        *head = headnode;
        return headnode;
    }
    else
        return enqueue(&(headnode->next), data);
}

void *dequeue(struct node **head)
{
    void *data;
    struct node *tmp;

    tmp = *head;
    if (tmp == NULL) /* Empty queue */
        data = NULL;
    else {
        data = tmp->data;
        *head = tmp->next;
        free(tmp);
    }
    
    return data;
}

int queue_size(const struct node *head)
{
    if (head == NULL)
        return 0;
    else 
        return 1 + queue_size(head->next);
}

void queue_destroy(struct node *head)
{
    if (head != NULL) {
        queue_destroy(head->next);
        free(head->data);
        free(head);
    }
}

int list_init(struct list *li, int capa)
{
    li->size = 0;
    li->capa = capa;
    li->items = (void **) malloc(capa * sizeof(void *));
    return li->items == NULL ? -1 : 0;
}

void *list_add(struct list *li, void *item)
{
    if (li->size == li->capa && list_resize(li) == -1)
        return NULL;
    li->items[li->size] = item;
    li->size += 1;
    return item;
}

void *list_remove(struct list *li, int i)
{
    void *item;
    
    if (i < 0 || li->size < i)
        return NULL;

    item = li->items[i];
    li->size -= 1;
    while (i < li->size) {
        li->items[i] = li->items[i + 1];
        ++i;
    }

    return item;
}

int list_resize(struct list *li)
{
    int i;
    void **new_items;
    li->capa *= 2;

    new_items = (void **) malloc(li->capa * sizeof(void *));
    if (li->items == NULL)
        return -1;
    
    for (i = 0; i < li->size; ++i)
        new_items[i] = li->items[i];
    li->items = new_items;
    return 0;
}

void list_destroy(struct list *li)
{
    int i;
    for (i = 0; i < li->size; ++i)
        free(li->items[i]); 
    free(li->items);
    li->size = 0;
    li->capa = 0;
    li->items = NULL;
}

enum req_cmd convert_req_cmd(const char *cmd) 
{
    if (strcmp(cmd, "help") == 0)
        return HELP;
    else if (strcmp(cmd, "list") == 0)
        return LIST;
    else if (strcmp(cmd, "readF") == 0)
        return READ_F;
    else if (strcmp(cmd, "writeT") == 0)
        return WRITE_T;
    else if (strcmp(cmd, "upload") == 0)
        return UPLOAD;
    else if (strcmp(cmd, "download") == 0)
        return DOWNLOAD;
    else if (strcmp(cmd, "quit") == 0)
        return QUIT;
    else if (strcmp(cmd, "killServer") == 0)
        return KILL_SERVER;
    else
        return -1;
}

char *strip_ext(char *fname)
{
    char *end;

    end = fname + strlen(fname);
    
    if (end == fname)
        return fname;

    while (end > fname && *end != '.')
        --end;

    /* If there is an extension, change the dot seperator with null char */
    if (end != fname) {
        *end = '\0';
        /* end points the extension */
        ++end;
    }

    return end;
}

void fname_version(char *fname, char *fnamev, int v)
{
    char *ext;

    ext = strip_ext(fname);    

    if (ext == fname) /* No extension */
        sprintf(fnamev, "%s(%d)", fname, v);
    else
        sprintf(fnamev, "%s(%d).%s", fname, v, ext);
}

int parse_command(char *cmd, char *cmd_argv[]) 
{
    int i = 1;

    // TODO: enchange the parser to recognize string quotes 
    // exp: writeT a.c "Everything is good" -o "Nothing bad"

    cmd_argv[0] = strtok(cmd, " ");
    
    while ((cmd_argv[i] = strtok(NULL, " ")) != NULL) 
        ++i;
    return i;
}

int str_to_int(const char *str, int *status) 
{
    int num;
    char *endptr;

    num = 0;
    errno = 0;
    num = strtol(str, &endptr, 10);
    *status = (*endptr != '\0' || errno != 0) ? -1 : 0;
    return num;
}

void err_exit(const char *err) 
{
    perror(err);
    exit(EXIT_FAILURE);
}