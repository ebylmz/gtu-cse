#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <semaphore.h>
#include "common.h"

int enqueue(struct queue *que, struct client_info *item) 
{
    /* Make sure there is an empty slot for the incoming item */
    if (que->size == que->capacity && resize_queue(que) == -1)
        return -1;
    
    /* Enqueue the item */
    if ((que->elements[que->rear] = malloc(sizeof(struct client_info))) == NULL)
        return -1;

    /* Copy the item */
    *(que->elements[que->rear]) = *item;
    /* Adjust the rear */
    que->rear = (que->rear == que->capacity - 1) ? 0 : que->rear + 1;
    que->size += 1;
    return 0;
}

struct client_info *dequeue(struct queue *que)
{
    struct client_info *item;

    if (que->size == 0)
        return NULL;

    /* Get the item and set the slot empty */
    item = que->elements[que->front];
    que->elements[que->front] = NULL;

    /* Adjust the front */
    que->front = (que->front == que->capacity - 1) ? 0 : que->front + 1;
    que->size -= 1;
    return item;
}

int resize_queue(struct queue *que) 
{
    int i, new_capa;
    struct client_info **new_elements;

    /* Allocate larger memory for the queue */
    new_capa = que->capacity == 0 ? 1 : 2 * que->capacity;  
    if ((new_elements = malloc(new_capa * sizeof(struct queue *))) == NULL) {
        perror("resize_queue malloc");
        return -1;
    }

    /* Rearrange the queue */
    for (i = 0; que->front != que->rear; ++i) { 
        new_elements[i] = que->elements[que->front];
        que->front = (que->front == que->capacity - 1) ? 0 : que->front + 1;
    }

    que->elements = new_elements;
    que->capacity = new_capa;
    que->front = 0;
    que->rear = i;  /* Size of the queue */
    return 0;
}

int init_queue(struct queue *que) 
{
    que->elements = NULL;
    que->front = 0;
    que->rear = 0;
    que->capacity = 0;
    que->size = 0;
    return 0;
}

void destroy_queue(struct queue *que)
{
    int i;
    if (que->size == 0)
        return;
    for (i = que->front; i < que->size; ++i) {
        que->front = (que->front == que->capacity - 1) ? 0 : que->front + 1;  
        free(que->elements[que->front]);
    }

    que->size = 0;
    que->capacity = 0;
    que->front = 0;
    que->rear = 0;

    free(que->elements);
}

int init_pid_list(struct pid_list *plist, int capacity)
{
    plist->capacity = capacity;
    plist->size = 0;
    if ((plist->pids = malloc(capacity * sizeof(pid_t))) == NULL)
        return -1;
    return 0;
}

int add_pid_list(struct pid_list * plist, pid_t pid) 
{
    if (plist->size == plist->capacity) 
        return -1;
    plist->pids[plist->size] = pid;
    plist->size += 1;
    return 0;
}

int remove_pid_list(struct pid_list *plist, pid_t pid) 
{
    int i, target;
    
    if ((target = find_pid_list(plist, pid)) == -1)
        return -1;

    plist->size -= 1;
    for (i = target; i < plist->size; ++i)
        plist->pids[i] = plist->pids[i + 1];

    return 0;
}

void destroy_pid_list(struct pid_list *plist)
{
    free(plist->pids);
}

int find_pid_list(const struct pid_list *plist, pid_t pid)
{
    int i;
    for (i = 0; i < plist->size; ++i)   
        if (plist->pids[i] == pid)
            return i;
    return -1;
}

struct client_info *create_client_info(pid_t pid, int wait, const char *cwd)
{
    struct client_info *client;

    if ((client = malloc(sizeof(struct client_info))) == NULL)
        return NULL;
    strcpy(client->cwd, cwd);

    client->pid = pid;
    client->wait = wait;

    return client;
}

int parse_command(char *cmd, char *cmd_argv[]) 
{
    int i = 1;

    // writeT a.c "Everything is gonna be okay"

    cmd_argv[0] = strtok(cmd, " ");
    
    while ((cmd_argv[i] = strtok(NULL, " ")) != NULL) 
        ++i;
    return i;
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

void err_exit(const char *err) 
{
    perror(err);
    exit(EXIT_FAILURE);
}