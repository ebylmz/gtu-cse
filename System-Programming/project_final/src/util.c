#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdarg.h>
#include <pthread.h>
#include "util.h"

void pthread_check_exit(int status, const char *s)
{
    if (status != 0) {
        fprintf(stderr, "%s: %s\n", s, strerror(status));
        pthread_exit(NULL);
    }
}

void check_exit(int status, const char *s)
{
    if (status == -1) {
        perror(s);
        exit(EXIT_FAILURE);
    }
}

int print_threadsafe(int fd, pthread_mutex_t *fd_mutex, const char *format, ...)
{
    va_list args; 
    int numbytes;

    if (pthread_mutex_lock(fd_mutex) != 0) {
        fprintf(stderr, "Failed to lock the mutex\n"); 
        return -1;
    } 

    va_start(args, format); 
    numbytes = vdprintf(fd, format, args);
    va_end(args);

    if (pthread_mutex_unlock(fd_mutex) != 0) {
        fprintf(stderr, "Failed to unlock the mutex\n"); 
        return -1;
    }

    return numbytes; 
}

int buff_push(struct circular_buffer *buff, void *item) 
{
    /* Make sure there is an empty slot for the incoming item */
    if (buff->size == buff->capa && buff_resize(buff) == -1)
        return -1;
    
    /* Enqueue the item */
    buff->items[buff->rear] = item;

    /* Adjust the rear */
    buff->rear = (buff->rear == buff->capa - 1) ? 0 : buff->rear + 1;
    buff->size += 1;
    return 0;
}

void *buff_pop(struct circular_buffer *buff)
{
    void *item;

    if (buff->size == 0)
        return NULL;

    /* Get the item and set the slot empty */
    item = buff->items[buff->front];
    buff->items[buff->front] = NULL;

    /* Adjust the front */
    buff->front = (buff->front == buff->capa - 1) ? 0 : buff->front + 1;
    buff->size -= 1;
    return item;
}

int buff_resize(struct circular_buffer *buff) 
{
    int i, new_capa;
    void **new_items;

    /* Allocate larger memory for the queue */
    new_capa = buff->capa == 0 ? 1 : 2 * buff->capa;  
    if ((new_items = (void **) malloc(new_capa * sizeof(void *))) == NULL) {
        perror("buff_resize malloc");
        return -1;
    }

    /* Rearrange the queue */
    for (i = 0; buff->front != buff->rear; ++i) { 
        new_items[i] = buff->items[buff->front];
        buff->front = (buff->front == buff->capa - 1) ? 0 : buff->front + 1;
    }

    buff->items = new_items;
    buff->capa = new_capa;
    buff->front = 0;
    buff->rear = i;  /* Size of the queue */
    return 0;
}

int buff_init(struct circular_buffer *buff, int capa) 
{
    buff->items = (void **) malloc(capa * sizeof(void *));
    if (buff->items == NULL)
        return -1;
    buff->front = 0;
    buff->rear = 0;
    buff->capa = capa;
    buff->size = 0;
    return 0;
}

void buff_destroy(struct circular_buffer *buff, void (*destroyer)(void *))
{
    while (buff->front != buff->rear) {
        if (destroyer != NULL)
            destroyer(buff->items[buff->front]);
        buff->front = (buff->front == buff->capa - 1) ? 0 : buff->front + 1;  
    }

    buff->size = 0;
    buff->capa = 0;
    buff->front = 0;
    buff->rear = 0;

    free(buff->items);
}

long str_to_long(const char *str, int *status) 
{
    long num;
    char *endptr;

    num = 0;
    errno = 0;
    num = strtol(str, &endptr, 10);
    *status = (*endptr != '\0' || errno != 0) ? -1 : 0;
    return num;
}

void err_exit(char *err)
{
    perror(err);
    exit(EXIT_FAILURE);
}

char *gettime() 
{
    time_t t;
    char *str_t;
    time(&t);
    str_t = ctime(&t);
    /* Trim the new line charachter */
    str_t[strlen(str_t) - 1] = '\0';
    return str_t;
}