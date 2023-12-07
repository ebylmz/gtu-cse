#ifndef UTIL_H
#define UTIL_H

struct circular_buffer {
    int front;
    int rear;
    int capa;
    int size;
    void **items;
};

void pthread_check_exit(int status, const char *s);

void check_exit(int status, const char *s);

int print_threadsafe(int fd, pthread_mutex_t *fd_mutex, const char *format, ...);

int buff_push(struct circular_buffer *buff, void *item);

void *buff_pop(struct circular_buffer *buff);

int buff_resize(struct circular_buffer *buff); 

int buff_init(struct circular_buffer *buff, int capa);

void buff_destroy(struct circular_buffer *buff, void (*destroyer)(void *));

long str_to_long(const char *str, int *status);

void err_exit(char *err);

char *gettime();

#endif