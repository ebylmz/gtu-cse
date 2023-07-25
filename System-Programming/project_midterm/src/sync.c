#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "sync.h"

int reader_enter_region(struct safe_file *sfile)
{
    /* A reader is trying to enter */
    if (sem_wait(&sfile->read_try) == -1) {
        perror("sem_wait");
        return -1;
    }

    /* Avoid race condition with other readers */
    if (sem_wait(&sfile->rmutex) == -1) {
        perror("sem_wait");
        return -1;
    }

    /* Report yourself as a reader entering */
    sfile->reader_count += 1;
    /* If you are the first reader, lock the resource and prevent writers */
    if (sfile->reader_count == 1)
        sem_wait(&sfile->rsc);

    /* Allow other readers */
    if (sem_post(&sfile->rmutex) == -1) {
        perror("sem_post");
        return -1;
    }

    /* You are done trying to access the resource */
    if (sem_post(&sfile->read_try) == -1) {
        perror("sem_post");
        return -1;
    }

    return 0;
}

int reader_exit_region(struct safe_file *sfile)
{
    /* Avoid race condition with other readers */
    if (sem_wait(&sfile->rmutex) == -1) {
        perror("sem_wait");
        return -1;
    }

    /* Indicate you are leaving  */
    sfile->reader_count -= 1;
    /* If you are last reader leaving, release the locked resource */
    if (sfile->reader_count == 0)
        sem_post(&sfile->rsc);

    /* Release exit section for other readers */
    if (sem_post(&sfile->rmutex) == -1) {
        perror("sem_post");
        return -1;
    }

    return 0;
}

int writer_enter_region(struct safe_file *sfile) 
{
    /* Avoid race condition with other writers */
    if (sem_wait(&sfile->wmutex) == -1) {
        perror("sem_wait");
        return -1;
    }

    /* Report yourself as a writer entering */
    sfile->writer_count += 1;
    /* If you are the first writer, dont allow no new readers */
    if (sfile->writer_count == 1)
        sem_wait(&sfile->read_try);

    /* Allow other writers */
    if (sem_post(&sfile->wmutex) == -1) {
        perror("sem_post");
        return -1;
    }

    /* Prevent other writers */
    if (sem_wait(&sfile->rsc) == -1) {
        perror("sem_wait");
        return -1;
    }

    return 0;
}

int writer_exit_region(struct safe_file *sfile) 
{
    /* Release the resource */
    if (sem_post(&sfile->rsc) == -1) {
        perror("sem_post");
        return -1;
    }

    /* Reserver exit section */
    if (sem_wait(&sfile->wmutex) == -1) {
        perror("sem_wait");
        return -1;
    }

    /* Indicate you are leaving */
    sfile->writer_count -= 1;
    /* If you are the last writer, unlock the readers */
    if (sfile->writer_count == 0)
        sem_post(&sfile->read_try);

    /* Release exit section for other writers */
    if (sem_post(&sfile->wmutex) == -1) {
        perror("sem_post");
        return -1;
    }

    return 0;
}


int init_sdir(DIR *server_dir, struct safe_dir *sdir)
{
    struct dirent *dentry;

    sdir->capa = NUM_OF_DIR_FILE; 
    sdir->size = 0;

    while ((dentry = readdir(server_dir)) != NULL) {
        /* Skip the hidden files */
        if (strcmp(dentry->d_name, ".") != 0 && strcmp(dentry->d_name, "..") != 0) {
            if (add_sfile(sdir, dentry->d_name) == NULL)
                return -1;
        }
    }

    return 0;
}

struct safe_file *add_sfile(struct safe_dir *sdir, const char *fname) 
{
    if (sdir->size == sdir->capa) {
        return NULL; 
    }

    if (init_sfile(&sdir->files[sdir->size], fname) == -1)
        return NULL;
    sdir->size += 1;  
    return &sdir->files[sdir->size - 1];
}

int init_sfile(struct safe_file *sfile, const char *fname) 
{
    /* Copy the name of the file */
    strcpy(sfile->fname, fname);


    /* İnitialize semaphores */
    if (sem_init(&sfile->read_try, 1, 1) == -1) {
        perror("sem_init read_try");
        return -1;
    }

    if (sem_init(&sfile->rmutex, 1, 1) == -1) {
        perror("sem_init rmutex");
        return -1;
    }

    if (sem_init(&sfile->wmutex, 1, 1) == -1) {
        perror("sem_init wmutex");
        return -1;
    }

    if (sem_init(&sfile->rsc, 1, 1) == -1) {
        perror("sem_init rsc");
        return -1;
    }       

    /* Initialize reader and writer counts */
    sfile->reader_count = 0;
    sfile->writer_count = 0;

    return 0;
}

struct safe_file *get_sfile(struct safe_dir *sdir, const char *file)
{
    for (int i = 0; i < sdir->size; ++i)
        if (strcmp(sdir->files[i].fname, file) == 0)
            return &sdir->files[i];
    return NULL;
}