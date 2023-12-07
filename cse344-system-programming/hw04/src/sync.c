#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "sync.h"

int reader_enter_region(struct safe_file *sfile)
{
    int status;

    status = pthread_mutex_lock(&sfile->mutex);
    if (status != 0)
        return status;

    /* Wait if there is any writer (writer prioritization) */
    while ((sfile->AW + sfile->WW) > 0) {
        sfile->WR += 1;
        status = pthread_cond_wait(&sfile->ok_to_read, &sfile->mutex);
        if (status != 0)
            return status;
        sfile->WR -= 1;
    }
    /* Reader become active */
    sfile->AR += 1;
    return pthread_mutex_unlock(&sfile->mutex);
}

int reader_exit_region(struct safe_file *sfile)
{
    int status;

    status = pthread_mutex_lock(&sfile->mutex);
    if (status != 0)
        return status;

    sfile->AR -= 1;

    if (sfile->AR == 0 && sfile->WW > 0) {
        status = pthread_cond_signal(&sfile->ok_to_write);
        if (status != 0)
            return status;
    }
    
    return pthread_mutex_unlock(&sfile->mutex);
}

int writer_enter_region(struct safe_file *sfile) 
{
    int status;

    status = pthread_mutex_lock(&sfile->mutex);
    if (status != 0)
        return status;

    /* Wait, if there ia any active reader or writer */
    while ((sfile->AW + sfile->AR) > 0) {
        sfile->WW += 1;

        status = pthread_cond_wait(&sfile->ok_to_write, &sfile->mutex);
        if (status != 0)
            return status;
        
        sfile->WW -= 1;
    }

    sfile->AW += 1;
    return pthread_mutex_unlock(&sfile->mutex);
}

int writer_exit_region(struct safe_file *sfile) 
{
    int status;

    status = pthread_mutex_lock(&sfile->mutex);
    if (status != 0) 
        return status;

    sfile->AW -= 1;
    if (sfile->WW > 0)
        status = pthread_cond_signal(&sfile->ok_to_write);
    else if (sfile->WR > 0)
        status = pthread_cond_broadcast(&sfile->ok_to_read);

    if (status != 0)
        return status;

    return pthread_mutex_unlock(&sfile->mutex);
}


struct safe_file *sfile_create(const char *fname) 
{
    struct safe_file *sfile;

    sfile = (struct safe_file *) malloc(sizeof(struct safe_file));
    if (sfile == NULL)
        return NULL;

    sfile->fname = malloc(sizeof(char) * (strlen(fname) + 1));
    if (sfile->fname == NULL) {
        free(sfile);
        perror("init_sfile: malloc");
        return NULL;
    }
    
    /* Copy the name of the file */
    strcpy(sfile->fname, fname);

    /* İnitialize mutexes and the condtion vairable */
    
    if (pthread_mutex_init(&sfile->mutex, NULL) != 0 ||
        pthread_cond_init(&sfile->ok_to_read, NULL) != 0 ||
        pthread_cond_init(&sfile->ok_to_write, NULL) != 0 ) {
        free(sfile->fname);
        free(sfile);
        perror("init_sfile: pthread init");
        return NULL;
    }

    /* Initialize reader and writer counts */
    sfile->AR = 0;
    sfile->AW = 0;
    sfile->WR = 0;
    sfile->WW = 0;

    return sfile;
}

struct safe_file *sfile_add(struct list *li, const char *fname)
{
    struct safe_file *sfile;

    sfile = sfile_create(fname);
    return sfile == NULL ? NULL : list_add(li, sfile);
}


struct safe_file *sfile_get(const struct list *li, const char *fname)
{
    int i;
    struct safe_file **sfiles;

    sfiles = (struct safe_file **) li->items;
    for (i = 0; i < li->size; ++i)
        if (strcmp(sfiles[i]->fname, fname) == 0)
            return sfiles[i];
    return NULL;
}

void sfile_list_destroy(struct list *li) 
{
    int i;
    struct safe_file **sfiles;

    sfiles = (struct safe_file **) li->items;
    for (i = 0; i < li->size; ++i) {
        free(sfiles[i]->fname);
        free(sfiles[i]);
        pthread_mutex_destroy(&sfiles[i]->mutex);
        pthread_cond_destroy(&sfiles[i]->ok_to_read);
        pthread_cond_destroy(&sfiles[i]->ok_to_write);
    }

    free(sfiles);
}