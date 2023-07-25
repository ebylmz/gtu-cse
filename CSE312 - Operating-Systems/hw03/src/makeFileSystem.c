#include <stdio.h>
#include <stdlib.h>
#include "fs.h"

int check_args(int argc, char *argv[], int *blocksize);

int main(int argc, char *argv[])
{
    struct filesystem *fs;
    int rv, blocksize;
    char *diskpath;
    
    if (check_args(argc, argv, &blocksize) == -1) {
        fprintf(stderr, "usage: %s [blocksize] [disk filename]\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    diskpath = argv[2];

    /* Convert to block size from kilobyte to byte */
    blocksize *= 1024; 

    fs = fs_create(blocksize);
    if (fs == NULL) {
        fprintf(stderr, "Failed to create file system\n");
        exit(EXIT_FAILURE);
    }

    /* Sync the diskfile with the filesystem object */
    rv = fs_sync(fs, diskpath);
    if (rv == -1) {
        perror("fs_sync");
        exit(EXIT_FAILURE);
    }

    /* Mounting from the disk file */
    rv = fs_mount(fs, diskpath);
    if (rv == -1) {
        perror("fs_sync");
        exit(EXIT_FAILURE);
    }

    fs_print(fs);

    fs_free(fs);
}

int check_args(int argc, char *argv[], int *blocksize)
{
    if (argc != 3)
        return -1;
    *blocksize = atoi(argv[1]);
    if (*blocksize <= 0)
        return -1;
    return 0;
}