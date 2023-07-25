#include <stdio.h>
#include <stdlib.h> 
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <limits.h>
#include <errno.h>

void usage_error(char * pname) {
    printf("Right usage: %s filename num-bytes [x]\n", pname);
}

int main(int argc, char * argv[]) {
    int i;
    int fd;
    int flags;
    long num_bytes;
    char * endptr;
    char c;

    if (argc != 3 && argc != 4) {
        usage_error(argv[0]);
        return 1;
    }

    /* default charachter to write file is z */
    c = (argc == 4) ? argv[3][0] : 'z';

    errno = 0;
    num_bytes = strtol(argv[2], &endptr, 0); 
    
    if (endptr == argv[2] || *endptr != '\0' || ((num_bytes == LONG_MIN || num_bytes == LONG_MAX) && errno == ERANGE)) {
        usage_error(argv[0]);
        return 1;
    }

    /* if a third command-line argument (x) is not supplied, then the file should be open with O_APPEND flag */
    flags = (O_RDWR | O_CREAT);
    if (argc == 3)
        flags |= O_APPEND;

    fd = open(argv[1], flags, S_IRUSR | S_IWUSR);
    if (fd == -1) {
        perror("open");
        return 1;
    }

    printf("Executing...\n");

    /*  
        There could be multiple processes/threads that work on the same file simultaneously.
        So SEEK_END can be change while a process is suspended and another one runs.
        For that reason, we should apply lseek before each write to set the current offset to the SEEK_END.
        If the file is opened with O_APPEND flag then all the file operations are
        done atomically and no need for any lseek. Otherwise use lseek to SEEK_END before each write,
        but this still won't prevent you from overriding situations, because file operations are not executed atomically.
    */

    for (i = 0; i < num_bytes; ++i) {
        if (argc == 4)
            lseek(fd, 0, SEEK_END);
        /* write a byte at a time */
        write(fd, &c, 1);
    } 
    
    printf("Finished. %ld bytes is written to file %s\n", num_bytes, argv[1]);

    if (close(fd) == -1)
        perror("close");

    return 0;
}