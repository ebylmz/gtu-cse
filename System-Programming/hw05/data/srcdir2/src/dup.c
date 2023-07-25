#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <errno.h>

#include <string.h>
#include <stdio.h>

int dup(int oldfd) {
    if (fcntl(oldfd, F_GETFL) == -1) {
        /* If oldfd is not valid, then the function should return –1 with errno set to EBADF. */
        printf("oldfd is not valid, dup() sets errno EBADF and returns -1\n");
        errno = EBADF; /* indicates fd is not an open file descriptor */
        return -1;
    }
    return fcntl(oldfd, F_DUPFD, 0);
}

int dup2(int oldfd, int newfd) {
    /* 
        The steps of closing and reusing the file descriptor newfd shoudl be performed atomically.
        Otherwise there could occur race conditions, whereby newfd might be reused between the two steps
    */
    
    if (fcntl(oldfd, F_GETFL) == -1) {
        /* If oldfd is not valid, then the function should return –1 with errno set to EBADF. */
        printf("oldfd is not valid, dup2() sets errno EBADF and returns -1\n");
        errno = EBADF; /* indicates fd is not an open file descriptor */
        return -1;
    }
    else if (oldfd == newfd) {
        /* If oldfd is a valid file descriptor, and newfd has the same value as oldfd, then dup2() does nothing, and returns newfd. */
        printf("oldfd and newfd have the same value, dup2() does nothing and returns newfd\n");
        return newfd;
    }
    else if (fcntl(newfd, F_GETFL) != -1) {
        /* If the file descriptor specified in newfd is already open, dup2() closes it first. */
        printf("File descriptor %d was previosly open, it is closed before being reused\n", newfd);
        printf("Closing...\n");
        if (close(newfd) == -1) {
            perror("close");
            return -1;
        }
        printf("Closed\n");
    }

    return fcntl(oldfd, F_DUPFD, newfd);
}

int test_dup() {
    char * fname = "test1.txt";
    int oldfd, resultfd;

    printf("TEST: dup\n");
    printf("---------------------------\n\n");

    printf("test case: oldfd oldfd is valid.\n");
    printf("opening file %s...\n", fname);

    oldfd = open(fname, O_RDWR | O_CREAT | O_APPEND, S_IRUSR | S_IWUSR);    
    if (oldfd == -1) {
        perror("open");
        return -1;
    }

    printf("%d was taken as file descriptor for file %s, keep it inside oldfd\n", oldfd, fname);

    printf("oldfd: %d\n", oldfd);
    printf("call dup(oldfd)\n");
    printf("duplicating...\n");
    resultfd = dup(oldfd);
    printf("dup returned %d\n\n", resultfd);


    printf("test case: oldfd is not valid.\n");
    printf("closing file %s...\n", fname);
       
    if (close(oldfd) == -1) {
        perror("close");
        return -1;
    }
    printf("now oldfd is invalid\n");
    printf("call dup(oldfd)\n");
    printf("duplicating...\n");
    resultfd = dup(oldfd);
    printf("dup returned %d\n\n", resultfd);

    return 0;
}

int test_dup2() {
    char * fname1 = "test1.txt";
    char * fname2 = "test2.txt";
    int oldfd, newfd, resultfd;

    printf("TEST: dup2\n");
    printf("---------------------------\n\n");

    printf("test case: oldfd and newfd are different and oldfd is valid.\n");
    printf("opening file %s...\n", fname1);

    oldfd = open(fname1, O_RDWR | O_CREAT | O_APPEND, S_IRUSR | S_IWUSR);    
    if (oldfd == -1) {
        perror("open");
        return -1;
    }

    newfd = 7;
    printf("%d was taken as file descriptor for file %s, keep it inside oldfd\n", oldfd, fname1);
    printf("let newfd become %d\n", newfd);

    printf("oldfd: %d, newfd: %d\n", oldfd, newfd);
    printf("call dup2(oldfd, newfd)\n");
    printf("duplicating...\n");
    resultfd = dup2(oldfd, newfd);
    printf("dup2 returned %d\n\n", resultfd);

    printf("test case: oldfd and newfd are same and oldfd is valid.\n");
    printf("let newfd become same as oldfd\n");
    newfd = oldfd;
    printf("oldfd: %d, newfd: %d\n", oldfd, newfd);
    printf("call dup2(oldfd, newfd)\n");
    printf("duplicating...\n");
    resultfd = dup2(oldfd, newfd);
    printf("dup2 returned %d\n\n", resultfd);

    printf("test case: newfd is already open.\n"); 
    printf("opening file %s...\n", fname2);
    newfd = open(fname2, O_RDWR | O_CREAT | O_APPEND, S_IRUSR | S_IWUSR);    
    if (newfd == -1) {
        perror("open");
        return -1;
    }

    printf("%d was taken as file descriptor for file %s, keep it inside newfd\n", newfd, fname2);
    printf("oldfd: %d, newfd: %d\n", oldfd, newfd);
    printf("call dup2(oldfd, newfd)\n");
    printf("duplicating...\n");
    resultfd = dup2(oldfd, newfd);
    printf("dup2 returned %d\n\n", resultfd);

    printf("test case: oldfd is not valid.\n");
    printf("closing file %s...\n", fname1);
       
    if (close(oldfd) == -1) {
        perror("close");
        return -1;
    }
    printf("now oldfd is invalid\n");
    printf("call dup2(oldfd, newfd)\n");
    printf("duplicating...\n");
    resultfd = dup2(oldfd, newfd);
    printf("dup2 returned %d\n\n", resultfd);

    if (close(newfd) == -1) {
        perror("close");
        return -1;
    }

    return 0;
}

int main(void) {
    test_dup();
    test_dup2();
}