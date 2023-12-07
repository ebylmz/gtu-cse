#include <unistd.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

void print_file_stat(int fd, struct stat fstat) {
    printf("Information for File Descriptor %d\n", fd);
    printf("----------------------------------\n");
    printf("%-20s %ld bytes\n", "File Size:", fstat.st_size);
    printf("%-20s %ld\n", "Number of Links:", fstat.st_nlink);
    printf("%-20s %ld\n", "File inode:", fstat.st_ino);

    printf("%-20s ", "File Permissions:");
    printf((S_ISDIR(fstat.st_mode)) ? "d" : "-");
    printf((fstat.st_mode & S_IRUSR) ? "r" : "-");
    printf((fstat.st_mode & S_IWUSR) ? "w" : "-");
    printf((fstat.st_mode & S_IXUSR) ? "x" : "-");
    printf((fstat.st_mode & S_IRGRP) ? "r" : "-");
    printf((fstat.st_mode & S_IWGRP) ? "w" : "-");
    printf((fstat.st_mode & S_IXGRP) ? "x" : "-");
    printf((fstat.st_mode & S_IROTH) ? "r" : "-");
    printf((fstat.st_mode & S_IWOTH) ? "w" : "-");
    printf((fstat.st_mode & S_IXOTH) ? "x" : "-");
    printf("\n");
    printf("This file %s a symbolic link\n\n", (S_ISLNK(fstat.st_mode)) ? "is" : "is not");
}

int main(void) {
    char * fname = "test1.txt";
    int fd1, fd2; 
    int stat_flag1, stat_flag2;
    int offset1, offset2;
    struct stat file_stat1, file_stat2;

    printf("Open file %s...\n", fname);

    fd1 = open(fname, O_RDWR | O_CREAT | O_APPEND, S_IRUSR | S_IWUSR);
    if (fd1 == -1) {
        perror("open");
        return -1;
    }

    printf("%d was taken as file descritor for file %s, and it became first fd\n", fd1, fname);
    printf("call dup(fd1)\n");
    printf("duplicating...\n");

    fd2 = dup(fd1);
    if (fd1 == -1) {
        perror("dup");
        return -1;
    }

    printf("dup returned %d, and it became second fd\n\n", fd2);

    /* get the offset of fds */
    offset1 = lseek(fd1, 0, SEEK_CUR);
    offset2 = lseek(fd2, 0, SEEK_CUR);

    printf("Offset for fd %d: %d\n", fd1, offset1);
    printf("Offset for fd %d: %d\n", fd2, offset2);

    printf("Write operations are performing...\n");

    /* make write operation  */
    write(fd1, "Written by fd1\n", 15);
    write(fd2, "Written by fd2\n", 15);

    /* get the offset of fds */
    offset1 = lseek(fd1, 0, SEEK_CUR);
    offset2 = lseek(fd2, 0, SEEK_CUR);

    printf("Offset for fd %d: %d\n", fd1, offset1);
    printf("Offset for fd %d: %d\n", fd2, offset2);

    if (offset1 == offset2)
        printf("Two file descriptor shares the same offset value\n\n");
    else
        printf("Two file descriptor have different offset value\n\n");

    stat_flag1 = fcntl(fd1, F_GETFL);
    stat_flag2 = fcntl(fd2, F_GETFL);

    printf("Flags for fd %d: %d\n", fd1, stat_flag1);
    printf("Flags for fd %d: %d\n", fd2, stat_flag2);

    if (stat_flag1 == stat_flag2)
        printf("Two file descriptor shares the same status flags\n\n");
    else
        printf("Two file descriptor have different status flags\n\n");

    if (fstat(fd1, &file_stat1) == -1) {
        perror("fstat");
        return 1;
    }

    if (fstat(fd2, &file_stat2) == -1) {
        perror("fstat");
        return 1;
    }

    printf("inode for fd %d: %ld\n", fd1, file_stat1.st_ino);
    printf("inode for fd %d: %ld\n", fd2, file_stat2.st_ino);

    if (file_stat1.st_ino == file_stat2.st_ino)
        printf("Two file descriptor share the same file, since both use the same inode\n\n");
    else
        printf("Two file descriptor does not share the same file, since each of them use different inodes\n\n");

    printf("Detailed information about file(s) for two file descriptor:\n");
    print_file_stat(fd1, file_stat1);
    print_file_stat(fd2, file_stat2);

    close(fd1);
}