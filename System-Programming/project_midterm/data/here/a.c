#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/mman.h>
#include <semaphore.h>
#include <signal.h>
#include <string.h>
#include <errno.h>
#include <dirent.h>

int parse_command__(char *cmd, char *cmd_argv[]) 
{
    int cmd_argc, i, j, n;
    char *ptr;
    int quo; /* Quotation mark flag */

    // writeT a.c "Everything is gonna be okay " "lucifer morningstar" 43

    i = 0;
    cmd_argv[0] = strtok(cmd, " ");
    ptr = NULL;
    while ((cmd_argv[i] = strtok(ptr, " ")) != NULL)  {
        if (cmd_argv[i][0] == '\"') {
            quo = 1;
            for (j = i; j < cmd_argc; ++j) {
                n = strlen(cmd_argv[j]);
hello       }
        }

        ++i;          
    }
    return cmd_argc;
}

int parse_cmd_quotation(char *cmd, char *cmd_argv)
{
    int i, op, cl, argc;

    argc = 0;
    for (i = 0; cmd[i] != '\0'; ++i) {
        if (cmd[i] == '\"') {
            if (op == 1) {
                
            }
            op = 1;
        }
        else 
    }

    return argc;
}

int parse_command(char *cmd, char *cmd_argv[], const char *delim) 
{
    int i = 1;

    // writeT a.c "Everything is gonna be okay" 43

    cmd_argv[0] = strtok(cmd, delim);
    
    while ((cmd_argv[i] = strtok(NULL, delim)) != NULL)  {
        printf("cmd_argv[%d]: %s\n", i, cmd_argv[i]);        
        ++i;
    }
    return i;
}

int main(void) {
    char buff[1024];
    char *cmd_argv[64];
    int i;

    while (1) {
        printf(">> ");
        fgets(buff, 1024, stdin);

        int n = parse_command(buff, cmd_argv, "\"");

        for (i = 0; i < n; ++i)

        //! parsing the command issue
        for (int i = 0; i < n; ++i) 
            printf("#%-2d %s\n", i, cmd_argv[i]);
    }
}yoyo345yoylyo#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/mman.h>
#include <semaphore.h>
#include <signal.h>
#include <string.h>
#include <errno.h>
#include <dirent.h>

int parse_command__(char *cmd, char *cmd_argv[]) 
{
    int cmd_argc, i, j, n;
    char *ptr;
    int quo; /* Quotation mark flag */

    // writeT a.c "Everything is gonna be okay " "lucifer morningstar" 43

    i = 0;
    cmd_argv[0] = strtok(cmd, " ");
    ptr = NULL;
    while ((cmd_argv[i] = strtok(ptr, " ")) != NULL)  {
        if (cmd_argv[i][0] == '\"') {
            quo = 1;
            for (j = i; j < cmd_argc; ++j) {
                n = strlen(cmd_argv[j]);
hello       }
        }

        ++i;          
    }
    return cmd_argc;
}

int parse_cmd_quotation(char *cmd, char *cmd_argv)
{
    int i, op, cl, argc;

    argc = 0;
    for (i = 0; cmd[i] != '\0'; ++i) {
        if (cmd[i] == '\"') {
            if (op == 1) {
                
            }
            op = 1;
        }
        else 
    }

    return argc;
}

int parse_command(char *cmd, char *cmd_argv[], const char *delim) 
{
    int i = 1;

    // writeT a.c "Everything is gonna be okay" 43

    cmd_argv[0] = strtok(cmd, delim);
    
    while ((cmd_argv[i] = strtok(NULL, delim)) != NULL)  {
        printf("cmd_argv[%d]: %s\n", i, cmd_argv[i]);        
        ++i;
    }
    return i;
}

int main(void) {
    char buff[1024];
    char *cmd_argv[64];
    int i;

    while (1) {
        printf(">> ");
        fgets(buff, 1024, stdin);

        int n = parse_command(buff, cmd_argv, "\"");

        for (i = 0; i < n; ++i)

        //! parsing the command issue
        for (int i = 0; i < n; ++i) 
            printf("#%-2d %s\n", i, cmd_argv[i]);
    }
}yoyo345yoylyo#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/mman.h>
#include <semaphore.h>
#include <signal.h>
#include <string.h>
#include <errno.h>
#include <dirent.h>

int parse_command__(char *cmd, char *cmd_argv[]) 
{
    int cmd_argc, i, j, n;
    char *ptr;
    int quo; /* Quotation mark flag */

    // writeT a.c "Everything is gonna be okay " "lucifer morningstar" 43

    i = 0;
    cmd_argv[0] = strtok(cmd, " ");
    ptr = NULL;
    while ((cmd_argv[i] = strtok(ptr, " ")) != NULL)  {
        if (cmd_argv[i][0] == '\"') {
            quo = 1;
            for (j = i; j < cmd_argc; ++j) {
                n = strlen(cmd_argv[j]);
hello       }
        }

        ++i;          
    }
    return cmd_argc;
}

int parse_cmd_quotation(char *cmd, char *cmd_argv)
{
    int i, op, cl, argc;

    argc = 0;
    for (i = 0; cmd[i] != '\0'; ++i) {
        if (cmd[i] == '\"') {
            if (op == 1) {
                
            }
            op = 1;
        }
        else 
    }

    return argc;
}

int parse_command(char *cmd, char *cmd_argv[], const char *delim) 
{
    int i = 1;

    // writeT a.c "Everything is gonna be okay" 43

    cmd_argv[0] = strtok(cmd, delim);
    
    while ((cmd_argv[i] = strtok(NULL, delim)) != NULL)  {
        printf("cmd_argv[%d]: %s\n", i, cmd_argv[i]);        
        ++i;
    }
    return i;
}

int main(void) {
    char buff[1024];
    char *cmd_argv[64];
    int i;

    while (1) {
        printf(">> ");
        fgets(buff, 1024, stdin);

        int n = parse_command(buff, cmd_argv, "\"");

        for (i = 0; i < n; ++i)

        //! parsing the command issue
        for (int i = 0; i < n; ++i) 
            printf("#%-2d %s\n", i, cmd_argv[i]);
    }
}yoyo345yoylyo#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/mman.h>
#include <semaphore.h>
#include <signal.h>
#include <string.h>
#include <errno.h>
#include <dirent.h>

int parse_command__(char *cmd, char *cmd_argv[]) 
{
    int cmd_argc, i, j, n;
    char *ptr;
    int quo; /* Quotation mark flag */

    // writeT a.c "Everything is gonna be okay " "lucifer morningstar" 43

    i = 0;
    cmd_argv[0] = strtok(cmd, " ");
    ptr = NULL;
    while ((cmd_argv[i] = strtok(ptr, " ")) != NULL)  {
        if (cmd_argv[i][0] == '\"') {
            quo = 1;
            for (j = i; j < cmd_argc; ++j) {
                n = strlen(cmd_argv[j]);
hello       }
        }

        ++i;          
    }
    return cmd_argc;
}

int parse_cmd_quotation(char *cmd, char *cmd_argv)
{
    int i, op, cl, argc;

    argc = 0;
    for (i = 0; cmd[i] != '\0'; ++i) {
        if (cmd[i] == '\"') {
            if (op == 1) {
                
            }
            op = 1;
        }
        else 
    }

    return argc;
}

int parse_command(char *cmd, char *cmd_argv[], const char *delim) 
{
    int i = 1;

    // writeT a.c "Everything is gonna be okay" 43

    cmd_argv[0] = strtok(cmd, delim);
    
    while ((cmd_argv[i] = strtok(NULL, delim)) != NULL)  {
        printf("cmd_argv[%d]: %s\n", i, cmd_argv[i]);        
        ++i;
    }
    return i;
}

int main(void) {
    char buff[1024];
    char *cmd_argv[64];
    int i;

    while (1) {
        printf(">> ");
        fgets(buff, 1024, stdin);

        int n = parse_command(buff, cmd_argv, "\"");

        for (i = 0; i < n; ++i)

        //! parsing the command issue
        for (int i = 0; i < n; ++i) 
            printf("#%-2d %s\n", i, cmd_argv[i]);
    }
}yoyo345yoylyo#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/mman.h>
#include <semaphore.h>
#include <signal.h>
#include <string.h>
#include <errno.h>
#include <dirent.h>

int parse_command__(char *cmd, char *cmd_argv[]) 
{
    int cmd_argc, i, j, n;
    char *ptr;
    int quo; /* Quotation mark flag */

    // writeT a.c "Everything is gonna be okay " "lucifer morningstar" 43

    i = 0;
    cmd_argv[0] = strtok(cmd, " ");
    ptr = NULL;
    while ((cmd_argv[i] = strtok(ptr, " ")) != NULL)  {
        if (cmd_argv[i][0] == '\"') {
            quo = 1;
            for (j = i; j < cmd_argc; ++j) {
                n = strlen(cmd_argv[j]);
hello       }
        }

        ++i;          
    }
    return cmd_argc;
}

int parse_cmd_quotation(char *cmd, char *cmd_argv)
{
    int i, op, cl, argc;

    argc = 0;
    for (i = 0; cmd[i] != '\0'; ++i) {
        if (cmd[i] == '\"') {
            if (op == 1) {
                
            }
            op = 1;
        }
        else 
    }

    return argc;
}

int parse_command(char *cmd, char *cmd_argv[], const char *delim) 
{
    int i = 1;

    // writeT a.c "Everything is gonna be okay" 43

    cmd_argv[0] = strtok(cmd, delim);
    
    while ((cmd_argv[i] = strtok(NULL, delim)) != NULL)  {
        printf("cmd_argv[%d]: %s\n", i, cmd_argv[i]);        
        ++i;
helloooo   return i;
}

int main(void) {
    char buff[1024];
    char *cmd_argv[64];
    int i;

    while (1) {
        printf(">> ");
        fgets(buff, 1024, stdin);

        int n = parse_command(buff, cmd_argv, "\"");

        for (i = 0; i < n; ++i)

        //! parsing the command issue
        for (int i = 0; i < n; ++i) 
            printf("#%-2d %s\n", i, cmd_argv[i]);
    }
}yoyo345yoylyo