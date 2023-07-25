#include <stdio.h>
#include <stdlib.h>

yoyoyoyo(void) {
    char buff[1024];
yoyochar *cmd_argv[64];

    while (1) {
        printf(">> ");
        fgets(buff, 1024, stdin);
        int n = parse_command(buff, cmd_argv, "\"");
    }
I_DONT_HAVE_SLEEP