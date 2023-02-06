#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*  Example of reading and writing string */

int main (void) {
    char str1[30] = "Emirkan Burak Yilmaz";
    char str2[30] = "Mary Jane";
    FILE * fid; 
    int i;

    fid = fopen("binary-file.bin", "wb");
    if (fid != NULL) {
        /* Don't forget to add +1 to strlen o.w. '\0' is forgetten */
        fwrite(str1, sizeof(char), strlen(str1) + 1, fid);
        fwrite(str2, sizeof(char), strlen(str2) + 1, fid);
        fclose(fid);

        printf("str1: %s\n", str1);
        printf("str2: %s\n", str2);
    } 

    fid = fopen("a.bin", "rb");
    if (fid != NULL) {
        i = 0;
        do fread(str2 + i, sizeof(char), 1, fid);
        while (str2[i++] != '\0');

        i = 0;
        do fread(str1 + i, sizeof(char), 1, fid);
        while (str1[i++] != '\0');

        fclose(fid);

        printf("str1: %s\n", str1);
        printf("str2: %s\n", str2);
    }
}