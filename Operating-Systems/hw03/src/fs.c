#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "fs.h"

struct filesystem *fs_create(int block_size)
{
    int i;
    struct filesystem *fs;
    
    fs = (struct filesystem *) malloc(sizeof(struct filesystem));
    if (fs == NULL)
        return NULL;

    /* Initialize the superblock */
    fs->sb.block_size = block_size;
    fs->sb.num_blocks = NUM_ENTRY; 
    fs->sb.fat_position = sizeof(struct superblock);
    fs->sb.root_directory_position = fs->sb.fat_position +  (NUM_ENTRY * sizeof(struct fat_entry));
    fs->sb.data_block_position = fs->sb.root_directory_position +  (NUM_ENTRY * sizeof(struct directory_entry));

    for (i = 0; i < fs->sb.num_blocks; i++) {
        /* Mark all FAT entries as free */
        fs->fat[i].value = 0x000;  

        /* Mark all directory entries as unused */
        fs->directory[i].filename[0] = '\0';  
        fs->directory[i].attributes = '0';
        fs->directory[i].file_size = 0;
        fs->directory[i].last_modified_date = 0;
        fs->directory[i].last_modified_time = 0;
        fs->directory[i].starting_block = 0;

        /* Initialize all the disk blocks */
        fs->dbs[i].next_disk_block = -1;
        fs->dbs[i].data = (char *) malloc(fs->sb.block_size);
        if (fs->dbs[i].data == NULL) {
            while (i > 0) {
                --i;
                free(fs->dbs[i].data);
            }
            free(fs);
            return NULL;
        }
    }

    return fs;
}

int fs_sync(const struct filesystem *fs, const char *path)
{
    FILE * file;
    int i;

    file = fopen(path, "w");
    if (file == NULL)
        return -1;

    fwrite(fs, sizeof(struct superblock), 1, file);
    fwrite(fs->fat, sizeof(struct fat_entry), fs->sb.num_blocks, file);
    fwrite(fs->directory, sizeof(struct directory_entry), fs->sb.num_blocks, file);
    for (i = 0; i < fs->sb.num_blocks; ++i) {
        fwrite(&(fs->dbs->next_disk_block), sizeof(int), 1, file);
        fwrite(fs->dbs, fs->sb.block_size, 1, file);
    }

    return 0;
}

int fs_mount(struct filesystem *fs, const char *path)
{
    FILE * file;
    int i;

    file = fopen(path, "r");
    if (file == NULL)
        return -1;

    fread(fs, sizeof(struct superblock), 1, file);
    fread(fs->fat, sizeof(struct fat_entry), fs->sb.num_blocks, file);
    fread(fs->directory, sizeof(struct directory_entry), fs->sb.num_blocks, file);
    for (i = 0; i < fs->sb.num_blocks; ++i) {
        fread(&(fs->dbs->next_disk_block), sizeof(int), 1, file);
        fread(fs->dbs, fs->sb.block_size, 1, file);
    }
    fclose(file);
    return 0;
}

void fs_free(struct filesystem *fs)
{
    int i;

    /* Free the data blocks */
    for (i = 0; i < fs->sb.num_blocks; ++i)
        free(fs->dbs[i].data);
    free(fs);
}

void fs_print(const struct filesystem *fs)
{
    int i;
    printf("superblock info\n");
    printf("\tfat position              : %d\n", fs->sb.fat_position);
    printf("\troot directory position   : %d\n", fs->sb.root_directory_position);
    printf("\tdata block position       : %d\n", fs->sb.data_block_position);
    printf("\tnum blocks                : %d\n", fs->sb.num_blocks);
    printf("\tblock size                : %d\n", fs->sb.block_size);

    printf("fat entries:\n");
    for (i = 0; i < fs->sb.num_blocks; ++i) {
        printf("\tentry no: %d value: ", i);
        switch (fs->fat[i].value) {
            case 0x000:
                printf("free\n");
                break;
            case 0xFF8:
                printf("bad cluster\n");
                break;
            case 0xFF7:
                printf("EOF\n");
                break; 
            default:
                printf("\n");
                break;
        }
    }

    printf("directory entries:\n");
    for (i = 0; i < fs->sb.num_blocks; ++i) {
        printf("\tentry no: %d name: %s attr: %c size: %d last modf: %d starting cluster %d\n",
            i,
            fs->directory[i].filename,
            fs->directory[i].attributes, 
            fs->directory[i].file_size,
            fs->directory[i].last_modified_date,
            fs->directory[i].starting_block);
    }
}

/*
void create_file(struct filesystem *fs, const char *filename)
{
    // Check if the file already exists
    // Find a free directory entry and allocate it
    // Find consecutive free clusters in the FAT and allocate them to the file
    // Update the directory entry with the file's attributes and starting cluster
}

void delete_file(struct filesystem *fs, const char *filename)
{
    // Find the directory entry corresponding to the file
    // Free the clusters allocated to the file in the FAT
    // Mark the directory entry as unused
}

void read_file(struct filesystem *fs, const char *filename, char *buffer, int size)
{
    // Find the directory entry corresponding to the file
    // Read the file data from the clusters allocated to the file using the FAT
    // Copy the data into the buffer
}

void write_file(struct filesystem *fs, const char *filename, const char *data, int size)
{
    // Find the directory entry corresponding to the file
    // Write the data to the clusters allocated to the file using the FAT
    // Update the file size and modification time in the directory entry
}

void list_directory(struct filesystem *fs)
{
    // Iterate through the directory entries and retrieve their attributes
    // Print the file names and other details
}
*/