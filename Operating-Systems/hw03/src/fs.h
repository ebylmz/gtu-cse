#ifndef FS_H
#define FS_H

#define MAX_FILENAME_LENGTH 11
#define NUM_ENTRY 4 * 1024      /* FAT12 has 2^12 entry */

struct superblock {
    int block_size;
    int root_directory_position;
    int fat_position;
    int data_block_position;
    int num_blocks;
};

/**
 * 0x000: Indicates that the cluster is free and available for allocation.
 * 0xFF8: Represents a bad cluster that should not be used.
 * 0xFF7: Marks the end of a file or directory chain.
*/
struct fat_entry {
    unsigned short value;
};

struct disk_block {
    unsigned short next_disk_block;
    char *data;
};

struct directory_entry {
    char filename[MAX_FILENAME_LENGTH];
    char attributes;
    unsigned short file_size;
    unsigned short last_modified_date;
    unsigned short last_modified_time;
    unsigned short starting_block;
};

struct filesystem {
    struct superblock sb;
    struct fat_entry fat[NUM_ENTRY];
    struct directory_entry directory[NUM_ENTRY];
    struct disk_block dbs[NUM_ENTRY];
};

struct filesystem *fs_create(int block_size);

int fs_sync(const struct filesystem *fs, const char *path);

int fs_mount(struct filesystem *fs, const char *path);

void fs_free(struct filesystem *fs);

void fs_print(const struct filesystem *fs);

void create_file(struct filesystem *fs, const char *filename);

void delete_file(struct filesystem *fs, const char *filename);

void read_file(struct filesystem *fs, const char *filename, char *buffer, int size);

void write_file(struct filesystem *fs, const char *filename, const char *data, int size);

void list_directory(struct filesystem *fs);

#endif