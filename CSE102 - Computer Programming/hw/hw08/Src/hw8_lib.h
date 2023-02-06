/*
** hw8_lib.h:
**
** The header file declaring library functions.
**
** Author: Yakup Genc. (c) 2018-2021
**
** Revision: 2021.04.22.19.55
** 
*/

#define WORDSIZE 17		
#define LINESIZE 1000

#define MAZESIZE 8
#define MAXDISK 7
typedef enum {cell_wall, cell_free, cell_target, cell_p1, cell_p2} cell_type;
typedef enum {move_left, move_right, move_up, move_down} move_type;


void clean_file(char *infile, char *outfile, char *words_to_delete[WORDSIZE], int number_of_words);
void write (char text[], char *words_to_delete[WORDSIZE],  int number_of_words, FILE *outfid);
void delete_words (FILE * infid, FILE * outfid, char * words_to_delete[WORDSIZE], int number_of_words);
int  wlen(char *s);
int  strdiff (char s1[], char s2[]);
int  is_inside (char *word, char *source[WORDSIZE], int n);

int maze_move(cell_type maze[][MAZESIZE], cell_type player, move_type move);
int left_movement (cell_type maze[][MAZESIZE], cell_type player);
int right_movement (cell_type maze[][MAZESIZE], cell_type player);
int up_movement (cell_type maze[][MAZESIZE], cell_type player);
int down_movement (cell_type maze[][MAZESIZE], cell_type player);
int find_location (cell_type maze[][MAZESIZE], cell_type player, int *nrow, int *nclo);
int is_in_range (int min, int max, int n);


void towers_of_hanoi(char start_peg, char end_peg, char aux_peg, int n);
void print_step (char start_peg, char end_peg, char aux_peg, int n);
void tower (int S[MAXDISK], int E[MAXDISK], int A[MAXDISK], int step, int n);
void print(int arr[], int n);
void init_zero (int arr[], int n);
void initialize (int arr[], int n);
void move (int *start_peg, int *end_peg);
