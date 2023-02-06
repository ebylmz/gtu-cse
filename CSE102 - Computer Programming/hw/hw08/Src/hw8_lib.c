/*
** hw8_lib.c:
**
** The source file implementing library functions.
**
** Author: Yakup Genc. (c) 2018-2021
**
** Revision: 2021.04.22.19.55
** 
*/

#include <stdio.h>
#include "hw8_lib.h"

void clean_file(char *infile, char *outfile, char *words_to_delete[WORDSIZE], int number_of_words)
{
	int 	error = 0;
	FILE 	*infid,
			*outfid;

	if (infid = fopen(infile, "r"))
	{
		if (outfid = fopen(outfile, "w"))
		{
			/* prints infile to the outfile, with deleting unwanted words(words_to_delete) */
			delete_words(infid, outfid, words_to_delete, number_of_words);

			fclose(infid);
			fclose(outfid);
		}
		else
			error = 1;
	}
	else
		error = 1;
	

	if (error)
		printf("\n[!] Source file cannot find or open properly\n");
	else
		printf("\n[>] Correction has been successfully completed\n");
}

/* 	It will read the input file one line at a time, remove the occurrences 
	of the words in the given list (words_to_delete) and write
	the resulting new string as a line in the output file. 					*/
void delete_words (FILE * infid, FILE * outfid, char * words_to_delete[WORDSIZE], int number_of_words)
{
	char text[LINESIZE];

	/* continue recursively by the end of the file (EOF) */
	if (fgets(text, LINESIZE, infid) != NULL)
	{
		/* 	remove the occurrences of the words in the given list
			and write as a line in the output file. 				*/
		write(text, words_to_delete, number_of_words, outfid);

		delete_words(infid, outfid, words_to_delete, number_of_words);
	}
}


void write (char *text, char *words_to_delete[WORDSIZE],  int n, FILE *outfid)
{
	char w[WORDSIZE];
	
	/* scan the first word in the scanned line (stored as text) */	
	sscanf(text, "%s", w);

	/* continue untill reached the end of the line or string */
	if (text[0] != '\n' && text[0] != '\0')
	{
		/* check if scanned word matchs any word in given list */
		if (! is_inside(w, words_to_delete, n))			
			fprintf(outfid, "%s ", w);		/* add space after every word */
		
		/* continue with next word by moved the pointer as much as length of the word */
		write(&text[wlen(w) + 1], words_to_delete, n, outfid);
	}
}

/* finds lenght of the given string or word */
int wlen(char *s)
{
	int ans;

	if (s[0] == '\0' || s[0] == ' ')
		ans = 0;
	else
		ans = 1 + wlen(&s[1]);

	return ans;
}

/* finds the letter difference of given two string */
int strdiff (char s1[], char s2[])
{
	int diff;

	/* when reaching the base case, initialize the diff as differences of length of s1 and s2 */
	if (s1[0] == '\0' || s2[0] == '\0')
		diff = wlen(s1) + wlen(s2);		/* one of s1 or s2 is 0 */
	else
	{
		diff = strdiff(&s1[1], &s2[1]);
		
		if (s1[0] != s2[0])
			++diff;
	} 

	return diff;
}

/* checks if given word is inside in given source */
int is_inside (char *word, char *source[WORDSIZE], int n)
{
	int ans;

	if (n == 0)
		ans = 0;
	else if (strdiff(word, source[0]) == 0 || strdiff(word, source[0]) == 1)
		ans = 1;
	else
		ans = is_inside(word, &source[1], n - 1);

	return ans;	
}



int maze_move(cell_type maze[][MAZESIZE], cell_type player, move_type move)
{
	int nrow = 0, nclo = 0;
	int status = 0;

	/* find the current position of player */
	if (find_location(maze, player, &nrow, &nclo))
	{
		/* check if the movement valid */
		switch (move)
		{
			case move_left:
						status = left_movement(maze, player);
						break;

			case move_right:
						status = right_movement(maze, player);
						break;

			case move_up:
						status = up_movement(maze, player);
						break;

			case move_down:
						status = down_movement(maze, player);
						break;
			default: 
						printf("[!] invalid movement\n");
		}
	}
	/* check if we reach there */
	if (status == 2)
		printf("Maze is solvedd\n");

	return status;
}

int find_location (cell_type maze[][MAZESIZE], cell_type player, int *r, int *c)
{
	int find;

	if (is_in_range(0, MAZESIZE - 1, *r) && is_in_range(0, MAZESIZE - 1, *c))
	{
		if (maze[*r][*c] == player)
			find = 1;
		else
		{
			if (*c == MAZESIZE - 1)
			{
				++(*r);
				*c = 0;
			}
			else
				++(*c);

			find = find_location(maze, player, r, c);
		}
	}
	else
		find = 0;

	return find;
}

int left_movement (cell_type maze[][MAZESIZE], cell_type player)
{
	int 	r = 0, c = 0,
			retval;

	if (find_location(maze, player, &r, &c) && c > 0)
	{
		switch (maze[r][c - 1])
		{
			case cell_target:
						retval = 2;
						maze[r][c] = cell_target;
						maze[r][c - 1] = player;
						break;

			case cell_free:
						retval = 1;
						maze[r][c] = cell_free;
						maze[r][c - 1] = player;
						break;

			case cell_wall:
						retval = 0;
						break;
		}
	}

	return retval;
}

int right_movement (cell_type maze[][MAZESIZE], cell_type player)
{
	int 	r = 0, c = 0,
		 	retval;

	if (find_location(maze, player, &r, &c) && c < MAZESIZE - 1)
	{
		switch (maze[r][c + 1])
		{
			case cell_target:
						retval = 2;
						maze[r][c] = cell_target;
						maze[r][c + 1] = player;
						break;

			case cell_free:
						retval = 1;
						maze[r][c] = cell_free;
						maze[r][c + 1] = player;
						break;

			case cell_wall:
						retval = 0;
						break;
		}
	}

	return retval;
}

int up_movement (cell_type maze[][MAZESIZE], cell_type player)
{
		int 	r = 0, c = 0,
		 		retval;

	if (find_location(maze, player, &r, &c) && r > 0)
	{
		switch (maze[r - 1][c])
		{
			case cell_target:
						retval = 2;
						maze[r][c] = cell_target;
						maze[r - 1][c] = player;
						break;

			case cell_free:
						retval = 1;
						maze[r][c] = cell_free;
						maze[r - 1][c] = player;
						break;

			case cell_wall:
						retval = 0;
						break;
		}
	}

	return retval;
}

int down_movement (cell_type maze[][MAZESIZE], cell_type player)
{
	int 	r = 0, c = 0,
		 	retval;

	if (find_location(maze, player, &r, &c) && r < MAZESIZE - 1)
	{
		switch (maze[r + 1][c])
		{
			case cell_target:
						retval = 2;
						maze[r][c] = cell_target;
						maze[r + 1][c] = player;
						break;

			case cell_free:
						retval = 1;
						maze[r][c] = cell_free;
						maze[r + 1][c] = player;
						break;

			case cell_wall:
						retval = 0;
						break;
		}
	}

	return retval;
}

int is_in_range (int min, int max, int n)
{
	return (min <= n && n <= max);
}


void towers_of_hanoi (char start_peg, char end_peg, char aux_peg, int n)
{
	int S[MAXDISK], E[MAXDISK], A[MAXDISK];

	initialize(S, n);
	init_zero(E, MAXDISK), init_zero(A, MAXDISK);

	if (n == 1)
	{
		/*printf("Move disk 1 from peg %c to peg %c\n", start_peg, end_peg); */
	}
	else
	{
		towers_of_hanoi(start_peg, aux_peg, end_peg, n - 1);

		/*tower(S, A, E, pow(2, n) - 1, n - 1); */
		
		/*printf("Move disk %d from peg %c to peg %c\n", n, start_peg, end_peg);*/
		
		towers_of_hanoi(aux_peg, end_peg, start_peg, n - 1); 
	}
}

void tower (int S[MAXDISK], int E[MAXDISK], int A[MAXDISK], int step, int n)
{	
	if (step > 1)
	{
		if (n == 1)
		{
			move(S, E);
		}
		else
		{
			tower(S, A, E, step - 1, n - 1);
			move(S, E);

			tower(A, E, S, step -1, n - 1);
		}
	}
}


void move (int *start_peg, int *end_peg)
{
	int val;

	/* move disk from peg to roher peg */
	pop(start_peg, MAXDISK, &val);

	push(end_peg, MAXDISK, val);
}

void print_step (char start_peg, char end_peg, char aux_peg, int n)
{

}

int push (int arr[], int n, int val)
{
	int retval;

	if (n == 0)
	{
		retval = 0;
	}
	else if (arr[0] == 0)
	{
		arr[0] = val;
		arr[1] = 0;
		retval = 1; 
	}
	else
		retval = push(&arr[1], n - 1, val);

	return retval;
} 


int pop (int arr[], int n, int *val)
{
	int retval;

	if (n == 0)
		retval = 0;
	else if (arr[0] == 0)
	{
		*val = arr[-1];
		arr[-1] = 0;

		retval = 1;
	}
	else
		retval = pop(&arr[1], n - 1, val); 

	return retval;	
}

void init_zero (int arr[], int n)
{
	if (n > 0)
	{
		arr[0] = 0;
		init_zero(&arr[1], n - 1);
	}
}

void initialize (int arr[], int n)
{
	int i;

	for (i = 0; i < n; ++i)
		arr[i] = i + 1;
	arr[i] = 0;
}

void print(int arr[], int n)
{
	int i;

	for (i = 0; i < n; ++i)
		printf("%d ", arr[i]);
	printf("\n");
}