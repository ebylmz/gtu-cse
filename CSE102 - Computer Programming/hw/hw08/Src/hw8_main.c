/*
** main.c:
**
** The test/driver program for the homework.
**
** Author: Yakup Genc. (c) 2018-2021
**
** Revision: 2021.04.22.19.55
** 
*/


#include <stdio.h>
#include "hw8_lib.h"

void test_clean_file () 
{
	char 	infname[] = "infile.txt", 
			outfname[] = "outfile.txt";

	char	*words_to_delete[WORDSIZE] = {"Apple", "markets", "are", "Home", "Jobs"};
	int 	number_of_words = 5;

	char 	text[] = "An interview with Steve < Jobs > filmed on 2/18/1981 about the future of < Apple, > Computers, the < Home > & Personal computer < markets, > video games, and more.";
	FILE 	*infid;

	/* create sample text file */
	infid = fopen(infname, "w");
	fprintf(infid, "%s\n", text);
	fclose(infid);
	
	/* call text function */
	clean_file(infname, outfname, words_to_delete, number_of_words);
}


void test_maze_move ()
{
	int 		nrow = 0, nclo = 0;
 	cell_type 	maze[][8] = {	{cell_wall, cell_wall, cell_wall, cell_wall, cell_wall, cell_wall, cell_target, cell_wall},
								{cell_wall, cell_free, cell_free, cell_free, cell_wall, cell_free, cell_free, 	cell_wall},
								{cell_wall, cell_free, cell_wall, cell_free, cell_wall, cell_free, cell_wall, 	cell_wall},
								{cell_wall, cell_free, cell_wall, cell_free, cell_wall, cell_free, cell_free, 	cell_wall},
								{cell_wall, cell_free, cell_wall, cell_p1, 	 cell_free, cell_wall, cell_free, 	cell_wall},
								{cell_wall, cell_free, cell_wall, cell_wall, cell_free, cell_wall, cell_free, 	cell_wall},
								{cell_wall, cell_free, cell_free, cell_free, cell_free, cell_free, cell_free, 	cell_wall},
								{cell_wall, cell_wall, cell_wall, cell_wall, cell_wall, cell_wall, cell_wall, 	cell_wall}	};

	/* control movement */
	move_type movement = move_up;

	find_location(maze, cell_p1, &nrow, &nclo);
	printf("\n%-20s row: %d clo: %d\n", "Starting location", nrow, nclo);
		
	
	if (maze_move(maze, cell_p1, movement))
	{
		nrow = 0, nclo = 0;
		find_location(maze, cell_p1, &nrow, &nclo);
		
		printf("%-20s row: %d clo: %d\n", "New location", nrow, nclo);
	}
	else
		printf("invalid movement\n");
}


void test_towers_of_hanoi ()
{
	printf("\nI cannot have enough time to implemet this but i try at last two hour and there is same codes, please check them\n");
}


/*
** main function for testing the functions...
**
*/
int main(void) {
	test_clean_file ();
	test_maze_move ();
	test_towers_of_hanoi ();
	return (0);
} /* end main */
