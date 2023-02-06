#include <stdio.h>
#include <stdlib.h>

#define N 10		/* size of puzzle (N * N) 			*/

#define SENT -1		/* sentinel value for exit the game */

typedef enum { left, right, up, down } towards;

typedef enum { false, true } bool;

void take_movement (int *target, towards *mov);

void create_puzzle (int puzzle[][N]);

bool is_solved (int puzzle[][N]);

bool is_solvable (int puzzle[][N]);

void swap (int *x, int *y);

int inversion (int arr[][N], int size);

int sub_inver (int target_row, int target_clo, int arr[][N], int size);

bool apply_movement (int puzzle[][N], towards direction, int row, int clo);

void target_location (int arr[][N], int target, int *row, int *clo);

void print_puzzle (int puzzle[][N]);

void horizontal_line (char ch, int n);

void instructions ();

void clear_stdin ();



int main(void)
{
	int 		puzzle[N][N],
		 		target,				/* the desired number to move 					*/
		 		row, clo,	
		 		go = 1;				/* indicated the station of the game go or exit */
	char 		ans;				/* answer of exit the game 						*/
	towards 	mov;				/* movement as up, down, left, right		    */

	/* create array for puzzle, be sure it's not already solved */
	create_puzzle(puzzle);

	/* print the game instructions */
	instructions();

	/* print the initial puzzle */
	print_puzzle(puzzle);

	do
	{
		/* take movement */
		take_movement(&target, &mov);
		
		if (target != SENT)
		{
			/* find the indexes of given target number */
			target_location(puzzle, target ,&row, &clo);

			/* apply movement, if the desired movement is valid */
			if (apply_movement(puzzle, mov, row, clo))
			{
				/* update and  print the current puzzle */
				print_puzzle(puzzle);
				
				/* check if puzzle is solved, or user give up */
				if (is_solved(puzzle))
				{
					printf("\n[#] Congratulations !\n");
					
					/* terminate the game */
					go = 0;
				}
			}
			else
			{				
				printf("[!] invalid movement\n\n");
			}
		}
		else 
		{
			/* be sure user wants to terminate the game */
			printf("Are you sure you want to give up [Y/y]: ");
			scanf("%c", &ans);

			if (ans == 'Y' || ans == 'y')
			{
				/* terminate the game */
				printf("Game is over\n\n");
				go = 0;
			}
			else
			{
				printf("Game is continue\n");
			}
			/* clear the user remainder input */
			clear_stdin();
		}
	}
	while (go);
}


/* take the wished movement from the user */
void take_movement (int *target, towards *mov)
{
	int 		max = N * N,	/* max value in the puzzle 						 */
	 			status, error;	/* flag to indicate scanf status value and error */
	
	/* test values for movement */
	char 		t_movement;		
	int 		t_target; 
	towards 	t_mov;					
	
	do
	{
		/* error flag up in case of error */
		error = 1;

		printf("[>>] ");
		status = scanf("%d", &t_target);

		if (status == 1)
		{
			/* check puzzle contains entered number */
			if (0 < t_target && t_target < max)
			{
				error = 0;

				if (getchar() == '\n')
				{
					printf("[>>] ");
				}

				/* take the movement information */
				scanf("%c", &t_movement);

				switch (t_movement)
				{
					case 'A':
					case 'a':
								t_mov = left;
								break;
					case 'D':
					case 'd':
								t_mov = right;
								break;	
					case 'W':
					case 'w':
								t_mov = up;
								break;	
					case 'S':
					case 's':
								t_mov = down;
								break;
					default: 
								printf("[!] invalid movement\n");
								
								error = 1;
				}
			}
			/* in case of user enter a sentinal value, just terminate the program */
			else if (t_target == SENT)
			{
				error = 0;
				t_movement = SENT;	
			}
			else
			{
				printf("[!] Please enter a number which is range (%d . . %d)\n", 1, max - 1);
			}	
		}
		else
		{
			printf("[!] Please enter an integer number\n");	
		}

		clear_stdin();
	}
	while (error);

	/* return the entered entries */
	*target = t_target;
	*mov = t_mov;
}

/* checks wished movement is valid, if it is applies wished movement by changing the order of puzzle */
bool apply_movement (int puzzle[][N], towards direction, int row, int clo)
{
	/* check the next location is empty */
	int 	i, j;
	int 	pre_row = row, pre_clo = clo;
	bool 	validity = false;

	/* check movement is valid, if so swap the previosly and new locations */
	switch (direction)
	{
		/*  firstly check if there is a blank charachter (0) in the movement direction,
			if there is it's valid movement, start with blank char and swap to target 
			value given as an input argument puzzle[row][clo] 							*/

		/* 	in case of right and left movement, only cloumn change 						*/
		case left: 	
					for (i = clo - 1; i >= 0; --i)
					{
						if (puzzle[row][i] == 0)
						{
							validity = true;

							while (i != clo)
							{
								swap(&puzzle[row][i], &puzzle[row][i + 1]);
								++i;
							}
							break;
						}
					}
					break;
		case right: 
					for (i = clo + 1; i < N; ++i)
					{
						if (puzzle[row][i] == 0)
						{
							validity = true;

							while (i != clo)
							{
								swap(&puzzle[row][i], &puzzle[row][i - 1]);
								--i;
							}
							break;
						}
					}
					break;

		/* in case of up and down movement, only row change */			
		case up: 
					for (i = row - 1; i >= 0; --i)
					{
						if (puzzle[i][clo] == 0)
						{
							validity = true;

							while (i != row)
							{
								swap(&puzzle[i][clo], &puzzle[i + 1][clo]);
								++i;
							}
							break;
						}
					}
					break;
		case down:
					for (i = row + 1; i < N; ++i)
					{
						if (puzzle[i][clo] == 0)
						{
							validity = true;

							while (i != row)
							{
								swap(&puzzle[i][clo], &puzzle[i - 1][clo]);
								--i;
							}
							break;
						}
					}
	}

	return validity;
}


/* returns the current lcoation of given value in given array as row and cloumn value */
void target_location (int arr[][N], int target, int *row, int *clo)
{
	int i, j;

	for (i = 0; i < N; ++i)
	{
		for (j = 0; j < N; ++j)
		{
			if (arr[i][j] == target)
			{
				/* return location of given number as row and cloumn numbers */
				*row = i;
				*clo = j;
				return;
			}
		}
	}
	
}


/* creates a puzzle which ordered randomly numbers respectivly from 0 to ((N * N) - 1) */
void create_puzzle (int puzzle[][N])
{
	int 	i, j,	 				/* counters									  */
			n,						/* value of (i * j ) + j 					  */
			inversion_num,
			swap_num,
			rand_row, rand_clo;		/* randomly chosen indexes for mix the puzzle */

	/* initialize puzzle elements */
	for (i = 0, n = 0; i < N; ++i)
	{
		for (j = 0; j < N; ++j)
		{
			puzzle[i][j] = n;
			++n;
		}
	}

	do
	{
		/* becasue of the address of variable changes to compile time, use as random number */
		swap_num = ((long)&n % 7) + 2;

		/* reorder the elements as randomly */
		while (--swap_num > 0)
		{
			for (i = 0; i < N; ++i)
			{
				for (j = 0; j < N; ++j)
				{
					/* assign the indexes randomly */
					rand_row = rand() % N;
					rand_clo = rand() % N;

					/* swap chosen randomly elements */
					swap(&puzzle[i][j], &puzzle[rand_row][rand_clo]); 
				}
			}
		}
	} while ( (! is_solvable(puzzle)) || is_solved(puzzle));
}


void swap (int *x, int *y)
{
	int tmp;

	tmp = *x;
	*x = *y;
	*y = tmp; 
}


/* cheks if the puzzle is solved or unsolved */
bool is_solved (int puzzle[][N])
{
	int 	i, j,
			n,					/* value of (i * j ) + j 						   */
			max = N * N,		/* max value of the elements inside of the array   */
			status = true;		/* status to indicate puzzle is solved or unsolved */

	 	
	/* use inverse order checking to be able to catch error quickly */
	for (i = N - 1, n = max; i >= 0; --i)
	{
		for (j = N - 1; j >= 0; --j)
		{	
			if (puzzle[i][j] != n)
			{
				/* if we are looking the last value it must be 0, otherwise there is a mistake */
				if (n == max)
				{
					if (puzzle[i][j] != 0)
					{
						status = false;
					}
				}
				else
				{
					status = false;
				}
			}
			/* decrease the value of n, (N * N) to 1 */
			--n;
		}
	}

	return status;
}


bool is_solvable (int puzzle[][N])
{
	int inversion_num,
		status = false,
		row, clo;


	/* find the number of inverison on the puzzle */
	inversion_num = inversion(puzzle, N);

	/* check if the puzzle solvable or not */
	if (N % 2 == 0)
	{	
		/* find the location of blank */
		target_location (puzzle, 0, &row, &clo);

		if (row % 2 == 0)
		{
			if (inversion_num % 2 != 0)
			{
				status = true;
			}
		}
		else
		{
			if (inversion_num % 2 == 0)
			{
				status = true;
			}
		}
	}	
	else if (inversion_num % 2 == 0)
	{
		status = true;
	}

	return status;
}


int inversion (int arr[][N], int size)
{
	int i, j,
		counter = 0;

	for (i = 0; i < size; ++i)
	{
		for (j = 0; j < N; ++j)
		{
			if (arr[i][j] != 0)
			{
				counter += sub_inver(i, j, arr, N);
			}
		}
	}
	return counter;
}

/* calculates the number of invertion inside of inversion function */
int sub_inver (int target_row, int target_clo, int arr[][N], int size)
{
	int i, j, counter = 0;

	int target = arr[target_row][target_clo];

	/* calculates the number of inversion at current row */
	for (j = target_clo + 1; j < N; ++j)
	{
		/* skip the blank char (0) */
		if (arr[target_row][j] != 0 && target > arr[target_row][j])
		{
			++counter;
		}
	}
	/* calculates the remain number of inversion */
	for (i = target_row + 1; i < size; ++i)
	{
		for (j = 0; j < N; ++j)
		{
			/* skip the blank char (0) */
			if (arr[i][j] != 0 && target > arr[i][j])
			{
				++counter;
			}
		}
	}

	return counter;
}

void print_puzzle (int puzzle[][N])
{
	int 	i, j,
			bor_len = N * 8 + 1;	/* border lenght */
	
	for (i = 0; i < N; ++i)
	{
		/* print the upper border */
		horizontal_line('*', bor_len);
		
		printf("\t*");
		
		/* print the values inside of the puzzle */
		for (j = 0; j < N; ++j)
		{
			/* print blank charachter for value 0 */
			if (puzzle[i][j] == 0)
			{
				printf("%8c", '*');
			}
			else
			{
				printf("%5d\t*", puzzle[i][j]);
			}
		}

		printf("\n");
	}
	/* print the lower border */
	horizontal_line('*', bor_len);
}

/* prints horizontal line according to given parameters */
void horizontal_line (char ch, int n)
{
	int i;

	printf("\t");

	for (i = 0; i < n; ++i)
	{
		printf("%c", ch);
	}

	printf("\n");
}


void instructions ()
{
	printf("\n----------------------------- # PUZZLE GAME # -----------------------------\n\n");
	
	printf("INSTRUCTIONS\n\n");

	printf(" \tW%84s\n", "1. Enter number and direction as <number> <direction> E.g 17 W or 5 A");
	printf(" A\t\tD\n");
	printf(" \tS%48s\n\n", "2. Enter -1 to terminate the game");
}

/* consume all the remaining input from user */
void clear_stdin ()
{
	do {

	} 	while (getchar() != '\n');
} 