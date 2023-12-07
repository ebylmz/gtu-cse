#include <stdio.h>
#include <stdlib.h>		/* for rand() function 					  */

#define N 10			/* the number of row and cloumn of matrix */


void expression0 (int x, int *result);

void expression1 (int x, int *result);

void expression2 (int x, int *result);

void addition (void (*func)(int, int *), int *sum, int n);

void calculation (int *sumArray, int n);

void createArray(int (*array)[10]);


int main(void)
{
	int i,
	 	n,			/* iteration number at sigma notation  */
		error;

	/* declare an array to store result of calculated data */
	int sums[3] = {0, 0, 0}; 

	/* declare an 2D array which have N row, N cloumn      */
	int matrix[N][N];	

	/* PART 1 */

	do
	{
		/* no error yet */
		error = 0;

		/* scan the value of n */
		printf("n: ");
		scanf("%d", &n);

		/* check the n is positive value */
		if (n <= 0)
		{
			printf("[!] Please choose n positive\n");
			
			/* an error is detected */
			error = 1;
		}
	}
	while (error);


	/* calculate  and store in sums array */
	calculation(sums, n);

	/* print the result in array sums */
	for (i = 0; i < 3; ++i)
	{
		printf("Sum of expression%d: %d\n", i, sums[i]);
	}
	printf("\n\n");


	/* PART 2 */
	createArray(matrix);

}

/* PART 1 */

/* calculation of the expression0 for the value of x */
void expression0 (int x, int *result)
{
	*result = (x * x) + 5;
}

/* calculation of the expression1 for the value of x */ 
void expression1 (int x, int *result)
{
	*result = (2 * x) + 1;	
}

/* calculation of the expression2 for the value of x */
void expression2 (int x, int *result)
{	
	*result = x * x;
}

/* obtaining the calculations for a single expression and return the result */ 
void addition (void (*func)(int, int *), int *sum, int n)
{
	int i,
		cur_result; 	/* result of curent calculation with i */ 

	for (i = 0, *sum = 0; i <= n; ++i)
	{
		/* apply the given function and take the result */
		func(i, &cur_result);

		/* add current result the sum  */
		*sum += cur_result;
	}
}

/* apply 3 different addition according to value n and store the result at given output array */
void calculation (int *sumArray, int n)
{
	/* apply expression 0 and store the result */		
	addition(expression0, &sumArray[0], n);
	
	/* apply expression 1 and store the result */
	addition(expression1, &sumArray[1], n);
	
	/* apply expression 2 and store the result */
	addition(expression2, &sumArray[2], n);
}	


/* PART 2 */

/* creates an matrix which consist of random numbers, and prints the value at desired row anc cloumn */
void createArray(int (*array)[N])
{
	int i, j,		/* current row i, and current cloumn j */
		status; 	/* flag to indicate run or terminate */

	/* create 2D array and put two digit random numbers */
	for (i = 0; i < N; ++i)
	{
		for (j = 0; j < N; ++j)
		{
			/* assign generated random number in current location */
			array[i][j] = (rand() % 90) + 10;

			/* print the new value of current location */
			printf("%d ", array[i][j]);
		}
		printf("\n");
	}

	printf("\n");

	/* set status as 1 */
	status = 1;


	do
	{
		/* take the desired location (row, cloumn) at matrix from user */
		printf("i: ");
		scanf("%d", &i);

		printf("j: ");
		scanf("%d", &j);

		/* be sure user inputs correspond a valid location, if they are not terminate the program */
		if ((0 <= i && i < N) && (0 <= j && j < N))
		{
			printf("%d. row %d. column of the matrix is %d\n\n", i, j, array[i][j]);
		}
		else
		{
			printf("Invalid input. Terminating...\n\n");

			status = 0;
		}
	}
	while (status);
}


