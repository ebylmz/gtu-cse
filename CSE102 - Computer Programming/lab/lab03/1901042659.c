#include <stdio.h>

#define KEY -1			/* key value to stop taking input in function find_odd_average */

int find_max_distance(int num_inputs);

void find_odd_average();


int main(void)
{	
	int size;

	/* PART 1 */
	printf("\nPART 1\n");

	/* take the number of input entered bu user */
	printf("How many number do you enter: ");
	scanf("%d", &size);

	/* call the first function of this project */
	find_max_distance(size);



	/* PART 2 */
	printf("\nPART 2\n");

	/* call the second function of this project */
	find_odd_average();


}

/* finds the max distance between entered numbers, assumes that given input is integer */
int find_max_distance(int num_inputs)
{
	int i;				/* accumulator for for loop */
	int pre, cur;		/* current and previos values given by user */

	int max_distance = 0,	/* max distance between two consecutive entered numbers */
		cur_distance = 0;	/* current difference between two consecutive entered numbers */
	
	/* check the number of input is positive, assumes that user enter only integer values */
	while (num_inputs < 0)
	{
		printf("Please enter positive number: ");
		scanf("%d", &num_inputs);
	}

	/* no need to run */
	if (num_inputs == 0)
	{
		return 0;
	}

	/* prompt for calculate given values */
	printf("Please enter %d numbers: \n", num_inputs);

	for (i = 1, pre = 0; i <= num_inputs; ++i)
	{
		/* print the number of input for user and take the input */
		printf("%d: ", i);
		scanf("%d", &cur);
		/* calculate the difference between two consecutive entered numbers as current dsitance */
		cur_distance = cur - pre;

		/* for keep track of inputs, assign current input value as a previous */
		pre = cur;

		/* be sure differance is absolute value */
		if (cur_distance < 0)
			cur_distance *= -1;

		/* if current distance bigger than previos calculated ones, assign as a new max distance */
		if (cur_distance > max_distance)
		{
			max_distance = cur_distance;
		}
	}

	/* print the max distance */
	printf("Max distance between two consecutive nums: %d\n", max_distance);

	/* return the value of max distance */
	return max_distance;
}


/* finds average of entered odd numbers, assumes that given input only integer */
void find_odd_average()
{
	int i,					/* loop accumulator 						*/
		n,					/* number of odd number entered by user 	*/
		in;					/* general input variable for 				*/

	double average = 0; 	/* average of odd numbers					 */

	/* promt for starting calculate */
	printf("Please enter positive numbers (enter -1 to stop entering)\n");

	i = 1;		/* initilize as 1, for user convenience */ 
	n = 0;		/* number of odd numbers initilize as 0 */

	do
	{
		/* take ith number from user */
		printf("%d: ", i);
		scanf("%d", &in);

		/* be sure given value is positive, otherwise warn user */
		if (in > 0)
		{
			/* if number is odd, add to average sum */
			if (in % 2 != 0)
			{
				average += in;
				/* increase the number of odd number by one  */
				++n;
			}
			/* increase the total input number */
			++i;
		}
		/* if given value is key value(-1) stop taking input, otherwise warn user and back the start of loop */
		else if (in != KEY)
		{
			printf("Please enter positive numbers\n");
		}

	}
	while (in != KEY);

	

	/* if user enter at least one odd number, calculate the average*/
	if (n != 0)
	{
		/* divide the sum of odd numbers by the number of entered odd numbers */
		average /= n;
		printf("Average of odd numbers %.2f\n", average);

	}
	else
	{
		printf("There is no odd number in given numbers\n");
	}
}