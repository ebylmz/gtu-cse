#include <stdio.h>

#define N 5								/* number of compared item in function difference_max_min 				*/
#define KEY '*'							/* sentinel value for terminate the executing program 					*/
#define ERROR_TOLERANCE 0.0000001		/* sensivity at fractional part, after that point others are ignored 	*/

void calculate_fibonacci_sequence();

void decide_perfect_harmonic_number();

int sudo_scanf(char key);

void difference_max_min();

void bmi_calculation ();

double get_safe_bmi_values(int type);

void part(int n);




int main(void)
{
	/* TESTING PART 1 */
	part(1);
	calculate_fibonacci_sequence();
	
	/* TESTING PART 2 */
	part(2);
	decide_perfect_harmonic_number();
	
	/* TESTING PART 3 */
	part(3);
	difference_max_min();

	/* TESTING PART 4 */
	part(4);
	bmi_calculation();
}

/*--------------------------------------  PART 1 FUNCTION DEFINITIONS  ------------------------------------------*/


/* prints first n element of fibonacci sequence, supports only first 46 terms ! */
void calculate_fibonacci_sequence()
{
	int i,						/* counter 												*/ 
		tmp,					/* temporary variable for swapping variables 			*/
		n, 						/* number of first n fibonacci sequence elements 		*/
		exit = 0;				/* flag value for continue(0) or exit(1) executing	  	*/	

	unsigned long int a1, a2;	/* fibonacci sequence variables 						*/

	/* calculate  */
	do
	{
		a1 = 0;
		a2 = 1;

		/* take number of desired fibonacci sequence, be sure proper format using sudo_scanf() */
		printf("\nPlease enter term(s) number ('%c' to exit) : ", KEY);
		
		/* takes the number of fibonacci elements surely */
		n = sudo_scanf(KEY);

		/* function does not support the terms after 46th term, because of unability to store such a big numbers */
		if (n > 46)
		{
			printf("Function can support only first 46 term\n");
		}
		/* if the input value is not the sentinal value(KEY) continue to calculate the fibonacci sequence */
		else if (n != KEY)
		{
			printf("\nFibonacci Sequence:\n");

			/* print the first nth term of fibonacci series */
			for (i = 0; i < n; ++i)
			{
				printf("%.lu\n", a2);
				
				/* swapping elements */
				tmp = a1 + a2;
				a1 = a2;
				a2 = tmp;
			}
		}
		/* n is KEY, exit flag is up */
		else
		{
			exit = 1;
		}
	}
	while ( ! exit);
}


/*--------------------------------------  PART 2 FUNCTION DEFINITIONS  ------------------------------------------*/


/* finds the given number is harmonic divisor number or not until, until user exit */
void decide_perfect_harmonic_number()
{
	int num,						/* the number entered by user 								 */
		i, 							/* accumulator			 									 */
		status,						/* flag for status of program continue(1) or exit(0) 		 */
		div_sum,					/* sum of divisors 					 						 */
		div_num,					/* number of divisors		 								 */
		div_up_bound;				/* half of the given number, to find all the evenly divisors */

	double harmomic_mean;			/* calculated harmonic mean 								 */


	status = 1;		/* initilize status flag as 1(continue) */

	do
	{
		/* take the number */
		printf("Please enter input number ('%c' to exit): ", KEY);
		num = sudo_scanf(KEY);

		/* check if it is sentinel value or not, if it is exit flag is up */
		if (num == KEY)
		{
			status = 0;
		}
		else
		{
			printf("\nNatural Number Divisors: ");

			/* initialize divisor number and sum as 0 */
			div_num = 0;
			div_sum = 0;
			harmomic_mean = 0;

			/* no need to try to find evenly divisor after the half of given number */
			div_up_bound = num / 2;

			/* find natural number divisors */
			for (i = 1; i <= div_up_bound; ++i)
			{
				/* if number is divided evenly by i */
				if (num % i == 0)
				{
					printf("%d, ", i);

					/* increase the number of divisors */
					++div_num;

					/* add the divisor value */
					div_sum += i;

					/* calculate harmonic mean */
					harmomic_mean += 1.00 / i;
				}
			}

			/* do last step outside the previous for loop to prevent adding number itself to div_sum */
			++div_num;
			harmomic_mean += 1.00/ num;

			/* print the number itself as divisor */
			printf("%d\n\n", num);


			/* if sum of all evenly divisor is equal to number itself, print Yes otherwise print No */
			printf("Is Perfect Number?          : ");

			if (div_sum == num)	
				printf("Yes\n");
				
			else
				printf("No\n"); 

			/* last step of harmonic mean */
			harmomic_mean = div_num / harmomic_mean;


			printf("Is Harmonic Divisor Number? : ");

			/* if harmonic mean is a integer, print Yes otherwise print NO */
			if (harmomic_mean - (int)harmomic_mean < ERROR_TOLERANCE)
				printf("Yes\n\n");
			
			else
				printf("No\n\n");
		}
	}
	while (status);
}

/* avaoids tricky inputs except sentinel key value, and returns only positive int or key value */
int sudo_scanf(char key)
{
	char bad_in; 	/* takes wrong/tricky input entered by user 	*/

	int in,			/* the proper type input 				 		*/
		error;		/* flag for input validation				 	*/


	error = 1;		/* error flag is up */

	do
	{
		/* if user entered integer value */
		if (scanf("%d", &in) == 1)
		{
			/* be sure entered number is positive */
			if (in <= 0)
			{
				printf("Please enter “positive” term(s) number: ");
			}
			/* otherwise error flag is down, no error */
			else
			{
				error = 0;
			}
		}
		/* if entered input type is invalid */
		else
		{
			/* takes the improper input from stdin */
			scanf("%c", &bad_in);

			/* check if it is sentinel(key) value or not, if so terminate the program returning the key value */
			if (bad_in == key)
			{
				return key;
			}

			/* otherwise, user must be entered improper charachter, warn user */
			printf("Please enter “numeric” term(s) number: ");			
		}


		/* if error flag is still up, consume all the bad inputs */
		if (error == 1)
		{
			/* clear all the line */
			do
			{
				scanf("%c", &bad_in);
			}
			while (bad_in != '\n');
		}
	}
	while (error);

	/* return the confirmed input value */
	return in;
}

/*--------------------------------------  PART 3 FUNCTION DEFINITIONS  ------------------------------------------*/


/* calculates the difference between max and min value in given N user entered value */
void difference_max_min()
{
 	int i,						/* counter	 				 		  				*/
 		n,						/* number of compared values 						*/
		error;					/* flag value for input validation					*/

	double 	maxx, minx,			/* maximum, minimum and   							*/
			tmp,				/* temporary values for store current user input 	*/
			difference;			/* difference between max and min value 			*/
	
	char extra_value;


	/* prompt for user to enter N number */
	printf("Please enter %d numbers: ", N);
	scanf("%lf", &tmp);

	/* assign first input as a starting point for both as a min and max value */
	minx = tmp;
	maxx = tmp;

	/* decrease the number of element(N), before go into loop */
	n = N - 1;

	/* find max and min within entered values */
	for (i = 0; i < n; ++i)
	{
		/* take the number */
		scanf("%lf", &tmp);

		/* entered value can be bigger than current max value, if so assign it as a new max value */
		if (tmp > maxx)
		{
			maxx = tmp;
		}
		/* otherwise it can be less than current min value, if so assign it as a new min value */
		else if (tmp < minx)
		{
			minx = tmp;
		}
	}
	
	printf("\n");

	/* assume there is no error */
	error = 0;

	/* print warning, if more than N values entered by user */
	do
	{
		scanf("%c", &extra_value);

		/* if it's number, print as ignored number */
		if ('0' <= extra_value && extra_value <='9')
		{
			printf("%c", extra_value);
			++error;
		}
		/* print minus sign */
		else if (extra_value == '-')
		{
			printf("%c", extra_value);
			++error;
		}
		/* print space between inputs */
		else if (extra_value == ' ')
		{
			printf("%c", extra_value);
		}
		/* last input comes from user, thefore print warning if there is error occurs */
		else if (extra_value == '\n' && error != 0)
		{
			printf(" (ignored numbers)\n");
			printf("***WARNING*** only first %d numbers included !\n", N);
			error = 0;
		}
	}
	while (error);


	/* calculate difference between max value and min value */
	difference = maxx - minx;


	/* print the max and min values in a proper format (not leading zeros) */	
	printf("\nMaximum number is: %g\n", maxx);
	printf("Minimum number is: %g\n", minx);

	/* print difference max and min values again in a proper format */
	printf("Difference between maximum and minimum is %g\n", difference);

}


/*--------------------------------------  PART 4 FUNCTION DEFINITIONS  ------------------------------------------*/


/* calculates and prints the bmi catogory according to given height and weight */
void bmi_calculation()
{
	double weight, height, bmi;
	
	/* get weight and height from user in a proper format */
	weight = get_safe_bmi_values(1);
	height = get_safe_bmi_values(2);

	/* calculate the bmi */
	bmi = weight / (height * height);


	/* select the proper bmi range and print the bmi category */
	printf("\nYour category: ");

	if (bmi < 16.0)
	{
		printf("Severely Underweight\n");
	}
	else if (bmi < 18.5)
	{
		printf("Underweight\n");
	}
	else if (bmi < 25)
	{
		printf("Normal\n");
	}
	else if (bmi < 30)
	{
		printf("Owerweight\n");
	}
	/* bmi is equal or more than 30.0 */
	else
	{
		printf("Obese\n");
	}
}



/* returns safe bmi values for given type, 1 for weight, 2 for height */
double get_safe_bmi_values(int type)
{
	double bmi_value;		/* safe bmi value 						  	*/
	char bad_in;			/* bad input from user 					 	*/
	int status = 0;			/* scanf status for detecting bad inputs	*/
	
	if (type == 1)
	{
		printf("Please enter weight(kg) : ");
	}
	else if (type == 2)
	{
		printf("Please enter height(m)  : ");
	}

	/* check scanf status, be sure user entered a number */	
	status = scanf("%lf", &bmi_value);

	while (status < 1)
	{
		/* consume all the bad inputs at current line */
		do
		{
			scanf("%c", &bad_in);
		}
		while (bad_in != '\n');

		if (type == 1)
		{
			printf("Please enter proper weight(kg) : ");
		}
		else if (type == 2)
		{
			printf("Please enter proper height(m) : ");
		}
		status = scanf("%lf", &bmi_value);
	}
	
	/* check if user enter a positive number */
	while(bmi_value <= 0)
	{
		if (type == 1)
		{
			printf("Please enter proper weight(kg) : ");
		}
		else if (type == 2)
		{
			printf("Please enter proper height(m) : ");
		}
		status = scanf("%lf", &bmi_value);
	}

	/* return the safety bmi value */
	return bmi_value;
}

/* print the nth part of program */
void part(int n)
{
	printf("\n-----> PART %d <-----\n", n);
}