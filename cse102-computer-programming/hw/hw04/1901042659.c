#include <stdio.h>
#include <math.h>				/* for fabs() and pow() functions */

#define SENSIVITY  0.0509999	/* sensivity for rounding numbers */

void clear_stdin();

int get_non_negative_int();

void part(int n);

void write_polynomial();

void pretty_polynomial_terms(char unknown, int degree, float coef, int sign_between);

float round_down(float num);

void print_float_int(float float_num);

void check_special_number();

int is_armstrong(int num);

int is_palindrome(int num);

void sum_prime_between();

int is_prime(int num);


/*---------------------------------------  MAIN FUNCTİON FOR TESTING   ------------------------------------------*/
int main(void)
{	
	/* PART 1 */
	part(1);
	write_polynomial();

	/* PART 2 */
	part(2);
	check_special_number();

	/* PART 3 */
	part(3);
	sum_prime_between();
}


/*------------------------------------  COMMON USED FUNCTIONS DEFINITIONS  --------------------------------------*/

/* clear all the input in current line */
void clear_stdin()
{
	char junk_in;

	do
	{
		scanf("%c", &junk_in);
	}
	while (junk_in != '\n');
}

/* forces user to enter an non negative integer value, scans and returns that value 				   				   */
/* function will maintain to be capable of detecting overflow for int value, for now it cannot handle very big numbers */
int get_non_negative_int()
{
	int error,
		status,		/* scanf status 		 */ 
		num;		/* scanning int variable */

	double test_num;

	do
	{	
		/* assume there is a error */
		error  = 1;

		status = scanf("%lf", &test_num);
		
		/* if user enter type char value, instead of type int */
		if (status != 1)
		{
			/* warn user to enter integer value */
			printf("[!] invalid input type (char)\n\n");
			printf("Please enter an integer number : ");
		}
		else
		{
			/* if entered number is floating point number */
			if (test_num - (int)test_num != 0)
			{
				printf("[!] invalid input type (floating point number))\n\n");
				printf("Please enter an integer number : ");
			}
			/* if entered number is negative */
			else if (test_num < 0)
			{
				printf("[!] invalid input value (negative)\n\n");
				printf("Please enter a non negative integer number: ");
			}
			/* entered value has correct type and non negative value */
			else
			{
				/* no error detected, value scanned succesfuly */
				error = 0;
				
				/* assign reliable value as return value */
				num = test_num;
			}
		}

		clear_stdin();
	}
	while (error);
	

	return num;
}


/* prints the nth part of program */
void part(int n)
{
	printf("\n----> PART %d <----\n", n);
}


/*--------------------------------------  PART 1 FUNCTION DEFINITIONS  ------------------------------------------*/

/* takes degree and coeffient of polynomial and prints it in a proper way */
void write_polynomial()
{
	int error,			/* error flag */
		status,			/* status of scanf 	*/
		degree,
		n,				/* exponent of nthm term 			    */
		neg_sign;
	
	float coef,		/* coefficient of the nth term      */
		  test_num;

	char char_bad_in,
		 unknown;		/* type of unknown like x, y, z     */

	printf("[*] sensivity of program :'+/- %.3f', all the numbers less than sensivity rounded to 0 !\n\n", SENSIVITY);

	/* take the representation character for unknown */
	do
	{
		/* no error yet */
		error = 0;
		
		printf("Please enter a character for represent the unknown (x, y, z...): ");
		scanf("%c", &unknown);

		/* if unknown is not a letter */
		if ( ! (('a' <= unknown && unknown <= 'z') || 'A' <= unknown && unknown <= 'Z'))
		{
			printf("[!] Please choose the unknown name in English alphabet\n");

			error = 1;
		}
		/* ignore the remainder input, if there is */
		clear_stdin();
	}
	while (error);
	

	do
	{
		/* error flag is up, in case of error */
		error = 1;

		/* prompt for taking degree and coefficients of polynomial */
		printf("Enter your polynomial [n a_n a_n-1 a_n-2 ... a_0]: ");

		/* take degree of the polynomial */
		status = scanf("%f", &test_num);

		/* be sure entered value is proper for degree (non negative integer)*/
		if (status != 1)
		{
			printf("[!] invalid degree (type: character)\n");
		}
		else if (test_num - (int)test_num != 0)
		{
			printf("[!] invalid degree (type: floating point number)\n");
		}
		else if (test_num < 0)
		{
			printf("[!] invalid degree (negative value)\n");
		}
		/* we have reliable value for degree */
		else
		{
			/* assign checked value as degree */
			degree = test_num;

			/* scan the first coefficent of nth degree term (a_n) */
			scanf("%f", &coef);

			/* since the values less than SENSIVITY are rounded 0, be sure first term coefficent larger than SENSIVITY */
			if (fabs(coef) - SENSIVITY < 0)
			{
				printf("[!] Entered coefficient '%.3f' is less than SENSIVITY '+/- %.3f'\n", coef, SENSIVITY);
				printf("Please enter larger value for the coefficent of first term\n");
			}
			else
			{
				printf("[>>>] ");
				printf("p(%c) = ", unknown);

				/* define the sign of first term */
				if (coef < 0)
				{
					neg_sign = 1;
				}			
				else
				{
					neg_sign = 0;
				}

				/* no error yet */
				error = 0;

				/* print the first term according to sign information */
				pretty_polynomial_terms(unknown, degree, coef , neg_sign);


				/* print the remainder terms as order of decreasing degree */
				for (n = degree - 1; n >= 0 && error == 0; --n)
				{
					status = scanf("%f", &coef);

					/* check if entered value is numeric */
					if (status == 0)
					{
						/* an error detected */
						error = 1;

						scanf("%c", &char_bad_in);
						printf("\n[!] invalid coefficient '%c', execution stopped\n", char_bad_in);
					}
					else
					{
						/* print the nth term  */
						pretty_polynomial_terms(unknown, n, coef, 1);
					}
				}
			}
		}
		printf("\n");
		
		/* ignore the remainder input, if there is */
		clear_stdin();
	}
	while (error);
}

/*	prints given polynomial term with defined sensivity, the values less than sensivity rounded to 0 and ignored 
    for sign_between = 1, write coefficent as signed. E.g +3.4 written as + 3.4
    for sign_between = 0, write absolute values of coefficent. E.g - 3.4 written as 3.4 						  */
void pretty_polynomial_terms(char unknown, int degree, float coef, int sign_between)
{
	char sign;	/* sign flag */

	/* in case of coefficent is less than SENSIVITY, don't do anything */
	if (fabs(coef) >= SENSIVITY)
	{
		/* define the sign, if user wants to write numbers with it's sign */
		if (sign_between)
		{
			/* find it's sign */
			if (coef > 0)
			{
				sign = '+';
			}
			else if (coef < 0)
			{
				sign = '-';
			}
			
			/* print the sign */
			printf("%c ", sign);
		}
		
		/* round down given coefficient and assign it's abosulute value */
		coef = round_down(coef);
		coef = fabs(coef);
	
		/* in case of constant value just write it's value */
		if (degree == 0)
		{
			print_float_int(coef);
		}
		/* otherwise write the value of coefficient, except +/- 1 */
		else 
		{
			/* since 1 == 1.050 is accepted by us, don't write  +/- 1 */
			if (coef < 1 || coef >= 1 + SENSIVITY)
			{
				print_float_int(coef);
			}

			/* in case of degree 1, print it without it's power (1) */
			if (degree == 1)
			{
				printf("%c ", unknown);
			}
			/* print the unknown with it's degree */
			else
			{
				printf("%c^%d ", unknown, degree);
			}
		}	
	}	
}


/* try to print given number as integer value, if any lose at fraction part (first digit) */
void print_float_int(float float_num)
{
	/* take given number as integer */
	int int_num = (int)float_num;

	if (fabs(float_num - int_num) < SENSIVITY)
	{
		printf("%d", int_num);
	}
	else
	{
		printf("%.1f", float_num);
	}	
}


/* round the given number, after decimal point as n digit */
float round_down(float num)
{
	int last_two_digit,
		negative;				/* flag for indicate negative number */

	float f_num;				/* fraction part of given number */

	/* check if the given number is negative, if so convert it to positive */
	if (num < 0)
	{
		num *= -1;

		negative = 1;
	}
	else
	{
		negative = 0;
	}


	f_num = num - (int)num;		/* assign the fraction part of given number   */
	num -= f_num;				/* update the number as without fraction part */


	/* move decimal point to the right 3 times */
	f_num *= 10 * 10 * 10;

	/* take last to digit */
	last_two_digit = (int)f_num % 100;


	/* if last two digit is more than 50 round up  */
	if (last_two_digit > 50)
	{
		f_num += 100;
	}

	/* convert last to digit of fraction as 0  */
	f_num -= last_two_digit;
	
	/* move decimal point it's initial point   */
	f_num /= 10 * 10 * 10;

	/* add rounded fraction part to number 	   */
	num += f_num;

	/* if negative flag is up, do not forget to turn back negative value */
	if (negative)
	{
		num *= -1;
	}

	return num;
}

/*--------------------------------------  PART 2 FUNCTION DEFINITIONS  ------------------------------------------*/


/* checks if the given number is Palindrome or Armstrong number or both */
void check_special_number()
{
	int palindrome_st,		/* status of being able to palindrome number (0, 1) */
		armstrong_st;		/* status of being able to armstrong number (0, 1)  */
	int num, 				/* input number 						 			*/
		status;				/* status of scanf for input validation  			*/

	printf("Please enter an integer number : ");
	num = get_non_negative_int();

	/* establish the status of given number */
	palindrome_st = is_palindrome(num);
	armstrong_st = is_armstrong(num);

	/* print the status of the number */
	if (!palindrome_st && !armstrong_st)

		printf("[>>>] This number does not satisfy any special cases.\n");

	else if (palindrome_st && armstrong_st)

		printf("[>>>] This number is both Palindrome and Armstrong number.\n");

	else if (palindrome_st)

		printf("[>>>] This number is only Palindrome number.\n");

	else

		printf("[>>>] This number is only Armstrong number.\n");

}

/* checks the given number is Armstrong number or not, it can work correctly till max int value (2,147,483,647) */
int is_armstrong(int num)
{
	int armstrong_sum,	/* sum of nth power of all digits (n is digit number)  */
		digit_num,		/* total digit number of given number 				   */
		last_digit,		/* last digit of calculating number 				   */
		tmp_num;		/* temporary value for calculating armstrong sum       */


	/* find the digit number */
	for (digit_num = 0, tmp_num = num; tmp_num > 0; ++digit_num)
	{
		tmp_num /= 10;
	}

	/* take power of all the digit as much as digit number, and add that values as armstrong sum */
	for (tmp_num = num, armstrong_sum = 0; tmp_num > 0; tmp_num /= 10)
	{
		/* take the last digit */
		last_digit = tmp_num % 10;

		/* add the power of the digit to armstrong sum */
		armstrong_sum += pow(last_digit, digit_num);
	}

	/* if number is equal to the calculated armstrong sum, return 1, otherwise return 0 */
	if (armstrong_sum == num)
		
		return 1;

	else

		return 0;
}

/* checks the given number is Palindrome number or not */
int is_palindrome(int num)
{	
	int first_digit,	/* first digit of regenerated number 	*/
		last_digit,		/* last digit of regenerated number  	*/
		digit_num,		/* number of digit 					 	*/
		gen_num,		/* regenereted number				 	*/
		status,			/* status of being a palindrome number  */
		pow_10;			/* the value of 10^(digit number - 1) 	*/


	/* find the number of digit */
	for (gen_num = num, digit_num = 0; gen_num != 0; ++digit_num)
	{
		gen_num /= 10;
	}

	/* assign the pow_10, according to digit number */
	pow_10 = pow(10, digit_num - 1);

	/* set status as 1 */
	status = 1;

	/* compare the digit of numbers until digit number is 0(even digit number) or 1(odd digit number) */
	for (gen_num = num; digit_num > 1; digit_num -= 2)
	{		
		/* find the first and last digit of number */
		last_digit = gen_num % 10;

		first_digit = (gen_num / pow_10);
		

		/* check last and first digit, if they are not same define status as 0 */
		if (last_digit != first_digit)
		{
			status = 0;
			break;
		}

		/* update the number for next check */
		gen_num = (gen_num - (first_digit * pow_10));		/* lose the first digit */
		gen_num /= 10;										/* lose the last digit  */
		pow_10 /= 100; 					  /* divide by 100, since we lost two digit */
	}

	return status;
}

/*--------------------------------------  PART 3 FUNCTION DEFINITIONS  ------------------------------------------*/


/* finds the sum of all prime numbers between given start and end values */
void sum_prime_between()
{
	int tmp,		/* tmp variable for run number by number 	   */
		sum;		/* sum of all the prime numbers in given range */

	int n_start,	/* started number of range, not included 	   */
		n_end;		/* ended number of range, not included   	   */

	int error;		/* flag for indicating error */

	do
	{
		/* no error yet */
		error = 0;

		/* take the start and end number from user to establish the range  */
		printf("Please enter first integer number : ");

		n_start = get_non_negative_int();

		printf("Please enter second integer number : ");
		
		n_end = get_non_negative_int();

		
		/* swap numbers, if first entered number bigger than second one */
		if (n_start > n_end)
		{
			tmp = n_end;
			n_end = n_start;
			n_start = tmp; 
		}
		
		/* check entered numbers are indicate an interval */
		if (n_start == n_end || n_end - n_start == 1)
		{
			printf("[!] There is no interval between entered numbers, try different numbers.\n\n");

			error = 1;
		}
	}
	while (error);

	/* find all prime numbers in range, if there is */
	for (tmp = n_start + 1, sum = 0; tmp < n_end; ++tmp)
	{
		/* if it is prime, add to sum */
		if (is_prime(tmp))
		{
			sum += tmp;
		}
	}

	if (sum != 0)
	{
		/* print the sum with range */
		printf("[>>>] Sum of prime numbers between %d and %d : %d\n", n_start, n_end, sum);
	}
	else
	{
		printf("[>>>] There is no prime number between %d and %d\n", n_start, n_end);
	}
	
}


/* finds given number is prime or not, returns 1 for YES, 0 for NO  */
int is_prime(int num)
{
	int tmp,		/* temporary value    				   			*/
		up_bound;	/* half of given number as a end point 			*/


	/* check if the given number is evenly divisible with any numbers except 1, till half of itself */
	for (tmp = 2, up_bound = num / 2; tmp < up_bound; ++tmp)
	{
		/* if it fully divisible any number smaller than itself, just return 0 */
		if (num % tmp == 0)
		{
			return 0;
		}
	}
	/* number is only divisible by itself */
	return 1;
}