/*
** hw5_lib.c:
**
** The source file implementing library functions.
**
** Author: Yakup Genc. (c) 2018-2021
**
** Revision: 2021.04.02.23.55
** 
*/

#include <stdio.h>
#include <math.h>
#include "hw5_lib.h"


/* applies addition substraction and multiplication with given two at most degree 3 polynomial */
void operate_polynomials (double *a3, double *a2, double *a1, double *a0, double *b3, double *b2, double *b1, double *b0, char op)
{
	switch (op)
	{
		case '+':
					add_polynomials(a3, a2, a1, a0, *b3, -1 * (*b2), *b1, *b0);
					break;

		case '-':
					add_polynomials(a3, a2, a1, a0, -1 * (*b3), -1 * (*b2), -1 * (*b1), -1 * (*b0));
					break;

		case '*':
					multiply_polynomials(a3, a2, a1, a0, b3, b2, b1, *b0);
					break;

		default:
					printf("[!] Undefined operator '%c'\n", op);
	}
}

/* takes ppolynomial term in proper format (degree, coefficent) (n, a_n) */
void take_polynomial_term(double *x3, double *x2, double *x1, double *x0)
{
	int 	degree,
			status,		/* status of scanf 											*/
			error, 		/* error flag 												*/
		 	go;			/* flag, to indicate user wants to continue entering input 	*/
	double 	coef,		/* coefficent 												*/
			test;		/* test double, variable to be sure given value is proper 	*/
	char 	c;			/* test char  												*/
	
	printf("\npolynomial: ");

	do
	{
		/* error flag is up in case of error */
		error = 1;
		go = 0;

		if (skip_blank_ch() == '(')
		{

			/* take degree as an positive integer which less than 4 */
			scanf("%lf", &test);

			if ((test - (int)test == 0) && (0 <= test && test <= 3))
			{
				degree = test;

				if(skip_blank_ch() == ',')
				{
					/* no error detected yet */
					error = 0;

					/* take the coefficent */
					status = scanf("%lf", &coef);

					if (status == 1)
					{
						/* assign coeffcient with corresponding degree */
						switch (degree)
						{
							case 3:
									*x3 = coef; 
									break;

							case 2:
									*x2 = coef; 
									break;

							case 1:
									*x1 = coef; 
									break;
						
							case 0:
									*x0 = coef; 
									break;

							default:
									printf("[!] invalid degree D(degree) = [0, 3]\n");

									/* error detected */
									error = 1;
						}

						if (error == 0)
						{
							if (skip_blank_ch() == ')')
							{
								scanf("%c", &c);
								
								if (c == ',')
								{
									go = 1;
								}
								else if (c != '\n')
								{
									error = 1;
								}
							}
						}
					}
				}
			}
		}


	if (error == 1)
	{
		printf("[!] invalid format, please enter in a proper way\n\n");
		printf("polynomial: \n");
		clear_stdin();
	}

	} while (error || go);
}


/* clear all the input at current line */
void clear_stdin()
{
	char in;

	do
	{
		scanf("%c", &in);

	} while (in != '\n');
}

/* applies addition on given two at most degree 3 polynomials */
void add_polynomials(double *a3, double *a2, double *a1, double *a0, double b3, double b2, double b1, double b0)
{
	*a3 += b3;
	*a2 += b2;
	*a1 += b1;
	*a0 += b0;
}

/* applies multiplication on given two at most degree 3 polynomials */
void multiply_polynomials (double* a3, double* a2, double* a1, double* a0, double* b3, double* b2, double* b1, double b0)
{
	/* coefficient of new polynomials  */
	double n6, n5, n4, n3, n2, n1, n0; 

	/* calculate degree 6 coefficient */
	n6 = (*a3) * (*b3);

	/* calculate degree 5 coefficient */
	n5 = (*a3) * (*b2) + (*a2) * (*b3);

	/* calculate degree 4 coefficient */
	n4 = (*a2) * (*b2) + (*a3) * (*b1) + (*b3) * (*a1);

	/* calculate degree 3 coefficient */
	n3 = (*a3) * (b0) + (*b3) * (*a0) + (*a2) * (*b1) + (*a1) * (*b2);

	/* calculate degree 2 coefficient */
	n2 = (*a2) * (b0) + (*b2) * (*a0) + (*a1) * (*b1);

	/* calculate degree 1 coefficient */
	n1 = (*a1) * (b0) + (*b1) * (*a0);

	/* calculate degree 0 coefficient */
	n0 = (*a0) * (b0);

	/* return the coeffficient as a_3 b_3 a_2 b_2 a_1 b_1 a_0 */
	*a3 = n6;
	*b3 = n5;
	*a2 = n4;
	*b2 = n3;
	*a1 = n2;
	*b1 = n1;
	*a0 = n0;
}


/* skips any blank charachter and scans the first input after last blank character */
char skip_blank_ch()
{
	char test;

	do
	{
		scanf("%c", &test);
	}
	while (test == ' ');

	return test;
}

/********************************************************************************************************************/

/* finds the average of each dimension across N vectors, calculate the longest Euclidean distance between two consecutive 4D vectors */
void four_d_vectors (double* mean_a0, double* mean_a1, double* mean_a2, double* mean_a3, double* longest_distance, int N)
{
	int n;	/* counter, the number of entered vector */
		
	double 	v1_x, v1_y, v1_z, v1_w,		/* coordinates of vector 1, respectively x, y, z, w 				*/
			v2_x, v2_y, v2_z, v2_w,		/* coordinates of vector 2, respectively x, y, z, w 				*/
			d0, d1, d2, d3,				/* differences of given two vector to calculate euclidian dsitance 	*/
			euclidian_distance;			

	printf("Please enter 4d vector respectively component x y z w (to terminate enter -1 -1 -1 -1 )\n");
	
	/* scan the first vector */
	printf("\nvector %d: ", 1);
	get_four_d_vector(&v1_x, &v1_y, &v1_z, &v1_w);	

	/* check if sentinel value is entered as first vector */
	if (v1_x == SENT && v1_y == SENT && v1_z == SENT && v1_w == SENT)
	{
		/* set avarage of all the points as 0 */
		*mean_a0 = 0, *mean_a1 = 0, *mean_a2 = 0, *mean_a3 = 0;
	}
	else
	{
		/* initialize mean of points as component of first entered vector */
		*mean_a0 = v1_x, *mean_a1 = v1_y, *mean_a2 = v1_z, *mean_a3 = v1_w; 

		for (n = 1; n < N; ++n)
		{
			printf("vector %d: ", n + 1);
			get_four_d_vector(&v2_x, &v2_y, &v2_z, &v2_w);	

			/* check if sentinel value is entered as nth vector, if it is entered, terminate taking input */
			if (v2_x == SENT && v2_y == SENT && v2_z == SENT && v2_w == SENT)
			{
				break;
			}
			else
			{
				/* add the points of vector 2 to mean */
				*mean_a0 += v2_x;
				*mean_a1 += v2_y;
				*mean_a2 += v2_z;
				*mean_a3 += v2_w;

				/* calculate the difference between two vectors */
				d0 = v2_x - v1_x;
				d1 = v2_y - v1_y;
				d2 = v2_z - v1_z;
				d3 = v2_w - v1_w;	

				/* calculate the euclidian distance between two consecutive vector v1 and v2 */
				distance_between_4d_points(d0, d1, d2, d3, &euclidian_distance);

				/* update the longest distance */
				if (euclidian_distance > *longest_distance)
				{
					*longest_distance = euclidian_distance;
				}

				/* set v2 as v1 for next iteration */
				v1_x = v2_x ;
				v1_y = v2_y ;
				v1_z = v2_z ;
				v1_w = v2_w ;	
			}
		}

		/* be sure at least one proper vector entered */
		if (n > 0)
		{
			/* Find the average of each dimension across N vectors */
			*mean_a0 /= n;
			*mean_a1 /= n;
			*mean_a2 /= n;
			*mean_a3 /= n;
		}	
	}
	printf("\n");
}

/* scans and returns the component of 4d vector */
void get_four_d_vector (double *x, double *y, double *z, double *w)
{
	scanf("%lf %lf %lf %lf", x, y, z, w);
}

/* takes the difference of two consecutive 4D vectors and find the euclidean distance between them */
void distance_between_4d_points (double d0, double d1, double d2, double d3, double* euclidian_distance)
{
	*euclidian_distance = sqrt ( (d0 * d0) + (d1 * d1) + (d2 * d2) + (d3 * d3) ); 
}

/********************************************************************************************************************/

/* applies dhont method and returns the number of seat of Parties, in case of equality, precedence is A > B > C > D > E */
void dhondt_method (int* partyA, int* partyB, int* partyC, int* partyD, int* partyE, int numberOfSeats)
{
	int 	i,				
		 	max_vote;
	
	/* assign the initial vote number of parties as current vote number */
	int 	cur_vote_A = *partyA, cur_vote_B = *partyB, cur_vote_C = *partyC, cur_vote_D = *partyD, cur_vote_E = *partyE; 
	
	int 	seatA = 0, seatB = 0, seatC = 0, seatD = 0, seatE = 0;

	/* apply dhont method till every there is no seat remain */
	for (i = 0; i < numberOfSeats; ++i)
	{
		/* find the winner's vote number as max_vote according to current vote numbers */
		max_vote = cur_vote_A;

		change_if_bigger(&max_vote, cur_vote_B);
		change_if_bigger(&max_vote, cur_vote_C);
		change_if_bigger(&max_vote, cur_vote_D);
		change_if_bigger(&max_vote, cur_vote_E);

		/* find the winner, increase their seat number, and update their current vote by appling dhont method */
		if (max_vote == cur_vote_A)
		{
			++seatA;
			cur_vote_A = *partyA / (seatA + 1);
		}
		else if (max_vote == cur_vote_B)
		{
			++seatB;
			cur_vote_B = *partyB / (seatB + 1);
		}
		else if (max_vote == cur_vote_C)
		{
			++seatC;
			cur_vote_C = *partyC / (seatC + 1);
		}
		else if (max_vote == cur_vote_D)
		{
			++seatD;
			cur_vote_D = *partyD / (seatD + 1);
		}
		else if (max_vote == cur_vote_E)
		{
			++seatE;
			cur_vote_E =  *partyE / (seatE + 1);
		}					
	} 

	/* return the seat numbers */
	*partyA = seatA;
	*partyB = seatB;
	*partyC = seatC;
	*partyD = seatD;
	*partyE = seatE;
}

/* takes two number and returns the bigger one as first actual argument */
void change_if_bigger (int *max_num, int value)
{
	if (value > *max_num)
	{
		*max_num = value;
	}
}

/********************************************************************************************************************/

/* orders the given three points in counter-clockwise respectivly (x1,y1), (x2,y2), (x3,y3) */
void order_2d_points_cc (double* x1, double* y1, double* x2, double* y2, double* x3, double* y3)
{
	double 	meanx, meany;		/* x and y components of midpoint */

	/* regenerate a new coordinate plate by setting the midpoint of given point as orjin */
	meanx = (*x1 + *x2 + *x3) / 3;
	meany = (*y1 + *y2 + *y3) / 3;

	*x1 -= meanx;
	*y1 -= meany;

	*x2 -= meanx;
	*y2 -= meany;
	
	*x3 -= meanx;
	*y3 -= meany;

	/* order the points in counter-clockwise respectivly (x1,y1), (x2,y2), (x3,y3) */
	order_two_points(x1, y1, x3, y3);
	order_two_points(x1, y1, x2, y2);
	order_two_points(x2, y2, x3, y3);

	/* turn back the initial values and return the cordinates */
	*x1 += meanx;
	*y1 += meany;
		
	*x2 += meanx;
	*y2 += meany;
		
	*x3 += meanx;
	*y3 += meany;
}

/* orders given two poins by looking their located quadrant */
void order_two_points (double *x1, double *y1, double *x2, double *y2)
{
	int quad1, quad2;

	/* find the quadrants of given two point */
	quad1 = get_quadrant(*x1, *y1);
	quad2 = get_quadrant(*x2, *y2);

	/* the smaller quadants component must be comes first, if it's not change them */
	if (quad2 < quad1)
	{
		swap(x1, x2);
		swap(y1, y2);
	}
}

/* find the located quadrant of given coordinate, returns 0 if it's not in specific quadrant */
int get_quadrant (double x, double y)
{
	int quadrant;

	if (x * y > 0)
	{
		if (x > 0)
		{
			quadrant = 1;
		}
		else
		{
			quadrant = 3;
		}
	}
	else if (x * y < 0)
	{
		if (x > 0)
		{
			quadrant = 4;
		}
		else
		{
			quadrant = 2;
		}
	}

	return quadrant;
}

/* swaps two double variable */
void swap (double *x, double *y)
{
	double tmp;

	tmp = *x;
	*x = *y;
	*y = tmp;
}


/********************************************************************************************************************/


/* apply some sort of encrytion algorithm to given number and returns their encrypted value */
void number_encrypt (unsigned char* number)
{
	char b7='-', b6='-', b5='-', b4='-', b3='-', b2='-', b1='-', b0='-';
	
	get_number_components (*number, &b7, &b6, &b5, &b4, &b3, &b2, &b1, &b0);
	
	reconstruct_components (number, b7, b6, b5, b4, b3, b2, b1, b0);
}

/* convert given number as 8 bits */
void get_number_components (unsigned char number, char* b7, char* b6, char* b5, char* b4, char* b3, char* b2, char* b1, char* b0)
{
	int i,	
		num,	/* integer equvialent of given char number */
		pow_2;	/* power of 2 */

	/* initialize all the bits as 0 */
	*b7 = '0', *b6 = '0', *b5 = '0', *b4 = '0', *b3 = '0', *b2 = '0', *b1 = '0', *b0 = '0';

	/* convert type char variable number to integer variable */
	num = (int)number;
	
	while (num > 0)
	{
		for (i = 0, pow_2 = 1; i <= 8; ++i, pow_2 *= 2)
		{

			if (num < pow_2)
			{
				switch (i - 1)
				{
					case 0:
							*b0 = '1';
							break;
					case 1:
							*b1 = '1';
							break;
					case 2:
							*b2 = '1';
							break;
					case 3:
							*b3 = '1';
							break;
					case 4:
							*b4 = '1';
							break;
					case 5:
							*b5 = '1';
							break;
					case 6:
							*b6 = '1';
							break;
					case 7:
							*b7 = '1';
							break;
				}

				num -= pow_2 / 2;
				break;
			}
		}
	}
}

/* apllies encrytion algoritm to given number */
void reconstruct_components (unsigned char* number, char b7, char b6, char b5, char b4, char b3, char b2, char b1, char b0)
{
	int 	i,
			n,
			num;	/* converted value of given bits from binary to decimal 				*/

	char 	sb0, sb1, sb2, sb3, sb4, sb5, sb6, sb7; 	/* shifted bits 					*/

	/* Change bit-7 with bit-2, bit-6 with bit-3, bit-5 with bit-0, bit 4 with bit-1. 		*/
	swap_char(&b7, &b2);
	swap_char(&b6, &b3);
	swap_char(&b5, &b0);
	swap_char(&b4, &b1);

	/* apply two circular left shift */	
	sb7 = b5;
	sb6 = b4;
	sb5 = b3;
	sb4 = b2;
	sb3 = b1;
	sb2 = b0;
	sb1 = b7;
	sb0 = b6;	

	/* convert number binary to decimal */
	num  = get_bit_value(sb7, 7); 
	num += get_bit_value(sb6, 6); 
	num += get_bit_value(sb5, 5); 
	num += get_bit_value(sb4, 4); 
	num += get_bit_value(sb3, 3); 
	num += get_bit_value(sb2, 2); 
	num += get_bit_value(sb1, 1);
	num += get_bit_value(sb0, 0);

	/* return the reconstructed number */
	*number = num;
}

/* returns the decimal equivalent of nth bit, start with 0 */
int get_bit_value (unsigned char bit, int n)
{
	int value;

	if (bit == '1')
	{
		value = pow(2, n);
	}
	else
	{
		value = 0;
	}

	return value;
}

/* change the values of given two char variable */
void swap_char(char *c1, char *c2)
{
	char tmp;

	tmp = *c1;
	*c1 = *c2;
	*c2 = tmp;
}