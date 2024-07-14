#include <stdio.h>

void russian_multiplication (unsigned int* multiplicand, unsigned int* multiplier);

void multiply_polynomials (double* a3, double* a2, double* a1, double* a0, double* b3, double* b2, double* b1, double b0);

double multiply_coef(double a, double b);



int main(void)
{
	unsigned int multiplicand,
				 multiplier;

	/* coeffcient of two polynomials */
	double a3, a2, a1, a0, b3, b2, b1, b0;

	/* take multiplicand and multiplier */
	printf("multiplicand: ");
	scanf("%u", &multiplicand);

	printf("multiplier: ");
	scanf("%u", &multiplier);

	/* calcualte russian multiplication */
	russian_multiplication(&multiplicand, &multiplier);

	printf("multiplicand: %u, ", multiplicand);
	printf("multiplier: %u\n", multiplier);

	/* part two */

	/* take the coefficent of first polynomial */
	printf("coeffcient of the first polynomial (a_3, a_2, a_1, a_0)\n");
	printf(">>> ");
	scanf("%lf %lf %lf %lf", &a3, &a2, &a1, &a0);

	/* take the coefficent of second polynomial */
	printf("coeffcient of the second polynomial (b_3, b_2, b_1, b_0)\n");
	printf(">>> ");
	scanf("%lf %lf %lf %lf", &b3, &b2, &b1, &b0);

	/* multiply coeffients */
	multiply_polynomials(&a3, &a2, &a1, &a0, &b3, &b2, &b1, b0);

	/* print the coefficents */
	printf("coefficients:\n");
	printf("n_6 : %.1f\n", a3);
	printf("n_5 : %.1f\n", b3);
	printf("n_4 : %.1f\n", a2);
	printf("n_3 : %.1f\n", b2);
	printf("n_2 : %.1f\n", a1);
	printf("n_1 : %.1f\n", b1);
	printf("n_0 : %.1f\n", a0);
}



void russian_multiplication (unsigned int* multiplicand, unsigned int* multiplier)
{			
	unsigned int sum,		/* accumulator for multiplication result */
				 s_num, 	/* multiplicand holder 					 */
				 l_num;		/* multiplier holder    				 */


	/* assignments */
	sum = 0;
	s_num = *multiplicand;
	l_num = *multiplier;

	/* apply russian multiplication algortim until multiplier is less than or equal to 1 */
	do
	{
		l_num /= 2;
		s_num *= 2;

		/* if multiplier is odd number, add multiplicands value to sum */
		if (l_num % 2 == 1)
		{
			sum += s_num;
		}
	}
	while (l_num > 1);

	/* add the last number */
	
	/* return the values */
	*multiplicand = sum;
	*multiplier = l_num;
}

void multiply_polynomials (double* a3, double* a2, double* a1, double* a0, double* b3, double* b2, double* b1, double b0)
{
	double n6, n5, n4, n3, n2, n1, n0; /* coefficient of new polynomials  */

	/* calculate degree 6 coefficient */
	n6 = multiply_coef(*a3, *b3);

	/* calculate degree 5 coefficient */
	n5 = multiply_coef(*a3, *b2) + multiply_coef(*a2, *b3);

	/* calculate degree 4 coefficient */
	n4 = multiply_coef(*a2, *b2) + multiply_coef(*a3, *b1) + multiply_coef(*b3, *a1);

	/* calculate degree 3 coefficient */
	n3 = multiply_coef(*a3, b0) + multiply_coef(*b3, *a0) + multiply_coef(*a2, *b1) + multiply_coef(*a1, *b2);

	/* calculate degree 2 coefficient */
	n2 = multiply_coef(*a2, b0) + multiply_coef(*b2, *a0) + multiply_coef(*a1, *b1);


	/* calculate degree 1 coefficient */
	n1 = multiply_coef(*a1, b0) + multiply_coef(*b1, *a0);

	/* calculate degree 0 coefficient */
	n0 = multiply_coef(*a0, b0);

	/* return the coeffficient as a_3 b_3 a_2 b_2 a_1 b_1 a_0 */
	*a3 = n6;
	*b3 = n5;
	*a2 = n4;
	*b2 = n3;
	*a1 = n2;
	*b1 = n1;
	*a0 = n0;
}

/* multiplies given two coeffcient in a proper way of polynomial */
double multiply_coef(double a, double b)
{
	/* if one of the coef is 0 just ignore it, otherwise multiply it */
	if (a == 0)
	{
		return b;
	}
	else if (b == 0)
	{
		return a;
	}
	else
	{
		return a * b;
	}
}