/*
** main.c:
**
** The test/driver program for the homework.
**
** Author: Yakup Genc. (c) 2018-2021
**
** Revision: 2021.03.03.20.16
** 
*/

#include <stdio.h>


#include "hw1_lib.h"
#include "hw1_io.h"


/*  main function for testing the functions */
int main(void)
{
	/* for compare results */
	double found;

	/* for numeric integration */
	double delta;

	/* result of our functions */
	double root_result4;
	double integral_result4;
	
	double root_result3;
	double integral_result3;

	/* coefficients */
	double coeff3[4];
	double coeff4[5];

	/* delta for numeric integration */
	delta = 0.01;


	/* coefficient of 3rd degree equation */
	coeff3[0] = -1;
	coeff3[1] = 0;
	coeff3[2] = 5.2;
	coeff3[3] = 2;;

	/* real results calculated by calculator */
	root_result3 = 2.61;
	integral_result3 = -86.6;


	/* testing both derivative and integral of 3th degree equations */
	write_polynomial3(coeff3[0], coeff3[1], coeff3[2], coeff3[3]);

	found = root3(coeff3[0], coeff3[1], coeff3[2], coeff3[3], 1, 3);
	printf("real root: %.1f vs found root: %.1f \n", root_result3, found);

	found = integral3(coeff3[0], coeff3[1], coeff3[2], coeff3[3], 1, 5, delta);
	printf("real integral result: %.1f vs found result: %.1f \n\n", integral_result3, found);
	


	/* coefficient of 4th degree equation */
	coeff4[0] =  0.2;
	coeff4[1] = -3;
	coeff4[2] = 7.1;
	coeff4[3] = 5.4;
	coeff4[4] =  2;

	/* real results calculated by calculator */
	root_result4 = 11.78;
	integral_result4 = 23.26;

	/* testing both derivative and integral of 4th degree equations */
	write_polynomial4(coeff4[0], coeff4[1], coeff4[2], coeff4[3], coeff4[4]);

	found = root4(coeff4[0], coeff4[1], coeff4[2], coeff4[3], coeff4[4], 10, 12);
	printf("real root: %.1f vs found root: %.1f \n", root_result4, found);


	found = integral4(coeff4[0], coeff4[1], coeff4[2], coeff4[3], coeff4[4], 1, 5, delta);
	printf("real integral result: %.1f vs found result: %.1f \n\n", integral_result4, found);
	
	

	return (0);
} /* end main */




/*
** Testing root finding..
*/
void test_root_finder3() 
{
	double root_known = 1.0;
	double root_found;
	root_found = root3(1.2, 1.0, -1.0, -1.2, root_known-0.1, root_known+0.1);
	printf("Ground truth root: %f vs found root: %f\n", root_known, root_found);
}