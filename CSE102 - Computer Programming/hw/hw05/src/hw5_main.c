/*
** main.c:
**
** The test/driver program for the homework.
**
** Author: Yakup Genc. (c) 2018-2021
**
** Revision: 2021.04.02.23.55
** 
*/

#include <stdio.h>
#include "hw5_lib.h"


void test_operate_polynomials () 
{	
	double 	a3, a2, a1, a0,
			b3, b2, b1, b0;
	char 	op;
	int 	error;

	printf("[!] y = ax^3 + bx^2 + cx + d, the input should be entered as: (3, a), (2, b), (1, c), (0, d)\n");
	printf("[!] degree of polynomial can be maximum 3\n");
	/* take the coeffient and degree information from user  */
	take_polynomial_term(&a3, &a2, &a1, &a0);
	take_polynomial_term(&b3, &b2, &b1, &b0);
	
	do
	{
		printf("Please select a proper operation (+, -, *)\n");
		printf("[>] ");
		scanf("%c", &op);
	} 
	while (!(op != '+' || op != '-' || op != '*'));

	/* apply selected operation on given polynomials */
	operate_polynomials (&a3, &a2, &a1, &a0, &b3, &b2, &b1, &b0, op);
	
	/* print the result */
	printf("new polynomial: ");
	
	switch (op)
	{
		case '+':
		case '-':
				printf("(3, %.2f), (2, %.2f), (1, %.2f), (0 %.2f)\n\n" , a3, a2, a1, a0);
				break;
				
		case '*':
				printf("(6, %.2f), (5, %.2f), (4, %.2f), (3, %.2f), (2, %.2f), (1 %.2f), (0, %.2f)\n\n" , a3, b3, a2, b2, a1, b1, a0);
				break;
	}
}


void test_four_d_vectors ()
{
	double 	mean_a0=0.0, mean_a1=0.0, mean_a2=0.0, mean_a3=0.0, longest_distance=0.0;
	int 	N=5;
	
	four_d_vectors (&mean_a0, &mean_a1, &mean_a2, &mean_a3, &longest_distance, N);
	
	printf("Mean a0: %f\nMean a1: %f\nMean a2: %f\nMean a3: %f\nThe longest distance between two points: %f\n\n\n", mean_a0, mean_a1, mean_a2, mean_a3, longest_distance);
}


void test_dhondt_method ()
{
	int 	partyA=100000, partyB=80000, partyC=30000, partyD=20000, partyE=10000, numberOfSeats=550;
	
	dhondt_method (&partyA, &partyB, &partyC, &partyD, &partyE, numberOfSeats);
	
	printf("Party A: %3d seat(s).\nParty B: %3d seat(s).\nParty C: %3d seat(s).\nParty D: %3d seat(s).\nParty E: %3d seat(s).\n\n\n", partyA, partyB, partyC, partyD, partyE);
}


void test_order_2d_points_cc ()
{
	double x1=1.0, y1=1.0, x2=-2.0, y2=2.0, x3=3.0, y3=-3.0;
	
	order_2d_points_cc (&x1, &y1, &x2, &y2, &x3, &y3);
	
	printf("Counter-Clockwise Order: (%.2f,%.2f) - (%.2f,%.2f) - (%.2f,%.2f)\n\n\n", x1, y1, x2, y2, x3, y3);
}


void test_number_encrypt ()
{
	unsigned char number=125;
	
	number_encrypt (&number);
	
	printf("Encrypted number: %d\n\n\n", number);
}


/*
** main function for testing the functions...
**
*/
int main(void) 
{
	test_operate_polynomials ();

	test_four_d_vectors ();

	test_dhondt_method ();

	test_order_2d_points_cc ();

	test_number_encrypt ();
	
	return (0);
} /* end main */
