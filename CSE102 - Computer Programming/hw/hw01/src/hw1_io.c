/*
** hw1_io.c:
**
** The source file implementing output functions.
**
** Author: Yakup Genc. (c) 2018-2021
**
** Revision: 2021.03.03.20.16
** 
*/


#include <stdio.h>
#include "hw1_io.h"

/* writes 3rd degree polynomial */
void write_polynomial3(double a0, double a1, double a2, double a3)
{
    /* degree for comparing flow state (x^2, i = 2, i < degree(3)) */
	int degree;
	int i;
	
	/* for holding related numbers in meaningful order */
	double coeff[4];

	/* because of given polynomial is third degree */
	degree = 3;

	coeff[0] = a3;
	coeff[1] = a2;
	coeff[2] = a1;
	coeff[3] = a0;

	/*
	**	the order of algorithm 
	**	(sign) (coefficient) (x and its degree) 
	*/
	for (i = degree; i >= 0; --i)
	{
		/* check the sign of the given coefficient */
		if (coeff[i] > 0 && i != degree)
		{
			printf("+");
		}

		/* there is no need to write '-' because it will automaticly write */

		/* in case of 0, there is nothing to do, continue the next */
		else if (coeff[i] == 0)
		{
			continue;
		}

		/* case of numbers except 1 and -1, 1 and -1 is special case */
		if (coeff[i] != 1 && coeff[i] != -1)
		{
			/* prevents writing extra zero after '.' , checks if the coefficient can be written as a integer without any lose */
			if (intCheck(coeff[i]))
			{
				printf("%d", (int)coeff[i]);
			}
			else
			{
				printf("%.1f",coeff[i]);
			}
		}

		/* prevents writing 1 for -1 */
		else if (coeff[i] == -1)
		{
			if (i != 0)
			{
				printf("-");
			}
			/* the last coefficient is not have x therefore we must indicate -1 without changing it */
			else
			{
				printf("%d", (int)coeff[i]);
			}
			
		}

		/* prints x with its degree */
		if (i > 1)
		{
			printf("x^%d", i);
		}
		else if (i == 1)
		{
			printf("x");
		}
	}

	/* for a good closing */
	printf("\n");
}

/* writes 4th degree polynomial */

void write_polynomial4(double a0, double a1, double a2, double a3, double a4)
{
	/* degree of the polynomial */
	int degree;
	int i;
	
	/* coeefficient of the given polynomial */
	double coeff[5];

	coeff[0] = a4;
	coeff[1] = a3;
	coeff[2] = a2;
	coeff[3] = a1;
	coeff[4] = a0;

	/* forth degree polynomial*/
	degree = 4;


	for (i = degree; i >= 0; --i)
	{
		/* check the sign of the given coefficient */
		if (coeff[i] > 0 && i != degree)
		{
			printf("+");
		}
		/* in case of 0, there is nothing to do */
		else if (coeff[i] == 0)
		{
			continue;
		}


		if (coeff[i] != 1 && coeff[i] != -1)
		{
			/* prevents writing extra zer0 after '.' , checks if the coefficient can be written as a integer without any lose */  
			if (intCheck(coeff[i]))
			{
				printf("%d", (int)coeff[i]);
			}
			else
			{
				printf("%.1f",coeff[i]);
			}
		}
		/* prevents writing 1 */
		else if (coeff[i] == -1)
		{
			if (i != 0)
			{
				printf("-");
			}
			else
			{
				printf("%d", (int)coeff[i]);
			}
			
		}
		/* prints x with its degree */
		if (i > 1)
		{
			printf("x^%d", i);
		}
		else if (i == 1)
		{
			printf("x");
		}
	}

	printf("\n");
}

/* checks if the given number can be written as a integer without any lose of floating number */ 
int intCheck(double num)
{
	if (num - (int)num == 0)
	{
		return 1;
	}
	else
	{
		return 0;
	}
}