/*
** hw4_lib.c:
**
** The source file implementing library functions.
**
** Author: Yakup Genc. (c) 2018-2021
**
** Revision: 2021.03.03.20.16
** 
*/

#include <stdio.h>
#include "hw1_lib.h"

/* calculates given polynomial */
double calculate(int degree, double coeff[], double x)
{
	/* result of the calculation */
	double result;
	int i;

	/* size is degree of given polynomial */
	int size;


	result = 0;
	size = degree + 1;

	/* to do not lose presence we try to delay multiplication as much as we can */
	for (i = 0; i < size; ++i)
	{
		result = (result * x ) + coeff[i];
	}

	return result;
}

/* calculates integral of 3rd degreee polynomial */
double integral3(double a0, double a1, double a2, double a3, double xs, double xe, double delta)
{	
	/* the result of given integration */
	double area;

	/* the variable for calculate area at specific point, it takes value form xs to xe */
	double tmp;

	/* coefficient of given equation */
	double coeff[4];
	coeff[0] = a0;
	coeff[1] = a1;
	coeff[2] = a2;
	coeff[3] = a3;
 	
 	area = 0.0;

	/* used trapezoidal rule for numeric calculation of the integral of the given polynomial */
	for (tmp = xs; tmp <= xe; tmp += delta)
	{
		/* area of the trapezoidals at intermediate points */
		if (tmp != xs && tmp != xe)
		{
			area += calculate(3, coeff, tmp) * delta;
		}
		/* area of the trapezoidals at endpoints */
		else
		{
			area += calculate(3, coeff, tmp) * (delta / 2.0);
		}
	}

    return area;
}

/* calculates integral of 4th degreee polynomial */
double integral4(double a0, double a1, double a2, double a3, double a4, double xs, double xe, double delta)
{
	/* the result of given integration */
	double area;

	/* the variable for calculate area at specific point, it takes value form xs to xe */
	double tmp;

	/* coefficient of given equation */
	double coeff[5];
	coeff[0] = a0;
	coeff[1] = a1;
	coeff[2] = a2;
	coeff[3] = a3;
 	coeff[4] = a4;

 	area = 0.0;

	/* used trapezoidal rule for numeric calculation of the integral of the given polynomial */
	for (tmp = xs; tmp <= xe; tmp += delta)
	{
		if (tmp != xs && tmp != xe)
		{
			/* area of the trapezoidals at intermediate points */
			area += calculate(4, coeff, tmp) * delta;
		}
		else
		{
			/* area of the trapezoidals at endpoints */
			area += calculate(4, coeff, tmp) * (delta / 2.0);
		}
	}

	return area;
}

/* finds the the root of the given 3rd degree polynomial  */
double root3(double a0, double a1, double a2, double a3, double xs, double xe)
{
	/* the fault tolerance of the function */
	double faultTolerance;

	/* mid point of xs and xe */
	double xm;

	double mVal;
	double eVal;
	double sVal;

	/* coefficient of given equation */
	double coeff[4];
	coeff[0] = a0;
	coeff[1] = a1;
	coeff[2] = a2;
	coeff[3] = a3;

	/* to have a more secure result, keep fault tolerance litle */
	faultTolerance = 0.00001;

	do
	{
		xm = (xs + xe) / 2.0;

		/* calculate the function at our specific points to compare signs */
		mVal = calculate(3, coeff, xm);
		eVal = calculate(3, coeff, xe);
		sVal = calculate(3, coeff, xs);

		/* there is a root at left half (xs between xm) */
		if (sVal * mVal < 0)
		{
			xe = xm;
		}

		/* there is a root at right half (xm between xe) */
		else if (mVal * eVal < 0)
		{
			xs = xm;
		}

		/* result of multiplication is zero, so root is found */
		else
		{
			if (sVal == 0)
			{
				return xs;
			}
			else if (eVal == 0)
			{
				return xe;
			}
			else
			{
				return xm;
			}
		}
	}
	while (xe - xs >= faultTolerance);

	/* we know that the root must be between xe and xs, since we reach the faulty tolerance, we can guess root as a xm */
	return xm;
}

/* finds the the root of the given 4th degree polynomial  */
double root4(double a0, double a1, double a2, double a3, double a4, double xs, double xe)
{
	/* the fault tolerance of the function */
	double faultTolerance;

	/* mid point of xs and xe */
	double xm;

	double mVal;
	double eVal;
	double sVal;

	/* coefficient of given equation */
	double coeff[5];
	coeff[0] = a0;
	coeff[1] = a1;
	coeff[2] = a2;
	coeff[3] = a3;
	coeff[4] = a4;

	/* to have a more secure result, keep fault tolerance litle */
	faultTolerance = 0.00001;

	do
	{
		/* calculate the mid point of given end points */
		xm = (xs + xe) / 2.0;

		/* calculate the function at our specific points to compare signs */
		mVal = calculate(4, coeff, xm);
		eVal = calculate(4, coeff, xe);
		sVal = calculate(4, coeff, xs);

		/* there is a root at left half (xs between xm) */
		if (sVal * mVal < 0)
		{
			xe = xm;
		}

		/* there is a root at right half (xm between xe) */
		else if (mVal * eVal < 0)
		{
			xs = xm;
		}

		/* result of multiplication is zero, so root is found */
		else
		{
			if (sVal == 0)
			{
				return xs;
			}
			else if (eVal == 0)
			{
				return xe;
			}
			else
			{
				return xm;
			}
		}
	}
	while (xe - xs >= faultTolerance);

	/* we know that the root must be between xe and xs, since we reach the faulty tolerance, we can guess root as a xm */
	return xm;
}



