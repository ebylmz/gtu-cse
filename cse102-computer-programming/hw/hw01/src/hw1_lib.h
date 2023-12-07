/*
** hw4_io.h:
**
** The header file declaring library functions.
**
** Author: Yakup Genc. (c) 2018-2021
**
** Revision: 2021.03.03.20.16
** 
*/


double integral3(double a0, double a1, double a2, double a3, double xs, double xe, double delta);


double integral4(double a0, double a1, double a2, double a3, double a4, double xs, double xe, double delta);


double root3(double a0, double a1, double a2, double a3, double xs, double xe);


double root4(double a0, double a1, double a2, double a3, double a4, double xs, double xe);


double calculate(int degree, double coeff[], double x);
