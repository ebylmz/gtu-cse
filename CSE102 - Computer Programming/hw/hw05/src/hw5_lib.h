/*
** hw5_lib.h:
**
** The header file declaring library functions.
**
** Author: Yakup Genc. (c) 2018-2021
**
** Revision: 2021.04.02.23.55
** 
*/
#define SENT -1		/* sentinel value for 4d four_d_vectors */

typedef enum 
{
	A, B, C, D, E
} party_t;


void operate_polynomials  (double* a3, double* a2, double* a1, double* a0, double* b3, double* b2, double* b1, double* b0, char op);

void take_polynomial_term(double *x3, double *x2, double *x1, double *x0);

void add_polynomials(double *a3, double *a2, double *a1, double *a0, double b3, double b2, double b1, double b0);

void multiply_polynomials (double* a3, double* a2, double* a1, double* a0, double* b3, double* b2, double* b1, double b0);

char skip_blank_ch();

void clear_stdin();


void four_d_vectors (double* mean_a0, double* mean_a1, double* mean_a2, double* mean_a3, double* longest_distance, int N);

void distance_between_4d_points (double d0, double d1, double d2, double d3, double* euclidian_distance);

void get_four_d_vector (double *x, double *y, double *z, double *w);


void dhondt_method (int* partyA, int* partyB, int* partyC, int* partyD, int* partyE, int numberOfSeats);

void change_if_bigger (int *max_num, int value);


void order_2d_points_cc (double* x1, double* y1, double* x2, double* y2, double* x3, double* y3);

void order_two_points (double *x1, double *y1, double *x2, double *y2);

int get_quadrant (double x, double y);

void swap (double *x, double *y);


void number_encrypt (unsigned char* number);

void get_number_components (unsigned char number, char* b7, char* b6, char* b5, char* b4, char* b3, char* b2, char* b1, char* b0);

void reconstruct_components (unsigned char* number, char b7, char b6, char b5, char b4, char b3, char b2, char b1, char b0);

void swap_char(char *c1, char *c2);

int get_bit_value (unsigned char bit, int n);

