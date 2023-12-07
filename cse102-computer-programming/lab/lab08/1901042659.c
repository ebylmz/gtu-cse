#include <stdio.h>
#include <math.h>

#define MAX_DEG 6
#define N 3
#define PI 3.14

typedef struct {
	double 	matrix[N][N],
			determ;
} matrix;

typedef struct {
	double x, y, z;
} vector;

typedef struct {
	double a0, a1, a2, a3;
} third_order_polynomial;

typedef struct {
	double 	coeff[7],
			integ_val;
	char 	constant;
} polynomial;

void print_matrix(matrix initial_matrix);
void inverse_matrix(matrix* initial_matrix, matrix* inverted_matrix);
void determinant_of_matrix(matrix* initial_matrix);
double determ2 (double c0, double c1, double c2, double c3);

double find_orthogonal(vector vec_1, vector vec_2, vector* output_vec);
double dot_product (vector * vec_1, vector * vec_2);
void cross_product (vector * u, vector * v, vector * pv);
double vector_magnitude (vector * u);

polynomial get_integral(third_order_polynomial p1, third_order_polynomial p2, int a, int b);
double take_integral (double coeff, int degree);
void mult_third_order_poly (third_order_polynomial p1, third_order_polynomial p2, polynomial * p3);
void mult_third_order_poly (third_order_polynomial p1, third_order_polynomial p2, polynomial * p3);
double calculate_poly (const double *coeff, double x);
void print_polynomial (polynomial *p);

int main(void)
{
	vector 	vec_1 = {1, 3, 4},
			vec_2 = {1, 3, 4},
			output_vec;
	double 	angle;
	
	third_order_polynomial 	p1 = {0, 0, 2, 1},
							p2 = {0, 0, 3, 1 };
	polynomial 				p3;
	
	int 					a = 5,
							b = 0;

	/* part 1 */

	/* part 2 */
	angle = find_orthogonal(vec_1, vec_2, &output_vec);
	printf("angle: %.1f\n", angle);
	
	/* part 3 */	
	p3 = get_integral(p1, p2, a, b);

	print_polynomial(&p3);
	printf("integration between %d and %d: %.1f\n", a, b, p3.integ_val);
}

void print_matrix(matrix initial_matrix)
{
	int i, j;

	for (i = 0; i < N; ++i)
	{
		for (j = 0; j < N; ++j)
			printf("%-6f", initial_matrix.matrix[i][j]);
		printf("\n");
	}
}

void inverse_matrix(matrix* initial_matrix, matrix* inverted_matrix)
{
	if (initial_matrix->determ != 0)
	{
		/* generate a new matrix to find inverse matrix */
		inverted_matrix->matrix[0][0] = determ2(initial_matrix->matrix[1][1], initial_matrix->matrix[1][2], initial_matrix->matrix[2][1], initial_matrix->matrix[2][2]);
		inverted_matrix->matrix[0][1] = -1 * determ2(initial_matrix->matrix[1][0], initial_matrix->matrix[1][2], initial_matrix->matrix[2][1], initial_matrix->matrix[2][2]);
		inverted_matrix->matrix[0][2] = determ2(initial_matrix->matrix[1][0], initial_matrix->matrix[1][1], initial_matrix->matrix[2][0], initial_matrix->matrix[2][1]);
		inverted_matrix->matrix[1][0] = -1 * determ2(initial_matrix->matrix[0][1], initial_matrix->matrix[0][2], initial_matrix->matrix[2][1], initial_matrix->matrix[2][2]);
		inverted_matrix->matrix[1][1] = determ2(initial_matrix->matrix[0][0], initial_matrix->matrix[0][2], initial_matrix->matrix[2][0], initial_matrix->matrix[2][1]);
		inverted_matrix->matrix[1][2] = -1 * determ2(initial_matrix->matrix[0][0], initial_matrix->matrix[0][1], initial_matrix->matrix[2][0], initial_matrix->matrix[2][1]);
		inverted_matrix->matrix[2][0] = determ2(initial_matrix->matrix[0][1], initial_matrix->matrix[0][2], initial_matrix->matrix[2][1], initial_matrix->matrix[2][2]);
		inverted_matrix->matrix[2][1] = -1 * determ2(initial_matrix->matrix[0][1], initial_matrix->matrix[0][2], initial_matrix->matrix[1][1], initial_matrix->matrix[1][2]);
		inverted_matrix->matrix[2][2] = determ2(initial_matrix->matrix[0][0], initial_matrix->matrix[0][1], initial_matrix->matrix[1][0], initial_matrix->matrix[1][1]);
		
		/* find transpose of this matrix */

		/*find the determinant of new generated matrix */
		determinant_of_matrix(inverted_matrix);

		/* multiply with 1 / M (M = determinant of initial matrix)*/
	}
}

void determinant_of_matrix(matrix* initial_matrix)
{
	double determ;

	determ  = initial_matrix->matrix[0][0] * determ2(initial_matrix->matrix[1][1], initial_matrix->matrix[1][2], initial_matrix->matrix[2][1], initial_matrix->matrix[2][2]);
	determ -= initial_matrix->matrix[0][1] * determ2(initial_matrix->matrix[1][0], initial_matrix->matrix[1][2], initial_matrix->matrix[2][0], initial_matrix->matrix[2][2]);
	determ += initial_matrix->matrix[0][2] * determ2(initial_matrix->matrix[1][0], initial_matrix->matrix[1][1], initial_matrix->matrix[2][0], initial_matrix->matrix[2][1]);

	/* return the calculated determinant */
	initial_matrix->determ = determ;	
}

/*	| c0  c1 |
	| c2  c3 |		*/
double determ2 (double c0, double c1, double c2, double c3)
{
	return c0 * c3 - c1 * c2;
}

/* 	calculate and return the angle between input vectors vec_1 and vec_2 in degrees, 
	and returns cross product of given two vector as output_vector					 */
double find_orthogonal(vector vec_1, vector vec_2, vector* output_vec)
{
	double val;

	/* vector cross product and return it in the output argument. */
	cross_product(&vec_1, &vec_2, output_vec);

	printf("cross_product of vector: vx: %.1f, vy: %.1f, vz: %.1f\n", output_vec->x, output_vec->y, output_vec->z);

	/* find cos value of desired angle from dot product equation */
	val = dot_product(&vec_1, &vec_2) / (vector_magnitude(&vec_1) * vector_magnitude(&vec_2));

	printf("value before acos %.1f\n", val);

	/* to prevent floating point numbers errors, acos range [-1, 1] */
	if (val > 0)
		val -= 0.00000001;
	else if (val < 0)
		val += 0.00000001;

	/* find the angle from arccos and return as degree */
	return acos(val) * (180 / PI);
}

void cross_product (vector * u, vector * v, vector * pv)
{
	pv->x = determ2(u->y, u->z, v->y, v->z);
	pv->y = -1 * determ2(u->x, u->z, v->x, v->z);
	pv->z = determ2(u->x, u->y, v->x, v->y);
}

double dot_product (vector * vec_1, vector * vec_2)
{
	return 	vec_1->x * vec_2->x +
			vec_1->y * vec_2->y +
			vec_1->z * vec_2->z ;
}

double vector_magnitude (vector * u)
{
	return sqrt(u->x * u->x + u->y * u->y + u->z * u->z);
}


/* multiplies the p1 and p2 and  return the integral of that polynomial in range [a, b */
polynomial get_integral(third_order_polynomial p1, third_order_polynomial p2, int a, int b)
{
	/* multiply two third degree polynomial */
	int i;
	polynomial p3;

	mult_third_order_poly(p1, p2, &p3);

	/* take integral of generated new polynomial, skip the constant p3.coeff[6] */
	for (i = 5; i > 0; --i)
		p3.coeff[i + 1] = take_integral(p3.coeff[i], 6 - i);
	
	/* cp3.coeff[0] is mut be 0 because there is no 6 degree component */
	p3.coeff[0] = 0;

	/* calculate the integration in range (a, b) */
	p3.integ_val = calculate_poly(p3.coeff, a) - calculate_poly(p3.coeff, b);

	return p3;
}

double calculate_poly (const double *coeff, double x)
{
	int 	i;
	double 	result = 0;
	
	for (i = 0; i <= MAX_DEG; ++i)
		result += coeff[i] * pow(x, MAX_DEG - i);


	return result;
}

void print_polynomial (polynomial *p)
{
	int i;

	for (i = 0; i < MAX_DEG; ++i)
		if (p->coeff[i] != 0)
			printf("%+.1fx^%d ", p->coeff[i], MAX_DEG - i);
	
	if (p->coeff[i] != 0)
		printf("%+.1f\n", p->coeff[i]);
	else
		printf("\n");
}

double take_integral (double coeff, int degree)
{
	/* multiply with coeffient and degree */
	return coeff * degree;
}

void mult_third_order_poly (third_order_polynomial p1, third_order_polynomial p2, polynomial * p3)
{
	/* multiply the polynomials */
	p3->coeff[0] = p1.a0 * p2.a0;
	p3->coeff[1] = p1.a0 * p2.a1 + p1.a1 * p2.a0;
	p3->coeff[2] = p1.a0 * p2.a2 + p1.a1 * p2.a1 + p1.a2 * p2.a0;
	p3->coeff[3] = p1.a0 * p2.a3 + p1.a1 * p2.a2 + p1.a2 * p2.a1 + p1.a3 * p2.a0;
	p3->coeff[4] = p1.a1 * p2.a3 + p1.a2 * p2.a2 + p1.a3 * p2.a1;
	p3->coeff[5] = p1.a2 * p2.a3 + p1.a3 * p2.a2;
	p3->coeff[6] = p1.a3 * p2.a3;												
}