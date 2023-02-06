#include <stdio.h>

#define PI 3.14

/* figure_data_t: circle_t, rectangle, square */ 

/* figure_t : shape, fig */

typedef struct {
	double 	radius;
} circle_t;

typedef struct {
	double 	width,
			height;
} rectangle_t;

typedef struct {
	double 	side;
} square_t;

typedef union {
	circle_t 	circle;
	rectangle_t	rectangle;
	square_t 	square;
} figure_data_t;

typedef struct {
	char 			shape;
	figure_data_t 	fig;
} figure_t;


void get_figure (figure_t *object);
void print_figure (const figure_t *object);
double compute_perim (const figure_t *object);
double compute_area (const figure_t *object);


int main (void)
{
	figure_t object;

	get_figure(&object);
	print_figure(&object);
}

/* get_figure, compute_area, compute_perim, print figure */

double compute_perim (const figure_t *object)
{
	double perim;

	switch (object->shape)
	{
		case 'C':
		case 'c':
				perim = 2 * PI * object->fig.circle.radius;
				break;

		case 'R':
		case 'r':
				perim = 2 * (object->fig.rectangle.width + object->fig.rectangle.height);  
				break;

		case 'S':
		case 's':
				perim = 4 * object->fig.square.side;
				break;

		default :
				perim = 0;
				printf("[!] Invalid shape\n");
	}

	return perim;
}

double compute_area (const figure_t *object)
{
	double area;

	switch (object->shape)
	{
		case 'C':
		case 'c':
				area = PI * object->fig.circle.radius * object->fig.circle.radius;
				break;

		case 'R':
		case 'r':
				area = object->fig.rectangle.width * object->fig.rectangle.height;
				break;

		case 'S':
		case 's':
				area = object->fig.square.side * object->fig.square.side;
				break;

		default :
				area = 0;
	}

	return area;
}


void get_figure (figure_t *object)
{
	printf("Enter a letter to indicate the object shape or Q to quit.\n");
	printf("C (circle), R (rectangle), or S (square): ");

	object->shape = getchar();

	switch (object->shape)
	{
		case 'C':
		case 'c':
				printf("Radius: ");
				scanf("%lf", &object->fig.circle.radius);
				break;

		case 'R':
		case 'r':
				printf("Width: ");
				scanf("%lf", &object->fig.rectangle.width);
				printf("Height: ");
				scanf("%lf", &object->fig.rectangle.height);
				break;

		case 'S':
		case 's':
				printf("Side: ");
				scanf("%lf", &object->fig.square.side);
				break;

		default :
				printf("[!] Invalid shape code\n");
	}
}	

void print_figure (const figure_t *object)
{
	double 	area = compute_area(object), 
			perim = compute_perim(object);
	
	switch (object->shape)
	{
		case 'C':
		case 'c':
				printf("Radius: %.1f\n", object->fig.circle.radius);
				printf("Perimeter: %.1f\n", perim);				
				printf("Area: %.1f\n", area);
				break;

		case 'R':
		case 'r':
				printf("Width: %.1f\n", object->fig.rectangle.width);
				printf("Height: %.1f\n", object->fig.rectangle.height);
				printf("Perimeter: %.1f\n", perim);
				printf("Area: %.1f\n", area);
				break;
		case 'S':
		case 's':
				printf("Side: %.1f\n", object->fig.square.side);
				printf("Perimeter: %.1f\n", perim);
				printf("Area: %.1f\n", area);
				break;

		default :
				printf("[!] Invalid shape\n");
	}
}