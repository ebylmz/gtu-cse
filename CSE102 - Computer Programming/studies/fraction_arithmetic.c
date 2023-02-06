#include <stdio.h>

void scan_fraction(int *np, int *dp);

char scan_operation();

void add_fraction(int n1, int d1, int n2, int d2, int *n_ansp, int *d_ansp);

void multiply_fraction(int n1, int d1, int n2, int d2, int *n_ansp, int *d_ansp);

void reduce_fraction(int *np, int *dp);

void print_fraction(int n1, int d1, char op, int n2, int d2, int n_ans, int d_ans);

void clear_stdin();



int main(void)
{
	/* nominator and denomitor pointers */
	int n1, d1, n2, d2;
	char op;
	char again;			/* yes or no */
	int n_ans, d_ans;	/* answers of operation */


	do
	{
		/* scan the numbers */
		scan_fraction(&n1, &d1);

		/* take the operation */
		op = scan_operation();

		/* scan the second nominator and denominator */
		scan_fraction(&n2, &d2);


		/* apply the proper operation */
		switch(op)
		{
			case '+':
				add_fraction(n1, d1, n2, d2, &n_ans, &d_ans);
				break;

			case '-':
				add_fraction(n1, d1, -n2, d2, &n_ans, &d_ans);
				break;

			case '*':
				multiply_fraction(n1, d1, n2, d2, &n_ans, &d_ans);
				break;

			case '/':
				multiply_fraction(n1, d1, d2, n2, &n_ans, &d_ans);
				break;
		}

		/* make simple the fraction as possible as */
		reduce_fraction(&n_ans, &d_ans);

		/* print the answer of given operation */
		print_fraction(n1, d1, op, n2, d2, n_ans, d_ans);

		/* continue or stop */
		printf("Do another problem? (y/n): ");
		scanf("%c", &again);
		clear_stdin();
	}
	while (again == 'Y' || again == 'y');
}



void scan_fraction(int *np, int *dp)
{
	char slash;
	int nom, denom;

	printf("Enter a common fraction as two integers seperated by slash: ");
	scanf("%d %c %d", &nom, &slash, &denom);
	clear_stdin();

	while (nom == 0 || denom == 0 || slash != '/')
	{
		printf("\nİnvalid fraction (%d%c%d), reenter fraction: ", nom, slash, denom);
		scanf("%d %c %d, n, &slash, d", &nom, &slash, &denom);
		clear_stdin();
	}


	/* return the nominator and denomitor */
	*np = nom;
	*dp = denom;
}

char scan_operation()
{
	char op;

	printf("Enter an arithmetic operator (+, -, *, /): ");
	scanf("%c", &op);
	clear_stdin();

	while (op != '+' && op != '-' && op != '*' && op != '/')
	{
		printf("\nİnvalid operation (%c), reenter operator (+, -, *, /): ", op);
		scanf("%c", &op);
		clear_stdin();
	}


	return op;
}


void add_fraction(int n1, int d1, int n2, int d2, int *n_ans, int *d_ans)
{
	int nom, denom;
	/* calculate the nominator */
	nom = (n1 * d2) + (n2 * d1);

	/* calculate the denominator */
	denom = d1 * d2;

	/* return nom and denom */
	*n_ans = nom;
	*d_ans = denom;
}

void multiply_fraction(int n1, int d1, int n2, int d2, int *n_ans, int *d_ans)
{
	int nom, denom;
	
	nom = n1 * n2;
	denom = d1 * d2;

	/* return nom and denom */
	*n_ans = nom;
	*d_ans = denom;
}

void reduce_fraction(int *n, int *d)
{
	int gcd; /* greatest common divisor */

	for (gcd = *n; gcd > 1; --gcd)
	{
		/* if both evenly divisible by gcd */
		if (*n % gcd == 0 && *d % gcd == 0)
		{
			*n /= gcd;
			*d /= gcd;
			break;
		}
	} 
}

void print_fraction(int n1, int d1, char op, int n2, int d2, int n_ans, int d_ans)
{
	printf("%d/%d  %c %d/%d = %d/%d\n", n1, d1, op, n2, d2, n_ans, d_ans);
}

void clear_stdin()
{
	char junk;

	do scanf("%c", &junk);
	while (junk != '\n');
}