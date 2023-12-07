#include <stdio.h>

void pow_2 (int *number, int n);

void reverse (int *number, int n);

void some_app (void (*func)(int, int *) , int n);

int main(void)
{
	some_app(&reverse , 5);
}

void some_app (void (*func)(int, int *) , int n)
{
	int result;

	()
}

void pow_2 (int *number, int n)
{
	*number = pow((*number),2);
}

void reverse (int *number, int n)
{
	int num;
	int rev_num = 0;

	for (num = *number, i = 0; num != 0; num /= 10, ++i)
	{

		rev_num = rev_num * pow(10, i) + num % 10; 
	}
}

void addition (void (*func)(int, int *), int *sum, int n){