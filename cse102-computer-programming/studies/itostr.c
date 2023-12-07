#include <stdio.h>
#include <stdlib.h>

char * itostr (int n)
{
	int i, dign = 0, tmp = n;
	char * s = NULL;

	/* find the digit number to allocate exact space */
	do tmp /= 10, ++dign;  
	while (tmp != 0);

	s = (char *) calloc (dign + 1, sizeof(char));
	if (s != NULL)
	{
		s[dign] = '\0';
		
		for (i = dign - 1; i >= 0; --i)
		{
			s[i] = (n % 10) + '0';
			n /= 10; /* lose the last digit of number */
		}
	}
	return s;
}

int main(void)
{
	char * s = itostr(1234567);
	puts(s);
}