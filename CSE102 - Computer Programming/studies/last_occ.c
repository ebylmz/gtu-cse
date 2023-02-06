#include <stdio.h>

char *foo (char str[], char c);


int main(void)
{
	char 	str[] = "abcdfge",
			c = 'x';

	char 	*p = foo(str, c);

	printf("%p : %s\n", p, p);
}


char *foo (char str[], char c)
{
	char *lastp;

	if (str[0] == '\0')
		lastp = NULL;
	else
	{
		lastp = foo(&str[1], c);

		if (lastp == NULL && str[0] == c)
			lastp = str;
	}

	return lastp;
}

