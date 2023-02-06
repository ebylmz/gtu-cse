#include <stdio.h>
#include <string.h>

typedef struct winfo_t {
	int slen, upcase, lowcase, digit, blank, non_alph;
} winfo_t;

int word_count (char * s);
int is_valid_date (int d, int m, int y);
int is_palindrome (char * s);
int is_valid_world (char * s);



int main (void)
{
	char * s = "12a21";
	printf(is_palindrome(s) ? "Palindome string\n" : "NonPalindrome string\n");
}

int word_count (char * s)
{
	int i = 0, j, n = 0;

	 while (s[i] != '\0')
	 {
	 	j = 0;
	 	while (s[i] != ' ' && s[i] != '\0') /* scan the charachter till the blank char */
	 		++i, ++j; 	

		while (s[i] == ' ') /* consume the blank char */
	 		++i;
	
	 	if (j > 0) /* if word detected */ 
	 		++n;
	 }
	return n;
}

int is_valid_date (int d, int m, int y)
{
	int maxd;
	int r = 1;
	
	/* check if year is leap */
	switch (m)
	{
		case 1:
		case 3:
		case 5:
		case 7:
		case 8:
		case 10:
		case 12:
			maxd = 31;
			break;
		case 4:
		case 6:
		case 9:
		case 11:
			maxd = 30;
			break;
		case 2:	
			if (y % 4 == 0) /* leap year case */
				maxd = 29;
			else 
				maxd = 28;
			break;
		default: /* invalid month */
			r = 0; 
	}
	if (r != 0)
		r = (d <= 0 && d <= maxd) && (0 <= y);

	return r;
}

int is_palindrome (char * s)
{
	int i, r = 1, slen = 0;

	while (s[slen] != '\0') ++slen;

	if (slen > 0)
	{
		for (i = 0; i < slen; ++i)
		{
			if (s[i] == s[slen - 1])
				--slen;
			else
			{
				r = 0;
				break;
			}
		}

	}
	return r;
}

int is_valid_world (char * s)
{
	int i, r;
	/* should contain vovel letter or 'y' */
	for (i = 0; s[i] != '\0'; ++i)
		switch (s[i])
		{
			case 'a':
			case 'e':
			case 'i':
			case 'o':
			case 'u':
			case 'y':
			case 'A':
			case 'E':
			case 'I':
			case 'O':
			case 'U':
			case 'Y':
				++r;
		}
	return r;
}


void get_winfo (winfo_t * w, char * s)
{
	int i;

	w->upcase = w->lowcase = w->digit = w->blank = w->non_alph = 0;
	w->slen = strlen(s);

	for (i = 0; s[i] != '\0'; ++i)
	{
		if ('A' <= s[i] && s[i] <= 'Z')
			++w->upcase;
		else if ('a' <= s[i] && s[i] <= 'z')
			++w->lowcase;
		else if ('0' <= s[i] && s[i] <= '9')
			++w->digit;
		else if (s[i] == ' ')
			++w->blank;
		else
			++w->non_alph;
	}
}