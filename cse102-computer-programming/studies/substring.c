#include <stdio.h>
#include <string.h>

typedef enum {false, true} bool;

bool is_substr (char sub[], char str[]);
void test_is_substr ();

int main(void)
{
	test_is_substr();
}

void test_is_substr ()
{
	char sub[] = "k",
		 str[] = "karakas";

	if (is_substr(sub, str)) 
		printf("YES\n");
	else
		printf("NO\n"); 
}

bool is_substr (char sub[], char str[])
{
	int ans,
		n = strlen(sub);

	printf("sub: %-15s str: %-15s\n", sub, str);

	/* if string length is less than substring, ans must be false */
	if (strlen(str) < n)
		ans = false;

	else if (strncmp(sub, str, n) == 0)
		ans = true;
	
	else
		ans = is_substr(sub, &str[1]);

	return ans;
}