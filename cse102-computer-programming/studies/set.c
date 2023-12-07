#include <stdio.h>
#include <string.h>

#define SETSIZE 15

typedef enum {false, true} bool;

bool is_empty (const char *set);
bool is_element (char ele, const char *set);
bool is_set (const char *set);
bool is_subset (const char *sub, const char *set);
void set_union (char *result, const char *set1, const char *set2);
void print_set  (const char *set);
void print_with_commas (const char *set);
void get_set (char *set);
void test_set();

int main(void)
{
	char set1[SETSIZE], set2[SETSIZE];

	printf("First set: ");
	get_set(set1);

	if (is_set(set1))
		printf("Valid set\n");
	else
		printf("Invalid set\n");

	printf("Second set: ");
	get_set(set2);

	if (is_set(set2))
		printf("Valid set\n");
	else
		printf("Invalid set\n");

	printf("Set 1: ");
	print_set(set1);
	putchar('\n');
	printf("Set 2: ");
	print_set(set2);

}

bool is_empty (const char *set)
{
	bool ans;

	if (set[0] == '\0')
		ans = true;
	else
		ans = false;

	return ans;
}

bool is_element (char ele, const char *set)
{
	bool ans;

	if (is_empty(set))
	{
		ans = false;
	}
	else if (set[0] == ele)
	{
		ans = true;
	}
	else 
	{
		ans = is_element(ele, &set[1]);
	}

	return ans;
}	

bool is_set (const char *set)
{
	bool ans;

	if (is_empty(set))
	{
		ans = true;
	}
	else if (is_element(set[0], &set[1]))
	{
		ans = false;
	}
	else
	{
		ans = is_set(&set[1]);
	}
}

bool is_subset (const char *sub, const char *set)
{
	bool ans;

	if (is_empty(&sub[0]))
	{
		ans = true;
	}
	else if (is_element(sub[0], set))
	{
		ans = is_subset(&sub[1], set);
	}
	else 
	{
		ans = false;
	}

	return ans;
}

void set_union (char *result, const char *set1, const char *set2)
{
	/* TODO */
}

void print_set  (const char *set)
{
	putchar('{');
	if (! is_empty(set))
		print_with_commas(set);
	putchar('}');
}

void print_with_commas (const char *set)
{
	if (strlen(set) == 1)
	{
		putchar(set[0]);
	}
	else
	{
		printf("%c, ", set[0]);
		print_with_commas(&set[1]);
	}
}


void get_set (char *set)
{
	char inset[SETSIZE];

	scanf("%s", inset);

	/* only take the set, don't deal with '{' '}' */
	strncpy(set, &inset[1], (strlen(inset) - 2));

	set[strlen(inset) - 2] = '\0';
}

