#include <stdio.h>
#include <time.h>
#include <math.h>

#define ARR_SIZE 20
#define COSN_MAX 100
#define SENT -1

int check_palindrome(int a[], int n);
int search_element(int arr[], int size, int input_number);
float cosx(int n, float x );


int main(void)
{
	int i = 0,			/* counter 												*/
		size,
		cont = 1,		/* (continue) flag for use during taking input process 	*/
		nums[ARR_SIZE];	/* number array 										*/

	int target;			/* target number for test search_element function 		*/

	int 	n;			
	float 	x;

	/* part 1 */

	/* take input from user */
	printf("please enter max %d number (enter to terminate %d): ", ARR_SIZE, SENT);

	do
	{
		scanf("%d", &nums[i]);

		/* check if sentinal value is entered */
		if (nums[i] == SENT)
			cont = 0;
		else
			++i;

	} while (cont);

	/* call check palindrom */
	if (check_palindrome(nums, i))
		printf("Your input is a palindrome\n\n");
	else
		printf("Your input is not a palindrome\n\n");

	/* part 2 */
	srand(time(NULL));

	/* chose random value for target number */
	target = rand() % 100;

	printf("random numbers\n");
	
	/* fill nums array with randomly generated numbers */
	for (i = 0; i < ARR_SIZE; ++i)
	{
		nums[i] = rand() % 100;

		/* at the same time print the generated value  */
		if (nums[i] == target)
			printf("[%d] ", nums[i]);
		else
			printf("%d ", nums[i]);
	}


	/* print the target value and test the search element function */
	printf("\ntarget value: %d ", target);

	if (search_element(nums, ARR_SIZE, target))
		printf(" (INSIDE)\n\n");
	else 
		printf(" (NOT INSIDE)\n\n");


	/* part 3 */
	printf("enter n and x: ");
	scanf("%d %f", &n, &x);

	printf("ans %.1f\n", cosx(n, x));

}

int check_palindrome(int a[], int n)
{
	int ans;

	/* if there is one number to check or there is no number remain, set asnwer as true */
	if (n == 1 || n == 0)
		ans = 1;

	/* if first and last digit not equal, this is not a palindrome */	
	else if (a[0] != a[n - 1])
		ans = 0;

	/* otherwise check the other digits */
	else
		ans = check_palindrome(&a[1], n - 2); 

	return ans;
}

/* checks if an input number is in the given array and return 1 if so and 0 otherwise */
int search_element(int arr[], int size, int input_number)
{
	int ans;

	/* if there is no entry to look, there is no target number */
	if (size == 0)
		ans = 0;

	/* check current value, if target is find set answer as true */
	else if (arr[0] == input_number)
		ans = 1;

	/* otherwise check the remain numbers */
	else 
		ans = search_element(&arr[1], size - 1, input_number);

	return ans;
}


float cosx(int n, float x)
{
	double 	a = 2 * n - 1;
		
	float ans;

	if (n == COSN_MAX)
		ans = 1;
	else
		ans = (1 - (x*x) / (a * (a + 1)) ) * cosx(n + 1, x);

	return ans;
}

