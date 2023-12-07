/*
	Note: 	set array size constant to enough room, then use how much you need
			you do not to use all the room pass the current size information as actual parameter 
*/


#include <stdio.h>
#include <string.h>

#define N 9

#define MAX 10

void average_finder (int row, int clo, int matrix[][N]);

void fill_matrix (int row, int clo, int matrix[][N]);

void intervew (const char * s1, const char * s2, char * s3);


int main(void)
{
	int 	matrix[N][N];
	char 	s1[MAX],
			s2[MAX],
			s3[2 * MAX];

	fill_matrix(3, 5, matrix);
	average_finder(3, 5, matrix);

	fill_matrix(4, 9, matrix);
	average_finder(4, 9, matrix);

	printf("string 1: ");
	scanf("%s", s1);

	printf("string 2: ");
	scanf("%s", s2);

	intervew(s1, s2, s3);

	printf("interview: %s\n", s3);
}


void average_finder (int row, int clo, int matrix[][N])
{
	int i, j, n;
	double sum;

	for (i = 0, sum = 0, n = 0; i < row; ++i)
	{
		for (j = 0; j < clo; ++j)
		{
			if ((i + j) % 2 == 0)
			{
				sum += matrix[i][j];
				++n;
			}
		}
	}

	printf("\nResult: %.1f\n\n", sum / n);
}


void fill_matrix (int row, int clo, int matrix[][N])
{
	int i, j, n;

	for (i = 0, n = 1; i < row; ++i)
	{
		for (j = 0; j < clo; ++j)
		{
			matrix[i][j] = (2 * n * n) - 3;
			++n;

			printf("%-5d", matrix[i][j]);
		}

	printf("\n");
	}
}

void intervew (const char * s1, const char * s2, char * s3)
{
	int i, j;
	char *str1, *str2;

	/* define the long string */
	if (strlen(s1) > strlen(s2))
	{
		str1 = s1;
		str2 = s2;
	}
	else
	{
		str1 = s2;
		str2 = s1;
	}

	/* str1 is longer than str2 */

	for (i = 0, j = 0; str2[j] != '\0'; i += 2, ++j)
	{
		s3[i] = 	str2[j];
		s3[i + 1] = str1[j];
	}

	while (str1[j] != '\0')
	{
		s3[i] = str1[j];

		++i;
		++j;
	}

	s3[i] = '\0';

}