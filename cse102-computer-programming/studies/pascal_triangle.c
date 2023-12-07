#include <stdio.h>

#define N 6

int main(void)
{
	int i, j,
		p[2][N];	/* no need to use 100x100 array */

	for (i = 0; i < N; ++i)
	{
		for (j = 0; j <= i; ++j)
		{
			/* edges are always 1 */
			if (j == 0 || j == i)
				p[1][j] = 1;
			/* otherwise the current value is sum of two left above and  */
			else
				p[1][j] = p[0][j - 1] + p[0][j];
		}
		/* print them and move the values at second row to first row  */
		for (j = 0; j <= i; ++j)
		{
			p[0][j] = p[1][j];

			printf("%-3d", p[1][j]);
		}
		printf("\n");
	}
}