/*
** main.c:
**
** The test/driver program for the homework.
**
*/


#include <stdio.h>
#include "hw2_lib.h"


void test_find_weekday_of_data() 
{
	int date_day = 14, date_month=3, date_year=2021, dayno;
	printf("\n--------------------------------------------------------------------------\n");
	printf("| --> Testing test_find_weekday_of_data...                               |\n");
	printf("--------------------------------------------------------------------------\n");
	dayno = find_weekday_of_data(date_day, date_month, date_year);
	printf("%02d-%02d-%04d is the %dth day of the week!\n",date_day, date_month, date_year, dayno);
}


void test_count_day_between_dates() 
{
	int start_day = 14, start_month=3, start_year=2021;
	int end_day = 23, end_month=3, end_year=2021;
	int numdays;
	printf("\n--------------------------------------------------------------------------\n");
	printf("| --> Testing test_count_day_between_dates...                            |\n");
	printf("--------------------------------------------------------------------------\n");
	numdays = count_day_between_dates(start_day, start_month, start_year, end_day, end_month, end_year);

	printf("The number of days between %02d-%02d-%04d and ",start_day, start_month, start_year);
	printf("%02d-%02d-%04d is %d.\n",end_day, end_month, end_year, numdays);
}


void test_find_angle() 
{
	double a = 1.2, b = 3.1, c = 2.0, alpha;
	printf("\n--------------------------------------------------------------------------\n");
	printf("| --> Testing test_find_angle...                                         |\n");
	printf("--------------------------------------------------------------------------\n");
	alpha = find_angle(a, b, c);
	printf("The angle for a=%f, b=%f and c=%f is %f.\n",a, b, c, alpha);
}


void test_print_tabulated() {
	printf("\n--------------------------------------------------------------------------\n");
	printf("| --> Testing test_print_tabulated...                                    |\n");
	printf("--------------------------------------------------------------------------\n");
	print_tabulated(1, 1.1, -1, 2, 2.0, 2, 3, 3.1, -3, '*');
}


/*
** main function for testing the functions...
**
*/
int main(void) {
	test_find_weekday_of_data();
	test_count_day_between_dates();
	test_find_angle();
	test_print_tabulated();
	return (0);
} /* end main */
