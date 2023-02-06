/*
** hw2_io.h:
**
** The header file declaring library functions.
**
*/

int find_weekday_of_data(int day, int month, int year);

int count_day_between_dates(int start_day, int start_month, int start_year, int end_day, int end_month, int end_year);

double find_angle(double a, double b, double c);

void print_tabulated(unsigned int r11, double r12, int r13, 
                     unsigned int r21, double r22, int r23, 
                     unsigned int r31, double r32, int r33, char border);
