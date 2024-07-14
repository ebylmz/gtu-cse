#include <stdio.h>
#include <stdlib.h>

#define STDMAX 40 	/* max student number in a class */
#define GAMEMAX 40 	/* max game */
#define MAXPLAT  10	/* max platform */
typedef struct {
	float 	x1, y1, x2, y2, x3, y3, 
			m;
} line;

typedef struct {
	float 	midterm,
			final,
			hw;
} grades;

typedef struct {
	char 	name[20],
			surname[20];
	int 	id;
	grades  result;
} student;

typedef struct {
	char name[20],
		 platform[10][20];
	int plat_num;
	float score;
} game;

void process_line ();
void process_students();
void take_student (student * s);
void calc_average_grades (student * s, int snum);
void process_game ();
void take_game (game * g);
void calc_average_score (game * g, int gnum);
int is_encountered (game * g, char plat[]);

int main (void)
{
	/*process_line(); */
	process_students();
}


void process_line ()
{
	line l;

	/* take the input from user */
	printf("Enter coordinates as x y\n");
	
	printf("x1 y1: ");
	scanf("%f %f", &l.x1, &l.y1);

	printf("x2 y2: ");
	scanf("%f %f", &l.x2, &l.y2);

	printf("x3   : ");
	scanf("%f", &l.x3);

	/* calculate the slope */
/* ! be sure points are not same o.w. x/0 sittitation occurs */
	l.m = (l.y2 - l.y1) / (l.x2 - l.x1);

	/* find the third point */
	l.y3 = l.y2 - l.m * (l.x2 - l.x3);

	/* print the result */
	printf("%-8s: %.1f\n%-8s: %.1f\n", "y3", l.y3, "slope", l.m);
}

void process_students ()
{
	student s[STDMAX];
	int 	i,
			search_id,
			count = 0,
			go;


	do {
		/* take current student data */
		take_student(&s[count]);
		++count;
			
		/* calculate and show the the average of all students results */
		calc_average_grades (s, count);

		/* ask user to continue or exit taking students data */
		printf("Do you want to continue (1/0): ");
		scanf("%d", &go);

	} while (go == 1 && count < STDMAX);

	printf("student id: ");
	scanf("%d", &search_id);

	do {		
		for (i = 0; i < count; ++i)
		{
			if (s[i].id == search_id)
			{
				printf("%-10s %-10s %-10s %-10s %-10s %-10s\n", "name", "surname", "id", "final", "midterm", "homework");
				printf("%-10s %-10s %-10d %10.1f %10.1f %10.1f\n", s[i].name, s[i].surname, s[i].id, s[i].result.final, s[i].result.midterm, s[i].result.hw);
			}
		}
		/* ask user to continue or exit */
		printf("Do you want to continue (1/0): ");
		scanf("%d", &go);
	} while (go == 1);
}

void calc_average_grades (student * s, int snum)
{
	int i;
	double 	midt_sum,
			final_sum,
			hw_sum;
	midt_sum = final_sum = hw_sum = 0;

	/* calculate all the students */
	for (i = 0; i < snum; ++i)
	{
		midt_sum += s[i].result.midterm;
		final_sum += s[i].result.final;
		hw_sum 	+= s[i].result.hw;
	}

	if (snum > 0)
	{
		printf("Average of all student results\n");
		printf("%-10s: %.1f\n", "midterm", midt_sum / snum);
		printf("%-10s: %.1f\n", "final", final_sum / snum);
		printf("%-10s: %.1f\n", "homework", hw_sum / snum);
	}
}

void take_student (student * s)
{
	printf("Personal Informations\n");
	printf("%-10s: ", "name");
	scanf("%s", s->name);	
	printf("%-10s: ", "surname");
	scanf("%s", s->surname);	
	printf("%-10s: ", "id");
	scanf("%d", &s->id);	

	printf("Exam Grades\n");
	printf("%-10s: ", "midterm");
	scanf("%f", &s->result.midterm);	
	printf("%-10s: ", "final");
	scanf("%f", &s->result.final);	
	printf("%-10s: ", "homeworks");	
	scanf("%f", &s->result.hw);
}

void process_game ()
{
	game g[GAMEMAX];
	int count = 0,
		go;

	do {

		take_game (&g[count]);
		++count;

		/* print the average of entered games */
		calc_average_score(g, count);

		/* ask user to continue or exit taking game data */
		printf("Do you want to continue (1/0): ");
		scanf("%d", &go);
	} while (go == 1 && count < GAMEMAX);

	/* print the games for specific platforms */

}

void take_game (game * g)
{	
	int i = 0,
		go;
	printf("name      : ");
	scanf("%s", g->name);

	do {
		printf("platform : ");
		scanf("%s", g->platform[i]);

		/* keep different platfroms */
		if (is_encountered(g, g->platform[i]))
		{
			/* copy platfrom name */
			++(g->plat_num);
		}
		
 	 	++i;
 	 	
		/* ask user to continue or exit taking game data */
		printf("Do you want to continue (1/0): ");
		scanf("%d", &go);

	} while (go == 1 && i < MAXPLAT);
	
	printf("score     : ");
	scanf("%f", &g->score);
}

void calc_average_score (game * g, int gnum)
{
	int i;
	double sum = 0;

	for (i = 0; i < gnum; ++i)
		sum = g[i].score;

	if (gnum > 0)
		printf("scores: %.1f\n", sum / gnum);
	else
		printf("No game to calculate\n");		
}

int is_encountered (game * g, char plat[])
{
	int i,
		retval = 0;

	for (i = 0; i < g->plat_num; ++i)
		if (strcmp(g->platform[i], plat) == 0)
			retval = 1;
	
	return retval;
}