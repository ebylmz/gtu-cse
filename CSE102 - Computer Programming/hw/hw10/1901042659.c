#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SMALLSTEP 20		/* step size */
#define LARGESTEP 1000		/* step size */

#define FNAME "Movies.txt"

#define MDATANUM 5 		/* the number of data type (name, budget ..) in each line  */

typedef struct {
	double 	budget;
	int 	genre;	/* int equavilent of type * */
	char *  name;
	double  score;
	int 	year;
} movie_t;

typedef struct {
	movie_t ** movie;
	char   	**	type;
	int 	msize,
			tsize;
} lib_t;

typedef enum {name, score, genre, year, budget} movie_data_t;

int process_movies (char fname[]);
int loadfile (FILE * infid, lib_t * lib);
int m_realloc (lib_t * lib, int newsize);
int scan_movie (FILE * infid, lib_t * lib, movie_t ** m, movie_data_t data_order[]);
char * scan_line_comp (FILE * infid);
int movie_update (lib_t * lib, movie_t * m);

int find_type_num (lib_t * lib, char unk_type[]);

void take_op (int * op);
void clear_stdin ();

void print_score_ave (lib_t * lib);
void find_movie_info (lib_t * lib);
void print_movie (lib_t * lib, int n);
void print_types (lib_t * lib);
void print_type_frequence (lib_t * lib);
void show_movie_data (lib_t * lib, movie_t * m);

void  list_through_year (lib_t * lib);
void  list_through_idmb (lib_t * lib);

void sort_data (lib_t * lib);
movie_t ** sort_all (lib_t * lib, int type);
void swap (movie_t ** p, movie_t ** v);


int main (void)
{
/*
	char fname[30];
	printf("Please enter the file name (file.txt): ");
	scanf("%s", fname);
*/
	char * fname = "Movies.txt";	
	process_movies(fname);
}

int process_movies (char fname[])
{
	int 	i, op;
	FILE 	* infid = NULL;
	lib_t 	lib = {NULL, NULL, 0, 0};	/* all the data about all the films in one variable */

	/* dynamicly allocated sorted array */
	infid = fopen(fname, "r");

	if (infid != NLL)
	{
		loadfile(infid, &lib);
		
		if (lib.msize > 0 && lib.movie != NULL)
		{
			printf("msize %d\n", lib.msize);
			
			do {
				/* take the operation code */
				take_op(&op);

				switch (op)
				{
					case 1:	/* List of the Sorted Data 				*/
							sort_data(&lib);
							break;
					case 2: /* List of the Genres 					*/
							print_types(&lib);
							break;
					case 3:	/* List of the Movie Through the Years 	*/
 							list_through_year(&lib);
							break;	
					case 4: /* List of the Movie Through the Scores */
							list_through_idmb(&lib);					
							break;	
					case 5: /* All information of a Single Movie	*/
							find_movie_info(&lib); 
							break;	
					case 6: /* Average of the IDMB Scores			*/  
							print_score_ave(&lib); 
							break;	
					case 7: /* Frequence of the Genres 				*/
 							print_type_frequence(&lib);
							break;	
					case 8: /* Exit 								*/
							break;			
				}
			} while (op != 8);
		}
		else
			printf("File cannot loaded properly\n");

		for (i = 0; i < lib.msize; ++i)
		{
			free(lib.movie[i]->name);
			free(lib.movie[i]);
		}

		fclose(infid);
	}
}

int loadfile (FILE * infid, lib_t * lib)
{
	int 	i, j,
			msize = 0,
			fill,
			status,
			maxsize;

	char  * buff = NULL;  /*!!!!!! free later */
 
	movie_data_t data_order[MDATANUM];

	if (infid != NULL)
	{
		/* define the data order */
		for (i = 0; i < MDATANUM; ++i)
		{
         	buff = scan_line_comp(infid);
			
			if (buff != NULL)
			{
				if (strcmp("name", buff) == 0)
					data_order[i] = name;
				else if (strcmp("score", buff) == 0)
					data_order[i] = score;				
				else if (strcmp("genre", buff) == 0)
					data_order[i] = genre;				
				else if (strcmp("year", buff) == 0 || strcmp("year", buff) == 1)
					data_order[i] = year;				
				else if (strcmp("budget", buff) == 0)
					data_order[i] = budget;	

				free(buff);
			}
		}

		i = maxsize = status = msize = 0;
		
		do {
			/* if we are run out of our allocated memory, allocate bigger pointer array */
			if (i == maxsize)
			{
				maxsize += LARGESTEP;
					
				/* allocate new pointer array */
				if (! m_realloc(lib, maxsize))
				{
					printf("No enough memory for new allocation\n"); break;
				}
			}

			/* scan current line */
 			status = scan_movie (infid, lib, &lib->movie[i], data_order);


			/* 	check if it encountered, use movie_update func, if so free 
				this memory and dont increase the counter */
			
			if (! movie_update(lib, lib->movie[i]) == 1)
			{
 				++i;
 				lib->msize = i; /* m_realloc needs to know the size */
			}
		

 			/* else free the current memory */


		/* continue till status value become EOF or 0 */
		} while (status > 0);
		
		/* return the movie size */
		lib->msize = i - 1;

		/* allocate exact space */
		m_realloc(lib, lib->msize);
	}
	else
		printf("File cannot reached\n");

	return msize;
}

int m_realloc (lib_t * lib, int newsize)
{
	int i,
		status = 0;
	movie_t ** newm = NULL;


	/* allocate a type new pointer array */
	if (lib != NULL)
	{
		newm = (movie_t **) calloc(newsize, sizeof(movie_t *));
		
		if (newm != NULL)
		{
			if (lib->movie != NULL && lib->msize > 0)
			{	
				/* copy all the blocks address to new allocated pointer array */
				for (i = 0; i < lib->msize; ++i)
				{
					newm[i] = lib->movie[i]; 
				}
				/* free the previous pointer array */
				free(lib->movie);
			}			

			lib->movie = newm;
			status = 1;
		}
	}
	return status;
}

/* scans and assigns the movie data in current line, in case of unsuccesfull scanning returns 0 */
int scan_movie (FILE * infid, lib_t * lib, movie_t ** m, movie_data_t data_order[])
{
	int i,
		status = 0;

	char 	* buff = NULL, 
			* tmp = NULL;
	char c;
	movie_t * newm = NULL;


	/* allocate new memory for a movie_t variable */
	/* what if EOF returned you shoul free this memoery !! */
	newm = (movie_t *) malloc(sizeof(movie_t));

	if (newm != NULL)
	{
		/* scan the current line and assgin data in proper way */
		for (i = 0; i < MDATANUM; ++i)
		{
			/* scan the next component char by char */
			buff = scan_line_comp(infid);

			if (buff != NULL)
			{
/*				printf("data_order %d buff: %s\n", data_order[i], buff); */

				/* put scanned data */
				switch (data_order[i])
				{
					case name:
							/* allocate exact space for name */
							tmp = (char *) calloc(strlen(buff) + 1 , sizeof(char));

							if (tmp != NULL)
							{
								strcpy(tmp, buff);
								newm->name = tmp;
							}
							break;

					case score:
							newm->score = strtod(buff, NULL);
							break;

					case genre:
	/* ! */					newm->genre = find_type_num(lib, buff);
							break;

					case year:
							newm->year = atoi(buff);
							break;

					case budget:
							newm->budget = strtod(buff, NULL);
							break;
				}
				free(buff);
			}
			else
			{
				/* printf("null buffer\n"); */
				break;
			}
		}
		/* return the allocated and filled address */
		if (i == MDATANUM)
		{
			*m = newm; 
			status = 1;
		}
		else
			free(newm);
	}

	return status;
}

int find_type_num (lib_t * lib, char unk_type[])
{
	int i, j,
		type_num = - 1;

	char 	** newlist = NULL,
			* newtype = NULL;

/* if we add extra empty line check it */
	if (lib != NULL && unk_type != NULL && strlen(unk_type) > 0)
	{
		for (i = 0; i < lib->tsize; ++i)
			if (strcmp(lib->type[i], unk_type) == 0)
			{
				/* + 1 because of types started from 1 */
				type_num = i + 1; break;	
			}

		/* means no mathing, add as new type */
		if (i == lib->tsize)
		{
			/* allocate bigger pointer array for types */

			newlist = (char **) calloc(lib->tsize + 1, sizeof(char *));

			/* copy address of all the allocated memories for keeping types to new pointer array */
			if (newlist != NULL)
			{
				if (lib->type != NULL)
				{
					for (j = 0; j < lib->tsize; ++j)
						newlist[j] = lib->type[j]; 
				
					free(lib->type);
				}
				
				/* allocate enough memory for new type */
		 		newtype = (char *) calloc(strlen(unk_type) + 1, sizeof(char)); 

			 	if (newtype != NULL)
			 	{
					strcpy(newtype, unk_type);

					newlist[lib->tsize] = newtype;
					
					lib->type = newlist;
					++(lib->tsize);

					/* assign the return value as the last index + 1 of the current ponter array */
					type_num = lib->tsize;
			 	}
			}
		}
	}
	return type_num;
}

char * scan_line_comp (FILE * infid)
{
	int i, j,
		status = 0,
		maxsize;

	char c;

	char * buff = NULL, * tmp = NULL;
	
	if (infid != NULL)
	{
		i = maxsize = 0;

		while (status = fscanf(infid, "%c", &c) > 0)
		{
			if (i == maxsize)
			{
				tmp = buff;
				maxsize += SMALLSTEP;

				buff = (char *) calloc(maxsize, sizeof(char));

				if (buff != NULL && tmp != NULL)
				{
					/* copy old scanned data and then continue with bigger buffer */
					for (j = 0; j < i; ++j)
						buff[j] = tmp[j];

					free(tmp);
				}
			}

			if (c == '\r')
			{
				/* consume \n */
				fgetc(infid); buff[i] = '\0'; break; 
			}
			else if (c == '\n' || c == ',')
			{
				buff[i] = '\0'; break;
				/* allocate exact space for scanning data */
			}
			else
			{
				buff[i] = c; ++i;
			}
		}
	}	
	return buff;
}

int movie_update (lib_t * lib, movie_t * m)
{
	int i,
		status = 0;

	if (lib != NULL && m != NULL)
	{
		for (i = 0; i < lib->msize; ++i)
		{	
			/* overwrite the new data to previos scanned data */
			if (strcmp(lib->movie[i]->name, m->name) == 0)
			{
			/*	printf("%d %s == %s\n",i, lib->movie[i]->name,  m->name); */
/* ! if it is updated */
				/* free this block */
				free(lib->movie[i]->name);
				free(lib->movie[i]);

				lib->movie[i] = m;
				status = 1;
			}
		}
	}

	return status;
}

void take_op (int * op)
{
	int err = 0,
		status;
	
	do {
		if (err != 0)
			printf("[!] Invalid input \n\n");
		
		printf("1. List of the Sorted Data\n");
		printf("2. List of the Genres\n");
		printf("3. List of the Movie Through the Years\n");
		printf("4. List of the Movie Through the Scores\n");
		printf("5. All information of a Single Movie\n");
		printf("6. Average of the IDMB Scores\n");
		printf("7. Frequence of the Genres\n");
		printf("8. Exit\n\n");
		printf("Please select an operation: ");

		status = scanf("%d", op); clear_stdin();
	} while (err = (status != 1 || *op < 1 || *op > 8));
	printf("\n");
}

void clear_stdin ()
{
	while (getchar() != '\n');
}

void print_score_ave (lib_t * lib)
{
	int i;
	double sum = 0;

	if (lib != NULL && lib->msize > 0)
	{
		for (i = 0; i < lib->msize; ++i)
			sum += lib->movie[i]->score;

		printf("\nAverage: %f\n\n", sum / lib->msize);
	}
	else
		printf("\nAverage: 0\n\n");
}

/* finds and print the desired movie information */
void find_movie_info (lib_t * lib)
{
	int i;
/* ! */	char mname[120];
	
	if (lib != NULL)
	{
		printf("Please enter the name of the movie: ");
		scanf("%[^\n]", mname); clear_stdin();

		for (i = 0; i < lib->msize; ++i)
			if (strcmp(lib->movie[i]->name, mname) == 0)
			{
				print_movie (lib, i); break;	/* !!! type ? */
			}

		if (i == lib->msize)
			printf("%s not found in loaded movies\n\n", mname);
	}
	else
		printf("NULL address detected in (find_movie_info()\n");
}

/* prints the given movie information in case of specic numbers converts numbers to string */
void print_movie (lib_t * lib, int i)
{
	printf("%-10s: ", "Budget");
	
	if (lib->movie[i]->budget == 0)
		printf("Unknown\n");
	else
		printf("%.0f\n",  lib->movie[i]->budget);

	/* genre value n is inside of pointer array lib->type[n -1] as string */
	printf("%-10s: %s\n", "Genre", lib->type[lib->movie[i]->genre - 1]);
	printf("%-10s: %s\n", "Name", lib->movie[i]->name);
	printf("%-10s: %.1f\n", "Score", lib->movie[i]->score);
	printf("%-10s: %d\n\n", "Year", lib->movie[i]->year);
}

void print_types (lib_t * lib)
{
	int i;

	if (lib != NULL && lib->type != NULL)
	{
		for (i = 0; i < lib->tsize; ++i)
			printf("%d. %s\n", i + 1, lib->type[i]);
		printf("\n");
	}
}

void print_type_frequence (lib_t * lib)
{
	int 	i,
		 	* count = NULL; /* dynamic array for counting type frequence */

	if (lib != NULL && lib->type != NULL && lib->movie != NULL)
	{
		/* allocate as much as memory that the number of having type  */
		count = (int *) calloc (lib->tsize, sizeof(int));	
		/* check the type of all movies */
		if (count != NULL)
		{
			/*  calloc initialize all the memory as 0, so just use ++,
			 	also genre number start from 1 so substract 1 			*/
			for (i = 0; i < lib->msize; ++i)
				++count[lib->movie[i]->genre - 1];
			
			for (i = 0; i < lib->tsize; ++i)
				printf("%-25s %7d\n", lib->type[i], count[i]);

			free(count);
		}
	}

	if (count != NULL)
		printf("\n");
	else
		printf("There is no data to print\n");
}

void  list_through_year (lib_t * lib)
{
	int status,
		i = 0,
		count,
		bound_year,		/* choosed year 					 	*/
		tperiod;		/* time period Until (0) or Since (1)  	*/

	if (lib != NULL && lib->movie != NULL)
	{
		do {
			printf("Enter a year: ");
			status  = scanf("%d", &bound_year);

			if (status == 1 && bound_year > 0)
			{
				printf("Until (0) or Since (1) %d: ", bound_year);
				status += scanf("%d", &tperiod); clear_stdin();

				if (status == 2)
				{
					count = 0;
					printf("\n");

					switch (tperiod)
					{
						case 0: /* until bound year */
							for (i = 0; i < lib->msize; ++i)
								if (lib->movie[i]->year <= bound_year)
								{
									++count;
									printf("%-5d %s\n", lib->movie[i]->year, lib->movie[i]->name); 
								}
							break;
					
						case 1: /* since bound year */
							for (i = 0; i < lib->msize; ++i)
								if (lib->movie[i]->year >= bound_year)
								{
									++count;
									printf("%-5d %s\n", lib->movie[i]->year, lib->movie[i]->name);
								}
							break;
						
						default:
								printf("\nInvalid Period\n");
					}	

					if (count == 0)
						printf("\nNo movie in that time period\n");
				}
				else
					printf("\nInvalid input\n");
			}
			else
				printf("\nPlease enter a valid year\n");

		} while (count == 0);		
	}
	printf("\n"); 
}


void  list_through_idmb (lib_t * lib)
{
	int 	i,
			range,
			count,
		 	status;
	double 	bound_score;


	if (lib != NULL && lib->movie != NULL)
	{
		do {
			printf("Enter a score: ");
			status = scanf("%lf", &bound_score);

			if (status == 1 && bound_score > 0)
			{
				printf("Less (0) or greater (1): ");
				status += scanf("%d", &range); clear_stdin();
				
				if (status == 2)
				{
					printf("\n");
					count = 0;
					switch (range)
					{
						case 0: /* bigger than bound score */
								for (i = 0; i < lib->msize; ++i)
									if (lib->movie[i]->score <= bound_score)
									{
										++count;
										printf("%-40s %.1f\n",lib->movie[i]->name, lib->movie[i]->score); 
									}
								break;

						case 1: /* smaller than bound score */
								for (i = 0; i < lib->msize; ++i)
									if (lib->movie[i]->score >= bound_score)
									{
										++count;
										printf("%-40s %.1f\n",lib->movie[i]->name, lib->movie[i]->score);
									}
								break;

						default:
								printf("\nInvalid period\n");
					}	

					if (count == 0)
						printf("\nNo movie in that score range\n");				
				}
			}
			else
				printf("\nPlease enter valid score\n");
		} while (count == 0);
	}
	printf("\n"); 
}

/* ascending order  1, 2, 3, 4 ... */
void sort_data (lib_t * lib)
{
	int i, type;

	movie_t ** sorted = NULL;

	do {
		printf("1. Budget\n");
		printf("2. Genre\n");
		printf("3. Name\n");
		printf("4. Score\n");
		printf("5. Year\n");

		printf("Please select an operation: ");
		scanf("%d", &type); clear_stdin();
	/*
		printf("1. Single Selection\n");
		printf("2. Multiple Selection\n");

		printf("Please select an operation\n");
	*/
		sorted = sort_all(lib, type);

		if (sorted != NULL)
		{
			for (i = 0; i < lib->msize; ++i)
	 			show_movie_data(lib, sorted[i]);
	 		free(sorted);
		}
	} while (sorted == NULL);
}

movie_t ** sort_all (lib_t * lib, int type)
{
	int i, j, fill;
	movie_t ** sorted = NULL;

	if (lib != NULL)
	{
		/* allocate an movie_t pointer array to sort addresses */
		sorted = (movie_t **) calloc(lib->msize, sizeof(movie_t *));

		if (sorted != NULL)
		{
			/* copy all the addresses new allocated pointer array */
			for (i = 0; i < lib->msize; ++i)
				sorted[i] = lib->movie[i];

			/* sort the addresses */
			for (i = 0; i < lib->msize; ++i)
			{
				fill = i;

				switch (type)
				{
					case 1:
							for (j = i + 1; j < lib->msize; ++j)
								if (sorted[j]->budget < sorted[fill]->budget)
									fill = j;
							break;
					
					case 2:
							for (j = i + 1; j < lib->msize; ++j)
								if (sorted[j]->genre < sorted[fill]->genre)
									fill = j;
							break;
					
					case 3:
							for (j = i + 1; j < lib->msize; ++j)
								if (strcmp(sorted[j]->name, sorted[fill]->name) < 0)
									fill = j;
							break;
					
					case 4:
							for (j = i + 1; j < lib->msize; ++j)
								if (sorted[j]->score < sorted[fill]->score)
									fill = j;
							break;
					
					case 5:
							for (j = i + 1; j < lib->msize; ++j)
								if (sorted[j]->year < sorted[fill]->year)
									fill = j;
				}
				/* sort by swapping */
				if (fill != i)
					swap(&sorted[i], &sorted[fill]);
			}
		}
	}

	return sorted;
}


void swap (movie_t ** p, movie_t ** v)
{
	movie_t * tmp;

	tmp = *p;
	*p = *v;
	*v = tmp;
}
 
void show_movie_data (lib_t * lib, movie_t * m)
{
	if (lib != NULL && m != NULL)
	{
		if (m->budget == 0)
				printf("%-15s", "Unknown");
			else
				printf("%-15.0f", m->budget);

			printf("%-15s", lib->type[m->genre - 1]);
			printf("%-60s", m->name);
			printf("%-10.1f", m->score);
			printf("%d\n\n", m->year);	
	}
} 