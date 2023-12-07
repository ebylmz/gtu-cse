#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define STEPSIZE 20     /* in case of reaching max buffer size, amount of incrementation for new allocation */
#define MDATANUM 5 		/* the number of data type (name, budget ..) in each line  */

typedef struct Movie_Name {
	char * name, * genre;
	double score;
	struct Movie_Name * next;
} Movie_Name;

typedef struct Movie_Budget {
	char * 	name;
	int 	budget, year;
	struct 	Movie_Budget * next;
} Movie_Budget;

typedef struct genre_t { /* keeps genre name and the occurance in the given file */
	char * genre;
	int frequence;
	struct genre_t * next; 
} genre_t;

typedef struct lib_t {
	Movie_Budget * ll_budget;	
	Movie_Name * ll_name;
	genre_t * ll_genre;
} lib_t;

typedef enum {budget, genre, name, score, year} movie_data_t; 	/* line data order */

int 	process_movies (char fname[]);
int 	loadfile (char fname[], lib_t * lib);
int 	get_movie (FILE * fid, lib_t * lib);
char * 	scan_line_comp (FILE * fid);
int 	update_movie (lib_t * lib, Movie_Budget * bp_new, Movie_Name * np_new);
void 	insert_movie (lib_t * lib, Movie_Budget * bp, Movie_Name * np);
void 	sort_data (lib_t * lib);
void 	sorted_merge (Movie_Budget * bl_1, Movie_Budget * bl_2, Movie_Name * nl_1, Movie_Name * n_l2, Movie_Budget ** bl_sorted, Movie_Name ** nl_sorted, movie_data_t type);
void 	split (Movie_Budget * bl_0, Movie_Budget ** bl_1, Movie_Name * nl_0, Movie_Name ** nl_1);
void 	merge_sort (Movie_Budget * bl, Movie_Name * nl, Movie_Budget ** bl_sorted, Movie_Name ** nl_sorted, movie_data_t type);
void  	list_through_year (Movie_Budget * l);
void  	list_through_score (Movie_Name * l);
void 	print_genres (const genre_t * g);
void 	print_genres_frequence (const genre_t * g);
void 	find_movie (lib_t * lib);
double 	calculate_idmb_ave (Movie_Name * l);
int 	take_op ();
void 	clear_stdin ();


int main (void)
{
	int err = 1, n;
	char fname[200];
	
	do {	/* be sure about the file is proper for execution */
		printf("Please enter the file name: ");
		scanf("%s", fname); clear_stdin();

		n = process_movies (fname);

		if (n > 0)		/* no error, proccessing movies is suffesfully done */
			err = 0;
		else if (n == 0)
			printf("File is empty\n");
		else if (n == -1)
			printf("File cannot open, missing or locked\n");
		printf("\n");
	} while (err);
}

/* reads the given file and offers some information about the movies like sorting in specific field, returns the number of scanned line */
int process_movies (char fname[])
{
	int n, r;
	double ave = -1;
	lib_t lib = {NULL, NULL, NULL};
	
	genre_t 		* gp;
	Movie_Budget 	* bp;
	Movie_Name 		* np;
 	
 	r = n = loadfile(fname, &lib);

 	if (n > 0) 
 	{
 		printf("%d line scanned, loading process is succesfully done", n);
 	 	n = 1;

		do {	
			if (n > 0 && n < 8)
			{
				printf("\nEnter to continue "); getchar();
			}
			n = take_op();	/* take the operation code */
			
			switch (n)
			{
				case 1:	/* List of the Sorted Data 				*/
						sort_data(&lib);
						break;
				case 2: /* List of the Genres 					*/
						print_genres(lib.ll_genre);
						break;
				case 3:	/* List of the Movie Through the Years 	*/
						list_through_year(lib.ll_budget);
						break;	
				case 4: /* List of the Movie Through the Scores */
						list_through_score(lib.ll_name);
						break;	
				case 5: /* All information of a Single Movie	*/
						find_movie(&lib);
						break;	
				case 6: /* Average of the IDMB Scores			*/ 
						if (ave < 0)
							ave = calculate_idmb_ave(lib.ll_name);
						printf("Average: %f\n", ave); 
						break;
				case 7: /* Frequence of the Genres 				*/
						print_genres_frequence(lib.ll_genre);
						break;	
				case 8: /* Exit 								*/
						printf("Exited");
						r = 1;	/* return value for succesful execution */
						break;
				default:
						printf("\nPlease choose an valid operation\n");			
			}
		} while (n != 8);

		/* free all the database */
		while (lib.ll_budget != NULL)
		{
			bp = lib.ll_budget;

			np = lib.ll_name;

			lib.ll_budget = lib.ll_budget->next;
			free(bp->name);
			free(bp);
			lib.ll_name = lib.ll_name->next;
			free(np);
		}

		while (lib.ll_genre != NULL)
		{
			gp = lib.ll_genre;
			lib.ll_genre = lib.ll_genre->next;
			free(gp->genre);
			free(gp);
		}
 	}
 	return r; /* return the number of scanned line in given file */
}

/* returns -1 for missing or locked file, 0 for empty file and positive numbers for the number of scanned line */
int loadfile (char fname[], lib_t * lib)
{
	int count = 0, c;
	FILE * fid = fopen(fname, "r"); /* file identifier */
	Movie_Budget 	* bp = NULL;	/* points the current node in ll_budget linked list */
	Movie_Name 		* np = NULL;	/* points the current node in ll_name linked list */
	
	if (fid == NULL)
		count = -1; 		/* to indicate file cannot open */
	else if (lib != NULL)
	{
		/* make sure to start everything from scratch */
		lib->ll_name = NULL; lib->ll_budget = NULL; lib->ll_genre = NULL;
		
		/* check if the file is empty, if not skip the first line since it contains the data types */
		do {
			c = fgetc(fid); 
		} while (c != EOF && c != '\n');

		if (c != EOF)
		{
			count = 1;	/* since first line skipped */
			
			while (get_movie(fid, lib) == 1) /* get all the movies */
				++count;
		}
		fclose(fid);
	}

	return count;
}

int get_movie (FILE * fid, lib_t * lib)
{
	int i = 0, status;
	double data;		/* buffer for double variables 						*/
	char * buff = NULL;	/* buffer for scanning and assigning data as string */

	Movie_Budget 	* bp, 	* back_bp;
	Movie_Name 		* np, 	* back_np;
	genre_t 		* gp, 	* back_gp;

	bp =  (Movie_Budget *) 	malloc(sizeof(Movie_Budget));
	np = 	(Movie_Name *) 	malloc(sizeof(Movie_Name));
	bp->name = np->genre = np->name = NULL;

	if (lib != NULL && bp != NULL && np != NULL)
	{
		for (i = 0; i < MDATANUM; ++i)
		{
			buff = scan_line_comp(fid);
			
			if (buff == NULL)
				break;
			else
			{
				/* data at the file line is ordered as budget,genre,name,score,year */
				switch (i)
				{
					case budget:
					case score:
					case year:
							data = strtod(buff, NULL);
							switch (i)
							{
								case budget:
										bp->budget = data;
										break;
								case score:
										np->score = data;
										break;
								case year:
										bp->year = data;
							}
							free(buff);
							break;

					case genre:
							/* 	check if the genre is encountered before, if so increase
							 	it's frequency, o.w. insert it as a new genre			*/
							back_gp = gp = lib->ll_genre;	/* keep the head of the genre list to comparison process below */
							
							while (gp != NULL)
							{
								/* compare candidate genre and haved genres */
								if (strcmp(buff, gp->genre) == 0)
								{
									/* current genre encountered before, so increase the frequence of it */
									++gp->frequence;
									/* no need to allocate a new memory, because this genre already exist */
									np->genre = gp->genre;
									free(buff);
									break;
								}
								else
								{
									back_gp = gp;
									gp = gp->next;
								}
							}
							
							/* new genre detected, so allocate a tpye genre_t node and insert it */
							if (gp == NULL)
							{
								gp = (genre_t *) malloc(sizeof(genre_t));
								
								if (gp != NULL)
								{
									gp->genre = buff; /* assign the address of genre as a string */
									gp->frequence = 1;
									gp->next = NULL;

									/* there is no node before, this is first insertion of the genre list */
									if (back_gp == NULL)
										lib->ll_genre = gp;
									else
										back_gp->next = gp;

									/* np->genre points and the node in ll_genre points the same memory location for same string  */
									np->genre = gp->genre; 
								}
							}
							break;

					case name:
							/* it is eneough to free just one, be careful double free !! */
							bp->name = np->name = buff;
							break;
				}
			}
		}
	}
	/* 	if all the data scanned succesfully, check the new movie is encountered
		before if so update the movie with new data o.w. just insert it 		*/
	if (i == MDATANUM)
	{		
		if (update_movie(lib, bp, np) != 1)
			insert_movie(lib, bp, np); 

		status = 1;
	}
	else
	{
		/* check if buffer and bp->name points the same where, if so just free one of them o.w. double free may occurs */
		if (buff != NULL) free(buff);
		if (bp->name != buff && bp->name != NULL) free(bp->name);
		if (bp != NULL) free(bp);
		if (np != NULL) free(np);
		status = 0;
	}

	return status; 
}

int update_movie (lib_t * lib, Movie_Budget * bp_new, Movie_Name * np_new)
{
	int status = 0;
	Movie_Budget 	* bp, 	* back_bp;
	Movie_Name 		* np, 	* back_np;
	genre_t 		* gp;

	if (lib != NULL)
	{
		back_bp = bp = lib->ll_budget;
		back_np = np = lib->ll_name;

		while (bp != NULL)
		{
			/* 	if year information does not change with new data, just change the old data
				with new data to do not deal with inserting by rule again, 
				o.w. we should re inserting again without breaking the order rule */
			if (strcmp(bp_new->name, bp->name) == 0)
			{
				/* decrease the frequence of genre of old movie */
				gp = lib->ll_genre;
				
				/* 	since we are using same place for genres no need to use strcmp,
					also no need to check if gp != NULL, because the if sentence above garentees
					that there must be at least two movie node and ll_genre contains at least one genre  */			
				while (gp->genre != np->genre)
					gp = gp->next;

				--(gp->frequence);

				/* to update the movie just insert to place of previos data, and free previos data */
				if (bp_new->year == bp->year)
				{
					/* link forward */
					bp_new->next = bp->next;
				 	np_new->next = np->next;

				 	/* link backward */
					if (back_bp == bp)
					{
						lib->ll_budget = bp->next;
						lib->ll_name = np->next;
					}
					else
					{
						back_bp->next = bp_new;
						back_np->next = np_new;
					}
				}
				/* remove the previos data and insert the new data without breaking order rule */
				else
				{
					if (back_bp == bp)
					{
						lib->ll_budget = bp->next;
						lib->ll_name = np->next;
					}
					else
					{
						back_bp->next = bp->next;
						back_np->next = np->next;
					}
					insert_movie(lib, bp_new, np_new);
				}

				/* remove/free the previos data */
				free(bp->name);
				free(bp);
				free(np);

				status = 1;	break;
			}
			else
			{
				back_bp = bp;
				back_np = np;

				bp = bp->next;
				np = np->next;
			}
		}
	}
	return status;
}

/* 	inserting given node as descending order by year, in case of same year looks budget */
void insert_movie (lib_t * lib, Movie_Budget * bp, Movie_Name * np)
{
	Movie_Budget * tmp_bp, * back_bp;
	Movie_Name 	 * tmp_np, * back_np;

	if (lib != NULL && bp != NULL && np != NULL)
	{
		/* initalize both tmp pointers as points the first node of the list */
		back_bp = tmp_bp = lib->ll_budget;
		back_np = tmp_np = lib->ll_name;

		/* continues till find a node which has lower value than new node */
		while (tmp_bp != NULL)	
		{
			/* stop if new node (bp) has higher value, o.w. pass the next node */
			if (bp->year > tmp_bp->year)
				break;
			/* in case of equality check, budget value */
			else if (bp->year == tmp_bp->year)
			{
				if (bp->budget > tmp_bp->budget || bp->budget == tmp_bp->budget)
					break;
				/* in case of equality, check the name as alpabetical order (A > B) */
			}
			/* iterate bp and np, keep the back node of current node for backward linking */
			back_bp = tmp_bp;
			back_np = tmp_np;

			tmp_bp = tmp_bp->next;
			tmp_np = tmp_np->next;
		}
	}

	/* since tmp_bp initialize as ll_budget, if tmp_bp does not change there is two possiblty */
	if (tmp_bp == lib->ll_budget)
	{
		/* one is there is no list, so change the head of the list as points new node */
		if (tmp_bp == NULL)
		{
			bp->next = NULL; 
			np->next = NULL; 

			/* link backward */
			lib->ll_budget = bp;  
			lib->ll_name = np;
		}
		/* second is after comparision we find the highest value as current node at first time */
		else
		{
			/* insert new node as first and points the previos first node */
			bp->next = tmp_bp; 
			np->next = tmp_np; 

			/* tmp_bp == lib->ll_budget, so bp will be first node of the ll_budget */
			lib->ll_budget = bp;  
			lib->ll_name = np;		
		}
	}
	else
	{
		/* after all the comparison, there is no where find so just insert as last node */
		if (tmp_bp == NULL)
		{
			bp->next = NULL;
			np->next = NULL;
		}
		else
		{
			bp->next = tmp_bp;
			np->next = tmp_np;
		}

		/* link backward */
		back_bp->next = bp;
		back_np->next = np;
	}
}

char * scan_line_comp (FILE * fid)
{
	int 	i, j,
			bsize;		/* max buffer size */
	char 	c, * tmp = NULL, * buff = NULL;

	if (fid != NULL)
	{
		bsize = i = 0;
		while (!feof(fid))
		{
			if (i == bsize)
			{
				bsize += STEPSIZE;
				tmp = (char *) calloc(bsize, sizeof(char));

				if (tmp != NULL)
				{
					/* copy scanned data to new allocated memory */
					for (j = 0; j < i; ++j)
						tmp[j] = buff[j];

					if (buff != NULL) free(buff);
					buff = tmp;
				}	
				/* in case of no enough memory, just return NULL to indicate unsuccesful scan */
				else
				{
					if (buff != NULL)
					{
						free(buff);
						buff = NULL; 		
					}
					break;
				}
			}

			c = fgetc(fid);
			
			if (c == ',' || c == '\n')
			{
				buff[i] = '\0'; break;
			}
			else if (c == '\r')
			{
				fgetc(fid);	/* after '\r' '\n' comes, so scan also it */
				buff[i] = '\0'; break;
			}
			else
				buff[i++] = c; 
		}
		/* in case of empty line scanned, buff only contains '\0', but this not an error */
		if (i > 0 && buff[0] == '\0')
		{
			free(buff);
			buff = NULL;
		}
	}
	return buff;
} 

void sort_data (lib_t * lib)
{
	int 			s, err, n = 0,
					count, select, start, end;
	movie_data_t 	type;
	Movie_Budget 	* bl_sorted = NULL;
	Movie_Name 	 	* nl_sorted = NULL;

	if (lib != NULL && lib->ll_budget == NULL)
		printf("There is no data to sort\n");
	else
	{
		/* take sorting features from user to sorting specific field */
		do {
			err = 0;
			switch (n)
			{
				case 0: /* scan the sorting data type */
					printf("1. Budget\n");
					printf("2. Genre\n");
					printf("3. Name\n");
					printf("4. Score\n");
					printf("5. Year\n");
					printf("\nPlease select an operation: ");
					s = scanf("%u", &type);

					if (s != 1 || type < 1 || type > 5)
						err = 1;
					else 
						type -= 1; /* to use as movie_data_t substract one */
					break;

				case 1:	/* scan the selection type */
					printf("1. Single Selection\n");
					printf("2. Multiple Selection\n");
					printf("\nPlease select an operation: ");
					s = scanf("%d", &select);

					if (s != 1 || select < 1 || select > 2)
						err = 1;
					else if (select == 2) /* skip case 2 which is for single selection */
						++n;
					break;

				case 2:	/* scan the index value for single selection */
					printf("Enter a value: ");
					s = scanf("%d", &start);

					if (s != 1 || start < 1)
						err = 1; 
					else
					{
						end = start;
						++n; /* skip case 3 which is for multiple selection */
					}
					break;
				
				case 3:	/* scan the start end value of range for multiple selection */
					printf("Enter start value: ");
					s = scanf("%d", &start);
					printf("Enter end value  : ");
					s += scanf("%d", &end);

					if (s != 2 || start < 1 || end < start)
						err = 1;
					break;
		
				case 4: /* apply merge sort and print the movies according to user specific selection */
					merge_sort(lib->ll_budget, lib->ll_name, &bl_sorted, &nl_sorted, type);

					/* since the head of the lists are change after sorting, update them as points the new sorted list */
					lib->ll_budget = bl_sorted;
					lib->ll_name = nl_sorted;

					s = 0, count = 1;
					
					/* skip the unknown budget if user specified that sort according to budget */
					if (type == budget)
					{
						while (bl_sorted != NULL && bl_sorted->budget == 0)
						{
							bl_sorted = bl_sorted->next;
							nl_sorted = nl_sorted->next;
						}
					}

					while (bl_sorted != NULL && count <= end)
					{
						if (count >= start)
						{
							printf(bl_sorted->budget ? "%-12d" : "Unknown     ", bl_sorted->budget);
							printf("%-15s %-60s %.1f\t %d\n", nl_sorted->genre, nl_sorted->name, nl_sorted->score, bl_sorted->year);
							++s; 	/* to indicate movie finded in user spesified range */
						}

						bl_sorted = bl_sorted->next;
						nl_sorted = nl_sorted->next;
						++count;
					}

					if (s == 0)	/* if there is no movie finded */
					{
						printf("There is no movie finded in given range, do you want to sort again (yes: 1, no: 0): ");
						s = scanf("%d", &select);

						if (s != 1 || select < 0 || select > 1)
							err = 1;
						else
						{
							if (select == 1)	
								n = -1;	/* start all the process from the beginning */
						}
					}
					else
						getchar();
			}
			/* check if scanning process is done succesfully */
			if (err)
			{
				clear_stdin();
				printf("\nPlease enter a valid selection\n");
			}
			else
				++n;
			printf("\n");
		} while (n < 5);
	}
}


void merge_sort (Movie_Budget * bl, Movie_Name * nl, Movie_Budget ** bl_sorted, Movie_Name ** nl_sorted, movie_data_t type)
{
	Movie_Budget 	* bl_1 = bl, * bl_2 = NULL;
	Movie_Name 		* nl_1 = nl, * nl_2 = NULL;

	/* check if there are at least two element in list, o.w. list is already sorted */
	if (bl != NULL && bl->next != NULL)
	{
		/* split given list as two list then sort them seperatly */
		split(bl_1, &bl_2, nl_1, &nl_2);
		
		merge_sort(bl_1, nl_1, bl_sorted, nl_sorted, type);
		bl_1 =  *bl_sorted;
		nl_1 = *nl_sorted;
		
		merge_sort(bl_2, nl_2, bl_sorted, nl_sorted, type);
		bl_2 =  *bl_sorted;
		nl_2 = *nl_sorted;

		/* after sorting seperatly, merge two sorted list l_1 and l_2 */
		sorted_merge(bl_1, bl_2, nl_1, nl_2, bl_sorted, nl_sorted, type);
	}
	else /* return the already sorted list */
	{
		*bl_sorted = bl;
		*nl_sorted = nl;
	}
}

/* initially _l0 point the head pointer of their list, eventually _l1 points the head of second list after splitting */
void split (Movie_Budget * bl_0, Movie_Budget ** bl_1, Movie_Name * nl_0, Movie_Name ** nl_1)
{
	/* when fast points the end of l0 list, slow points the end of the l1, so slow->next points l1 */
	Movie_Budget 	* fast_bp, * slow_bp; 	
	Movie_Name      * fast_np, * slow_np; 

	if (bl_0 == NULL)
	{
		*bl_1 = NULL;
		*nl_1 = NULL;
	}
	else if (bl_1 != NULL && nl_1 != NULL)
	{
		slow_bp = fast_bp = bl_0;
		slow_np = fast_np = nl_0;

		/* since bl_0 and nl_0 parallel, compare just one of them to indicate end of list */
		while (fast_bp != NULL)
		{
			fast_bp = fast_bp->next;
			fast_np = fast_np->next;

			if (fast_bp != NULL && fast_bp->next != NULL)
			{
				fast_bp = fast_bp->next;
				slow_bp = slow_bp->next;

				fast_np = fast_np->next;
				slow_np = slow_np->next;
			}
			/* in that point slow_bp and slow_np point the last node of their first list */
			else
			{
				*bl_1 = slow_bp->next;			/* now l1 is head of the second list   	 */
				slow_bp->next = NULL;			/* put NULL end of the first list 		 */

				*nl_1 = slow_np->next;
				slow_np->next = NULL;
				break;
			}
		}
	}
}

void sorted_merge (Movie_Budget * bl_1, Movie_Budget * bl_2, Movie_Name * nl_1, Movie_Name * n_l2, Movie_Budget ** bl_sorted, Movie_Name ** nl_sorted, movie_data_t type)
{
	int 			status;
	Movie_Budget 	* b_tmp;
	Movie_Name 		* n_tmp;

	if (bl_sorted != NULL)
	{
		/* check if both of given list _l1 and _l2 contains element o.w. no need to sorting process */
		if (bl_1 == NULL)
		{
			*bl_sorted = bl_2;
			*nl_sorted = n_l2;
		}
		else if (bl_2 == NULL)
		{
			*bl_sorted = bl_1;
			*nl_sorted = nl_1;
		}
		else
		{
			/* to sort ascending order choose the node have lower value at specific type(budget, genre ...) */
			switch (type)
			{
				case budget:
					status = (bl_1->budget < bl_2->budget);
					break;
				case genre:
					status = (strcmp(nl_1->genre, n_l2->genre) < 0);
					break;
				case name:
					status = (strcmp(nl_1->name, n_l2->name) < 0);
					break;
				case score:
					status = nl_1->score < n_l2->score; 
					break;
				case year:
					status = (bl_1->year < bl_2->year);
				default:
					*bl_sorted = NULL;
					*nl_sorted = NULL;
			}
			/* continue sorting process according to status */
			if (status == 1)
			{
				*bl_sorted = bl_1;
				*nl_sorted = nl_1;
				sorted_merge(bl_1->next, bl_2, nl_1->next, n_l2, &bl_1->next, &nl_1->next, type);		
			}
			else
			{
				*bl_sorted = bl_2;
				*nl_sorted = n_l2;
				sorted_merge(bl_1, bl_2->next, nl_1, n_l2->next, &bl_2->next, &n_l2->next, type);
			}
		}
	}
}

void find_movie (lib_t * lib)
{
	char s[200];
	Movie_Budget 	* bp;
	Movie_Name 		* np;

	if (lib != NULL)
	{
		printf("Please enter the name of the movie: ");
		scanf("%[^\n]", s); clear_stdin();

		bp = lib->ll_budget;
		np = lib->ll_name;

		/*search the movie */
		while (bp != NULL)
		{
			if (strcmp(bp->name, s) == 0)
				break;

			bp = bp->next;
			np = np->next;
		} 

		if (bp == NULL)
			printf("%s not found in library\n\n", s);
		else
		{
			if (bp->budget == 0)
				printf("\nBudget : %s\n", "Unknown");
			else
				printf("\nBudget : %d\n", bp->budget);

			printf("Name   : %s\n", bp->name);
			printf("Genre  : %s\n",np->genre);
			printf("Score  : %.1f\n", np->score);
			printf("Year   : %d\n", bp->year);
		}
	}
}

void  list_through_score (Movie_Name * l)
{
	int i;
	double score = 0;
	Movie_Name * np = l;
	enum {less, greater} range; 

	do {
		printf("Enter a score: ");
		for (i = scanf("%lf", &score); i != 1 || score < 0; i = scanf("%lf", &score))
		{
			printf("Please enter an valid score\nEnter a score: ");
			clear_stdin();		
		}

		printf("Less (0) or greater (1): ");
		for (i = scanf("%u", &range); i != 1 || range != 1 && range != 0; i = scanf("%u", &range))
		{
			printf("Please enter an valid range\nLess (0) or greater (1): "); 
			clear_stdin();		
		}
		i = 0;

		switch (range)
		{
			case less:
					while (np != NULL)
					{
						if (np->score < score)
							printf("%-4d %-50s %.1f\n", ++i, np->name, np->score);
						np = np->next;
					}
					break;

			case greater:
					while (np != NULL)
					{
						if (np->score > score)
							printf("%-4d %-50s %.1f\n", ++i, np->name, np->score);
						np = np->next;
					}
		}
		clear_stdin();

		if (i == 0)
			printf("\nNo movie in that score range\n");				
	} while (i == 0);
}

void  list_through_year (Movie_Budget * l)
{	
	int i, year = 0;
	Movie_Budget * bp = l;
	enum {until, since} range; 

	do {
		printf("Enter a year: ");
		for (i = scanf("%d", &year); i != 1 || year < 0; i = scanf("%d", &year))
		{
			printf("Please enter an valid year\nEnter a year:  ");
			clear_stdin();		
		}

		printf("Until (0) or Since (1) %d: ", year);
		for (i = scanf("%u", &range); i != 1 || range != 1 && range != 0; i = scanf("%u", &range))
		{
			printf("Please enter an valid range\nUntil (0) or Since (1) %d: ", year);
			clear_stdin();		
		}
		i = 0;

		switch (range)
		{
			case until:
					while (bp != NULL)
					{
						if (bp->year < year)
							printf("%-4d %-50s %d\n", ++i, bp->name, bp->year);
						bp = bp->next;
					}
					break;

			case since:
					while (bp != NULL)
					{
						if (bp->year > year)
							printf("%-4d %-50s %d\n", ++i, bp->name, bp->year);
						bp = bp->next;
					}
		}
		clear_stdin();

		if (i == 0)
			printf("\nNo movie in that year range\n");				
	} while (i == 0);
}

double calculate_idmb_ave (Movie_Name * l)
{
	int n = 0;
	double sum = 0;

	while (l != NULL)
	{
		sum += l->score;
		l = l->next;
		++n;
	}

	if (n != 0)
		sum /= n;

	return sum;
}

void print_genres (const genre_t * g)
{
	if (g == NULL || g->genre == NULL)
		printf("There is no genre\n");
	else
	{
		while (g != NULL)
		{
			printf("%s\n", g->genre);
			g = g->next;
		}
	}
}

void print_genres_frequence (const genre_t * g)
{
	if (g == NULL || g->genre == NULL)
		printf("There is no genre\n");
	else
	{
		printf("%-20s %s\n\n", "genre", "frequence");

		while (g != NULL)
		{
			printf("%-20s %-5d\n", g->genre, g->frequence);
			g = g->next;
		}
	}
}

int take_op ()
{
	int op, s = 1;
	
	do {
		printf("\n1. List of the Sorted Data\n");
		printf("2. List of the Genres\n");
		printf("3. List of the Movie Through the Years\n");
		printf("4. List of the Movie Through the Scores\n");
		printf("5. All information of a Single Movie\n");
		printf("6. Average of the IDMB Scores\n");
		printf("7. Frequence of the Genres\n");
		printf("8. Exit\n");
		printf("\nPlease select an operation: ");

		s = scanf("%d", &op); clear_stdin();

		if (s == 0)
			printf("Please select a valid operation\n");
		printf("\n");
	} while (s != 1);
	
	return op;
}

void clear_stdin ()
{
	while (getchar() != '\n');
}