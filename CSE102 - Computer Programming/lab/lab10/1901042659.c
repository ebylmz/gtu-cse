#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define STEPSIZE 20 /* in case of allocated memory filled, the amount of increament for new allocate */
#define STRLEN 50	/* string length */

typedef struct {
	char 	title[STRLEN],
	 		author[STRLEN],
			subject[STRLEN];
	int 	year;
} book;

void clear_stdin ();
void library ();
void print_book_inf (book * b);
void swap (book ** p1, book ** p2);
void sort_by_year (book * lib, int booksize);


int main (void)
{
	library();
}

void library ()
{

	int 	i, 
			op,				/* operation 										  */
			booksize = 0,
			libsize = 0;
	book 	* lib = NULL,
			*tmp;			/* pointer to keep old address, during new allocation */

	char buff[STRLEN];		/* buffer for user input							  */

	do {
		printf("************************\nMENU\n");
		printf("\t1. Add New Book\n");
		printf("\t2. List Books\n");
		printf("\t3. Exit\n");

		printf("Choose : ");
		scanf("%d", &op); clear_stdin();
		
		printf("************************\nSUBMENU\n");

		if (op == 1)
		{
			/* check the libsize */
			if (booksize == libsize)
			{
				/* increase the library size */
				libsize += STEPSIZE;

				/* keep the address of current allocated memeory to free later */
				tmp = lib;

				/* allocate memory as new defined size */
				lib = (book *) calloc(libsize, sizeof(book));

				/* free previos memory */
				if (tmp != NULL)
					free(tmp);
			}
			
			if (lib != NULL)
			{
				/* take the book information */
				printf("Book Title: ");
				scanf("%[^\n]", lib[booksize].title); clear_stdin();
				printf("Book Author: ");
				scanf("%[^\n]", lib[booksize].author); clear_stdin();
				printf("Book Subject: ");
				scanf("%[^\n]", lib[booksize].subject); clear_stdin();
				printf("Book Year: ");
				scanf("%d", &lib[booksize].year); clear_stdin();

				/* new book added */
				++booksize;
			}
			else
			{
				printf("No memory to add new book\n");
				op = 3;
			}
		}
		else if (op == 2)
		{
			if (lib != NULL)
			{
				printf("\t 1. Get By Title\n");
				printf("\t 2. Get By Author\n");
				printf("\t 3. Get By by Subject\n");
				printf("\t 4. Sorted List By Year (DESC)\n");
				printf("\t 5. List All Books\n");
				printf("\t 6. EXIT SUBMENU\n");

				printf("Choose: ");
				scanf("%d", &op);	clear_stdin();

				printf("************************\n");

				switch (op)
				{
					case 1:
							/* take the book title to search it */
							printf("Book Title: ");
							scanf("%[^\n]", buff); clear_stdin();
							
							/* search the wished book */
							for (i = 0; i < booksize; ++i)
							{
								if (strcmp(lib[i].title, buff) == 0)
								{
									print_book_inf(&lib[i]); break;
								}
							}

							if (i == booksize)
								printf("Not Found\n");

							break;
					
					case 2:
							printf("Book Author: ");
							scanf("%[^\n]", buff); clear_stdin();

							/* search the book author to search it */
							for (i = 0; i < booksize; ++i)
							{
								if (strcmp(lib[i].author, buff) == 0)
								{
									print_book_inf(&lib[i]); break;
								}
							}

							if (i == booksize)
								printf("Not Found\n");

							break;

					case 3:
							/* take the book subject to search it */
							printf("Book Subject: ");
							scanf("%[^\n]", buff); clear_stdin();

							for (i = 0; i < booksize; ++i)
							{
								if (strcmp(lib[i].subject, buff) == 0)
								{
									print_book_inf(&lib[i]); break;
								}
							}

							if (i == booksize)
								printf("Not Found\n");

							break;
					
					case 4:
 							sort_by_year (lib, booksize);
							break;
					
					case 5:
							for (i = 0; i < booksize; ++i)
							{
								printf("************************\n%d. Book;\n", i + 1);
								print_book_inf(&lib[i]);
							}
							break;
					
					case 6:
							printf("Directed to MENU\n");
							break;
					
					default:
						printf("Invalid choose\n");
				}
			}
			else
				printf("No data to manupulate\n");
		}
		else if (op == 3)
			printf("Program terminating\n");
		else
			printf("Invalid choose\n");

	} while (op != 3);

	/* free all the allocated memory for books */
	if (lib != NULL)
		free(lib);
}

/* consumes all the user inputs at the current line */
void clear_stdin ()
{
	while (getchar() != '\n');
}

void print_book_inf (book * b)
{
	if (b != NULL)
	{
		printf("Title: %s\n", b->title);
		printf("Author: %s\n", b->author);
		printf("Subject: %s\n", b->subject);
		printf("Year: %d\n", b->year);
	}
}

/* sort and prints the given library */
void sort_by_year (book * lib, int booksize)
{
	int i, j,
		fill;	/* fill index, indicate the current max year */
	/* sort print and free everything */
	if (lib != NULL)
	{
		/* allocate exact space for pointer array */
		book ** slib = (book **) calloc (booksize, sizeof(book *));


		if (slib != NULL)
		{

			/* copy all the addresses */
			for (i = 0; i < booksize; ++i)
				slib[i] = &lib[i];

			/* sort them */
			for (i = 0; i < booksize; ++i)
			{
				fill = i;

				/* select the biggest year */
				for (j = i + 1; j < booksize; ++j)
					if (slib[j]->year > slib[fill]->year)
						fill = j;
				
				/* swap the addresses */
				if (fill != i)
					swap(&slib[i], &slib[fill]);
			}

			/* print the sorted data */
			for (i = 0; i < booksize; ++i)
			{
				printf("************************\n%d. Book;\n", i + 1);
				print_book_inf(slib[i]);
			}

			/* free all the allocated memory */
			free(slib);
		}
	}
}

/* swap to book type pointer */
void swap (book ** p1, book ** p2)
{
	book * tmp;

	tmp = *p1;
	*p1 = *p2;
	*p2 = tmp;
}