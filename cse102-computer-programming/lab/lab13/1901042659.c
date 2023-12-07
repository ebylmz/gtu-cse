#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define STEPSIZE 20
#define OUTFNAME "country.txt"

typedef struct {
	char country[20], 
		 capital[20];
  	int population;
  	bool driving_side;
} country_t;

typedef struct node_t {
	country_t cdata;
	struct node_t * next;
} node_t;

node_t * insert_end(node_t ** l, node_t * newnode);

int main (void)
{
	int 	i, n, csize = 0, maxsize = 0;	/* the number of current country and max country capacity */
	char s[20];
	FILE * infid, * outfid;
	country_t 	* c = NULL, /* keeps the country data as a dynamic arrays */
				* tmp = NULL;

	node_t * llist = NULL,	/* keeps the country data as a linked list */
		   * newnode,  * l;

	do {
		printf("\n");
		printf("1: Enter new record\n");
		printf("2: Write to the file\n");
		printf("3: Read from the file\n");
		printf("4: Print the linked list\n");
		printf("5: Exit\n");

		printf("Your choice: ");
		scanf("%d", &n);

		switch(n)
		{
			case 1:	/* take new country data from user */ 
				/* check if more memory is needed */
				if (csize == maxsize)
				{
					maxsize += STEPSIZE; 
					/* allocate a new memory block */
					tmp = (country_t *) calloc(maxsize, sizeof(country_t));
					
					if (tmp == NULL)
					{
						printf("No enough memory for new allocation\n");
						break;
					}
					else
					{
						/* copy previos data to new memory block and free the previos one */
						for (i = 0; i < csize; ++i)
							tmp[i] = c[i];

						if (c != NULL)
							free(c);
						c = tmp;
					}
				}
				/* scan the movie data from user */					
				printf("Country name: ");
				scanf("%s", c[csize].country);
				printf("Capital: ");
				scanf("%s", c[csize].capital);
				printf("population of %s: ", c[csize].country);
				scanf("%d", &c[csize].population);
				printf("Do people in %s drive on right side (Yes: 1, No: 0): ", c[csize].country);
				scanf("%d", &c[csize].driving_side);

				++csize;
				break;

			case 2:	/* write all the country data scanned until now to file */
				outfid = fopen(OUTFNAME, "w");

				if (outfid == NULL)
					printf("Writing process fail\n");
				else
				{
					/* write all the data about one country in a one line */
					for (i = 0; i < csize; ++i)
					{
						fprintf(outfid, "%s %s %d ", c[i].country, c[i].capital, c[i].population);

						if (c[i].driving_side == 0)
							fprintf(outfid, "Left\n");
						else
							fprintf(outfid, "Right\n");
					}
					printf("Writing process succesfully done\n");
					fclose(outfid);
				}
				break;
	
			case 3: /* create a linked list with scanned data from infile */
				/* take the name of the input file */
				printf("Name of the source file ");
				scanf("%s", s);

				infid = fopen(s, "r");

				if (infid == NULL)
					printf("Source file cannot open\n");
				else
				{
					/* scan tha data at the line to linked list */
					while (!feof(infid))
					{
						/* allocate a new node */
						newnode = (node_t *) malloc(sizeof(node_t));
						
						if (newnode == NULL)
							printf("No enough memory to store country data\n");
						else
						{
							fscanf(infid, "%s %s %d %s", newnode->cdata.country, newnode->cdata.capital, &(newnode->cdata.population), s);
							newnode->cdata.driving_side = (strcmp(s, "Right") == 0);
							insert_end(&llist, newnode);
						}
					}
					printf("Reading process done properly\n");
					fclose(infid);
				}
				break;

			case 4:	/* print the linked list */
				l = llist;
				
				while (l != NULL)
				{
					printf("Country: %-20s Capital: %-20s Population: %-10d Driving side: ", l->cdata.country, l->cdata.capital, l->cdata.population);

					if (l->cdata.driving_side == 0)
						printf("Left\n");
					else
						printf("Right\n");

					/* jump the next node */
					l = l->next;
				}
				break;

			case 5: /* free all the allocated memory and exit the program */
				/* free linked list */
				while (llist != NULL)
				{
					l = llist;
					llist = llist->next;
					free(l);
				}
				/* free dynamic allocated array */
				if (c != NULL)
					free(c);
				break;

			default:
				printf("Please enter an valid op code\n");
		}
		/* clean remainder input from user */
		while (getchar() != '\n');
	} while (n != 5);
}

node_t * insert_end(node_t ** l, node_t * newnode)
{
	node_t * tmp;

	if (l != NULL)
	{
		/* insert as first node of the list, head needs to update */
		if (*l == NULL)
			*l = newnode;
		/* o.w. find the last node of the list then insert new node */
		else
		{
			tmp = *l;

			while (tmp->next != NULL)
				tmp = tmp->next;
			tmp->next = newnode;
		}
	}
}