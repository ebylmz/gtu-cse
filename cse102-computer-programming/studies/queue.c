#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct queue_t {
	char * cust;
	struct queue_t * next;	
} queue_t;

queue_t * create_queue ();
void push (queue_t ** q, char * s);
char * pop (queue_t ** q);
void peek (const queue_t * q);
void print_queue (const queue_t * q);
void free_queue (queue_t * q);


int main (void)
{
	int choose;
	char * cust, str[20];
	queue_t * q = create_queue();

	do {
		printf("Enter to continue "); getchar();
		printf("\n0. Exit\n1. Create new queue\n2. Show queue\n3. Add customer\n4. Remove customer\n");
		printf("----------------------------------\nChoose: ");
		scanf("%d", &choose);
		putchar('\n');

		switch (choose)
		{
			case 0:
				if (q != NULL) free_queue(q);
				break;
			case 1:
				if (q != NULL) free_queue(q);
				q = create_queue();
				break;
			case 2:
				print_queue(q);
				break;
			case 3:
				printf("Customer name: ");
				getchar(); 
				scanf("%[^\n]", str);
				push(&q, str);
				break;
			case 4:
				cust = pop(&q);
				printf("Current customer: %s\n", cust);
				free(cust);
				printf("Next customer: "); peek(q);
				break;
			default: 
				printf("Invalid choose !!");
		}
		while (getchar() != '\n');
	} while (choose != 0);

}

queue_t * create_queue (){return NULL;}

void push (queue_t ** q, char * s)
{

	queue_t * tmp, * node;

	if (q != NULL)
	{
		node = (queue_t *) malloc(sizeof(queue_t));
		if ( node != NULL)
		{
			node->cust = (char *) calloc(strlen(s) + 1, sizeof(char));
			if (node->cust == NULL)
				free(node);
			else
			{
				strcpy(node->cust, s);
				node->next = NULL;

				if (*q == NULL)
					*q = node;
				else
				{
					tmp = *q;

					while (tmp->next != NULL) tmp = tmp->next;

					tmp->next = node;
				}
			}
		}
	}
}

char * pop (queue_t ** q)
{
	char * data = NULL;
	queue_t * tmp;
	
	if (*q != NULL && q != NULL)
	{
		data = (*q)->cust;
		tmp = *q;
		*q = (*q)->next;
		free(tmp);
	}
	return data;
}

void peek (const queue_t * q)
{
	if (q != NULL)
		printf("%s\n", q->cust);
}

void print_queue (const queue_t * q)
{
	int i = 1;

	while (q != NULL)
	{
		printf("%d - %s\n", i++, q->cust);
		q = q->next;
	}

	if (i == 1) printf("Empty queue\n");
	else putchar('\n');
}

void free_queue (queue_t * q)
{
	queue_t * tmp;

	if (q != NULL)
	{
		free_queue(q->next); /* free the rest of the list then free the current node */
		free(q->cust);
		free(q);
	}
}