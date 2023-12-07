#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<stdbool.h>

struct node_list
{
    int data;
    float number;
    char* name;
    struct node_list *next;
};

/*Do not modify the following function.*/

void insert_end(struct node_list **head, int val, float num, char* name_)
{ 
    struct node_list *newnode = (struct node_list *) malloc(sizeof(struct node_list));
 	struct node_list *lastnode = NULL;
    newnode->data = val;
    newnode->number = num;
    newnode->name = name_;
    newnode->next = NULL;

    if(*head == NULL)
 		*head = newnode;
    else
    {
        lastnode = *head;

        while (lastnode->next != NULL)
        {
			printf("%d\n", lastnode->data);

			lastnode = lastnode->next;
        }

        lastnode->next = newnode;
    }
}

/*Do not modify the following function.*/

void print_list(struct node_list *head)
{
	struct node_list *temp = head;

    while(temp != NULL)
    {
		printf("%d->", temp->data);
		printf("%f->", temp->number);
		printf("%s", temp->name);
		printf("\t");
		temp = temp->next;
    }
    
    printf("NULL\n\n");
}

/*Do not modify the following function.*/

void print_array(struct node_list *array, int sizeof_array)
{
	int i;
	
	for(i=0;i<sizeof_array;i++)
	{
		printf("%d->", (array+i)->data);
    	printf("%f->", (array+i)->number);
		printf("%s", (array+i)->name);
		printf("\t");
	}
		
    	printf("NULL\n\n");
}

/*You can modify following functions.*/

struct node_list* merge_list(struct node_list* head_1, struct node_list* head_2)
{
	/* first find the last node of the first list, if there is a list, o.w. just return second list */
	struct node_list * tmp = head_1;

	if (head_1 != NULL)
	{
		/* set tmp  as points the last node of the first list */
		while (tmp->next != NULL)
			tmp = tmp->next;
		
		/* link two list */
		tmp->next = head_2;
	}
	else
		head_1 = head_2;

	return head_1;
}

struct node_list* merge_interleaved(struct node_list* head_1, struct node_list* head_2)
{
	struct node_list * ip = head_1, * ipnext,
					 * jp = head_2, * jpnext;

	while (ip != NULL && jp != NULL)
	{
		/* keep the next nodes to do not lose */
		ipnext = ip->next;
		jpnext = jp->next;

		ip->next = jp;

		if (ipnext == NULL && jpnext != NULL)
			break;
		else
			jp->next = ipnext;

		/* update ip and jp points the next node of their list */
		ip = ipnext;
		jp = jpnext;		
	}

	return head_1;
}

struct node_list* merge_array(struct node_list* a, int na, struct node_list* b, int nb)
{
	int i;
	struct node_list * tmp;
	/* create an dynamic array to keep the values */
	struct node_list * arr = calloc(na + nb, sizeof(struct node_list));

	if (arr != NULL)
	{
		for (i = 0; a != NULL && b != NULL; ++i)
		{
			arr[i] = *a;
			arr[++i] = *b;

			/* free the current node and pass the next node */
			tmp = a;
			a = a->next;
			free(tmp);
			tmp = b;
			b = b->next;
			free(tmp);
		}

		/* check if there is remains node one of the lists */
		if (a != NULL)
			tmp = a;
		else if (b != NULL)
			tmp = b;
		else
			tmp = NULL;

		while (tmp != NULL)
		{
			arr[i++] = *tmp;
			a = tmp;			/* use a as temparaiy storage */
			tmp = tmp->next;
			free(a);		
		}
		b = a = NULL;
	}
	return arr;
}


int main()
{
	/*Do not modify anything between 95 and 130 lines.*/
	
	struct node_list *head_1 = NULL;
	struct node_list *head_2 = NULL;
	struct node_list *merged = NULL;
	struct node_list *interleaved = NULL;
	struct node_list * array = NULL;
	int na, nb;
	
	insert_end(&head_1,10,1.5,"hello1");
	insert_end(&head_1,30,3.5,"hello3");
	insert_end(&head_1,50,5.5,"hello5");
	 
	insert_end(&head_2,20,2.5,"hello2");
	insert_end(&head_2,40,4.5,"hello4");
	insert_end(&head_2,60,6.5,"hello6");
	
	printf("Print List 1: \n");
	 
	print_list(head_1);
	 
	printf("Print List 2: \n");
	 
	print_list(head_2);
	 
	merged=merge_list(head_1, head_2);
	
	printf("Print List Merged: \n");

    print_list(merged);

	interleaved=merge_interleaved(head_1, head_2);

	insert_end(&head_1,70,7.5,"hello7");
	
	printf("Print List Interleaved: \n");
	
	print_list(interleaved);
 	
 	/*Do not modify anything between 95 and 130 lines.*/    
 	
	/*YOU CAN MODIFY BELOW THIS LINE FOR ONLY PART 3.*/
	
	
	head_1 = NULL;
	head_2 = NULL; 

	na = 0, nb = 0;
	insert_end(&head_1,10,1.5,"hello1"); ++na;
	insert_end(&head_1,30,3.5,"hello3"); ++na;
	insert_end(&head_1,50,5.5,"hello5"); ++na;
	
	insert_end(&head_2,20,2.5,"hello2"); ++nb;
	insert_end(&head_2,40,4.5,"hello4"); ++nb;
	insert_end(&head_2,60,6.5,"hello6"); ++nb;

    array = merge_array(head_1, na, head_2, nb);
	
	printf("Print List merge array: \n");
	print_array(array, na + nb);
	
	return 0;
}
