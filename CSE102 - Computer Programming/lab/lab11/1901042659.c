#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

struct node
{
    int data;
    float number;
    char* name;
    struct node *next;
};


void insert_end(struct node **head, int val, float num, char* name_)
{
    /* create a new node */
    struct node *newNode = malloc(sizeof(struct node));
    newNode->data = val;
    newNode->number = num;
    newNode->name = name_;
    newNode->next = NULL;

    /*if head is NULL, it is an empty list */
    if(*head == NULL)
         *head = newNode;
    /*Otherwise, find the last node and add the newNode */
    else
    {
        struct node *lastNode = *head;

        /*last node's next address will be NULL. */
        while(lastNode->next != NULL)
        {
            lastNode = lastNode->next;
        }

        /*add the newNode at the end of the linked list */
        lastNode->next = newNode;
    }

}

void printList_withLoop(struct node *head)
{
    /* iterate the entire linked list and print the data with loop*/
    while (head != NULL)
    {
        printf("%d->%f->%s,\n", head->data, head->number, head->name);
        head = head->next;
    }
}

void printList_withRecursive(struct node *list) {
   
   /* iterate the entire linked list and print the data with recursive*/
   if (list != NULL)
   {
        printf("%d->%f->%s ", list->data, list->number, list->name);
        printList_withRecursive(list->next);     
   }
   else
    printf("\n");
}


void sortedInsert(struct node** head_ref, struct node* new_node)
{
    struct node * tmp, * bp;

    if (head_ref != NULL)
    {
        if (*head_ref != NULL)
        {
            /* insert beginning of the list */
            if (strcmp(new_node->name, (*head_ref)->name) < 0)
            {
                new_node->next = (*head_ref);
                (*head_ref) = new_node; 
            }
            else
            {
                bp = *head_ref;       /* bp initially points first node */ 
                tmp = bp->next;     /* tmp points second node if there is */

                while (tmp != NULL)
                {
                    if (strcmp(new_node->name, tmp->name) < 0)
                    {
                        new_node->next = tmp;   /* link forward     */
                        bp->next = new_node;    /* link backward    */
                        break;
                    }
                    else
                    {
                        bp = tmp;
                        tmp = tmp->next;
                    }
                }

                /* bp points last node and tmp points NULL, insert end */
                if (tmp == NULL)
                {
                    new_node->next = NULL;
                    bp->next = new_node;  
                }
            }
        }
        else
            *head_ref = new_node;
    }
}
 
 
/* A utility function to create a new node */
struct node* newNode(int new_data, float num, char* name_)
{
    /* allocate node */
    struct node * np = (struct node *) malloc(sizeof(struct node));

    /* put in the data  */
    if (np != NULL)
    {
        np->data = new_data;
        np->number = num;
        np->name = name_;
        np->next = NULL;
    }
    return np;
}


bool search(struct node* head, int x)
{
    bool ans = 0;

    /* Search according to "data" which defined in node struct */
	while (head != NULL)
    {
        if (head->data == x)
        {
            ans = 1; break; 
        }
        else
            head = head->next;
    }
    return ans;
}

int main()
{
     struct node *head = NULL;

     insert_end(&head,10,5.5,"hello1");
     insert_end(&head,20,5.5,"hello2");
     insert_end(&head,30,5.5,"hello3");


     printf("Print List with Loop: \n");
     printList_withLoop(head);
     
     struct node* new_node = newNode(1,5.5, "a");

     sortedInsert(&head, new_node);

     
     new_node = newNode(3,5.5, "c");
     sortedInsert(&head, new_node);

     
     new_node = newNode(2,5.5, "d");
     sortedInsert(&head, new_node);
     
     new_node = newNode(2,5.5, "b");
     sortedInsert(&head, new_node);
     
     printf("Print List with Recursive: \n");
	 printf("[");
     printList_withRecursive(head);
	 printf("\nIs 20 belong this linked list?: " );
     printf(search(head, 20) ? "YES \n" : "NO \n");
     
     printf("\nIs 18 belong this linked list?: " );
     printf(search(head, 18) ? "YES \n" : "NO \n");


     return 0;
}