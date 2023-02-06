#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct stack_t {
	int data;
	struct stack_t * next;
} stack_t;

stack_t * create_stack ();
void push(stack_t ** s, int data);
int pop (stack_t ** s);
void print_stack (const stack_t * s);


/* (4 5 6 * 3 / + ) = ((5 * 6) / 3) + 4 */
int main (void)
{
	int n1, n2;
	char op[120], * sn;
	stack_t * s = create_stack();

	printf(">> ");
	fgets(op, 120, stdin);
	
	op[strlen(op) - 1] = ' '; /* to deliminate */
	sn = strtok(op, " ");

 	while (sn != NULL)
 	{
		if (sn[0] == '=')
		{
			printf("result: %d\n", pop(&s));
			break;
		}
		else
		{
			if ('0' > sn[0] || sn[0] > '9') /* if user enter an operator */
			{
				n2 = pop(&s);
				n1 = pop(&s);
			}
			switch (sn[0])
			{
				case '+': push(&s, n1 + n2); break;
				case '-': push(&s, n1 - n2); break;
				case '*': push(&s, n1 * n2); break;
				case '/': push(&s, n1 / n2); break;
				case '%': push(&s, (n1 % n2)); break;
				default:  push(&s, strtod(sn, NULL));
			}
		}
		sn = strtok(NULL, " ");
	}

	if (sn != NULL)
}

stack_t * create_stack ()
{
	return NULL;
}

void push(stack_t ** s, int data)
{
	stack_t * new;

	new = (stack_t *) malloc(sizeof(stack_t));
	if (new != NULL)
	{
		new->data = data;
		new->next = *s;
		*s = new; 
	}
}

int pop (stack_t ** s)
{
	int r = 0;
	stack_t * tmp;

	if (*s != NULL)
	{
		r = (*s)->data;
		
		tmp = *s;
		*s = (*s)->next;
		free(tmp);
	}
	else
		printf("Err: empty stack\n");

	return r;
}

void print_stack (const stack_t * s)
{
	if (s != NULL)
	{
		printf("%d ", s->data);
		print_stack(s->next);
	}
	else
		printf("\n");
}