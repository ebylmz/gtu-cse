#include <stdio.h>

#define NAMELEN 20	
#define MAXCUST 50  /* max number of customer that banking system keeps in same time */

typedef enum {employee, retired, student, unemployed, other} label_t;

typedef struct {
	char 		name[NAMELEN];
	int 		age;
	label_t 	label;
} customer_t;

typedef struct {
	int 	   	filled;			/* to indicate empty or filled piece of memory */
	customer_t	cust_data;		/* keeps customer data 				   		   */	
} sequence_t;

void  process_seq (sequence_t * seq[]);
void  add_seq (sequence_t * seq[], sequence_t * new_cust);
int  find_empty_data_block (const sequence_t cust[]);
int  find_seq_num (sequence_t * seq[], label_t new_cust_label);
void initialize_system (sequence_t * seq[], sequence_t cust[]);
void print_seq (sequence_t * seq[]);
int  scan_process (sequence_t * seq[]);
void scan_cust (sequence_t * cust);
void clearstdin ();


int main(void)
{ 					
	int 		fill_index,
				process;
	sequence_t	cust[MAXCUST],	/* keeps customers data */	
			 	* seq[MAXCUST]; /* to sort cust data, keep their addresses in pointer array */
	
	/* initalize all the block unfilled (cust[]), and all the pointers (seq[]) as NULL */
	initialize_system(seq, cust);
	
	do {
		process = scan_process(seq);

		switch (process)
		{
			case 1:
					/* find empty place in cust[] array */
					fill_index = find_empty_data_block (cust);

					/* if there is empty space in banking system */
					if (fill_index >= 0)
						add_seq(seq, &cust[fill_index]);
					else
						printf("Banking system is full.\n");
					
					break;

			case 2:
					process_seq(seq);
					break;
			case 0:
					printf("Have a good day.\n");
					break;

			default:
					printf("[!] Invalid process");
		}
	} while (process != 0);
}

/* remove the first customer at currnt sequence */
void process_seq (sequence_t * seq[])
{
	int 	i, n,
			seq_len = seqlen(seq);

	/* be sure there is at least one cust */
	if (seq_len > 0)
	{
		/* process means forgetting first cust address and setting their data block as unfilled */
		printf("Proceed customer is %s.\n", seq[0]->cust_data.name);
		seq[0]->filled = 0;
		
		/* 	reorder the sequence by shifting all the elements one left 
		 	if there is only one cust, there is no need any shifting 	*/
		for (i = 0, n = seq_len - 1; i < n; ++i)
			seq[i] = seq[i + 1];

		/* since we shift every address one left, set last pointer as NULL */
		seq[seq_len - 1] = NULL;
	}
	else
		printf("There is not any customer in bank system sequence.\n");
}

/* scans a new cust data and, add in sequence properly */
void add_seq (sequence_t * seq[], sequence_t * new_cust)
{
	int 	i,
			seq_len = seqlen(seq),
			fill_index;		/* new cust sequence number */

	/* scan customer data */
	scan_cust(new_cust);

	/* find the new cust location in sequence */
	fill_index = find_seq_num (seq, new_cust->cust_data.label);

	/* 	shift every cust 1 right till reaching the fill_index because
		remainder cust has higher priority, they must be remain same location	 */
	for (i = seq_len; i > fill_index; --i)
		seq[i] = seq[i - 1];

	/* assign the address of new cust */
	seq[fill_index] = new_cust;
}


int find_seq_num  (sequence_t * seq[], label_t new_cust_label)
{
	int 	i, n,
			seq_len = seqlen(seq),
			count,
			occurance,		/* occurance flag E.g. 1-1-1 or 0-0-0 				*/
			max_occ,		/* max occurance, depends on the customer label 	*/
			fill_index;		/* index of seq array to assign new_cust address 	*/

	/* define the max occurance according to cust label */
	switch (new_cust_label)
	{
		case employee:
					max_occ = 5;
					break;

		case retired:
		case student:
					max_occ = 3;
					break;

		case unemployed:
		case other:
					max_occ = 2;
					break;
	}

 	occurance = count = 0;

	/* if there is a occurance, selects fill_index as last member of that occurance */
	for (i = seq_len - 1; i >= 0; --i)
	{	
		/* there is a occurance, but we also check if it exceeds the max_occ or not */
		if (new_cust_label == seq[i]->cust_data.label)
		{	
			++count;
			
			/* keep the end of the row to insert new_cust */
			if (count == 1)
			{
				occurance = 1;
				fill_index = i + 1;
			}
			/* detected occurance reached the max, put the customer end of the sequence */
			else if (count == max_occ)
			{
				/* find head of the last row */
				n = seq_len + 1;
				
				for (i = seq_len - 1; i > 0; --i)
					if (seq[i]->cust_data.label < seq[i - 1]->cust_data.label)
						n = i;

				/* if there is no another sequence create a new one */
				if (i == 0)
					fill_index = seq_len;
				else
				{
				 	for (i = seq_len - 1; i >= n; --i)
				 	{
				 		/* look the label priorty */
				 		if (new_cust_label < seq[i]->cust_data.label) 
				 		{
							fill_index = i; break;
				 		}
				 		else if (new_cust_label == seq[i]->cust_data.label)
				 		{
				 			fill_index = i + 1; break;
				 		}
				 	}
				}

			}
		}
		/* 	we just pass the row which doesn't exceed, new_cust should be at the
		 	end of the row, but we already assign it so we find the fill_index  */
		else if (occurance)
			break;
	}

	/* if there is no occurance, just look priority order */
	if (! occurance)
	{
		for (i = 0, n = seq_len; i < n; ++i)
		{
			/* if the new cust has higher priorty than comparing cust, new_cust must be there */
			if (new_cust_label < seq[i]->cust_data.label)
			{
				fill_index = i;	break;
			}
		}
		/* if we cannot find right place, means new cust has lowest priorty, and must be at the end of seq */
		if (i == seq_len)
			fill_index = seq_len;
	}

	return fill_index;
}

/* looks unfilled block in cust array, returns -1 in case of no empty block */
int find_empty_data_block  (const sequence_t cust[])
{
	int i,
		fill_index = -1;
	
	for (i = 0; i < MAXCUST; ++i)
		if (cust[i].filled == 0)
		{
			fill_index = i; break;
		}

	return fill_index;
}

int scan_process (sequence_t * seq[])
{
	int 	process,
			stat;		/* status value for scanf */

	/* print the Current Sequence: */
	putchar('\n');
	print_seq(seq);
	
	printf("1-Add customer\n");
	printf("2-Process customer\n");
	printf("0-Exit\n");
	printf(">> ");

	/* be sure user enter valid input */
	for (stat = scanf("%d", &process); stat != 1 || process < 0 || process > 2; stat = scanf("%d", &process))
	{
		printf("[!] invalid process\n");
		printf(">> ");

		/* skip the junk inputs at the current line */
		clearstdin();	
	}

	clearstdin();
	return process;		
}

/* takes customer address and fills inside it according to user inputs */
void scan_cust (sequence_t * cust)
{
	int stat,	/* scanf status value */
		age,
		label;	/* cust label 		  */
	
	printf("%-42s: ", "Please enter the name of the customer ");
	scanf("%[^\n]%*c", cust->cust_data.name); 
	
	
	printf("%-42s: ", "Please enter the age of the customer ");
	
	for (stat = scanf("%d", &age); stat != 1 || age  < 0; stat = scanf("%d", &age))
	{
		clearstdin();
		printf("%-42s: ", "[!] invalid age, please enter proper age ");
	}
	

	printf("%-42s: ", "Please enter the label of the customer ");
	
	for (stat = scanf("%d", &label); stat != 1 || (label < 0 || label > 4); stat = scanf("%d", &label))
	{
		clearstdin();

		printf("[!] invalid label, be sure entering valid labels.\n");
		printf("0-employee \n1-retired \n2-student \n3-unemployed \n4-other\n");
		printf("label: ");
	}
	
	/* don't forget to assigning 1 to indicate filled */
	cust->cust_data.label = label;
	cust->cust_data.age = age; 
	cust->filled = 1;
	
	clearstdin();
}

/* intialize every memory block as unfilled (0) and iniitialize all the pointers for that blocks as NULL */
void initialize_system (sequence_t * seq[], sequence_t cust[])
{
	int i;

	/* initialy there is no filled memory block so every pointer must be NULL pointer */
	for (i = 0; i < MAXCUST; ++i)
	{
		cust[i].filled = 0;
		seq[i] = NULL;
	}
}

void print_seq (sequence_t * seq[])
{
	int 	i,
			seq_len = seqlen(seq);

	if (seq_len == 0)
		printf("There is not any customer in bank system sequence.\n");
	else
	{
		printf("Current Sequence: %d ", seq[0]->cust_data.label);

		for (i = 1; i < seq_len; ++i)
			printf("- %d ", seq[i]->cust_data.label);
		printf("\n");
	}
}

/* finds the length of the sequence by searching NULL pointer */
int seqlen (sequence_t * seq[])
{
	int len;

	/* be sure array size does not exceed, o.w. seq[MAXCUST] can point anywhere */
	for (len = 0; seq[len] != NULL && len < MAXCUST; ++len);

	return len;
}

void clearstdin ()
{
	while (getchar() != '\n');
}