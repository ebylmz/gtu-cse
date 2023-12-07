#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LINESIZE 1200
#define BUFFSIZE 120 
#define END_OF_RECORD '\0'	/* to incicate end of user record, put end of the last line of record */

typedef enum {antonym, synonym} word_t;

typedef struct snode_t { 	/* keeps the antonym/synonym of a word as a element of sublist */
	char * s;	
	struct snode_t * next;
} snode_t;

typedef struct wlist_t { 	/* a sublist to keps a word to ask user and also keeps the number of correct/wrong answer */
	char 	* word;
	int 	true_ans, false_ans;
	double 	probability;
	snode_t 	* list;		
	struct 	wlist_t * next;			
} wlist_t;

typedef struct user_rec_t {	/* keeps address of the user recording to read/write operation */
	wlist_t * data;		
 	struct user_rec_t * next;
} user_rec_t;

typedef struct {
	wlist_t 	* ll_synonym,
				* ll_antonym;
	user_rec_t 	* ll_rec_synonym,
				* ll_rec_antonym;
} lib_t;

void insert_list(char * ins_word, char * s, wlist_t ** mainlist);
void load_user_record (char * fname, lib_t * lib);
int read_textf (char * fname, wlist_t ** mainlist);
int write_textf (char * fname, wlist_t * mainlist, word_t qtype);
int take_in_range (char * prompt, int min, int max);
int write_binaryf (FILE * bfid, user_rec_t * l);
void update_list (FILE * bfid, wlist_t * mainlist, user_rec_t ** ll_record);
void calculate_probability (lib_t * lib, word_t qtype, wlist_t * l);
int ask_misunderstood (user_rec_t * l, word_t qtype);
int ask_word (wlist_t * l, word_t qtype);
wlist_t * merge_sort (wlist_t * l);
wlist_t * split (wlist_t * l);
wlist_t * sorted_merge (wlist_t * l1, wlist_t * l2);
void system_free(lib_t * lib);
void free_record (user_rec_t * l);
void free_all_list (wlist_t * mainlist);
void prevent_segfault ();
void clear_stdin();


int main (void)
{
	int 		i, status, choose, err = 0;
	char 		str1[BUFFSIZE], str2[BUFFSIZE], userfname[BUFFSIZE];
	word_t 		qtype = 0,
				inserted = 0; /* counter to prevent writing same data on the system files */
	FILE 		* bfid, * tfid;
	lib_t lib = {NULL, NULL, NULL, NULL}; 
	wlist_t 	* wordlist, * backwlist, * ant_list, * syn_list;
	user_rec_t 	* tmprec;
	snode_t 	* tmpnode, * backnode, * username_list = NULL;

	if (read_textf("synonyms.txt", &lib.ll_synonym) || read_textf("antonyms.txt", &lib.ll_antonym))
		printf("System files cannot loaded properly\n");
	else
	{
		printf("\nWELCOME TO WORD LEARNING GAME\n\n");

		tfid = fopen("username.txt", "r");
		if (tfid != NULL) 
		{
			while (fgets(userfname, BUFFSIZE, tfid))
			{
				userfname[strlen(userfname) - 1] = '\0';
				tmpnode = (snode_t *) malloc(sizeof(snode_t));
				if (tmpnode != NULL)
				{
					tmpnode->s = (char *) calloc(strlen(userfname) + 1, sizeof(char)); 
					if (tmpnode->s != NULL)
					{
						strcpy(tmpnode->s, userfname);
					
						tmpnode->next = username_list;
						username_list = tmpnode;  
					}
					else prevent_segfault();
				}
			}
			fclose(tfid);
		} 

		do { /* take the user name and if there is any record about user, load into system */
			err = 0;
			if (username_list == NULL)
				choose = take_in_range ("0. Exit\n1. New game\n", 0, 1);
			else
				choose = take_in_range ("0. Exit\n1. New game\n2. Continue\n", 0, 2);

			switch (choose) /* check if there is a record about user, if so update lists with user record */
			{
				case 0:
 					system_free(&lib);
					printf("See you soon...\n");
 					exit(1);

				case 1: /* add new user */
					printf("username: ");
					scanf("%[^\n]%*c", userfname); 
	
 					err = 0; tmpnode = username_list;
					
					while (tmpnode != NULL)
					{
						if (strcmp(userfname, tmpnode->s) == 0)
						{
							if (userfname[0] != '\n')
								printf("!! This user name already in use\n");
							err = 1;
							break;
						}
						tmpnode = tmpnode->next;
					}

					if (! err) /* append the username to the username file */
					{
						tfid = fopen("username.txt", "a");
						if (tfid != NULL)
						{
							fprintf(tfid, "%s\n", userfname); 
							strcat(userfname, ".worddat");
						}
						else
						{
							printf("User record file cannot open properly\n");
							err = 1;
						}
					}
					break;
				
				case 2: /* continue with loading user previos session records */
					tmpnode = username_list; i = 0;

					printf("0. Exit submenu\n");
					while (tmpnode != NULL) /* print the user names */
					{
						printf("%d. %s\n", ++i, tmpnode->s);
						tmpnode = tmpnode->next;
					}

					choose = take_in_range(NULL, 0, i);
					tmpnode = username_list;
					
					if (choose == 0)
						err = 1;
					else
					{
						while (--choose)	/* take the user name from the list */
							tmpnode = tmpnode->next;

						strcpy(userfname, tmpnode->s);
						strcat(userfname, ".worddat");

						bfid = fopen(userfname, "rb");
						if (bfid != NULL)
						{
							update_list(bfid, lib.ll_antonym, &lib.ll_rec_antonym);
							update_list(bfid, lib.ll_synonym, &lib.ll_rec_synonym);
							fclose(bfid);

							/* sort the updated list as decreasing probability ratio */
							lib.ll_antonym = merge_sort(lib.ll_antonym);
							lib.ll_synonym = merge_sort(lib.ll_synonym);
							err = 0;
						}
						else printf("User file cannot open for writing user record\n");
					}
			}
		} while (err);

		while (username_list != NULL) /* free username list when everthing is done about selecting user */
		{
			tmpnode = username_list;
			if (tmpnode->s != NULL) free(tmpnode->s);
			username_list = username_list->next;
		}

		ant_list = lib.ll_antonym;
		syn_list = lib.ll_synonym;

		do {
			printf("\n------------------ MAIN MENU -------------------------\n");
			choose = take_in_range("0. Exit the program\n1. Exercise questions\n2. Add new word\n3. Exit\n", 0, 3);
			
			switch (choose)
			{
				case 0:	choose = 3; break; /* set choose as exit */
				case 1: /* Exercise questions 						*/
					printf("Escape values as answer of question (0: Exit, ?: Show answer). Enter to start "); clear_stdin();

					/* check if user answered any question */
					if (lib.ll_rec_antonym != NULL || lib.ll_rec_synonym != NULL)
						choose = take_in_range ("0. Exit submenu\n1. Ask most misunderstood words\n2. Ask new words\n", 0, 2);
					else
						choose = 2;

					switch (choose)
					{
						case 0: break;
						case 1:	/* Ask most misunderstood words */
							status = ask_misunderstood(lib.ll_rec_antonym, antonym);
							status += ask_misunderstood(lib.ll_rec_synonym, synonym);
								
							if (status == 0)
								printf("There is no question answered\n");
							else
								printf("\nPerfect! all the misunderstood words solved\n");

							break;
						case 2:	/* Ask new words */
							do {
								/* choose the word that asked accordingly probability order and iterate the list which choosed */
								if (ant_list->probability > syn_list->probability)
									qtype = antonym;
								else if (syn_list->probability > ant_list->probability)
									qtype = synonym;
								else /* select the reverse type of previous question, just for a difference */
									qtype = (qtype == antonym) ? synonym : antonym;

								switch (qtype)
								{
									case antonym:
										wordlist = ant_list;
										ant_list = ant_list->next;
										break;
									case synonym:									
										wordlist = syn_list;
										syn_list = syn_list->next;
								}

								status = ask_word(wordlist, qtype);

								if (status == -1) printf("Returned to main menu\n");
								else
								{
									switch (status)
									{
										case 0:	++(wordlist->false_ans); break;
										case 1:	++(wordlist->true_ans); break;
										case 2:	++inserted, ++(wordlist->true_ans);
									}
									calculate_probability(&lib, qtype, wordlist);
									
									tmprec = (user_rec_t *) malloc(sizeof(user_rec_t));
									if (tmprec != NULL) /* insert the address of asked word in user record list */
									{
										tmprec->data = wordlist;
										
										if (qtype == antonym)
										{
											tmprec->next = lib.ll_rec_antonym;
											lib.ll_rec_antonym = tmprec;
										}
										else
										{
											tmprec->next = lib.ll_rec_synonym;
											lib.ll_rec_synonym = tmprec;
										}
									}
									else prevent_segfault();
								}
							} while (status != -1);	/* till user entered '0' and ask_question function return -1 */ 
					}					
					break;

				case 2:	/* add new word 					*/
					choose = take_in_range ("0. Exit submenu\n1. Antonym words list\n2. Synonym words list\n", 0, 2);

					if (choose != 0)
					{
						printf("Word: ");
						scanf(" %[^\n]%*c", str1); 
						printf((choose == 1) ? "Antonym of word: " : "Synonym of word: ");
						scanf(" %[^\n]%*c", str2);

						switch (choose)
						{
							case 1: insert_list(str1, str2, &lib.ll_antonym); break;
							case 2: insert_list(str1, str2, &lib.ll_synonym);
						}
						printf("New word added. Enter to continue "); clear_stdin();
					}
					break;

				case 3: /* update both system files and user records then exit */					
					if (inserted) /* if user enter any words update the system files */
					{
						write_textf("antonyms.txt", lib.ll_antonym, antonym);
						write_textf("synonyms.txt", lib.ll_synonym, synonym);
					}

					bfid = fopen(userfname, "wb");
					
					if (bfid == NULL) 
						printf("User file cannot open for writing user record\n");
				
					write_binaryf(bfid, lib.ll_rec_antonym);
					write_binaryf(bfid, lib.ll_rec_synonym);
					fclose(bfid);

					system_free(&lib);
					printf("See you soon...\n");
			}
		} while (choose != 3);
	}
}

void system_free(lib_t * lib)
{
	if (lib != NULL)
	{
		free_all_list(lib->ll_antonym);
		free_all_list(lib->ll_synonym);

		free_record(lib->ll_rec_antonym);
		free_record(lib->ll_rec_synonym);
	}
}

int ask_misunderstood (user_rec_t * l, word_t qtype)
{
	int r = 0;

	if (l != NULL)
	{
		if (l->data->false_ans > 0)
		{
			ask_word(l->data, qtype);
			++r;
		}
		l = l->next;
	}
	return r;
}

/* type 1: antonym que, type 2: synonym que return values: -1: exit, 0: false ans,  1: true ans, 2: add word */
int ask_word (wlist_t * l, word_t qtype)
{
	char 	* que = qtype ? "Antonym" : "Synonym",
		 	ans[BUFFSIZE];
	int choose, r;
	snode_t * wordlist, * backnode, * tmpnode;

	if (l != NULL)
	{
		printf("\n%s of %s: ", que, l->word);
		scanf("%[^\n]%*c", ans);

		if (ans[0] == '0')	/* user enter exit code */
			r = -1;
		else
		{
			if (ans[0] == '?') /* show the answers */
				choose = 2;
			else  /* check user answer */
			{
			 	backnode = wordlist = l->list;
				while (wordlist != NULL && strcmp(wordlist->s, ans))
				{
					backnode = wordlist;
					wordlist = wordlist->next;
				}

				if (wordlist != NULL) /* user answer exist in sublist */
				{
					r = 1;
					choose = 0;
				} 
				else
					choose = take_in_range("\nYour answer does not exist in the list\n\n0. Exit submenu\n1. Add to list\n2. See the answers\n", 0, 2);
			}
				
			switch (choose)
			{
				case 0: r = 0; break; /* return value: false answer */
				case 1:
					tmpnode = (snode_t *) malloc(sizeof(snode_t));
					if (tmpnode != NULL)
					{
						tmpnode->s = (char *) calloc(strlen(ans) + 1, sizeof(char));
						if (tmpnode->s != NULL)
						{
							strcpy(tmpnode->s, ans);
							tmpnode->next = NULL;
							
							if (backnode == NULL)
								l->list = tmpnode;
							else
								backnode->next = tmpnode;
						}
						else prevent_segfault();
					}
					else prevent_segfault();
					r = 2;	/* return value: add list */
					break;
				
				case 2:
					putchar('\n');
					tmpnode = l->list;
					while (tmpnode != NULL)
					{
						printf("# %s\n", tmpnode->s);
						tmpnode = tmpnode->next;
					}
					printf("Enter to continue "); clear_stdin();
					r = 0; /* return value: false answer */
			}
		}
	}
	return r;
}


void free_record (user_rec_t * l)
{
	user_rec_t * tmp;

	while (l != NULL)
	{	
		tmp = l;
		l = l->next;
		free(tmp);
	}
}

void free_all_list (wlist_t * mainlist)
{
	wlist_t * w;
	snode_t * n, * sublist;

	while (mainlist != NULL)
	{
		free(mainlist->word);

	 	sublist = mainlist->list;
		while (sublist != NULL) /* free the sublist */
		{
			free(sublist->s);

			n = sublist;
			sublist = sublist->next;
			free(n);
		}
		w = mainlist;
		mainlist = mainlist->next;
		free(w);
	}
}

/* splits given lits two separate and sorts them respectivly and finally merges these two sorted list */
wlist_t * merge_sort (wlist_t * l)
{
	wlist_t * l1, * l2;
	
	/* to sort list, list must contain at least two entry o.w. list is already sorted */
	if (l != NULL && l->next != NULL)
	{
		l1 = l;
		l2 = split(l);

		l1 = merge_sort(l1);
		l2 = merge_sort(l2);

		l = sorted_merge(l1, l2); /* merge these two sorted list */
	}
	return l;
}

/* split the list and returns the head of the second list */
wlist_t * split (wlist_t * l)
{
	wlist_t 	* f = l,	/* fast pointer, eventually points at end of the list 				*/
	 			* s = l; 	/* slow pointer, eventually points head of the splitted second list	*/

	while (f != NULL)
	{
		f = f->next;

		if (f != NULL && f->next != NULL)
		{
			f = f->next;
			s = s->next;
		}
		/* in that point s points the last node of the first list 	*/
		else
		{
			l = s->next;	/* now l is head of the second list   	*/
			s->next = NULL;	/* split the given list 				*/
			break;
		}
	}
	return l;
}

wlist_t * sorted_merge (wlist_t * l1, wlist_t * l2)
{
	wlist_t * l;

	if (l1 == NULL)
		l = l2;
	else if (l2 == NULL)
		l = l1;
	else 	/* select the bigger probability and continue recursivly with l->next */
	{
		if (l1->probability > l2->probability)
		{
			l = l1;
			l->next = sorted_merge(l1->next, l2);
		}
		else
		{
			l = l2;
			l->next = sorted_merge(l1, l2->next);
		}
	}
	return l;
}

/* the probablity of any word determind by */
void calculate_probability (lib_t * lib, word_t qtype, wlist_t * l)
{
	wlist_t * tmp; /* tmp pointer points the reverse type of given list */
	int n;

	if (lib != NULL && l != NULL)
	{
		tmp = qtype ? lib->ll_antonym : lib->ll_synonym;
		n = l->true_ans + l->false_ans;		/* the number of asked time */
		/* in case of n = 0, the probability calculated as 1 */
		l->probability = (double) (l->false_ans + 1) / (n + 1);

		if (n > 1)
		{
			while (tmp != NULL && strcmp(tmp->word, l->word))
				tmp = tmp->next;
			
			if (tmp != NULL) /* increase the probability of reverse type word as n */
				tmp->probability = (tmp->probability / (n - 1)) * n;
		}
	}
}

/* returns the given 'text' files as linked list, in case of error function returns 1 */
int read_textf (char * fname, wlist_t ** mainlist)
{
	/* ip points the node in mainlist, jp points the node in sublist which inside of main list */
	int 		i, err = 0;
	char 		line[LINESIZE], *s, * piece;
	FILE 		* infid;
	wlist_t 	* sublist, * ip;
	snode_t 	* sublistword, * jp;
	
	if (mainlist == NULL)
		err = 1;
	else
	{
		infid = fopen(fname, "r");
		if (infid == NULL)
			err = 1;
		else
		{
			while (fgets(line, LINESIZE, infid))
			{
				/* to use strtok change '\n' with ' ' */
				for (i = 0; i < LINESIZE && line[i] != '\0'; ++i)
					if (line[i] == '\n')
						line[i] = ' ';

				/* take the first word of line and assign as word of mainlist */
				piece = strtok(line, " ");
				strtok(NULL, " ");			/* consume <> or = in current line */
				
				/* create a new sublist and fill with parsing and assigning the line */
				sublist = (wlist_t *) malloc(sizeof(wlist_t));

				if (sublist != NULL)
				{
					s = (char *) calloc(strlen(piece) + 1, sizeof(char));
					if (s != NULL)
					{
						strcpy(s, piece);
						sublist->word = s;
						sublist->false_ans = sublist->true_ans = 0;
						sublist->probability = 1;
						sublist->list = NULL;
						sublist->next = NULL;

						err = 0;
						/* fill created sublist with the line data until strtok returns NULL(end of the line) */
						while (err == 0 && (piece = strtok(NULL, " ")))
						{
							sublistword = (snode_t *) malloc(sizeof(snode_t));
							if (sublistword != NULL)
							{
								s = (char *) calloc(strlen(piece) + 1, sizeof(char));
								if (s != NULL)
								{
									strcpy(s, piece);
									sublistword->s = s;
									sublistword->next = NULL;

									/* check if this is the first insertion of the list */
									if (sublist->list == NULL)
										jp = sublist->list = sublistword;
									/* insert new word and update jp as points the new inserted node */
									else
									{
										jp->next = sublistword;
										jp = sublistword;
									}
								}
								else
									err = 1;
							}
							else
								err = 1;
						}
						/* insert new created sublist */
						if (! err)
						{
							if (*mainlist == NULL)
								ip = *mainlist = sublist;
							else
							{
								ip->next = sublist;
								ip = sublist;						
							}
						}
					}
				}
				else prevent_segfault();					
			}
		}
	}
	/* print_list(*mainlist); */
	return err;
}

int write_textf (char * fname, wlist_t * mainlist, word_t qtype)
{
	int err;
	char * symbol = qtype ? "<>" : "=";
	FILE * outfid;
	snode_t * tmpnode;

	if (mainlist == NULL)
		err = 1;
	else 
	{
		outfid = fopen(fname, "w");
		if (outfid == NULL) err = 1;
		else
		{
			while (mainlist != NULL)	/* print the mainlist */
			{
				tmpnode = mainlist->list;
				fprintf(outfid, "%s %s", mainlist->word, symbol);

				while (tmpnode != NULL)	/* print each of the sublist */
				{
					fprintf(outfid, " %s", tmpnode->s);
					tmpnode = tmpnode->next;
				}
				/* pass the next line */
				mainlist = mainlist->next;
				if (mainlist != NULL)
					fputc('\n', outfid);
			}
			fclose(outfid);
		}
	}
	return err;
}

void update_list (FILE * bfid, wlist_t * mainlist, user_rec_t ** ll_record)
{
	int i, status;
	char buff[BUFFSIZE];
	user_rec_t * new_record;
	wlist_t * tmplist;

	if (bfid != NULL && mainlist != NULL && ll_record != NULL)
	{
		do {
			for (i = 0; status = fread(&buff[i], sizeof(char), 1, bfid); ++i)
			{
				if (buff[i] == '\0')
				{
					if (i == 0)	/* end of record */
					{
						status = 0;
						break;
					}

					new_record = (user_rec_t *) malloc(sizeof(user_rec_t));
					if (new_record != NULL)
					{
						tmplist = mainlist;
						while (tmplist != NULL)
						{
							if (strcmp(buff, tmplist->word))
								tmplist = tmplist->next;
							else /* read data into original memory block */
							{
								fread(&tmplist->true_ans, sizeof(int), 1, bfid);
								fread(&tmplist->false_ans, sizeof(int), 1, bfid);
								break;
							}
						}
						/* save the address of that block as user record, insert begin */
						new_record->data = tmplist;	
						new_record->next = *ll_record;
						*ll_record = new_record;
						break;
					}
					else prevent_segfault();					
				}
			}
		} while (status);	
	}
}


/* stores user performance for asking word and the number of true and false answers */
int write_binaryf (FILE * bfid, user_rec_t * l)
{
	int err = 1;
	char c = END_OF_RECORD;

	if (bfid != NULL)
	{
		while (l != NULL)
		{
			fwrite(l->data->word, sizeof(char), strlen(l->data->word) + 1, bfid);
			fwrite(&l->data->true_ans, sizeof(int), 1, bfid);
			fwrite(&l->data->false_ans, sizeof(int), 1, bfid);
			l = l->next;
		}
		fwrite(&c, sizeof(char), 1, bfid); /* put END_OF_RECORD charachter */	
	}
	return err;
}

/* for inserting new word by user */
void insert_list(char * ins_word, char * s, wlist_t ** mainlist)
{
	char buff[BUFFSIZE];
	wlist_t * sublist, * backlp;
	snode_t * curnode, * backnode;	/* for searching and inserting words in sublist */
	
	if (mainlist != NULL)
	{
		backlp = sublist = *mainlist;
		
		/* check the sublists, if already list exist skip creating a new sublist */
		while (sublist != NULL && strcmp(sublist->word, ins_word) != 0)
		{
			backlp = sublist;
			sublist = sublist->next;
		}

		if (sublist == NULL) /* there is no such record, so create a new sublist */
		{
			sublist = (wlist_t *) malloc(sizeof(wlist_t));
			
			if (sublist != NULL)
			{
				sublist->word = calloc (strlen(ins_word) + 1, sizeof(char));
				if (sublist->word != NULL)
				{
					strcpy(sublist->word, ins_word);
					sublist->true_ans = sublist->false_ans = 0;
					sublist->probability = 1;
					sublist->list = NULL;
					sublist->next = NULL;
					
					if (backlp == NULL) 	 
						*mainlist = sublist;
					else
						backlp->next = sublist;
				}
				else prevent_segfault();
			}
			else prevent_segfault();
		}

		/* check if word is already inside of the sublist, if so no need to add new word */
		backnode = curnode = sublist->list;
		while (curnode != NULL && strcmp(curnode->s, s))
		{
			backnode = curnode;
			curnode = curnode->next;
		}

		if (curnode == NULL)
		{
			curnode = (snode_t *) malloc(sizeof(snode_t));
			
			if (curnode != NULL)
			{
				curnode->s = (char *) calloc(strlen(s) + 1, sizeof(char));
				if (curnode->s != NULL)
				{
					strcpy(curnode->s, s);
					curnode->next = NULL;
				}
				else prevent_segfault();
			}
			/* insert into the list */
			if (backnode == NULL)
				sublist->list = curnode;
			else
				backnode->next = curnode;
		}
		else
			printf("Word is already existing\n");
	}
}


int take_in_range (char * prompt, int min, int max)
{
	int status, r, tmp;

	if (min > max)
	{
		tmp = min;
		min = max;
		max = tmp;
	}
	if (prompt != NULL) 
		printf("%s", prompt);
	printf("-------------------------------------------------\nChoose: ");

	for (status = scanf("%d", &r); !status || (min > r || r > max); status = scanf("%d", &r))
	{
		printf("Please enter a proper choose: ");
		clear_stdin();
	}
	clear_stdin(); putchar('\n');

	return r;
}

void prevent_segfault ()
{
	char choose;
	printf("There is no enough memory to new allocation\n");
	printf("What do you want\n");
	choose = take_in_range("1. Wait for a couple of second\n2. Terminate the program\n", 1, 2);

	switch (choose)
	{
		case 1:
			getchar();	/* it's enough for memory repairing itself, it can also loop or time.h functions  */
			break;
		case 2:
			exit(1);
			break;
	}
}

void clear_stdin()
{
	while(getchar() != '\n');
}
