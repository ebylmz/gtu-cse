#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define WORD_LEN 20 	/* max string length 			*/
#define MAX_CAND 30 	/* maximum number of candidates */

typedef enum {false, true} bool;

void fix_spelling_error2 (char dic_name[], char text_name[]);
void fix_spelling_error (char dic_name[], char text_name[]);
void fscan_word (FILE *file, char w[]);
int  strdiff (const char w1[], const char w2[]);
int  find_best_correction(FILE *dic_file, char textw[]);
void fowerwrite (FILE *file, const char w[]);
void sens_strcpy (char des[], const char source[]);
bool is_letter (char c);
bool is_same_letter (char c1, char c2);
bool is_vowel (char c);
bool is_cons_to_vowel (const char consw[],const char vovelw[]);
bool is_encountered (char arr[][WORD_LEN], int n, const char str[]);


int main(void)
{	
	char 	c,
			dic_name[] = "dictionary.txt",
			text_name[] = "text.txt";
	

	printf("Name of source files\n");
	printf("%-20s%20s\n", "Dictionary file:", dic_name);
	printf("%-20s%20s\n", "Text file:",text_name);

	printf("\nDo you want to change the name of files [Y/y]: ");
	c = getchar();

	if (c == 'Y' || c == 'y')
	{
		printf("%-20s%5s", "Dictionary file", ": ");
		scanf("%s", dic_name);
		printf("%-20s%5s", "Text file", ": ");
		scanf("%s", text_name);
	}


	fix_spelling_error(dic_name, text_name); 
	/* fix_spelling_error2(dic_name, text_name); */
} 

/* fixs words inside text file by extracting and comparing with dictionay file */
void fix_spelling_error(char dic_name[], char text_name[])
{
	int 	i, 
			error = 0;				/* error flag 				   */
	char 	textw[WORD_LEN];		/* word scanned from text file */
	FILE 	*dicfile,
			*textfile;
	
	/* open the dictionary file with read mode */
	if (dicfile = fopen(dic_name, "r"))
	{
		/* open the text file with read and write mode */
		if (textfile = fopen(text_name, "r+"))
		{
			/* scan letter by letter and construct a word (a word is sequence of letters) 						*/
			while (! feof(textfile))
			{
				/* be sure that words are extracted properly (white spaces and punctuation marks) 				*/
				fscan_word(textfile, textw);

				/* 	if scanned string is consist of only one letter accept is as true, beacuse all the 
					letters are unique and can imply specific meanings E.g a.c or r&b 							*/

				/* check if it is spelled correctly, if not replace it with closest word inside dictionary 	file */				
				if (strlen(textw) > 1 && find_best_correction(dicfile, textw))
					fowerwrite(textfile, textw);				
			}
			fclose(dicfile);
			fclose(textfile);
		}
		else
		{
			fclose(dicfile);
			error = 1;
		}
	}
	else
		error = 1;


	if (error == 0)
		printf("\n[>] Correction has been successfully completed\n");
	else
		printf("\n[!] Source file cannot find or open properly\n");
}

int find_best_correction(FILE *dicfile, char textw[])
{
	int 	i, j,
			matching = 0,		  /* number of candidates that to be best correction words	 	*/
			best_match = 0,
			cmp_result,			  /* compare result comes from strdiff function 				 	*/
			mistake_sens = 2,	  /* the number of max letter mistake that function can handle   */
			result;				  /* return value, implyies if there is a mathcing word or not 	*/

	char 	dicw[WORD_LEN],		  /* word scanned from dictionary 								*/
			cands[MAX_CAND][WORD_LEN];  /* candidates 													*/

	/* set cursor the start of the file, to read from start to end */
	fseek(dicfile, 0, SEEK_SET);
	
	while (! feof(dicfile))
	{
		fscanf(dicfile, "%s", dicw);

		/* first check length of two words, if they are not equal skip this dictionary word */
		if (strlen(textw) == strlen(dicw))
		{
			cmp_result = strdiff(textw, dicw);

			/* if there is a 100% matching, word is spelled correctly */
			if (cmp_result == 0)
			{
				break;
			}
			/* if there is a matching with 1 or 2 letter difference, store it */
			else if (cmp_result <= mistake_sens && (! is_encountered(cands, matching, dicw)))
			{
				/* 	if we encountered a matching word with one letter difference, 
					increase the mistake sensivit as 1 letter difference				*/
				if (cmp_result == 1 && mistake_sens == 2)
				{
					/* forget the matching words with 2 letter difference */
					matching = 0;
					mistake_sens = 1;
				}	
				strcpy(cands[matching++], dicw);

				/*  in case of matching words number reach maximum number candidates,
					to prevent buffer owerflow continue to store candidates start of the array (cands) */
				if (matching == MAX_CAND)
					matching = 0;
			}
		}
	}

	/* if word is mispelled (cmp_result != 0) and there is a matching word */
	if (cmp_result != 0 && matching > 0)
	{
		/* because of correction is happened set result (return value) as 1 */
		result = 1;

		/* if more than one matching choose the word changes from consonant to vowel as correct */
		if (matching != 1)
		{
			for (i = 0, j = 0, best_match = 0; i < matching; ++i)
			{
				if (is_cons_to_vowel(textw, cands[i]))
				{
					best_match = i;
					
					/* in case of a tie applying consonant to vowel rule choose the second occurrence */
					if (++j == 2)
						break;
				}		
			}
		}
		/* replace the founded best correction with given misspelled word */
		sens_strcpy(textw, cands[best_match]);
	}
	else
		result = 0;


	return result;
}

bool is_letter (char c)
{
	bool ans;

	if (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'))
		ans = true;
	else
		ans = false;

	return ans;
}

/* finds the number of different letter between two strings */
int strdiff (const char w1[], const char w2[])
{
	int 	i,
			diff = 0;

	/* compare two words till encountered null terminator */
	for (i = 0; w1[i] != '\0' && w2[i] != '\0'; ++i)
		if (! is_same_letter(w1[i], w2[i]))
			++diff;

	/* in case of different word length, add the diffrence */
	return diff + abs(strlen(w1) - strlen(w2));
}

/* scans word in file by extracting a word white spaces and punctuation marks, and puts in given location w */
void fscan_word (FILE *file, char w[])
{
	char c;
	int i = 0;

	for (c = getc(file); is_letter(c); c = getc(file))
	{	
	 	w[i++] = c;
	}

	/* add NULL charachter at the end of the word */
	if (i > 0)
		w[i] = '\0';
}

void fowerwrite (FILE *file, const char w[])
{
	/* go back as much as length of the word and one non-letter charachter (' ' or ',') */
	fseek(file, -1 * (strlen(w) + 1), SEEK_CUR);
	fprintf(file, "%s", w);

	/* turn back the initial point */
	fseek(file, 1, SEEK_CUR);
}

bool is_same_letter (char c1, char c2)
{
	bool ans;

	/* be sure entries are letter, then check the case upper lower letter situation */
	if (is_letter(c1) && is_letter(c2) && (c1 == c2 || abs(c1 - c2) == 'a' - 'A'))
		ans = true;
	else
		ans = false;

	return ans;
}

/* copies given source to destination with upper/lower case sensivity */
void sens_strcpy (char des[], const char source[])
{	
	int i;

	for (i = 0; source[i] != '\0'; ++i)
	{
		if (! is_same_letter(des[i], source[i]))
			des[i] = source[i];
	}

	des[i] = '\0';
}

/* looks if the given string exist in given array of string with size of n */
bool is_encountered (char arr[][WORD_LEN], int n, const char str[])
{
	int 	i;
	bool 	ans = false;

	for (i = 0; i < n; ++i)
	{
		if (strcmp(str, arr[i]) == 0)
		{
			ans = true;
			break;
		}

	}

	return ans;
}

/* checks if there is a letter in given word that changes from consonant to vowel */
bool is_cons_to_vowel (const char consw[], const char vovelw[])
{
	int 	i;
	bool 	ans;

	for (i = 0; consw[i] != '\0'; ++i)
	{
		if (consw[i] != vovelw[i])
		{
			if (! is_vowel(consw[i]) && is_vowel(vovelw[i]))
				ans = true;
			else
				ans = false;
		}
	}

	return ans;
}

bool is_vowel (char c)
{
	int 	i;
	bool 	ans = false;
	char 	vowels[] = {'a', 'e', 'i', 'o', 'u'}; 

	for (i = 0; i < 5; ++i)
	{
		/* to ensure upper/lower case sensivity use is_same_letter function */
		if (is_same_letter(c, vowels[i]))
		{
			ans = true;
			break;
		}
	}

	return ans;
}

/* ı am sory but this is better solution than copy pasting :) */
void fix_spelling_error2(char dic_name[], char text_name[])
{
	fix_spelling_error(dic_name, text_name);
}