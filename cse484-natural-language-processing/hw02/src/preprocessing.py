import re
import nltk
from typing import Iterator
from syllable import Encoder

nltk.download('punkt')

START_TOKEN = '<s>'
END_TOKEN = '</s>'
SPACE_TOKEN = '<space>'

def turkish_to_english(text):
  # Define Turkish and English character mappings
  turkish_chars = 'çğıöşüÇĞİÖŞÜ'
  english_equivalents = 'cgiosuCGIOSU'

  # Convert Turkish characters to English equivalents
  translation_table = str.maketrans(turkish_chars, english_equivalents)
  translated_text = text.translate(translation_table)

  return translated_text

def remove_non_alphabetical(input_string):
    # Regular expression pattern to match non-alphabetical characters
    pattern = re.compile(r'[^a-zA-Z ]')  # Matches any character that is not a letter

    # Use the pattern to substitute non-alphabetical characters with an empty string
    cleaned_string = re.sub(pattern, '', input_string)
    cleaned_string = re.sub(r'\s+', ' ', cleaned_string.strip())

    return cleaned_string

def realize_tokens(text: str):
    text = text.replace(SPACE_TOKEN, " ")
    text = text.replace(START_TOKEN, "")
    text = text.replace(END_TOKEN, ".")
    return text

def syllable_tr(text: str) -> list:
    syllables = []
  
    # Configure Turkish syllable encoder
    encoder = Encoder(lang="tr", limitby="vocabulary", limit=3000)
    
    for sentence in nltk.tokenize.sent_tokenize(text):
        # Remove all the punctuations and numbers
        sentence = sentence.lower()
        sentence = turkish_to_english(sentence)
        sentence = remove_non_alphabetical(sentence)
        # Split the sentence according to spaces to insert space token 
        words = sentence.split(' ')
        if not words:
            continue

        # Start of sentence token
        syllables.append(START_TOKEN)

        # Syllable the first word
        first_word_syllables = encoder.tokenize(words.pop(0))
        for sy in first_word_syllables.split(' '):
            syllables.append(sy)

        for word in words:
            syllables.append(SPACE_TOKEN)
            word_syllables = encoder.tokenize(word)
            for sy in word_syllables.split(' '):
                syllables.append(sy)
        # End of sentence token
        syllables.append(END_TOKEN)

    return syllables

# Return tokenized lines one at a time from a tokenized text
def get_tokenized_lines(tokenized_file: str) -> Iterator[list]:
    with open(tokenized_file) as file_handle:
        for line in file_handle:
            # remove the trailing newline character
            yield line.strip().split(' ')

def tokenize_wikipedia_dump(infile: str, outfile: str, min_per_line_token=5, line_limit=False, max_num_lines=20000) -> int:
    num_lines = 0
    with open(infile, 'r', encoding='utf-8') as input_file, open(outfile, 'w', encoding='utf-8') as output_file:
        for line in input_file:
            # Check specific patterns and exclude empty lines
            if line.startswith('<doc') or line.startswith('</doc') or not line.strip():
                continue
            # Replace multiple spaces with a single space in the line
            syllables = syllable_tr(line)
            if len(syllables) > min_per_line_token:
                syllables = ' '.join(syllables).strip()
                processed_line = re.sub(r'\s+', ' ', syllables)
                output_file.write(processed_line + '\n')
                num_lines += 1
                if line_limit and num_lines > max_num_lines:
                    break
    return num_lines