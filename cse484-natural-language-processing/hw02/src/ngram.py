from collections import Counter
import math
import random
import preprocessing

def get_vocabulary(corpus: list) -> set:
    return set(corpus)

def calculate_num_ngrams(corpus_size, ngram_size):
    return corpus_size - ngram_size + 1

def add_ngram(ngrams: dict, tokens: list):
    trav = ngrams
    for token in tokens[:-1]:
        trav[token] = trav.get(token, {})
        trav = trav[token]
    trav[tokens[-1]] = trav.get(tokens[-1], 0) + 1 

def generate_ngrams_file(infile: str, n: int) -> (dict, int, set):
    ngrams = {}
    vocabulary = set()
    num_ngrams = 0
    
    for line in preprocessing.get_tokenized_lines(infile):    
        c = calculate_num_ngrams(len(line), n)
        num_ngrams += c
        for i in range(c):
            add_ngram(ngrams, line[i:(i + n)])
            vocabulary.add(line[i])
    return ngrams, num_ngrams, vocabulary

def generate_ngrams(corpus: list, n: int) -> dict:
    num_ngrams = len(corpus) - n + 1
    ngrams = {}
    for i in range(num_ngrams):
        add_ngram(ngrams, corpus[i:(i + n)])
    return ngrams

# Counts the number of ngrams in the given ngram dictionary 
def count_ngrams(ngrams: dict) -> int | float:
    count = 0

    for _, value in ngrams.items():
        if type(value) is dict:
            count += count_ngrams(value)
        else:
            count += value       
    return count

def get_counts(ngrams: dict, ngram: tuple):
    trav = ngrams
    for token in ngram:
        # If the given tuple size does not match with the ngram, or the tuple is not seem
        if (type(trav) is not dict) or (not trav.get(token, None)):
            return 0
        trav = trav[token]
    # trav becomes count at the end
    return trav

def get_gt_frequencies(ngrams: dict) -> Counter:
    frequencies = Counter()
    for _, value in ngrams.items(): 
        if type(value) is dict:
            frequencies += get_gt_frequencies(value)
        else:
            frequencies[value] += 1
    return frequencies

class Ngram:
    def __init__(self, n):
        # Model hyperparameters
        self.n = n
        self.ngrams = {}
        self.num_ngrams = 0
        # For Good-Turing smoothing
        # Since there is a limitation for the representation of the small numbers, we use the natural logarithm 
        self.log_zero_probability = 0.0 
        self.gt_frequencies = Counter()
        self.vocabulary = set()

    def train(self, train_file: str) -> None:
        # Create ngrams and necessary statistics        
        self.ngrams, self.num_ngrams, self.vocabulary = generate_ngrams_file(train_file, self.n)

    def evaluate(self, test_file: str) -> None:
        sum = 0.0
        num_lines = 0 
        for line in preprocessing.get_tokenized_lines(test_file):    
            sum += self.get_perplexity(line)
            num_lines += 1
        # Normalize for the number of lines
        return sum / num_lines

    def __is_smoothed(self):
        return self.log_zero_probability == 0.0

    def __update_counts_gt(self, table: dict, new_counts: Counter, k=5):
        for key, value in table.items():             
            # Recursively continue 
            if type(value) is dict: # SOMETHING WRONG HERE
                # maybe return is needed
                table[key] = self.__update_counts_gt(value, new_counts, k)
            elif value <= k:
                table[key] = new_counts[value]
        return table 

    # Use the count of things we've seen once to help estimate the count of things we've never seen
    # Apply Good–Turing smoothing for c <= k
    def smooth(self, k=5):
        # Count the frequency of frequencies
        self.gt_frequencies = get_gt_frequencies(self.ngrams)

        all_possibilities = len(self.vocabulary) ** self.n
        observed_unique_pairs = len(self.ngrams)
        N0 = all_possibilities - observed_unique_pairs 
        N1 = self.gt_frequencies.get(1, 1) # frequency of 1 should be there!
        N = self.num_ngrams
        
        # p0 = N1 / N
        if self.n == 1:
            self.log_zero_probability = 0.0 
        else:
            self.log_zero_probability = math.log((N1 / N) / N0)

        # Reconstitute the counts
        new_counts = {}
        for c in range (1, k + 1):
            # c* = (c + 1) * N_c+1 / N_c 
            N_c = self.gt_frequencies[c]
            N_c1 = self.gt_frequencies[c + 1]
            new_counts[c] = (c + 1) * (N_c1 / N_c)

        # Update the counts
        self.__update_counts_gt(self.ngrams, new_counts, k)

    # Perplexity is the probability of the test set (assigned by the language model), normalized by the number of words:
    def get_perplexity(self, sentence: list):
        log_sums = self.get_sentence_probability(sentence, log_output=True)
        return math.exp(log_sums * (- 1 / len(sentence))) 

    def get_random_start_contex(self) -> tuple:
        context = (preprocessing.START_TOKEN,)
        
        # Get all the N-grams starting with sentence start token
        non_complete_ngram_dict = get_counts(self.ngrams, context)
        for _ in range(self.n - 2):
            key, value = random.choice(list(non_complete_ngram_dict.items()))
            # Produce most likely context (n-1 ngram) 
            context += (key,)
            non_complete_ngram_dict = value

        return context

    def get_start_probability(self, ngram: tuple):
        non_complete_ngram_dict = get_counts(self.ngrams, ngram)
        return count_ngrams(non_complete_ngram_dict) / self.num_ngrams

    def get_ngram_probability(self, ngram: tuple):
        if len(ngram) < self.n:
            return 0.0
        p = get_counts(self.ngrams, ngram) / self.num_ngrams
        
        if p == 0 and self.__is_smoothed():
            p = math.exp(self.log_zero_probability)
        return p  

    def get_context_window(self, context: tuple) -> dict:
        trav = self.ngrams
        for token in context:
            if trav.get(token, None) is None: 
                return {}
            trav = trav[token]
        return trav

    def get_context_probability(self, context: tuple):
        count = 0
        context_window = self.get_context_window(context)
        if not context_window:
            return 0.5
        for _, c in context_window.items():
            count += c

        # context is just (n-1)-gram 
        return count / (self.num_ngrams + 1)

    # P(a|b) = P(a^b) / P(b)
    # Calculates P(x|w): conditional probability x given context w 
    def get_conditional_probability(self, x: str, context: tuple):
        # Markov assumption, context is only the n-1 token
        context = tuple(context[-(self.n - 1):])
        p_context = self.get_context_probability(context)

        # produce the ngram
        ngram = context + (x,)

        p_ngram = self.get_ngram_probability(ngram)

        # Avoid division by 0
        return p_ngram / p_context if p_context != 0.0 else 0.0 
    
    # Calculating the probability of sentence using chain rule with the Markov assumption
    def get_sentence_probability(self, sentence: list, log_output=False):
        logs_sum = 0.0

        if self.n == 1:
            for i in range(len(sentence)):
                p = self.get_ngram_probability((sentence[i],))
                logs_sum += 0.0 if p == 0.0 else math.log(p)
        else:
            context = tuple([preprocessing.START_TOKEN] * (self.n - 1))
            for i in range(len(sentence)):
                p = self.get_conditional_probability(sentence[i], context)
                
                logs_sum += 0.0 if p == 0.0 else math.log(p)
                context = context[-1:] + (sentence[i],)

        return logs_sum if log_output else math.exp(logs_sum)

    def get_count_start_padded_ngram(self, w: tuple):
        count = 0
        # Find the number of non-padding tokens + first start padding
        n = len(w) - w.count(preprocessing.START_TOKEN)  + 1
        for ngram, c in self.ngrams.items():
            # Compare the last n token of w and first n token of ngram 
            if w[-n:] == ngram[:n]:
                count += c
        return count

    def get_nexts_padded_start_context(self, w: tuple) -> Counter:
        # Find the number of non-padding tokens + first start padding
        nexts = Counter()

        n = len(w) - w.count(preprocessing.START_TOKEN)  + 1
        for context, context_info in self.contexts.items():
            # Compare the last n token of w and first n token of ngram 
            if w[-n:] == context[:n]:
                for token, count in context_info.nexts.items():
                    nexts[token] += count
        return nexts

    def get_random(self, context: tuple, window_size=5) -> str: 
        context_window = self.get_context_window(context)
        if context_window: 
            candidates = Counter(context_window).most_common(window_size)
        else: # No context window       
            candidates = random.choice(list(self.vocabulary))

        # Randomly choose from the most probable n (window_size) syllable
        selected = random.choice(candidates)
        
        # Remove the count if necessary
        if type(selected) is tuple:
            selected = selected[0] 

        return selected

    def generate_random_sentence(self, window_size=5, limit=100): # we dont take the input as token, instead take "words"
        sentence = ""

        # A unigram model doesn't have any context
        if self.n == 1:
            candidates = Counter(self.ngrams).most_common(window_size)
            for _ in range(limit):
                token = random.choice(candidates)[0]
                sentence += token
        else:
            context = self.get_random_start_contex() 
            for _ in range(limit):
                token = self.get_random(context, window_size)
                sentence += token
                # Update the context
                context = context[1:] + (token,)

        return preprocessing.realize_tokens(sentence) 
