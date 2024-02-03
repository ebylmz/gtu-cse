from enum import Enum

class WordClass(Enum):
    Niv = 1
    Nic = 2
    Nuv = 3
    Nuc = 4
    Niiv = 5
    Niic = 6
    Nuuv = 7
    Nuuc = 8

def get_word_class(word: str) -> WordClass:
    last_vowel = get_last_vowel(word)
    is_vowel_ending = last_vowel == word[-1] 
    
    if last_vowel in 'aı':
        return WordClass.Niv if is_vowel_ending else WordClass.Nic    
    elif last_vowel in 'ou':
        return WordClass.Nuv if is_vowel_ending else WordClass.Nuc    
    elif last_vowel in 'ei':
        return WordClass.Niiv if is_vowel_ending else WordClass.Niic    
    elif last_vowel in 'öü':
        return WordClass.Nuuv if is_vowel_ending else WordClass.Nuuc    

def get_last_vowel(input_string: str) -> str:
    last_vowel = None

    for c in reversed(input_string):
        if is_vowel(c):
            last_vowel = c
            break 

    return last_vowel

def is_vowel(c: str) -> bool:
    turkish_vowels = 'aeıioöuüAEIİOÖUÜ'
    return c in turkish_vowels

def is_hard_consonant(c: str) -> bool:
    return c in ['p', 'ç', 't', 'k', 'P', 'Ç', 'T', 'K'] 

# Returns the soft equivalent of the given hard consonant 
def apply_consonent_softening(c: str) -> list:
    if c == 'p':
        return ['b']
    elif c == 'ç':
        return ['c']
    elif c == 't':
        return ['d']
    elif c == 'k':
        return ['g', 'ğ']
    return []

def check_consonant_softening(before: str, after: str) -> bool:
    return after in apply_consonent_softening(before) 

def is_consonant_lenition(word_last: str, suffix_first) -> bool: 
    return is_hard_consonant(word_last) and is_vowel(suffix_first)

def get_suffix_vowel(wc: WordClass) -> str:
        # Consonant ending word, before suffix there needs to  be additional a vowel
        if wc == WordClass.Niv or wc == WordClass.Nic: 
            # a, ı becomes ı
            return 'ı'        
        elif wc == WordClass.Nuv or wc == WordClass.Nuc:
            # o, u becomes u
            return 'u'        
        elif wc == WordClass.Niiv or wc == WordClass.Niic:
            # e, i becomes i
            return 'i'        
        else: # wc == WordClass.Nuuv or wc == WordClass.Nuuc:
            # ö, ü becomes ü
            return 'ü'        

def generate_possessive_suffixes(wc: WordClass) -> list:
    plural_suffix = 'lar' if wc in [WordClass.Niv, WordClass.Nic, WordClass.Nuv, WordClass.Nuc] else 'ler'

    vowel = get_suffix_vowel(wc)

    if wc in [WordClass.Niv, WordClass.Nuv, WordClass.Niiv, WordClass.Nuuv]:
        # Vowel ending word, additional consonant 's' for the 3rd singular person
        s1 = 'm'
        s2 = 'n'
        s3 = 's' + vowel
        p1 = 'm' + vowel + 'z'
        p2 = 'n' + vowel + 'z'
        p3 = plural_suffix + vowel
    elif wc in [WordClass.Nic, WordClass.Nuc, WordClass.Niic, WordClass.Nuuc]:
        # Consonant ending word, before suffix there needs to  be additional a vowel
        s1 = vowel + 'm'
        s2 = vowel + 'n'
        s3 = vowel
        p1 = vowel + 'm' + vowel + 'z'
        p2 = vowel + 'n' + vowel + 'z'
        p3 = plural_suffix + vowel

    return [s1, s2, s3, p1, p2, p3]