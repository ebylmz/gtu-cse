import openfst_python as fst
from . import word_class as wc

EPSILON = "<eps>"
NOUN_TAG = "<N>"
PLURAL_TAG = "<pl>"
POSS_TAGS = ["<p1s>", "<p2s>", "<p3s>", "<p1p>", "<p2p>", "<p3p>"]

class TurkishFST:
    def __init__(self, lexicon: list):
        self.f = fst.Fst()
        # Define the input and output symbol tables
        self.input_sym = fst.SymbolTable()
        self.output_sym = fst.SymbolTable()
        # Dictionary to keep the states for the word classes to be able to extend the FST
        self.wc_dict = {}

        # Epsilon symbol for representing empty transitions, by convention, it's always has symbol zero
        self.input_sym.add_symbol(EPSILON)  
        self.output_sym.add_symbol(EPSILON)  
        self.output_sym.add_symbol(NOUN_TAG)  
        
        for c in 'abcçdefgğhıijklmnoöprsştuüvyzABCÇDEFGĞHIİJKLMNOÖPRSŞTUÜVYZ':
            self.input_sym.add_symbol(c)
            self.output_sym.add_symbol(c)
            # Additional output symbol for consonant softening
            if wc.is_hard_consonant(c):
                self.input_sym.add_symbol('^' + c)

        self.f.set_input_symbols(self.input_sym)
        self.f.set_output_symbols(self.output_sym)

        start_state = self.f.add_state() 

        eps_sym = self.output_sym.find(EPSILON) # 0
        noun_sym = self.output_sym.find(NOUN_TAG)

        for word in lexicon:
            word = word.lower()
            # For the union operation
            if len(lexicon) > 1:
                word_start_state = self.f.add_state() 
                self.f.add_arc(start_state, fst.Arc(eps_sym, eps_sym, None, word_start_state))
            else:
                word_start_state = start_state

            word_end_state = self.append_linear_fst(self.f, word_start_state, word, softening=True)

            # Keep the word class states as final state
            wclass = wc.get_word_class(word)
            
            if not wclass in self.wc_dict:
                wc_state = self.wc_dict[wclass] = self.f.add_state()
                self.f.set_final(wc_state)
            else:
                wc_state = self.wc_dict[wclass]
            self.f.add_arc(word_end_state, fst.Arc(eps_sym, noun_sym, None, wc_state))

        # Start state is the beginning of words with epsilon transition, and the end states are word classes
        self.f.set_start(start_state)

    def append_plural_fst(self) -> None:
        pl_lar_start_state = self.f.add_state() 
        pl_ler_start_state = self.f.add_state() 
        
        eps_sym = self.f.input_symbols().find(EPSILON)
            
        # Connect word classes with plural start state
        # Add suffix '+lar' to word classes Niv, Nic, Nuv, Nuc  
        for wclass in [wc.WordClass.Niv, wc.WordClass.Nic, wc.WordClass.Nuv, wc.WordClass.Nuc]:
            if wclass in self.wc_dict:
                self.f.add_arc(self.wc_dict[wclass], fst.Arc(eps_sym, eps_sym, None, pl_lar_start_state))

        # Add suffix '+ler' to word classes Niiv, Niic, Nuuv, Nuuc      
        for wclass in [wc.WordClass.Niiv, wc.WordClass.Niic, wc.WordClass.Nuuv, wc.WordClass.Nuuc]:
            if wclass in self.wc_dict:
                self.f.add_arc(self.wc_dict[wclass], fst.Arc(eps_sym, eps_sym, None, pl_ler_start_state))

        lar_end_state = self.append_linear_fst(self.f, pl_lar_start_state, "lar")
        ler_end_state = self.append_linear_fst(self.f, pl_ler_start_state, "ler")
        
        # Update the symbol table by adding plural word morph 
        pl_sym = self.output_sym.add_symbol(PLURAL_TAG) 
        self.f.set_output_symbols(self.output_sym)

        pl_lar_end_state = self.f.add_state() 
        pl_ler_end_state = self.f.add_state() 

        # Epsilon transition to output plural morph
        self.f.add_arc(lar_end_state, fst.Arc(eps_sym, pl_sym, None, pl_lar_end_state))
        self.f.add_arc(ler_end_state, fst.Arc(eps_sym, pl_sym, None, pl_ler_end_state))
        self.f.set_final(pl_lar_end_state)
        self.f.set_final(pl_ler_end_state)

        # Make sure Nic and Niic are defined
        for wclass in [wc.WordClass.Nic, wc.WordClass.Niic]:
            if not wclass in self.wc_dict:
                wc_state = self.wc_dict[wclass] = self.f.add_state()
                self.f.set_final(wc_state)

        # After having plural suffix, the word class of the root turns into Nic (+lar), Niic (+ler)
        self.f.add_arc(pl_lar_end_state, fst.Arc(eps_sym, eps_sym, None, self.wc_dict[wc.WordClass.Nic]))
        self.f.add_arc(pl_ler_end_state, fst.Arc(eps_sym, eps_sym, None, self.wc_dict[wc.WordClass.Niic]))

    def append_possessive_fst(self) -> None:
        eps_sym = self.f.input_symbols().find(EPSILON)

        # Add possessive class symbols in order 
        poss_classes_sym = []

        for sym in POSS_TAGS:
            poss_classes_sym.append(self.output_sym.add_symbol(sym))

        self.f.set_output_symbols(self.output_sym)

        # Define states for the possessive classes p1s, p2s, p3s, p1p, p2p, p3p as final states
        poss_states = []
        for _ in range(6):
            state = self.f.add_state()
            self.f.set_final(state)
            poss_states.append(state) 

        for wclass in wc.WordClass:
            if not wclass in self.wc_dict:
                continue
            suffixes = wc.generate_possessive_suffixes(wclass)
            wc_start_state = self.wc_dict[wclass]
            for i, suffix in enumerate(suffixes):
                # Append the suffix
                suffix_end_state = self.append_linear_fst(self.f, wc_start_state, suffix) 
                # Output the possessive class with epsilon transition
                self.f.add_arc(suffix_end_state, fst.Arc(eps_sym, poss_classes_sym[i], None, poss_states[i]))
        
    def morphological_analyzer(self, surface_form: str) -> set:
        surface_form_lower = surface_form.replace('I', 'ı').lower()
        unique_forms = set()  # Create a set to store unique forms     
        self._transduce(self.f, 0, surface_form_lower, "", unique_forms, root_observed=False)
        return unique_forms
    
    def _is_final_state(self, f: fst.Fst, state: int) -> bool:
        return f.final(state) != fst.Weight.Zero(f.weight_type())

    def _transduce(self, f: fst.Fst, current_state: int, remaining: str, output: str, all_forms: set, root_observed: bool) -> None:
        if not remaining and self._is_final_state(f, current_state):
            # Input is finished, and we are in a final state
            all_forms.add(output)
        
        for arch in f.arcs(current_state):
            next_state = arch.nextstate
            
            isym = f.input_symbols().find(arch.ilabel)
            osym = f.output_symbols().find(arch.olabel)
           
            # Continue with the next state, without consuming an input
            if isym == EPSILON:
                if osym == NOUN_TAG:
                    root_observed = True
                new_output = output + osym if osym != EPSILON else output 
                self._transduce(f, next_state, remaining, new_output, all_forms, root_observed)
            elif remaining: 
                accept = False
                if isym.startswith('^'):
                    input = isym[1]
                    if len(remaining) > 1:
                        if not wc.is_vowel(remaining[1]):
                            accept = True
                        elif wc.check_consonant_softening(input, remaining[0]):  
                            accept = True
                    elif input == remaining[0]:
                        accept = True
                elif isym == remaining[0]: 
                    accept = True

                if accept: # Continue with the path
                    # After observing the root, do not include any surface level input into the output
                    updated_output = output if root_observed else output + osym
                    self._transduce(f, next_state, remaining[1:], updated_output, all_forms, root_observed)

    def append_linear_fst(self, f: fst.Fst, start_state: int, word: str, softening=False) -> int:
        current_state = start_state
        for i, c in enumerate(word):
            # Apply 
            if i == len(word) - 1 and softening and wc.is_hard_consonant(c):
                inc = '^' + c
            else:
                inc = c
            next_state = f.add_state()
            in_sym = f.input_symbols().find(inc)
            out_sym = f.output_symbols().find(c)
            f.add_arc(current_state, fst.Arc(in_sym, out_sym, None, next_state))
            current_state = next_state
        # Returns the last state
        return current_state