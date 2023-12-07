#include <iostream>
#include <string>
#include <map>

using std::map;

void frequency (std::string s) {
    map<char, int> mp;

    // find the frequency of all the letter in given string
    for (int i = 0; i < s.length(); ++i) {
        if (isalpha(s[i])) {
            char letter = s[i];
            // check if current letter exist in map
            auto it = mp.find(letter); 
            if (it == mp.end())
                mp.insert(std::pair<char, int>(letter, 1));
            else 
                ++mp[letter];
        }
    }

    // print the letters and it's frequencies
    for (map<char, int>::iterator it = mp.begin(); it != mp.end(); ++it)
        std::cout << it->first << " " << it->second << std::endl;
}

int main (void) {
    std::string s = "If you go chasing rabbits, you know you're going to fall";
    frequency(s);
}