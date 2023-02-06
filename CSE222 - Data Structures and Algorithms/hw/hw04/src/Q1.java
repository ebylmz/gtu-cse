package src;

public class Q1 {
     /**
     * Returns the index of the nth occurrence of the query string
     * @param str string  
     * @param pattern pattern which will search in str
     * @param occurance nth occurance
     * @return if nth occurance doesn't exist returns -1, else the index of nth occurance, 
     * @throws IllegalArgumentException if occurance is not positive integer
     */
    public static int findPattern(String str, String pattern, int occurance) {
        if (occurance < 1)
            throw new IllegalArgumentException("Occurance should be positive integer");
        if (str.length() < pattern.length())
            return -1;
        return findPattern(str, pattern, occurance, 0);
    }

    /**
     * Returns the index of the nth occurrence of the query string
     * @param str string that contains pattern 
     * @param pattern pattern which will search in str
     * @param occurance nth occurance
     * @param index starting index of the pattern for search
     * @return if nth occurance doesn't exist returns -1, else the index of nth occurance, 
     */
    private static int findPattern(String str, String pattern, int occurance, int index) {
        if (index == str.length())
            return -1;
        else {
            // pattern found
            if (isPattern(str, pattern, index, 0))
                // return the index of the result
                if (occurance == 1)
                    return index;
                else
                // continue with next iteration to find desired occurance
                // increase index by one not by pattern length, because it may 
                // cause lose of occurances (for exp. pattern = "abcabc", str="abcabcabc")
                    return findPattern(str, pattern, occurance - 1, index + 1);
            else
                // continue searching with next index
                return findPattern(str, pattern, occurance, index + 1);
        }
    }

    /**
     * Checks if the str has pattern which starts at given index
     * @param str string that contains pattern 
     * @param pattern pattern which will search in str
     * @param index starting index of the pattern for search
     * @return true if the pattern is found, otherwise is false 
     */
    private static boolean isPattern(String str, String pattern, int index, int i) {
        // all the indexes checked and pattern is find
        if (i == pattern.length())
            return true;
        else if (index == str.length() || str.charAt(index + i) != pattern.charAt(i))
            return false;
        else
            return isPattern(str, pattern, index, i + 1);
    }

    /** Tests findPattern method with different test cases */
    public static void test() {
        System.out.println("================ Q1 TEST ================");

        String str = "abc abc abc abc abc";

        // N: number of total occurrences
        // I: desired occurrence 

        // T1.	No pattern exists in the big string.
        System.out.println("\nT1:");
        String pattern = "antonis";
        int occurance = 7;
        int index = findPattern(str, pattern, occurance);
        System.out.printf("pattern index: %d\n", index);

        pattern = "abc";
        
        // T2.	I < 1
        System.out.println("\nT2:");
        try {
            occurance = -1;
            index = findPattern(str, pattern, occurance);
            System.out.printf("pattern index: %d\n", index);
        } catch (Exception e) {
            System.out.println(e);
        }

        // T3.	I > N
        System.out.println("\nT3:");
        occurance = 7;
        index = findPattern(str, pattern, occurance);
        System.out.printf("pattern index: %d\n", index);

        // T4.	I < N
        System.out.println("\nT4:");
        str = "123412341234-AAAAA-XYZ";
        pattern = "AA";
        occurance = 2;
        index = findPattern(str, pattern, occurance);
        System.out.printf("pattern index: %d\n", index);

        // T5.	I = N
        System.out.println("\nT5:");
        pattern = "12";
        occurance = 3;
        index = findPattern(str, pattern, occurance);
        System.out.printf("pattern index: %d\n", index);

        // T6.	Big string size is smaller than pattern size.
        System.out.println("\nT6:");
        index = findPattern(pattern, str, occurance);
        System.out.printf("pattern index: %d\n", index);
        System.out.println("\n================ END ================");
    }
}
