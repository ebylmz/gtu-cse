package src;

import src.AnsiEscape;

public class Q5 {

    /**
     * Displays all the combinations of colored blocks whose lenght varies from 3 to given length 
     * @param lenght length of the blocks array
     * @return number of combinations (empty array is not included)
     */
    public static int possibleBlocks(int lenght) {
        return possibleBlocks(lenght, 3);
    }

    /**
     * Displays all the combinations of colored blocks whose lenght varies from 3 to given length 
     * @param lenght length of the blocks array
     * @param blockMinLength minimum colored block length
     * @return number of combinations (empty array is not included)
     */
     public static int possibleBlocks(int length, int blockMinLength) {
        boolean[] blocks = new boolean[length];
        for (int i = 0; i < length; ++i)
            blocks[i] = false;
        return possibleBlocks(blocks, blockMinLength, 0, 0);
    }   

    /**
     * @param lenght length of the blocks array
     * @param blockMinLength minimum colored block length
     * @return number of combinations (empty array is not included)
     */
    /**
     * Displays all the combinations of colored blocks  
     * @param blocks boolean array of blocks to to distinguish colored and empty blocks
     * @param blockMinLength minimum lenght of colored blocks
     * @param index current index for starting new combination
     * @param combinations number of combination that find so far
     * @return final number of combinations (empty array is not included)
     */
    private static int possibleBlocks(boolean[] blocks, int blockMinLength, int index, int combinations) {
        // make sure there are enough blocks left
        if (index + blockMinLength <= blocks.length) {
            // fill an minimum allowed size block for beginning
            for (int i = 0; i < blockMinLength; ++i)
                blocks[index + i] = true;    // indicates filled place
            // print the current configuration
            // System.out.println(blocks);
            display(blocks);
            
            // print all the combination consist of current configuration
            combinations = 1 + possibleBlocks(blocks, blockMinLength, index + blockMinLength + 1, combinations);

            // try new configuration by bigger blocks
            int blockSize = blockMinLength;
            while (index + blockSize + 1 <= blocks.length) {
                ++blockSize;
                blocks[index + blockSize - 1] = true;
                display(blocks);
                combinations = 1 + possibleBlocks(blocks, blockMinLength, index + blockSize + 1, combinations);
            } 
            // remove all the filled blocks for new configurations 
            for (int i = 0; i < blockSize; ++i)
                blocks[index + i] = false;    // indicates empty place

            // display all the configuration which are started at index + 1 position (before is empty)
            combinations = possibleBlocks(blocks, blockMinLength, index + 1, combinations);

            // at the end remove all the filled blocks 
            for (int i = 0; i < blockMinLength; ++i)
                blocks[index + i] = false;    // indicates empty place
        }
        return combinations;
    }

    /**
     * Displays the current view of blocks array
     * @param blocks array of boolean values which will display as
     * true as red block and false as white block
     */
    private static void display(boolean[] blocks) {
        for (int i = 0; i < blocks.length; ++i) {
            AnsiEscape.setBGColor(blocks[i] ? AnsiEscape.Color.RED : AnsiEscape.Color.WHITE);
            System.out.print(" ");
            AnsiEscape.setBGColor(AnsiEscape.Color.DEFAULT);
            System.out.print(" ");
        }
        System.out.println("\n");
    }

    /** Tests possibleBlocks method with different test cases */
    public static void test() {
        System.out.println("\n================ Q5 TEST ================");

        // T1.	 L > N
        System.out.println("\nT1:");
        int combinations = possibleBlocks(12);
        System.out.printf("number of combination: %d\n", combinations);

        // T2.	 L < N
        System.out.println("\nT2:");
        combinations = possibleBlocks(3, 6);
        System.out.printf("number of combination: %d\n", combinations);
        
        // T3.	 L = N
        System.out.println("\nT3:");
        combinations = possibleBlocks(11, 11);
        System.out.printf("number of combination: %d\n", combinations);

        System.out.println("================ END ================");
    }
}