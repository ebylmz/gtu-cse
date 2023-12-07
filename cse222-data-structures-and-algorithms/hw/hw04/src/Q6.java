package src;

import src.AnsiEscape;

public class Q6 {
    /**
     * Displays all the possible bending snakes in NxN matrix
     * @param n size of the matrix
     */
    public static void bendingSnakes(int n) {
        int[][] matrix = new int[n][n];
        // to print all the combinations iterate over all the indexes (nested for loop)
        for (int i = 0; i < n; ++i)
            bendingSnakes(matrix, n, n, n, 0, i);
        // bendingSnakes(matrix, n, n, n, 0, 0);
    }

    /**
     * 
     * @param matrix NxN matrix
     * @param snakeLen length of the snake (constant)
     * @param n number of snake that needs to place to the matrix
     * @param remainLen remaining length to complete a snake
     * @param r row in matrix
     * @param c column in matrix
     */
    private static void bendingSnakes(int[][] matrix, int snakeLen, int n, int remainLen, int r, int c) {
        // make sure the target cell is not exceeding and it's empty 
        if (0 <= r && r < matrix.length && 0 <= c && c < matrix[r].length && matrix[r][c] == 0) {
            // add current cell to snake (to indicate cell has owner)
            matrix[r][c] = snakeLen * n - (snakeLen - remainLen);
            // if the snake is completed with this last addition  
            if (remainLen == 1) {
                // if this was the last snake that completes the combination, print the combination 
                if (n == 1) // BASE CASE
                    display(matrix);
                else { // create new snake which will become (n - 1) th snake
                    // next snake will start at the adjacent of current cell (4 cardinal direction and 4 diagonal direction) 
                    bendingSnakes(matrix, snakeLen, n - 1, snakeLen, r - 1, c - 1);    // up-left 
                    bendingSnakes(matrix, snakeLen, n - 1, snakeLen, r - 1, c);        // up 
                    bendingSnakes(matrix, snakeLen, n - 1, snakeLen, r - 1, c + 1);    // up-right
                    bendingSnakes(matrix, snakeLen, n - 1, snakeLen, r, c - 1);        // left
                    bendingSnakes(matrix, snakeLen, n - 1, snakeLen, r, c + 1);        // right 
                    bendingSnakes(matrix, snakeLen, n - 1, snakeLen, r + 1, c - 1);    // down-left
                    bendingSnakes(matrix, snakeLen, n - 1, snakeLen, r + 1, c);        // down
                    bendingSnakes(matrix, snakeLen, n - 1, snakeLen, r + 1, c + 1);    // down-right 
                }
            }
            else { // continue to placing the current snake
                // use 4 cardinal direction to place next part of the snake (bending happens here)
                bendingSnakes(matrix, snakeLen, n, remainLen - 1, r - 1, c); // up 
                bendingSnakes(matrix, snakeLen, n, remainLen - 1, r, c - 1); // left
                bendingSnakes(matrix, snakeLen, n, remainLen - 1, r, c + 1); // right
                bendingSnakes(matrix, snakeLen, n, remainLen - 1, r + 1, c); // down
            }
            // after using this cell in combination, left it empty to use another combinations 
            matrix[r][c] = 0;
        }
    }

    /**
     * Displays current state of the matrix
     * @param matrix 
     */
    private static void display(int[][] matrix) {
        int n = matrix.length;
        for (int i = 0; i < matrix.length; ++i) {
            for (int j = 0; j < matrix[i].length; ++j) {
                int cellVal = matrix[i][j];
                if (0 < cellVal && cellVal <= n) 
                    AnsiEscape.setFGColor(AnsiEscape.Color.BLUE); 
                else if (n < cellVal && cellVal <= 2 * n)
                    AnsiEscape.setFGColor(AnsiEscape.Color.CYAN);
                else if (2 * n < cellVal && cellVal <= 3 * n)
                    AnsiEscape.setFGColor(AnsiEscape.Color.MAGENTA);
                else if (3 * n < cellVal && cellVal <= 4 * n)
                    AnsiEscape.setFGColor(AnsiEscape.Color.RED);
                else if (4 * n < cellVal && cellVal <= 5 * n)
                    AnsiEscape.setFGColor(AnsiEscape.Color.GREEN);
                else if (5 * n < cellVal && cellVal <= 6 * n)
                    AnsiEscape.setFGColor(AnsiEscape.Color.BLACK);
                else if (6 * n < cellVal && cellVal <= 7 * n)
                    AnsiEscape.setFGColor(AnsiEscape.Color.YELLOW);
                else if (7 * n < cellVal && cellVal <= 8 * n)
                    AnsiEscape.setFGColor(AnsiEscape.Color.WHITE);
                else if (8 * n < cellVal && cellVal <= 9 * n)
                    AnsiEscape.setFGColor(AnsiEscape.Color.WHITE);
                else
                    AnsiEscape.setFGColor(AnsiEscape.Color.DEFAULT);
                System.out.printf("%02d ", cellVal);
            }
            System.out.println();
        }
        AnsiEscape.setFGColor(AnsiEscape.Color.DEFAULT); 
        System.out.println();
    }

    /** Tests bendingSnakes method */
    public static void test() {
        System.out.println("\n================ Q6 TEST ================");

        long startTime = System.currentTimeMillis();
        bendingSnakes(5);
        long endTime = System.currentTimeMillis();
        double executionTime = endTime - startTime; 
        System.out.printf("Execution time: %.2fms\n", executionTime);
        System.out.printf("Execution time: %.2fs\n", executionTime / 1000f);
        System.out.printf("Execution time: %.2fs\n", executionTime / 60000f);
        
        System.out.println("================ END ================");
    }
}
