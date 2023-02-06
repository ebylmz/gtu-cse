package src;

public class AnsiEscape {
    public static enum Color {
        BLACK, RED, GREEN, YELLOW, BLUE, MAGENTA, CYAN, WHITE, DEFAULT
    }

    /** Deletes all the data at the screen */
    public static void clearScreen() {
        System.out.printf("\u001B[2J");
    }

    /**
     * Sets the cursor at given (row, col) location
     * (O, 0) is the top left, (row, col) is the right bottom
     * @param row
     * @param col
     */
    public static void setCursor(int row, int col) {
        System.out.printf("\033[%d;%dH", row, col);
    }

    /**
     * Sets font color of the screen 
     * @param c font color
     */
    public static void setFGColor(AnsiEscape.Color c) {
        String colorCode = null;
        switch (c) {
            case BLACK:
                colorCode = "\u001B[30m";
                break;
            case RED:
                colorCode = "\u001B[31m";
                break;
            case GREEN:
                colorCode = "\u001B[32m";
                break;
            case YELLOW:
                colorCode = "\u001B[33m";
                break;
            case BLUE:
                colorCode = "\u001B[34m";
                break;
            case MAGENTA:
                colorCode = "\u001B[35m";
                break;
            case CYAN:
                colorCode = "\u001B[36m";
                break;
            case WHITE:
                colorCode = "\u001B[37m";
                break;
            case DEFAULT:
                colorCode = "\u001B[0m";
                break;
        }
        System.out.printf("%s", colorCode);
    }

    /**
     * Sets BG color of the screen
     * @param c background color
     */
    public static void setBGColor(AnsiEscape.Color c) {
        String colorCode = null;
        switch (c) {
            case BLACK:
	            colorCode ="\u001B[40m";
                break;
            case RED:
	            colorCode = "\u001B[41m";
                break;
            case GREEN:
	            colorCode ="\u001B[42m";
                break;
            case YELLOW:
	            colorCode ="\u001B[43m";
                break;
            case BLUE:
	            colorCode = "\u001B[44m";
                break;
            case MAGENTA:
	            colorCode ="\u001B[45m";
                break;
            case CYAN:
	            colorCode = "\u001B[46m";
                break;
            case WHITE:
	            colorCode ="\u001B[47m";
                break;
            case DEFAULT:
                colorCode = "\u001B[0m";
                break;
        }
        System.out.printf("%s", colorCode);
    }
}