package maze;

public class Maze {
    public static void main(String args[]) {
        char map[][] = {
            {Maze.EMPTY, Maze.EMPTY, Maze.BARRIER, Maze.EMPTY, Maze.EMPTY, Maze.EMPTY},
            {Maze.EMPTY, Maze.EMPTY, Maze.BARRIER, Maze.EMPTY, Maze.EMPTY, Maze.EMPTY},
            {Maze.BARRIER, Maze.EMPTY, Maze.BARRIER, Maze.EMPTY, Maze.EMPTY, Maze.EMPTY},
            {Maze.EMPTY, Maze.EMPTY, Maze.BARRIER, Maze.BARRIER, Maze.EMPTY, Maze.BARRIER},
            {Maze.EMPTY, Maze.EMPTY, Maze.EMPTY, Maze.EMPTY, Maze.EMPTY, Maze.EMPTY},
            {Maze.EMPTY, Maze.EMPTY, Maze.EMPTY, Maze.EMPTY, Maze.BARRIER, Maze.EMPTY}
        };

        Maze maze = new Maze(map);
        boolean result = maze.findMazePath();
        System.out.printf("Path found: %b\n", result);
        System.out.println(maze);
    }

    public static final char EMPTY = '.';
    public static final char BARRIER = '|';
    public static final char PATH = '+';
    public static final char VISITED = 'o';
    
    private char[][] maze;
    private int row;   
    private int col;   

    public Maze(char[][] maze) {
        this.maze = maze;
        row = maze.length;
        col = maze[0].length;
    }

    public boolean findMazePath() {
        return findMazePath(0, 0);
    }

    private boolean findMazePath(int r, int c) {
        if (r < 0 || r >= row || c < 0 || c >= col)
            return false;
        else if (maze[r][c] == BARRIER || maze[r][c] == VISITED)
            return false;
        else if (r == row - 1 && c == col - 1) {
            maze[r][c] = PATH;
            return true;
        }
        else {
            // set this cell visited to not check again
            maze[r][c] = VISITED;
            // try each neighbourhood cell
            if (
                findMazePath(r - 1, c) || findMazePath(r, c + 1) ||
                findMazePath(r + 1, c) || findMazePath(r, c - 1)) {
                maze[r][c] = PATH;
                return true;    
            }
            return false;
        }
    }

    @Override
    public String toString() {
        StringBuffer buffer = new StringBuffer();
        for (int i = 0; i < row; ++i) {
            for (int j = 0; j < col; ++j)
                buffer.append(maze[i][j] + " ");
            buffer.append('\n');
        }
        return buffer.toString();
    }
}
