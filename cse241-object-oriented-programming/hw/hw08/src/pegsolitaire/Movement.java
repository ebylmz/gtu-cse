/**
 * @file    Movement.java
 * @author  Emirkan Burak Yılmaz 
 * @brief   Game Movement
 * @version 0.1
 * @date    2022-01-28
 * 
 * @copyright Copyright (c) 2021
 */

package pegsolitaire;

import java.security.InvalidParameterException;
import java.util.Random;
import java.util.Vector;

import pegsolitaire.Cell.CellType;

public class Movement implements Cloneable {
    private Cell[][] __board; // game board for checking validty of movement
    private Cell __startCell; // start position of movement
    private Cell __jumpCell; // jump position of movement (between start and end)
    private Cell __endCell; // end position of movement

    /** Movement Directions */
    public static enum Direction {UP, DOWN, LEFT, RIGHT}

    /**
     * Construct a Movement with full required data
     * @param board game board
     * @param start start position of the movement 
     * @param end end position of the movement
     */
    public Movement(Cell[][] board, Cell start, Cell end) {
        __board = board;
        try {
            setStart(start);
            setEnd(end);
        } catch (InvalidParameterException e) {
            __startCell = __endCell = __jumpCell = null;
            System.err.println("Invalid parameter for Movement Constructor");
        }
    }

    public Movement(Cell[][] board, Cell start) {this(board, start, null);}

    public Movement(Cell[][] board) {this(board, null, null);}
    
    public Movement() {this(null, null, null);}

    /*** Start position of movement */
    public Cell start() {return __startCell;}

    /*** End position of movement */
    public Cell end() {return __endCell;}

    /*** Jump position of movement (between start and end) */
    public Cell jump() {return __jumpCell;}

    /**
     * Returns all the possible movements that can start position made
     * @return all the possible movements
     */
    public Vector<Cell> nextPossibleMov() {
        Vector<Cell> v = new Vector<Cell>();
        if (start() != null)
            for (Direction d : Direction.values())
                if (setMovement(start(), d))
                    v.add(end());
        return v.size() > 0 ? v : null;
    }

    /**
     * Set start position of the movement
     * @param start
     * @throws InvalidParameterException if given button does not exist in the game board
     */
    public void setStart(Cell start) throws InvalidParameterException {
        // be sure given Cell is in the current game board
        if (start != null && findLocation(start) == null)
            throw new InvalidParameterException("given cell not exist in game board");
        __startCell = start;
    }

    /**
     * Sets end position of movement
     * @param end
     * @throws InvalidParameterException if given button does not exist in the game board
     */
    public void setEnd(Cell end) throws InvalidParameterException {
        // be sure given Cell is in the current game board
        if (end != null && findLocation(end) == null)
            throw new InvalidParameterException("given cell not exist in game board");
        __endCell = end;
    }

    /**
     * Sets jump position by automaticly
     * @throws InvalidParameterException no enough information to find jump button (no board or no start/end position)
     */
    public void setJump() throws InvalidParameterException {
        if (__board == null || start() == null || end() == null)
            throw new NullPointerException("no enough information to find jump button");

        int[] startIndexes = findLocation(start());
        int[] endIndexes = findLocation(end());

        if (startIndexes != null && endIndexes != null) {
            int row = -1; // jump button row
            int col = -1; // jump button coloumn

            // starBtn and endBtn are at same row
            if (startIndexes[0] == endIndexes[0]) {
                row = endIndexes[0];

                int diff = endIndexes[1] - startIndexes[1];
                if (diff == 2)
                    col = endIndexes[1] - 1;
                else if (diff == -2)
                    col = endIndexes[1] + 1;
            }
            // starBtn and endBtn are at same coloumn
            else if (startIndexes[1] == endIndexes[1]) {
                col = endIndexes[1];

                int diff = endIndexes[0] - startIndexes[0];
                if (diff == 2)
                    row = endIndexes[0] - 1;
                else if (diff == -2)
                    row = endIndexes[0] + 1;
            }

            // be sure jump row and col are in range, otherwise set it as null
            __jumpCell = (0 <= row && row < __board.length && 0 <= col && col < __board[row].length)
                    ? __board[row][col]
                    : null;
        }
    }

    /**
     * sets game board
     * @param board
     */
    public void setBoard(Cell[][] board) {
        __board = board;
        // be sure given buttons are still valid
        if (findLocation(__startCell) == null)
            __startCell = null;
        if (findLocation(__endCell) == null)
            __startCell = null;
        if (findLocation(__jumpCell) == null)
            __jumpCell = null;
    }

    /**
     * Sets movement from given starting position to given direction
     * @param start start position of the movement
     * @param d movement direction
     * @return true if there is a valid movement at given direction for start position
     * @throws InvalidParameterException
     */
    public boolean setMovement(Cell start, Direction d) throws InvalidParameterException {
        try {
            setStart(start);    // can throw InvalidParameterException
            boolean r = false;
            if (start.getCellType() == CellType.PEG) {
               int[] indexes = findLocation(start); 
               if (indexes != null) {
                    switch (d) {
                        case UP: 
                            r = setUpMovement(indexes[0], indexes[1]); break;
                        case DOWN: 
                            r = setDownMovement(indexes[0], indexes[1]); break;
                        case LEFT: 
                            r = setLeftMovement(indexes[0], indexes[1]); break;
                        case RIGHT: 
                            r = setRightMovement(indexes[0], indexes[1]); break;
                    }
                }
            }
            return r;
        }
        catch (InvalidParameterException e) {
            System.err.println("start Cell is invalid parameter for setting");
            throw e;
        }            
    }

    /**
     * Sets random valid movement
     * @return false if there is no movement left means if game is over
     */
    public boolean setRandomMovement() {
        Random rand = new Random();
        // choose an random starting position
        int row = rand.nextInt(__board.length);
        int col = rand.nextInt(__board[row].length);

        // start with selected position (row, col) and try each cell to make movement
        for (int i = 0; i < __board.length; ++i) {
            for (int j = 0; j < __board[i].length; ++j) {
                // check movement
                setStart(__board[row][col]);
                if (    
                    setMovement(__board[row][col], Movement.Direction.RIGHT) ||
                    setMovement(__board[row][col], Movement.Direction.LEFT) ||
                    setMovement(__board[row][col], Movement.Direction.UP) ||
                    setMovement(__board[row][col], Movement.Direction.DOWN)
                ) 
                    return true;
                // iterate coloumn
                col = (col == __board[row].length - 1) ? 0 : col + 1;
            }
            // iterate row
            row = (row == __board.length - 1) ? 0 : row + 1;
        }
        return false;
    }

    private boolean setUpMovement(int row, int col) {
        if (0 <= row - 2) {
            Cell jump = __board[row - 1][col];
            Cell end = __board[row - 2][col];
            if (isProperJumpEndCell(jump, end)) {
                __jumpCell = jump;
                __endCell = end;
                return true;
            } 
        }
        return false;
    }

    private boolean setDownMovement(int row, int col) {
        if (row + 2 < __board.length ) {
            Cell jump =  __board[row + 1][col];
            Cell end = __board[row + 2][col];
            if (isProperJumpEndCell(jump, end)) {
                __jumpCell = jump;
                __endCell = end;
                return true;
            } 
        }
        return false;
    }

    private boolean setLeftMovement(int row, int col) {
        if (0 <= col - 2) {
            Cell jump = __board[row][col - 1];
            Cell end = __board[row][col - 2];
            if (isProperJumpEndCell(jump, end)) {
                __jumpCell = jump;
                __endCell = end;
                return true;
            }
        }
        return false;
    }

    private boolean setRightMovement(int row, int col) {
        if (col + 2 < __board[col].length) {
            Cell jump = __board[row][col + 1];
            Cell end = __board[row][col + 2];
            if (isProperJumpEndCell(jump, end)) {
                __jumpCell = jump;
                __endCell = end;
                return true;
            }
        }
        return false;
    }

    private boolean isProperJumpEndCell(Cell jump, Cell end) {
        return jump.getCellType() == CellType.PEG && end.getCellType() == CellType.EMPTY;
    }

    /**
     * Checks if current Movement can made valid movement
     * @return
     */
    public boolean isValidMovement() {
        setJump();
        // jump becomes null, if start and end buttons are not in proper position
        return jump() != null &&
                __startCell.getCellType() == CellType.PEG &&
                __jumpCell.getCellType() == CellType.PEG &&
                __endCell.getCellType() == CellType.EMPTY;
    }

    /**
     * Finds the location of given button from the 
     * @param cell
     * @return if button exist in the game board [0]: row, [1]: col, else null
     * @throws NullPointerException
     */
    public int[] findLocation(Cell cell) throws NullPointerException {
        int indexes[] = null;
        if (__board != null && cell != null) {
            for (int i = 0; i < __board.length && indexes == null; ++i)
                for (int j = 0; j < __board[i].length && indexes == null; ++j)
                    if (__board[i][j] == cell) {
                        indexes = new int[2];
                        indexes[0] = i; // assign row
                        indexes[1] = j; // assign col
                    }
        }
        return indexes;
    }

    public Movement clone() {
        try {
            Movement r = (Movement) super.clone();
            r.__board = __board;
            r.__startCell = __startCell;
            r.__endCell = __endCell;
            r.__jumpCell = __jumpCell;
            return r;
        } catch (CloneNotSupportedException e) {
            // this will never be happen
            return null;
        }
    }
} 