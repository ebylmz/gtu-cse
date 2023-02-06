package blob;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import java.io.*;
import java.util.*;

/** TwoDimGrid is a two dimensional array of buttons.
 *  Each button can be toggled between two colors by
 *  clicking it with the mouse, or its color can be
 *  changed/queried under program control.
 *  @author Koffman and Wolfgang
* */

public class TwoDimGrid
    extends JPanel
    implements GridColors {

  // Data Fields
  /** Prefered button size */
  private static final int PREFERED_BUTTON_SIZE = 60;

  /** Default number of rows */
  private static final int DEFAULT_COLS = 20;

  /** Default number of columns */
  private static final int DEFAULT_ROWS = 20;

  /** A two dimensional grid of buttons */
  private JButton[][] theGrid;

  /** Number of rows */
  private int nRows;

  /** Number of columns */
  private int nCols;

  // Constructors
  /** Construct a TwoDimGrid of the specified size and of the
   *  specified colors
   *  @param nRows - Number of rows
   *  @param nCols - Number of columns
   */
  public TwoDimGrid(int nRows, int nCols) {
    this.nRows = nRows;
    this.nCols = nCols;
    setPreferredSize(new Dimension(nCols * PREFERED_BUTTON_SIZE,
                                   nRows * PREFERED_BUTTON_SIZE));
    setLayout(new GridLayout(nRows, nCols));
    theGrid = new JButton[nCols][];
    for (int i = 0; i != nCols; ++i) {
      theGrid[i] = new JButton[nRows];
      for (int j = 0; j != nRows; ++j) {
        theGrid[i][j] = new JButton(i + ", " + j);
        theGrid[i][j].setBackground(BACKGROUND);
        theGrid[i][j].addActionListener
            (new ToggleColor(theGrid[i][j]));
      }
    }

    // Add the buttons to the button panel
    for (int j = 0; j != nRows; ++j) {
      for (int i = 0; i != nCols; ++i) {
        add(theGrid[i][j]);
      }
    }
  }

  // Accessors and Mutators
  /** Get the number of columns
   *  @return nCols */
  public int getNCols() {
    return nCols;
  }

  /** Get the number of rows
   *  @return nRows */
  public int getNRows() {
    return nRows;
  }

  /** Get the color at a given coordinate
   *  @param x - The column number
   *  @param y - The row number
   *  @return The color at the given coordinate */
  public Color getColor(int x, int y) {
    return theGrid[x][y].getBackground();
  }

  /** Change the color at a given coordinate
   * @param x - The column number
   * @param y - The row number
   * @param newColor - The color to set the button to */
  public void recolor(int x, int y, Color newColor) {
    theGrid[x][y].setBackground(newColor);
  }

  /** Set the color of each square in the grid that correspond
   *  to the elements of the given array with the value 1
   *  @param bitMap - An array of 0's and 1's the same size as the grid
   *  @param aColor - The color to be set
   *  @throws ArrayIndexOutOfBounds if the array size and the
   *  grid size differ  */
  public void recolor(char[][] bitMap, Color aColor) {
    for (int i = 0; i != bitMap.length; ++i) {
      for (int j = 0; j != bitMap[i].length; ++j) {
        if (bitMap[i][j] == '1') {
          theGrid[j][i].setBackground(aColor);
        }
      }
    }
  }

  public void recolor(Color tempColor, Color newColor) {
    for (int i = 0; i != getNCols(); ++i) {
      for (int j = 0; j != getNRows(); ++j) {
        if (theGrid[i][j].getBackground().equals(tempColor))
          theGrid[i][j].setBackground(newColor);
      }
    }
  }

  // Inner class
  /** ActionListener class to toggle color when clicked */
  private class ToggleColor
      implements ActionListener {
    // DataField
    /** The button to be responded to */
    private JButton me;

    // Constructor
    /** Construct ToggleColor object for a given button
     *  @param theButton - The button to be responded to */
    public ToggleColor(JButton theButton) {
      me = theButton;
    }

    // Methods
    /** Action in response to button push
     *  @param e - Event object is ignored */
    public void actionPerformed(ActionEvent e) {
      if (me.getBackground().equals(BACKGROUND)) {
        me.setBackground(NON_BACKGROUND);
      }
      else {
        me.setBackground(BACKGROUND);
      }
    }
  }
}
