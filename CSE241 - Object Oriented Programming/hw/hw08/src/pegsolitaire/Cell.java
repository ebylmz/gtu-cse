/**
 * @file    Cell.java
 * @author  Emirkan Burak Yılmaz 
 * @brief   Cells in Game Board
 * @version 0.1
 * @date    2022-01-28
 * 
 * @copyright Copyright (c) 2021
 */

package pegsolitaire;

import java.awt.event.ActionListener;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JPanel;

// Abstract data type Cell 
public class Cell extends JButton {
    private CellType __value;

    final private ImageIcon PEG_ICON = new ImageIcon("system/img/pegCell.png");
    final private ImageIcon EMPTY_ICON = new ImageIcon("system/img/emptyCell.png");
    final private ImageIcon SELECTED_ICON = new ImageIcon("system/img/selectedCell.png");
    final private ImageIcon POSSIBLE_ICON = new ImageIcon("system/img/possibleCell.png");

    /** CellType  */
    public static enum CellType {
        PEG('P'), 
        EMPTY('.'),
        WALL(' ');

        private final char c;   // char representation

        private CellType(char c) {this.c = c;}

        public char toChar () {return c;}

        @Override
        public String toString() {return String.format("%c", c);}
    }

    /**
     * @param t cell type
     * @param panel panel to add this cell
     * @param listener 
     */
    public Cell(CellType t, JPanel panel, ActionListener listener) {
        super();

        // set cell color as predefined colors (black bg, red fg)
        ColorScheme.setColor(this, ColorScheme.BLACK, ColorScheme.RED);
        
        // set an empty border to button
        setBorder(BorderFactory.createEmptyBorder());
        setCellType(t);

        panel.add(this);
        addActionListener(listener);
    }

    /**
     * @return type of the cell
     */
    public CellType getCellType() {return __value;}

    /**
     * Sets the cell as given CellType
     * @param v CellType value
     */
    public void setCellType(CellType v) {
        __value = v;
        setActionCommand(__value.toString());

        if (v == CellType.PEG)
            setIcon(PEG_ICON);
        else if (v == CellType.EMPTY)
            setIcon(EMPTY_ICON);
        else 
            setEnabled(false);  // set wall button as non-clickable
    }

    /**
     * make cell selected by applying hover effect
     */
    public void setSelected(boolean selected) {
        if (selected)
            setIcon(SELECTED_ICON);
        else // wall cells are unclickable, so we don't deal with them
            setIcon(__value == CellType.PEG ? PEG_ICON : EMPTY_ICON);
    }

    /**
     * make cell possibke by applying hover effect
     */
    public void setPossible(boolean isPossible) {
        if (isPossible)
            setIcon(POSSIBLE_ICON);
        else // wall cells are unclickable, so we don't deal with them
            setIcon(__value == CellType.PEG ? PEG_ICON : EMPTY_ICON);
    }

    @Override
    public String toString() {
        return getActionCommand();
    }
}