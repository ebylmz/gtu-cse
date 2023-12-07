/**
 * @file    ColorScheme.java
 * @author  Emirkan Burak Yılmaz 
 * @brief   Peg Solitaire Color Thema Implementation
 * @version 0.1
 * @date    2022-01-28
 * 
 * @copyright Copyright (c) 2021
 */

package pegsolitaire;

import java.awt.Component;
import java.awt.Color;

/** Color schome/thema of PegSolitaire */
public enum ColorScheme {
    BLACK (new Color(28, 34, 38)), 
    BLUE (new Color(0xCDE0FF)),
    RED (new Color(0xE10550)),
    GRAY (new Color(143, 155, 166)),
    GREEN (new Color(0x129468)),
    WHITE (new Color(0xFFFFFF));

    private final Color color;

    // private constructor
    private ColorScheme (Color c) {color = c;}

    /**
     * Returns selected color value as java.awt.Color
     * @return selected color (java.awt.Color)
     */
    public Color getColor () {return color;}

    /**
     * Sets the color properties of given component
     * @param comp component to change its color
     * @param bg background color
     * @param fg foreground color
     */
    public static void setColor (Component comp, ColorScheme bg, ColorScheme fg) {
        comp.setBackground(bg.getColor());
        comp.setForeground(fg.getColor());
    }

    /**
     * Sets the color properties of given component
     * @param comp component to change its color
     * @param bg background color
     */
    public static void setColor (Component comp, ColorScheme bg) {
        comp.setBackground(bg.getColor());
    }
}
