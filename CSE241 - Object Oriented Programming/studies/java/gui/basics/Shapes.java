package basics;

import java.awt.Graphics;
import javax.swing.JPanel;

public class Shapes extends JPanel {
    private int _choice;

    public Shapes (int choice) {_choice = choice;}

    public void paintComponent (Graphics g) {
        super.paintComponent(g);
        
        switch (_choice) {
            case 1:
                for (int i = 0; i <  10; ++i) {
                    int x = 10 + i * 10;    // x axis
                    int w = 50 + i * 10;    // width
                    g.drawRect(x, x, w, w);
                }
                break;
            case 2:
                for (int i = 0; i <  10; ++i) {
                    int arg1 = 10 + i * 10;    
                    int arg2 = 50 + i * 10;   
                    g.drawOval(arg1, arg1, arg2, arg2);
                }
                break;
        }    
    }
}