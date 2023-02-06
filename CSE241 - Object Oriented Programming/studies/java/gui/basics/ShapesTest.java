package basics;

import javax.swing.JFrame;
import javax.swing.JOptionPane;

public class ShapesTest {   
    public static void main (String args[]) {
        String input;
        do {
            input = JOptionPane.showInputDialog(
                "Select your shape\n" +    
                "1. Rectangles\n" + 
                "2. Ovals\n");
                                                    
            int choice = Integer.parseInt(input);
            
            Shapes panel = new Shapes(choice);

            JFrame application = new JFrame();

            application.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            application.add(panel);
            application.setSize(300, 300);
            application.setVisible(true);
            
            //* not a perpect design but good for sake of learning
            input = JOptionPane.showInputDialog(
                "Choose\n" +    
                "1. Continue\n" + 
                "2. Exit\n");
            application.remove(panel);
            application.setVisible(false);

        } while (Integer.parseInt(input) != 2);
    }
}
