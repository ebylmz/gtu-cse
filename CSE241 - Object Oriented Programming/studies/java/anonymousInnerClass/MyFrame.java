import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

  /*    Anonymous Class
        ================================      
        An inner class without a name only a single object is created from one.
        The object may have “extras” or "changes" and no need to create a separate innerclass
        when it only need it once. Helps us to avoid cluttering code with a class name
 
        Syntax is similar to a constructor,except that there is also a class definition
        within a block of code. GREAT FOR LISTENERS
  */

public class MyFrame extends JFrame {
    private JButton btn1;
    private JButton btn2;
    private JButton btn3;

    MyFrame () {

        // add buttons
        btn1 = new JButton("#1");
        btn1.addActionListener(new ActionListener () { 
            
            public void actionPerformed(ActionEvent e) {
                System.out.println("You pressed button #1");
            }
        });

        btn2 = new JButton("#2");
        btn2.addActionListener(new ActionListener () {
            
            public void actionPerformed(ActionEvent e) {
                System.out.println("You pressed button #2");
            }
        });

        btn3 = new JButton("#3");
        btn3.addActionListener(new ActionListener() {
            
            public void actionPerformed(ActionEvent e) {
                System.out.println("You pressed button #3");
            }
        });
        
        add(btn1);
        add(btn2);
        add(btn3);

        setDefaultCloseOperation(EXIT_ON_CLOSE);
        setLayout(new FlowLayout());
        setSize(500, 500);
        setVisible(true);
    }
}
