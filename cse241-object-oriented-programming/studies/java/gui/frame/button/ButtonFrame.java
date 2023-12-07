package frame.button;

import java.awt.FlowLayout;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JOptionPane;

public class ButtonFrame extends JFrame {
    private JButton plainJButton;   // button with just text
    private JButton fancyJButton;   // button with icons

    public ButtonFrame () {
        super("Testing Buttons");
        setLayout(new FlowLayout());
        
        plainJButton = new JButton("Plain Button");
        add(plainJButton);

        Icon img1 = new ImageIcon(getClass().getResource("cat.png"));
        Icon img2 = new ImageIcon(getClass().getResource("santa.png"));
        
        fancyJButton = new JButton("Fancy Button", img1);   // set image
        // set rollover image (appears when mouse is positioned over a button)
        fancyJButton.setRolloverIcon(img2); 
        add(fancyJButton);

        // create new ButtonHandler for button event handling
        ButtonHandler handler = new ButtonHandler();
        fancyJButton.addActionListener(handler);
        plainJButton.addActionListener(handler);
    }

    private class ButtonHandler implements ActionListener {
        public void actionPerformed(ActionEvent event) {
            // this refers to the current inner-class object which is  
            // ButtonHandler. So to refer the outer class this, use ButtonFrame.this
            JOptionPane.showMessageDialog(ButtonFrame.this, String.format("You pressed: %s", event.getActionCommand()));
        }
    }
}
