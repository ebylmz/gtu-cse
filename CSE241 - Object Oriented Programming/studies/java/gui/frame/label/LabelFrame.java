package frame.label;

import java.awt.FlowLayout; // specifies how components are arranged
import javax.swing.JFrame; // provides basic window features
import javax.swing.JLabel; // displays text and images
import javax.swing.SwingConstants; // common constants used with Swing
import javax.swing.Icon; // interface used to manipulate images
import javax.swing.ImageIcon; // loads images

public class LabelFrame extends JFrame {
    private JLabel label1;     
    private JLabel label2;
    private JLabel label3;

    /** LabelFrame constructor */
    public LabelFrame () {
        super("Testing JLabel");
        setLayout(new FlowLayout()); 
        // A flow layout arranges components in a left-to-right flow, 
        // much like lines of text in a paragraph. Flow layouts are
        // typically used to arrange buttons in a panel. It will 
        // arrange buttons left to right until no more buttons
        // fit on the same line. Each line is centered.

        // JLabel constructor with a string argument
        label1 = new JLabel("Label with text");
        label1.setToolTipText("This is label 1");
        add(label1); // add label1 to JFrame

        // JLabel constructor with string, Icon and alignment arguments
        Icon duck = new ImageIcon(getClass().getResource("duck.gif"));
        label2 = new JLabel("Label with text and icon", duck, SwingConstants.LEFT);
        label2.setToolTipText("This is label 2");
        add(label2); // add label2 to JFrame

        label3 = new JLabel();
        label3.setText("Label with icon and text at bottom center"); 
        label3.setIcon(duck);
        label3.setHorizontalTextPosition(SwingConstants.CENTER);
        label3.setVerticalTextPosition(SwingConstants.BOTTOM);
        label3.setToolTipText("This is label 3");
        add(label3); // add label3 to JFrame
    }

    public LabelFrame (String img) {
        super("Testing JLabel");
        setLayout(new FlowLayout());
        Icon icon = new ImageIcon(getClass().getResource(img));
        label1 = new JLabel("Label with text and icon");
        label1.setIcon(icon);
        label1.setHorizontalTextPosition(SwingConstants.CENTER);
        label1.setVerticalTextPosition(SwingConstants.BOTTOM);
        label1.setToolTipText("This is label 1");
        add(label1);
    }
}