package frame.checkBox;

import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ItemListener;
import java.awt.event.ItemEvent;
import javax.swing.JFrame;
import javax.swing.JTextField;
import javax.swing.JCheckBox;

public class CheckBoxFrame extends JFrame {
    private JTextField textField;   // displays text in changing fonts
    private JCheckBox boldJCheckBox; // to select/deselect bold
    private JCheckBox italicCheckBox; // to select/deselect italic

    public CheckBoxFrame () {
        super("JCheckBox Test");
        setLayout(new FlowLayout());

        // set up JTextField and set its font
        textField = new JTextField("Watch the font style change", 20);
        textField.setFont(new Font("Serif", Font.PLAIN, 14));
        add(textField);

        boldJCheckBox = new JCheckBox("Bold");
        add(boldJCheckBox);
        italicCheckBox = new JCheckBox("Italic");
        add(italicCheckBox);

        // register listeners for JCheckBoxes
        CheckBoxHandler handler = new CheckBoxHandler();
        boldJCheckBox.addItemListener(handler);
        italicCheckBox.addItemListener(handler);
    }

    private class CheckBoxHandler implements ItemListener {
        private int valBold = Font.PLAIN;
        private int valItalic = Font.PLAIN;

        @Override
        public void itemStateChanged(ItemEvent e) {
            // process bold checkbox events
            if (e.getSource() == boldJCheckBox)
                valBold = boldJCheckBox.isSelected() ? Font.BOLD : Font.PLAIN;
            
                // process italic checkbox events
            if (e.getSource() == italicCheckBox)
                valItalic = italicCheckBox.isSelected() ? Font.ITALIC : Font.PLAIN;
            
            // set text field font (operator+ is doing something like bitwise OR...)
            textField.setFont(new Font("Serif", valBold + valItalic, 14));
        }
    }
}
