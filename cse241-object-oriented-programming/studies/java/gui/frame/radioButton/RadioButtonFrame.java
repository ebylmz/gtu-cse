package frame.radioButton;

import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ItemListener;
import java.util.EventListener;
import java.awt.event.ItemEvent;
import javax.swing.JFrame;
import javax.swing.JTextField;
import javax.swing.JRadioButton;
import javax.swing.ButtonGroup;

public class RadioButtonFrame extends JFrame {
    private JTextField textField;
    private Font plainFont ;
    private Font boldFont;
    private Font italicFont;
    private Font boldItalicFont;
    private JRadioButton plainRadioButton;
    private JRadioButton boldRadioButton;
    private JRadioButton italicRadioButton;
    private JRadioButton boldItalicRadioButton;
    private ButtonGroup radioGroup; //! buttongroup to hold radio buttons

    public RadioButtonFrame() {
        super("RadioButton Test");
        setLayout(new FlowLayout());

        textField = new JTextField("Watch the font style change", 25);
        add(textField);

        // create radio buttons
        plainRadioButton = new JRadioButton("Plain", true);
        boldRadioButton = new JRadioButton("Bold", false);
        italicRadioButton = new JRadioButton("Italic", false);
        boldItalicRadioButton = new JRadioButton("Bold/Italic", false);
        add(plainRadioButton);
        add(boldRadioButton);
        add(italicRadioButton);
        add(boldItalicRadioButton);
    
        // create logical relationship between JRadioButtons
        radioGroup = new ButtonGroup();
        radioGroup.add(plainRadioButton);
        radioGroup.add(boldRadioButton);
        radioGroup.add(italicRadioButton);
        radioGroup.add(boldItalicRadioButton);

        // create font object
        plainFont = new Font("Serif", Font.PLAIN, 14);
        boldFont = new Font("Serif", Font.BOLD, 14);
        italicFont = new Font("Serif", Font.ITALIC, 14);
        boldItalicFont = new Font("Serif", Font.BOLD + Font.ITALIC, 14);
        // set initial font as plain
        textField.setFont(plainFont);

        // register events for JRadioButtons
        plainRadioButton.addItemListener(new RadioButtonHandler(plainFont));
        boldRadioButton.addItemListener(new RadioButtonHandler(boldFont));
        italicRadioButton.addItemListener(new RadioButtonHandler(italicFont));
        boldItalicRadioButton.addItemListener(new RadioButtonHandler(boldItalicFont));
    }

    private class RadioButtonHandler implements ItemListener {
        private Font font; 

        public RadioButtonHandler (Font f) {
            font = f;
        }

        // handle radio button events
        @Override
        public void itemStateChanged(ItemEvent e) {
            textField.setFont(font);
        }
    }
}