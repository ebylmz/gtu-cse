package frame.checkBox;

import javax.swing.JFrame;

public class TestCheckBoxFrame {
    public static void main (String[] args) {
        CheckBoxFrame checkBoxFrame = new CheckBoxFrame();
        checkBoxFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        checkBoxFrame.setSize(300, 120);
        checkBoxFrame.setVisible(true);
    }
}
