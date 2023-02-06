package frame.textField;

import javax.swing.JFrame;

public class TestTextFieldFrame {
    public static void main (String[] args) {
        TextFieldFrame frame = new TextFieldFrame();
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setSize(325, 150);
        frame.setVisible(true);
    } 
}