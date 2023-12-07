package frame.button;

import javax.swing.JFrame;

public class TestButtonFrame {
    public static void main (String[] args) {
        ButtonFrame bframe = new ButtonFrame();
        bframe.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        bframe.setSize(275, 110);
        bframe.setVisible(true);
    }
}
