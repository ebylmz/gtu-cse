package frame.label;

import javax.swing.JFrame;

public class TestLabelFrame {
    public static void main (String[] args) {
        // test1();
        test2();
    } 

    public static void test1 () {
        LabelFrame lframe = new LabelFrame();
        lframe.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        lframe.setSize(400, 600);

        // pop-up and our program on different threads
        // by setting visible pop-up still open and execute
        // otherwise it's closed with the java program
        lframe.setVisible(true);
    }
    
    
    public static void test2 () {
        // LabelFrame lframe = new LabelFrame("mari o.gif");
        LabelFrame lframe = new LabelFrame("cat.gif");
        lframe.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        lframe.setSize(600, 675);
        lframe.setVisible(true);
    }
}
