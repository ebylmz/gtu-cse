package grid;
import java.awt.GridLayout;
import java.awt.Container;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import javax.swing.JFrame;
import javax.swing.JButton;

public class GridLayoutFrame extends JFrame implements ActionListener {
    private JButton buttons[];
    private final String names[] = {"one", "two", "three", "four", "five", "six"};
    private boolean toggle = true;  // toogle between two layout
    private Container container;
    private GridLayout gridLayout1; 
    private GridLayout gridLayout2;
    
    public GridLayoutFrame () {
        super("GridLayout Test");
        gridLayout1 = new GridLayout(2, 3, 5, 5);   // 2 row 3 col; gaps of 5
        gridLayout2 = new GridLayout(3, 2); // 3 by 2; no gaps
        container = getContentPane();
        setLayout(gridLayout1);

        buttons = new JButton[names.length];
        for (int i = 0; i < names.length; ++i) {
            buttons[i] = new JButton(names[i]);
            buttons[i].addActionListener(this);
            add(buttons[i]);
        }
    }
    
    public void actionPerformed(ActionEvent event) {
        if (toggle)
            container.setLayout(gridLayout2);
        else   
            container.setLayout(gridLayout1);
        toggle = !toggle;
        container.validate();   // re-layout container
    }
}
