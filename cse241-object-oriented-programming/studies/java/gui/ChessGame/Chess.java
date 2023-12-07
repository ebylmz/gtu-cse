import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;

@SuppressWarnings("serial")
public class Chess extends JFrame implements ActionListener {

    private JButton[][] tiles; 

    public Chess() {

        setTitle("Chess");
        setSize(500, 500);

        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        setVisible(true);

        setLayout(new BorderLayout());

        JPanel board = new JPanel();

        board.setLayout(new GridLayout(8, 8));

        tiles = new JButton[8][8];

        for(int y = 0; y < tiles.length; y++) {

            for(int x = 0; x < tiles[y].length; x++) {

                tiles[x][y] = new JButton();

                tiles[x][y].setActionCommand(x + " " + y);
                tiles[x][y].addActionListener(this);

                board.add(tiles[x][y]);
            }
        }

        add(board, BorderLayout.CENTER);

        JPanel options = new JPanel();
        options.setLayout(new GridLayout(1, 3));

        JButton newGame = new JButton("New");
        newGame.addActionListener(this);

        options.add(newGame);

        JButton openGame = new JButton("Open");
        openGame.addActionListener(this);

        options.add(openGame);

        JButton setTime = new JButton("Set Time");
        setTime.addActionListener(this);

        options.add(setTime);

        add(options, BorderLayout.SOUTH);

        revalidate();
    }

    public void actionPerformed(ActionEvent event) {

        String command = event.getActionCommand();

        System.out.println(command);

        revalidate();
    }

    public static void main(String[] args) {
        new Chess();
    }
}