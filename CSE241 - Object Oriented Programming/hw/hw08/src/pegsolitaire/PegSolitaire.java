/**
 * @file    PegSolitaire.java
 * @author  Emirkan Burak Yılmaz 
 * @brief   Peg Solitaire Game Implementation
 * @version 0.1
 * @date    2022-01-28
 * 
 * @copyright Copyright (c) 2021
 */

package pegsolitaire;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Scanner;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.SwingUtilities;
import javax.swing.Timer;

import pegsolitaire.GamePanel.BoardType;
import pegsolitaire.GamePanel.GameMode;

public class PegSolitaire extends JFrame {
    private GamePanel __gamePanel;
    private JButton __homeBtn;  // turn backs to the main menu from game panel
    private final HomeButtonEventHandler HOME_BUTTON_EVENT_HANDLER = new HomeButtonEventHandler();
    
    private JPanel __mainMenuPanel;
    private JLabel __introLabel;        // intro for the main menu panel (contains text and image)

    private JPanel __newGameMenuPanel; 
    private final NewGameMenuEventHandler NEW_GAME_MENU_EVENT_HANDLER = new NewGameMenuEventHandler();
    private JRadioButton[] __gameTypeBtn;   // computer or user
    private ButtonGroup __gameTypeBtnGroup;
    private JRadioButton[] __boardTypeBtn;  // six different board
    private ButtonGroup __boardTypeBtnGroup;
    private GamePanel.BoardType __boardType;
    private GamePanel.GameMode __gameType;
    
    private JPanel __commandPanel;
    private JButton __backToMainMenuBtn;    // turn backs to the main menu from settings panel
    private JButton __createGameBtn;        // creates new game

    private JPanel __continueGameMenuPanel;
    private JPanel __userListPanel;
    private ArrayList<String[]> __userList;
    private ButtonGroup __usernameBtnGroup;
    private final ContinueGameMenuEventHandler CONTINUE_GAME_MENU_EVENT_HANDLER = new ContinueGameMenuEventHandler();
    private int __selectedUserIndex;    // keeps the user index in __userListBtn array

    private JPanel __curDisplayPanel;   // the currently dislaying panel on top of JFrame

    private final String REGISTER_USER_PATH = "system/login.txt";
    private final String IMAGE_PATH = "system/img/";

    /**
     * Initializes the game Peg Solitaire
     */
    public PegSolitaire() {
        super("PegSolitaire");
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setIconImage(new ImageIcon(IMAGE_PATH + "logo.png").getImage());
        setLayout(new BorderLayout());
        setSize(650, 750);
        
        // set GUI panels
        __mainMenuPanel = createMainMenuPanel();
        __commandPanel = createCommandPanel();
        __newGameMenuPanel = createNewGameMenuPanel();
        __continueGameMenuPanel = createContinueGameMenuPanel();

        // display main menu panel as welcome page
        displayPanel(__mainMenuPanel);
        // set frame visible
        setVisible(true);      
    }

    /**
     * Sets the main menu which contains new game, continue and exit options 
     */
    public JPanel createMainMenuPanel() {
        // set null layout for menu panel
        JPanel mainMenuPanel = new JPanel(null); 
        mainMenuPanel.setBackground(ColorScheme.BLACK.getColor());
        
        JButton[] maincreateMenuBtn = new JButton[3]; // keeps newgame, load, exit buttons
        
        int x = 70;   // x position of button
        int y = 100;   // y position of button 
        int verticalDistance = 100;

        // set common button properties
        for (int i = 0; i < maincreateMenuBtn.length; ++i, y += verticalDistance) {
            maincreateMenuBtn[i] = createMenuBtn("", ColorScheme.BLUE, ColorScheme.RED, true);
            maincreateMenuBtn[i].setSize(150, 50);
            maincreateMenuBtn[i].setLocation(x, y);
            mainMenuPanel.add(maincreateMenuBtn[i]);
        }

        // set specific button properties
        maincreateMenuBtn[0].setText("New Game");
        maincreateMenuBtn[0].addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                // display game settings menu for settings of new game
                displayPanel(__newGameMenuPanel);            
            }
        });        

        maincreateMenuBtn[1].setText("Continue");
        maincreateMenuBtn[1].addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                displayPanel(__continueGameMenuPanel);                  
            }
        });

        maincreateMenuBtn[2].setText("Exit");
        maincreateMenuBtn[2].addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                // destroy current frame/window and close the Java VM (if there is no other frame exist)
                dispose();
            }
        });

        // set a welcome message
        __introLabel = new JLabel();
        __introLabel.setForeground(ColorScheme.RED.getColor());
        
        __introLabel.setText(
            "<html><p>" +
                "<pre>  Warriors</pre>" +
                "<pre>     of</pre>" +
                "<pre>PegSolitaire</pre>" +
            "</p></html>");
        
        __introLabel.setIcon(new ImageIcon(IMAGE_PATH + "warrior.png"));
        __introLabel.setLocation(300, 0);
        __introLabel.setSize(300, 500);
        __introLabel.setIconTextGap(20);
        
        __introLabel.setFont(new Font("MV Boli", Font.PLAIN, 18));
        __introLabel.setHorizontalTextPosition(JLabel.CENTER);
        __introLabel.setVerticalTextPosition(JLabel.BOTTOM);
        mainMenuPanel.add(__introLabel);

        return mainMenuPanel;
    } 

    /**
     * Sets the game settings panel which user can choose board type and game mode
     */
    public JPanel createNewGameMenuPanel() {
        JPanel gameTypePanel = createGameTypePanel();
        JPanel boardTypePanel = createBoardTypePanel();
        
        JLabel gameModeText = new JLabel("Game Mode");
        ColorScheme.setColor(gameModeText, ColorScheme.BLACK, ColorScheme.RED);
        gameModeText.setFont(new Font("MV Boli", Font.PLAIN, 30));
        gameModeText.setHorizontalAlignment(JLabel.CENTER);
        
        JLabel gameBoardText = new JLabel("Game Board");
        ColorScheme.setColor(gameBoardText, ColorScheme.BLACK, ColorScheme.RED);
        gameBoardText.setFont(new Font("MV Boli", Font.PLAIN, 30));
        gameBoardText.setHorizontalAlignment(JLabel.CENTER);

        // add two panel to the top Game Settings panel
        JPanel newGameMenuPanel = new JPanel(new GridLayout(5, 1));
        ColorScheme.setColor(newGameMenuPanel, ColorScheme.BLACK);
        newGameMenuPanel.add(gameModeText);
        newGameMenuPanel.add(gameTypePanel);
        newGameMenuPanel.add(gameBoardText);
        newGameMenuPanel.add(boardTypePanel);
        newGameMenuPanel.add(__commandPanel);
        return newGameMenuPanel;
    }

    /**
     * Sets the game type selection panel
     * @return game type panel
     */
    private JPanel createGameTypePanel() {
        JPanel gameTypePanel = new JPanel(); 
        ColorScheme.setColor(gameTypePanel, ColorScheme.BLACK, ColorScheme.RED);

        __gameTypeBtn = new JRadioButton[2];   
        __gameTypeBtnGroup = new ButtonGroup();
        for (int i = 0; i < __gameTypeBtn.length; ++i) {
            __gameTypeBtn[i] = new JRadioButton(); 
            __gameTypeBtn[i].addActionListener(NEW_GAME_MENU_EVENT_HANDLER);
            ColorScheme.setColor(__gameTypeBtn[i], ColorScheme.BLACK, ColorScheme.RED);
            // add related buttons to the same group
            __gameTypeBtnGroup.add(__gameTypeBtn[i]);
            gameTypePanel.add(__gameTypeBtn[i]);
        }        
        
        // initially set as null, user will choose game type
        __gameType = null; 
        __gameTypeBtn[0].setText("User");
        __gameTypeBtn[1].setText("Computer");
        
        return gameTypePanel;
    } 

    /**
     * Sets the board type panel which user can choose game board
     * @return board type panel
     */
    private JPanel createBoardTypePanel() {
        // set the board type buttons (six different board)
        JPanel boardTypePanel = new JPanel(/* new GridLayout(2, 3, 0, 50) */);
        ColorScheme.setColor(boardTypePanel, ColorScheme.BLACK, ColorScheme.RED);
        __boardTypeBtn = new JRadioButton[6];  // 
        __boardTypeBtnGroup = new ButtonGroup();
        for (int i = 0; i < __boardTypeBtn.length; ++i) {
            __boardTypeBtn[i] = new JRadioButton();
            // set the text at the bottom center of the button
            __boardTypeBtn[i].setHorizontalTextPosition(JButton.CENTER);
            __boardTypeBtn[i].setVerticalTextPosition(JButton.BOTTOM);
            __boardTypeBtn[i].setIconTextGap(20);
            ColorScheme.setColor(__boardTypeBtn[i], ColorScheme.BLACK, ColorScheme.RED);
            __boardTypeBtn[i].addActionListener(NEW_GAME_MENU_EVENT_HANDLER);

            // add buttons to the same group to permit only one selection
            __boardTypeBtnGroup.add(__boardTypeBtn[i]);
            boardTypePanel.add(__boardTypeBtn[i]);
        }        

        // initially set as null, user will choose board type
        __boardType = null;
        __boardTypeBtn[0].setText("French");
        __boardTypeBtn[0].setActionCommand("French");
        __boardTypeBtn[0].setIcon(new ImageIcon(IMAGE_PATH + "frenchBoard.png"));
        __boardTypeBtn[1].setText("German");
        __boardTypeBtn[1].setActionCommand("German");
        __boardTypeBtn[1].setIcon(new ImageIcon(IMAGE_PATH + "germanBoard.png"));
        __boardTypeBtn[2].setText("Asymetrical");
        __boardTypeBtn[2].setActionCommand("Asymetrical");
        __boardTypeBtn[2].setIcon(new ImageIcon(IMAGE_PATH + "asymetricalBoard.png"));
        __boardTypeBtn[3].setText("English");
        __boardTypeBtn[3].setActionCommand("English");
        __boardTypeBtn[3].setIcon(new ImageIcon(IMAGE_PATH + "englishBoard.png"));
        __boardTypeBtn[4].setText("Diamond");
        __boardTypeBtn[4].setActionCommand("Diamond");
        __boardTypeBtn[4].setIcon(new ImageIcon(IMAGE_PATH + "diamondBoard.png"));
        __boardTypeBtn[5].setText("Triangular");
        __boardTypeBtn[5].setActionCommand("Triangular");
        __boardTypeBtn[5].setIcon(new ImageIcon(IMAGE_PATH + "triangularBoard.png"));
        return boardTypePanel;
    }

    /**
     * Sets the panel which user can create game or back to the main menu
     * @return command panel 
     */
    private JPanel createCommandPanel() {
        // set start and back button
        JPanel commandPanel = new JPanel();
        ColorScheme.setColor(commandPanel, ColorScheme.BLACK);

        Font menuFont = new Font("MV Boli", Font.PLAIN, 25);

        // create button not enable till board and game type selected 
        __createGameBtn = createMenuBtn("Create", ColorScheme.BLACK, ColorScheme.RED, false);
        __createGameBtn.setFont(menuFont);

        // create game event handler (create new game (computer or user))
        __createGameBtn.addActionListener(new ActionListener() {
            // make one movement per second 
            // (500ms select movement, 500ms apply movement)
            int delay = 500; 
            ActionListener taskPerformer = new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    // This code will be called once the timeout of 1/2 seconds has been passed
                    // if user exit from game panel, stop the auto play
                    if (__curDisplayPanel != __gamePanel)
                        ((Timer)e.getSource()).stop();

                    if (__gamePanel.curMovement().start() == null) {
                        // if random movement cannot made that means game is over
                        if (__gamePanel.curMovement().setRandomMovement()) {
                            __gamePanel.curMovement().start().setSelected(true); 
                            __gamePanel.curMovement().end().setPossible(true);
                        }
                        else {
                            // display game score
                            JOptionPane.showMessageDialog(__gamePanel, String.format(
                                "Number of Movement: %d\n" + 
                                "   Remaining Peg: %d", 
                                __gamePanel.numOfMov(), __gamePanel.numOfPeg()
                            ), "Game is Over", JOptionPane.INFORMATION_MESSAGE);     
                            ((Timer)e.getSource()).stop();
                        }
                    }
                    else if (__gamePanel.makeMove(__gamePanel.curMovement()))
                        __gamePanel.curMovement().setStart(null);
                }
            };

            @Override
            public void actionPerformed(ActionEvent e) {
                if (__curDisplayPanel == __continueGameMenuPanel) {
                    boolean done = false;
                    // get the password
                    do {
                        String password = JOptionPane.showInputDialog(__continueGameMenuPanel, "Enter your password", "User Login", JOptionPane.QUESTION_MESSAGE);
                        if (__userList.get(__selectedUserIndex)[1].equals(password)) {
                            done = true;
                            // create the game which properties specified
                            String username = __userList.get(__selectedUserIndex)[0];
                            __gamePanel = createGamePanel(username);
                            displayPanel(__gamePanel);
                        }
                        else if (password == null) // user hits cancel
                            done = true;                        
                        else {
                            int selection = JOptionPane.showConfirmDialog(__continueGameMenuPanel, "Wrong password, try again", "Error", JOptionPane.ERROR_MESSAGE);
                            done = (selection == 0) ? false : true; // returns 0 for yes
                        }
                    } while (!done);
                }
                else {
                    if (__gameType == GamePanel.GameMode.COMPUTER)
                        new Timer(delay, taskPerformer).start();
                    // create the game which properties specified
                    __gamePanel = createGamePanel();
                    displayPanel(__gamePanel);
                }                  
            }
        });

        __backToMainMenuBtn = createMenuBtn("Back", ColorScheme.BLACK, ColorScheme.RED, true);
        __backToMainMenuBtn.setFont(menuFont);
        __backToMainMenuBtn.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                displayPanel(__mainMenuPanel);
            }
        });

        commandPanel.add(__createGameBtn);
        commandPanel.add(__backToMainMenuBtn);    
        return commandPanel;
    }

    /**
     * Sets load game menu which user can select and load the saved games
     */
    private JPanel createContinueGameMenuPanel() {
        // set user list panel
        __userListPanel = createUserListPanel();
        
        JPanel continueGameMenuPanel = new JPanel(new GridLayout(2, 1));
        continueGameMenuPanel.setBackground(ColorScheme.BLACK.getColor());
        continueGameMenuPanel.add(__userListPanel);
        continueGameMenuPanel.add(__commandPanel);
        return continueGameMenuPanel;
    }
    
    private JPanel createUserListPanel() {
        try (Scanner reader = new Scanner(new File(REGISTER_USER_PATH));) {
            JPanel userListPanel = new JPanel();
            ColorScheme.setColor(userListPanel, ColorScheme.BLACK, ColorScheme.RED);
            
            __usernameBtnGroup = new ButtonGroup();
            if (__userList == null)
                __userList = new ArrayList<String[]>();
            else
                __userList.clear();

            for (int userIndex = 0; reader.hasNextLine(); ++userIndex) {
                // read each username and password and save to later uses (username & password checking)
                // parse the line as [0]: username, [1]: password
                String[] user = reader.nextLine().split(", ");  
                __userList.add(user);
                
                JRadioButton userBtn = new JRadioButton(__userList.get(userIndex)[0]);
                ColorScheme.setColor(userBtn, ColorScheme.BLACK, ColorScheme.RED);
                userBtn.addActionListener(CONTINUE_GAME_MENU_EVENT_HANDLER);
                // add the order of user (later will be used as __selectedUserIndex) 
                userBtn.setActionCommand("" + userIndex);  
                __usernameBtnGroup.add(userBtn);
                userListPanel.add(userBtn);
            }
            
            // put each user in seperate row to do that use GridLayout
            userListPanel.setLayout(new GridLayout(__userList.size(), 1));
            return userListPanel;

        }
        catch (FileNotFoundException e) {
            System.out.println("Something went wrong");
            e.printStackTrace();
            return null;
        }         
    }

    private class ContinueGameMenuEventHandler implements ActionListener {
        @Override
        public void actionPerformed(ActionEvent e) {
            var selectedBtn = __usernameBtnGroup.getSelection();
            if (selectedBtn != null) {
                __selectedUserIndex = Integer.parseInt(selectedBtn.getActionCommand());
                __createGameBtn.setEnabled(true);
            }
        }
    }

    /**
     * Sets the game panel for user to play the game
     * @param username which contains the user game progress information 
     * @return true for successful execution 
     */
    public GamePanel createGamePanel(String username) {
        if (__gamePanel != null)
            remove(__gamePanel);

        __homeBtn = new JButton();
        __homeBtn.addActionListener(HOME_BUTTON_EVENT_HANDLER);
        return new GamePanel(__homeBtn, username);
    }

    /**
     * Sets the game panel
     */
    public GamePanel createGamePanel() {
        if (__gamePanel != null)
            remove(__gamePanel);

        __homeBtn = new JButton();
        __homeBtn.addActionListener(HOME_BUTTON_EVENT_HANDLER);
        return new GamePanel(__homeBtn, __gameType, __boardType);
    }

    private class HomeButtonEventHandler implements ActionListener {
        @Override
        public void actionPerformed(ActionEvent e) {
            // ask if user wants to save the progress (only in USER MODE)
            if (__gamePanel.gameMode() == GameMode.USER) {
                int select = JOptionPane.showConfirmDialog(
                    __gamePanel, "Save your progress?", "Save Progress", JOptionPane.YES_NO_CANCEL_OPTION);
                if (select == 0) // yes(0), no(1), cancel(2)
                    __gamePanel.saveGameButton().doClick();
            }
            displayPanel(__mainMenuPanel);                    
        }
    }

    /**
     * Display the given panel in game frame 
     * @param panel next panel for display the screen
     */
    public void displayPanel(JPanel newPanel) {
        // clear
        if (newPanel == __newGameMenuPanel) {
            __gameTypeBtnGroup.clearSelection();
            __gameType = null;
            __boardTypeBtnGroup.clearSelection();
            __boardType = null;
            __newGameMenuPanel.add(__commandPanel);
            __createGameBtn.setEnabled(false);  // default value
        }
        else if (newPanel == __continueGameMenuPanel) {
            __usernameBtnGroup.clearSelection();
            // update registered user list for displaying previous game sections
            __continueGameMenuPanel.remove(__userListPanel);
            __userListPanel = createUserListPanel();
            __continueGameMenuPanel.add(__userListPanel);    
            
            __continueGameMenuPanel.add(__commandPanel);
            __createGameBtn.setEnabled(false);  // default value
        }
        
        // remove the current displaying panel
        if (__curDisplayPanel != null)
            remove(__curDisplayPanel);
    
        // set new display panel and update the game frame
        __curDisplayPanel = newPanel;
        add(__curDisplayPanel);
        SwingUtilities.updateComponentTreeUI(this); 
    }

    private class NewGameMenuEventHandler implements ActionListener {
        @Override
        public void actionPerformed(ActionEvent e) {
            boolean selected = false;

            // GAME TYPE BUTTONS
            for (int i = 0; i < __gameTypeBtn.length && !selected; ++i)
                if (e.getSource() == __gameTypeBtn[i]) {
                    __gameType = __gameTypeBtn[i].getText().equals("User") ? 
                        GameMode.USER : GameMode.COMPUTER;
                    selected = true;
                }
            
            // BOARD TYPE BUTTONS
            var selectedBtn = __boardTypeBtnGroup.getSelection();
            if (selectedBtn != null) {
                selected = true;
                switch (selectedBtn.getActionCommand()) {
                    case "French":
                        __boardType = BoardType.FRENCH; break;
                    case "German":
                        __boardType = BoardType.GERMAN; break;
                    case "Asymetrical":
                        __boardType = BoardType.ASYMETRICAL; break;
                    case "English":
                        __boardType = BoardType.ENGLISH; break;
                    case "Diamond":
                        __boardType = BoardType.DIAMOND; break;
                    case "Triangular":
                        __boardType = BoardType.TRIANGULAR; break;
                }
            }
                
            // if two selection made, enable the button which creates game  
            if (__boardType != null && __gameType != null) 
                __createGameBtn.setEnabled(true);
        }
    }

    /**
     * Creates an menu button
     * @param text button text
     * @param listener action listener for button
     * @param bg background
     * @param fg foreground
     * @param isEnable clickable or not
     * @return new created button
     */
    private JButton createMenuBtn (String text, ColorScheme bg, ColorScheme fg, boolean isEnable) {
        JButton btn = new JButton(text);
        ColorScheme.setColor(btn, bg, fg);
        btn.setEnabled(isEnable);
        return btn;
    }
}
