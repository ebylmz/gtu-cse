package basics;

import javax.swing.*;  

public class FirstSwingExample {  
    public static void main (String[] args) {  
        JFrame f = new JFrame();      
        
        // JComponent: 
        // JLabel, JList, JTable, JComboBox, JSlider, JMenu, AbstractButton, JButton
        
        // Commonly used Methods Of Component class:
        // add(Component c), setsize(int w, int h), setLayout(LayoutManager m), setVisible(boolean b)
        
        JButton b = new JButton("click me");     
        b.setBounds(130, 100, 100, 40); // x axis, y axis, width, height  
        
        f.add(b);                   // adding button in JFrame  

        f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE); // set the frame to exit when it is closed
        f.setSize(400,500);         // 400 width and 500 height  
        f.setLayout(null);          // using no layout managers  
        f.setVisible(true);         // making the frame visible, it is false by default  
    }  
}  