package basics;

import javax.swing.JOptionPane;

public class Addition {
    public static void main (String[] args) {
        String first = JOptionPane.showInputDialog("Enter first integer");
        String second = JOptionPane.showInputDialog("Enter second integer");

        // it would be better two use Integer object here
        // because eventually int become Integer with auto-boxing
        int n1 = Integer.parseInt(first);
        int n2 = Integer.parseInt(second);
        int sum = n1 + n2;  

        // JOptionPane.showMessageDialog(parentComponent, message, title, messageType);
        JOptionPane.showMessageDialog(null, "The sum is " + sum, 
            "Sum of two integers", JOptionPane.INFORMATION_MESSAGE);
        // Message Types
        // JOptionPane.PLAIN_MESSAGE    
        // JOptionPane.ERROR_MESSAGE
        // JOptionPane.INFORMATION_MESSAGE
        // JOptionPane.WARNING_MESSAGE
        // JOptionPane.QUESTION_MESSAGE
    }
}