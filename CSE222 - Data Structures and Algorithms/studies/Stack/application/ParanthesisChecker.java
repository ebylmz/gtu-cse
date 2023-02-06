package application;

import java.util.Stack;

public class ParanthesisChecker {
    public static void main(String[] args) {
        String[] expression = {
            "{(a + b) * c}", // balanced
            "{(a + b)) * c}", // not balanced
            "{(a + b) {} * c}", // balanced
            "[{(a + b) * c}", // not balanced
            "[a {b / (c - d) + e/( f + g)} - h]", // balanced 
            "{a [b + (c + 2)/d ] + e) + f }", // not balanced
            "[a {b + [c (d + e) - f ] + g}", // not balanced
            "Test with no paranthesis" // balanced
        };

        for (var e : expression)
            System.out.println(e + ": " + isBalanced(e));
    }

    private static final String OPEN = "({[";
    private static final String CLOSE = ")}]";

    public static boolean isBalanced(String expression) {
        Stack<Character> stack = new Stack<>();
        boolean balanced = true;
        
        for (int i = 0; i < expression.length() && balanced; ++i) {
            char nextChar = expression.charAt(i); 
            if (isOpen(nextChar))
                stack.push(nextChar);
            else if (isClose(nextChar))
                // compare open and closed paranthesis
                balanced = !stack.isEmpty() && OPEN.indexOf(stack.pop()) == CLOSE.indexOf((nextChar));
        }
        return balanced && stack.isEmpty();
    }

    private static boolean isClose(char c) {
        return CLOSE.indexOf(c) != -1;
    }

    private static boolean isOpen(char c) {
        return OPEN.indexOf(c) != -1;
    }
}
