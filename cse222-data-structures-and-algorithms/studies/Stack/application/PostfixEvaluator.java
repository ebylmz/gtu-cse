package application;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.StringTokenizer;

public class PostfixEvaluator {
    private final String OPERATORS = "+-*/"; 

    private Deque<Integer> operandStack;

    public PostfixEvaluator() {
        operandStack = new ArrayDeque<>();
    }

    public int eval(String expression) throws SyntaxErrorException {
        StringTokenizer tokens = new StringTokenizer(expression);
        while (tokens.hasMoreTokens()) {
            String next = tokens.nextToken();
            if (Character.isDigit(next.charAt(0))) {
                // push number to operand stack
                int n = Integer.parseInt(next);
                operandStack.push(n);
            }
            else if (next.length() == 1 && isOperator(next.charAt(0))) {
                // evaluate operation and push the result to operand stack
                int result = evalOperation(next.charAt(0));
                operandStack.push(result);
            }
            else
                throw new SyntaxErrorException("Invalid syntax");
        }

        int result = operandStack.pop();
        if (!operandStack.isEmpty())
            throw new SyntaxErrorException("Stack should be empty");
        return result;
    }
    
    private int evalOperation(char op) {
        if (operandStack.size() < 2 || !isOperator(op))
            throw new IllegalStateException();
        int rhs = operandStack.pop();
        int lhs = operandStack.pop();
        int result = 0;
        switch (op) {
            case '+':   
                result = lhs + rhs; 
                break;
            case '-':   
                result = lhs - rhs; 
                break;
            case '*':   
                result = lhs * rhs; 
                break;
            case '/':   
                result = lhs / rhs; 
                break;
        }
        return result;
    }


    private boolean isOperator(char c) {
        return OPERATORS.indexOf(c) != -1;
    }    

    // Nested Class
    /** Class to report a syntax error. */
    public static class SyntaxErrorException extends Exception {
        /** Construct a SyntaxErrorException with the specified
            message.
            @param message The message
        */
        SyntaxErrorException(String message) {
            super(message);
        }
    }
}
