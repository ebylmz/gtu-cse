package application;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.StringTokenizer;

public class InfixToPostfix {
    private static final String OPERATORS = "+-/*()";
    
    private static final int[] PRECEDENCE = {1, 1, 2, 2, -1, -1}; 
    
    private Deque<Character> operatorStack;
    
    private StringBuffer postfix;

    public String convert(String infix) throws SyntaxErrorException {
        postfix = new StringBuffer();
        operatorStack = new ArrayDeque<>();
        StringTokenizer tokens = new StringTokenizer(infix);
        // process each token in the infix string
        while (tokens.hasMoreTokens()) {
            String nextToken = tokens.nextToken();
            char firstChar = nextToken.charAt(0);

            if (Character.isDigit(firstChar)) {
                postfix.append(nextToken);
                postfix.append(' ');
            }
            else if (nextToken.length() == 1 && isOperator(firstChar)) 
                processOperator(firstChar);            
            else
                throw new SyntaxErrorException("Unexpected Character Encountered: " + firstChar);
        }
        
        // Pop any remaining operators and append them to postfix.
        while (!operatorStack.isEmpty()) {
            char op = operatorStack.pop();
            // Any '(' on the stack is not matched.
            if (op == '(')
                throw new SyntaxErrorException("Unmatched opening parenthesis");
            postfix.append(op);
            postfix.append(' ');
        }
        // assert: Stack is empty, return result.
        return postfix.toString();
    }
    
    private void processOperator(char op) {
        // push opening paranthesis because it has biggest priorty in math (not in this application see it's priorty value -1)
        if (! operatorStack.isEmpty() && op != '(') {
            char topOp = operatorStack.peek();
            // high precedence operator requries operand to evaluation, 
            // so push it for its operands that will come later 
            while (precedence(topOp) >= precedence(op)) {
                operatorStack.pop();
                if (topOp == '(') // op == ')'
                    break;
                postfix.append(topOp);
                postfix.append(' ');
                if (operatorStack.isEmpty())
                    break;
                else // reset topOp.
                    topOp = operatorStack.peek(); 
            }
        }
        // operator stack is empty or operator is opening paranthesis
        // or current operator precedence > top of stack operator precedence
        // no need to push closing paranthesis since paranthesis have lowest priorty value 
        if (op != ')')
            operatorStack.push(op);
    }

    private static int precedence(char op) {
        return PRECEDENCE[OPERATORS.indexOf(op)];
    }

    private static boolean isOperator(char c) {
        return OPERATORS.indexOf(c) != -1;
    }

    /** Class to report a syntax error. */
    public static class SyntaxErrorException extends Exception {
        /** Construct a SyntaxErrorException with the specified message.
            @param message The message
        */
        SyntaxErrorException(String message) {
            super(message);
        }
    }
}
