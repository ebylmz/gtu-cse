package application;

import application.InfixToPostfix.SyntaxErrorException;

public class InfixToPostfixTest {
    public static void main(String[] args) {
        InfixToPostfix infixToPostfix = new InfixToPostfix();
        
        String[] expression = {
            "4 + 5 / 4 - 3",
            "1 * 2 - 4 * 3",
            "4 * 7 - 20",
            "3 + 4 * 7 / 2",
            // "3 + 4 * 7 / a 2",
            "1 + 2 * ( 4 - 1 )"
        };

        for (var e : expression) {
            try {
                System.out.printf("%-30s: %s\n", e, infixToPostfix.convert(e));
            } catch (SyntaxErrorException excep) {
                System.out.println(excep);
            }
        }
    }
}
