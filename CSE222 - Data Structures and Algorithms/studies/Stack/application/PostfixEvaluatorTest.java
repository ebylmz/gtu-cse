package application;

import application.PostfixEvaluator.SyntaxErrorException;

public class PostfixEvaluatorTest {
    public static void main(String[] args) {
        String[] expression = {
            "1 2 + 4 *", // 12
            "1 2 3 + - 4 *", // -16
            "1 1 1 1",
            "1 2 3 + - *"
        };
        
        PostfixEvaluator postfixEvaluator = new PostfixEvaluator();
        for (var e : expression) {
            try {
                System.out.printf("%-30s: %d\n", e, postfixEvaluator.eval(e));
            } catch (SyntaxErrorException except) {
                System.out.println(except);
            }
        }
    }
}
