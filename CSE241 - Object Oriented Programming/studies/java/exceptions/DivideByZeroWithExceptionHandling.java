import java.util.Scanner;
import java.util.InputMismatchException;

public class DivideByZeroWithExceptionHandling {
    public static int quotient (int numerator, int denominator) throws ArithmeticException {
        return numerator / denominator; // possible division by zero
    }
    public static void main (String args[]) {
        boolean exit = false;
        Scanner scanner = new Scanner(System.in);

        do {
            try {
                System.out.printf("numerator: ");
                int numerator = scanner.nextInt();
                System.out.printf("denominator: ");
                int denominator = scanner.nextInt();
                
                int result = quotient(numerator, denominator);
                System.out.printf("%d / %d = %d\n", numerator, denominator, result);
                exit = true;
            }
            catch (InputMismatchException e) {
                System.err.printf("\nException: %s\n", e);
                scanner.nextLine(); // discard input so user can try again

                System.out.println("You must enter integers. Please try again.\n" );
            }
            catch (ArithmeticException e) {
                System.err.printf("\nException: %s\n", e);
                System.out.println("Zero is an invalid denominator. Please try again.\n");
            }
        } while (!exit);
        scanner.close();
    }
}
