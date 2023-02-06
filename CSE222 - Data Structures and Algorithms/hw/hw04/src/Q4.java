package src;

public class Q4 {
    public static int multiply(int x, int y) {
        // apply multiplication if one of the number has single digit
        if (x < 10 || y < 10)
            return x * y;
        int half = Math.max(numOfDigit(x), numOfDigit(y)) / 2;
        int power = pow(10, half);
        
        int a = x / power; // lefth half of x
        int b = x % power; // right half of x
        int c = y / power; // lefth half of y
        int d = y % power; // right half of y

        int bd = multiply(b, d);
        int ac = multiply(a, c);
        int ab_plus_cd = multiply(a + b, c + d) - ac - bd;
        return ac * pow(10, 2 * half) + ab_plus_cd * pow(10, half) + bd; 
    }

   /**
    * Recursive power method 
    * pre: n >= 0
    * @param x the number being raised to a power
    * @param n the exponent
    * @return x raised to the power n
    */
    private static int pow(int x, int n) {
        if (n == 0)
            return 1;
        else
            return x * pow(x, n -1);
    }


    /**
     * Finds number of digit in given number 
     * pre: n >= 0
     * @param n number 
     * @return number of digit
     */
    private static int numOfDigit(int n) {
        if (n == 0)
            return 0;
        else 
            return 1 + numOfDigit(n / 10);
    }

    /** Tests multiply method */
    public static void test() {
        System.out.println("\n================ Q4 TEST ================");

        System.out.println("\nT1:");
        System.out.println(multiply(6, 10));
        System.out.println("\nT2:");
        System.out.println(multiply(1234, 567));
        System.out.println(multiply(12345, 67899));
        System.out.println(multiply(16, 57));
     
        System.out.println("================ END ================");
    }
}
