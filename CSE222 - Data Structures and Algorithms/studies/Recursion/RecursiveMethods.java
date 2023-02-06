public class RecursiveMethods {

    public static <E> int linearSearch(E[] items, E target) {
        return linearSearch(items, target, 0);
    }

    private static <E> int linearSearch(E[] items, E target, int index) {
        if (index == items.length)
            return -1;
        else if (target.equals(items[index]))
            return index;
        else 
            return linearSearch(items, target, index + 1);
    }

    /**
     * wrapper method for binary search 
     */
    public static <E > int binarySearch(E[] items, Comparable<E> target) {
        return binarySearch(items, target, 0, items.length - 1);
    }

    /**
     * Here in binarySearch Comparable interface is needed because equals says
     * only if the two object has same value or not. But in binary search we need to
     * know the exact relationship like larger or smaller to search specific part of the array.
     */

    private static <E> int binarySearch(E[] items, Comparable<E> target, int left, int right) {
        if (left > right)
            return -1;
        else {
            int mid = (left + right) / 2;
            int compResult = target.compareTo(items[mid]);
            if (compResult == 0) // target found
                return mid; 
            else if (compResult < 0)  // target could be left side of the middle element
                return binarySearch(items, target, left, mid - 1);
            else // target could be right side of the middle element
                return binarySearch(items, target, mid + 1, right);
        }
    }

    /** 
     * Recursive method to calculate Fibonacci numbers
     * pre: n >= 1
     * @param n The position of the Fibonacci number being calculated
     * @return The Fibonacci number
     */
    public static int fibonacci(int n) {
        // first 
        return fibonacci(1, 0, n);
    } 

    /** Recursive O(n) method to calculate Fibonacci numbers
      * pre: n >= 1
      * @param fibCurrent The current Fibonacci number
      * @param fibPrevious The previous Fibonacci number
      * @param n The count of Fibonacci numbers left to calculate
      * @return The value of the Fibonacci number calculated so far
      */
    private static int fibonacci(int fibCurrent, int fibPrev, int n) {
        if (n == 1)
            return fibCurrent;
        else 
            return fibonacci(fibCurrent + fibPrev, fibCurrent, n - 1);
    }

    /** Recursive gcd method
      * pre: m > 0 and n > 0
      * @param m The larger number
      * @param n The smaller number
      * @return Greatest common divisor of m and n
      */
    public static double gcd(int m, int n) {
        if (m % n == 0)
            return n;
        else if (m < n)
            return gcd(n, m); // Transpose arguments.
        else
            return gcd(n, m % n);
    }
}
