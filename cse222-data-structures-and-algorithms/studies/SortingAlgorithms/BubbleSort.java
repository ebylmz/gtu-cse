public class BubbleSort {
    /** Sort the array using bubble sort algorithm
     * pre:  array contains Comparable objects
     * post: array is sorted
     * @param arr The array to be sorted
     */
    public static <E extends Comparable<E>> void sort(E[] arr) {
        int end = arr.length - 1;  
        boolean exchanges = false;
        do {
            // Invariant: Elements after end + 1 are in place (sorted)
            exchanges = false;
            // in each iteration move the current max item to the end
            for (int i = 0; i < end; ++i) {
                // compare each pair of adjacent elements
                if (arr[i].compareTo(arr[i + 1]) > 0) {
                    E temp = arr[i];
                    arr[i] = arr[i + 1];
                    arr[i + 1] = temp;
                    exchanges = true; // set the flag
                }
            }
            --end; // current max item is at index end
            // if there is no exchange is made then the array is sorted 
        } while (exchanges); 
        // assert: Array is sorted
    }
}