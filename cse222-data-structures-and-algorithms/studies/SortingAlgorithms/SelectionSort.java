public class SelectionSort {
    /** Sort the array using selection sort algorithm
     * pre:  array contains Comparable objects
     * post: array is sorted
     * @param arr The array to be sorted
     */
    public static <E extends Comparable<E>> void sort(E[] arr) {
        // n = arr.length - 1
        for (int i = 0; i < arr.length; ++i) {
            int posMin = i;
            for (int j = i + 1; j < arr.length; ++j) {
                // Invariant: arr[posMin] is the smallest item in arr[i . . . j - 1]
                if (arr[j].compareTo(arr[posMin]) < 0)
                   posMin = j;
            }
            // Assert: arr[posMin] is the smallest item in arr[i . . . n]
            E temp = arr[i];
            arr[i] = arr[posMin];
            arr[posMin] = temp;
            // Assert: arr[i] is the smallest item in arr[i . . . n]
        }
        // Assert: array is sorted
    }   
}
