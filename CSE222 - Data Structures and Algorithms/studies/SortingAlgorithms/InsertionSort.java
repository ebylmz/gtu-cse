public class InsertionSort {
    /** Sort the array using insertion sort algorithm
     * pre:  array contains Comparable objects
     * post: array is sorted
     * @param arr The array to be sorted
     */
    public static <E extends Comparable<E>> void sort(E[] arr) {
        for (int nextPos = 1; nextPos < arr.length; ++nextPos) {
            // Invariant: arr[0 . . . nextPos - 1] is sorted
            // insert element at position nextPos in the sorted subarray
            insert(arr, nextPos);
        }
    }

    /**
     * Insert the element at nextPos where it belongs in the array
     * @param arr The array being sorted
     * @param nextPos The position of the element to insert
     */
    private static <E extends Comparable<E>> void insert(E[] arr, int nextPos) {
        E nextVal = arr[nextPos];
        while (nextPos > 0 && nextVal.compareTo(arr[nextPos - 1]) < 0) {
            arr[nextPos] = arr[nextPos - 1]; // shift down
            --nextPos;
        }
        // insert nextVal at nextPos
        arr[nextPos] = nextVal;
    }
}
