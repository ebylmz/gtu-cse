public class ShellSort {
    /** Sort the array using shell sort algorithm
     * pre:  array contains Comparable objects
     * post: array is sorted
     * @param arr The array to be sorted
     */
    public static <E extends Comparable<E>> void sort(E[] arr) {
        int gap = arr.length / 2; // gap between adjacent elements.
        while (gap > 0) {
            // insert element at nextPos in its subarray (defined with gap value)
            for (int nextPos = 1; nextPos < arr.length; ++nextPos)
                insert(arr, nextPos, gap);
            // select smaller gap value
            gap = (gap == 2) ? 1 : (int) (gap / 2.2);
        }
    }

    /**
     * Insert the element at nextPos where it belongs in the array
     * pre:  Elements through nextPos - gap in subarray are sorted
     * post: Elements through nextPos in subarray are sorted
     * @param arr The array being sorted
     * @param nextPos The position of the element to insert
     * @param gap The gap between elements in the subarray  
     */
    private static <E extends Comparable<E>> void insert(E[] arr, int nextPos, int gap) {
        E nextVal = arr[nextPos];
        // shift all values > nextVal in subarray down by gap
        while (nextPos - gap >= 0 && nextVal.compareTo(arr[nextPos - gap]) < 0) {
            arr[nextPos] = arr[nextPos - gap];
            nextPos -= gap; // check next item
        } 
        // insert nextVal
        arr[nextPos] = nextVal;
    }
}
