public class QuickSort {
    /** Sort the array using quick sort algorithm
     * pre:  array contains Comparable objects
     * post: array is sorted
     * @param arr The array to be sorted
     */
    public static <E extends Comparable<E>> void sort(E[] arr) {
        quickSort(arr, 0, arr.length - 1);
    } 

    /**
     * Sort a part of the array using quick sort algorithm
     * pre: table contains Comparable objects.
     * post: table is sorted.
     * @param arr The array to be sorted
     * @param first The index of the low bound
     * @param last The index of the high bound
     */
    public static <E extends Comparable<E>> void quickSort(E[] arr, int first, int last) {
        if (first < last) {
            // partition the array
            int pivIndex = partition(arr, first, last);
            // sort right and left side of the pivot position
            quickSort(arr, first, pivIndex - 1);
            quickSort(arr, pivIndex + 1, last);
        }
    }

    /**
     * Partition the table so that values from first to pivIndex 
     * less than or equal to the pivot value, and the values from
     * pivIndex to last are greater than the pivot value. 
     * @param arr The array to be sorted
     * @param first The index of the low bound
     * @param last The index of the high bound
     * @return The index of pivot value
     */
    private static <E extends Comparable<E>> int partition(E[] arr, int first, int last) {
        // select the first item as pivot value
        E pivot = arr[first];
        int up = first; 
        int down = last;
        do {
            /*  Invariant:
                All items in arr[first . . . up - 1] <= pivot
                All items in arr[down + 1 . . . last] > pivot
            */
            // find the first item that is larger than pivot value
            // no need to check first and last item since they are already placed 
            while (up < last && pivot.compareTo(arr[up]) >= 0)
                ++up;
            // find the first item that is smaller than or equal to pivot value
            while (down > first && pivot.compareTo(arr[down]) < 0)
                --down;
            // assert: up equals last or arr[up] > pivot
            // assert: down equals first or arr[down] <= pivot
            if (up < down)
                swap(arr, up, down);            
        } while (up < down); // continue till array is traversed from first to last
        // place the pivot 
        swap(arr, first, down);
        return down;    // return index of pivot
    }

    /**
     * Swaps the items in arr[i] and arr[j]
     * @param arr The array contains items
     * @param i The index of one item
     * @param j The index of other item
     */
    static <E> void swap(E[] arr, int i, int j) {
        E temp = arr[i];
        arr[i] = arr[j];
        arr[j] = temp;
    }
}
