package src.sorting;

public class QuickSort {
    /**
     * Sorts the given array using quicksort algorithm
     * @param arr Array which will become sorted
     */
    public static <T extends Comparable<T>> 
    void sort(T[] arr) {
        quickSort(arr, 0, arr.length - 1);
    }

    /**
     * Sorts a part of the given array using the quicksort algorithm.
     * @param arr The array which will be sorted
     * @param first Start index of subarray
     * @param last End index of subarray
     */
    private static <T extends Comparable <T>> 
    void quickSort(T[] arr, int first, int last) {
        if (first < last) { 
            // since partition splits the array at pivot position 
            // and if the two side of the pivot is sorted then the array is sorted
            // partition the array and get the index of pivot value
            int pivIndex = partition(arr, first, last);
            // Sort the left part of pivot
            quickSort(arr, first, pivIndex - 1);
            // Sort the right part of pivot
            quickSort(arr, pivIndex + 1, last);
        }
    }

    /**
     * Partitions the given array so that values from first to pivIndex
     * are less than or equal to the pivot value, and values from
     * pivIndex to last are greater than the pivot value
     * @param arr The array
     * @param first Start index of subarray
     * @param last End index of subarray
     * @return
     */
    private static <T extends Comparable<T>> 
    int partition(T[] arr, int first, int last) {
        // select the first item as the pivot value (random selection is also okey)
        T pivot = arr[first];
        int up = first;
        int down = last;
        do {
            // All items in arr[first . . . up - 1] <= pivot
            // All items in arr[down + 1 . . . last] > pivot
            while ((up < last) && (pivot.compareTo(arr[up]) >= 0)) 
                up++;
            // assert: up equals last or arr[up] > pivot.
            while (pivot.compareTo(arr[down]) < 0)
                down--;
            // assert: down equals first or arr[down] <= pivot.
            if (up < down)  // if up is to the left of down.
                swap(arr, up, down); 
        } while (up < down); 
        // swap arr[first] and arr[down] to put the pivot value where it belongs
        swap(arr, first, down);
        // Return the index of the pivot value.
        return down;
  }

    /**
     * Swaps the items of given arr
     * @param i The index of one item
     * @param j The index of the other item
     */
    private static <T extends Comparable<T>> 
    void swap(T[] arr, int i, int j) {
        T temp = arr[i];
        arr[i] = arr[j];
        arr[j] = temp;
    }
}
