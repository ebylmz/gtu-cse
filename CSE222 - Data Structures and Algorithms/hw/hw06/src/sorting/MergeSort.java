package src.sorting;

public class MergeSort {
    /**
     * Sorts the given array using mergesort algorithm
     * @param arr Array
     */
    @SuppressWarnings("unchecked")
    public static <T extends Comparable<T>> void sort(T[] arr) {
        // an array with one element is sorted already
        if (arr.length > 1) {
            // split array into halves to sort them seperatly
            int halfSize = arr.length / 2;
            T[] leftPart = (T[])new Comparable[halfSize];
            T[] rightPart = (T[])new Comparable[arr.length - halfSize];
            System.arraycopy(arr, 0, leftPart, 0, halfSize);
            System.arraycopy(arr, halfSize, rightPart, 0, arr.length - halfSize);
            // Sort two halves and merge them together
            sort(leftPart);
            sort(rightPart);
            merge(arr, leftPart, rightPart);
        }
    }

   /**
    * Merges two given array into the another output array 
    * @param out Output array
    * @param left An array
    * @param right An array
    */
    private static <T extends Comparable <T>> void merge( 
        T[] out, T[] left, T[] right) {
        int i = 0; // index into the left array
        int j = 0; // index into the right array
        int k = 0; // index into the output array
        // in each iteration select and place the smaller item into the output array
        while (i < left.length && j < right.length) 
            if (left[i].compareTo(right[j]) < 0)
                out[k++] = left[i++];
            else
                out[k++] = right[j++];
        // if there are remaining items in one of left or right array 
        // place them directly place the remainder items 
        while (i < left.length)
            out[k++] = left[i++];
        while (j < right.length)
            out[k++] = right[j++];
    }
}
