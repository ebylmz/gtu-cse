public class MergeSort {
    /** Sort the array using merge sort algorithm
     * pre:  array contains Comparable objects
     * post: array is sorted
     * @param arr The array to be sorted
     */
    @SuppressWarnings("unchecked")
    public static <E extends Comparable<E>> void sort(E[] arr) {
        // a table with one element is sorted already
        if (arr.length > 1) {
          // split array into halves
            int halfSize = arr.length / 2;
            E[] left = (E[]) new Comparable[halfSize];
            E[] right = (E[]) new Comparable[arr.length - halfSize];
            System.arraycopy(arr, 0, left, 0, halfSize);
            System.arraycopy(arr, halfSize, right, 0, arr.length - halfSize);
            // sort the halves seperatly
            sort(left);
            sort(right);
            // merge the sorted halves
            merge(arr, left, right);
        }
    }

    /**
     * Merge two sequences
     * pre: left and right sequences are sorted
     * post: out is the merged result and is sorted
     * @param <E> 
     * @param out The destination
     * @param left The left input
     * @param right The right input
     */
    private static <E extends Comparable<E>> void merge(E[] out, E[] left, E[] right) {
        int o = 0; // index into the output sequence
        int r = 0; // index into the right sequence
        int l = 0; // index into the left sequence

        // while there is data in both input sequences
        // find the smaller and insert it into the output sequence
        while (r < right.length && l < left.length) 
            out[o++] = (left[l].compareTo(right[r]) < 0) ?
                left[l++] : right[r++];
        // assert: one of the sequences has more items to copy
        // copy remaining input from right sequence into the output
        while (r < right.length)
            out[o++] = right[r++];
        // copy remaining input from left sequence into the output
        while (l < left.length)
            out[o++] = left[l++];
    }
}
