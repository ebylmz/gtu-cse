public class HeapSort {
    /** Sort the array using heap sort algorithm
     * pre:  array contains Comparable objects
     * post: array is sorted
     * @param arr The array to be sorted
     */
    public static <E extends Comparable<E>> void sort(E[] arr) {
        // build a max heap
        buildHeap(arr);
        // extract the heap
        shrinkHeap(arr);
    } 

    /**
     * Transforms the array into a heap
     * pre:  The array contains at least one item
     * post: All items in the array are in heap order
     * @param arr The array to be transformed into a heap
     */
    private static <E extends Comparable<E>> void buildHeap(E[] arr) {
        int n = 1; // size of the heap
        while (n < arr.length) {
            int child = n;
            int parent = (child - 1) / 2;
            while (parent >= 0 && arr[child].compareTo(arr[parent]) > 0) {
                swap(arr, child, parent);
                child = parent;
                parent = (child - 1) / 2;
            }
            ++n; // new item is inserted
        }
    }

    /**
     * Transforms a heap into a sorted array
     * pre: All items in the array are in heap order.
     * post: The array is sorted.
     * @param arr The array to be sorted
     */
    private static <E extends Comparable<E>> void shrinkHeap(E[] arr) {
        int n = arr.length; // size of the heap
        while (n > 0) {
            --n; // one item is extracted 
            // swap fist and last item of heap
            swap(arr, 0, n);
            int parent = 0;
            while (true) {
                // find the larger child
                int leftChild = 2 * parent + 1;
                if (leftChild >= n)
                    break; // no child
                int maxChild = leftChild;
                int rightChild = leftChild + 1;
                // find the larger of the two children.
                if (rightChild < n && arr[rightChild].compareTo(arr[leftChild]) > 0)
                    maxChild = rightChild;
                // check the max heap property
                if (arr[parent].compareTo(arr[maxChild]) < 0) {
                    // swap the parent and child
                    swap(arr, parent, maxChild);
                    // continue at child level
                    parent = maxChild;
                }
                else // max heap property is restored
                    break;
            }
        }
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
