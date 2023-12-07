package src.sorting;

public class NewSort {
    /**
     * Sorts the given part of the array in ascending order
     * @param arr Array
     */
    public static <E extends Comparable<E>> void sort(E[] arr) {
        sort(arr, 0, arr.length - 1);
    }

    /**
     * Sorts the given part of the array in ascending order
     * @param arr Array
     * @param l Left boundry
     * @param r Right boundry
     */
    public static <E extends Comparable<E>> void sort(E[] arr, int l, int r) {
        if (l < r) {
            int[] extreme = findMinMax(arr, l, r);
            swap(arr, l, extreme[0]);
            swap(arr, r, extreme[1]);
            sort(arr, l + 1, r - 1);
        }
    }

    /**
     * Find extereme values of the arr in given boundries
     * @param arr Array
     * @param l Left boundry
     * @param r Right boundry
     * @return extreme min: extreme[0], max: extreme[1]
     */
    public static <E extends Comparable<E>> int[] findMinMax(E[] arr, int l, int r) {
        int[] extreme = new int[2]; 
        if (isSmaller(arr[l], arr[r])) {
            extreme[0] = l;
            extreme[1] = r;
        }
        else {
            extreme[0] = r;
            extreme[1] = l;
        }

        findMinMax(arr, extreme, l + 1, r - 1);
        return extreme;
    }

    /**
     * Find extereme values of the arr in given boundries
     * @param arr Array
     * @param extreme min: extreme[0], max: extreme[1]
     * @param l Left boundry
     * @param r Right boundry
     */
    public static <E extends Comparable<E>> 
    void findMinMax(E[] arr, int[] extreme, int l, int r) {
        if (l <= r) {
            int mini = l;
            int maxi = r;
            if (isSmaller(arr[r], arr[l])) {
                mini = r;
                maxi = l;
            }

            if (isSmaller(arr[mini], arr[extreme[0]]))
                extreme[0] = mini;
            if (isSmaller(arr[extreme[1]], arr[maxi]))
                extreme[1] = maxi;
            findMinMax(arr, extreme, l + 1, r - 1);
        }
    }

    /**
     * Determine whether a is smaller than b
     * @param <E>
     * @param a An item 
     * @param b An item 
     * @return  True if a is smaller than b
     */
    private static <E extends Comparable<E>> boolean isSmaller(E a, E b) {
        return a.compareTo(b) < 0;
    }

    /**
     * Swap two array item
     * @param arr Array
     * @param i An index
     * @param j An index
     */
    private static <E> void swap(E[] arr, int i, int j) {
        E tmp = arr[i];
        arr[i] = arr[j];
        arr[j] = tmp;
    }
}