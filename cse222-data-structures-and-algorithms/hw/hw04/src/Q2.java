package src;
public class Q2 {
    /**
     * Finds the number of items in the array between two given integer values
     * @param nums Integer array which is sorted in increasing order
     * @param smaller smaller number 
     * @param larger larger number
     * @return number of items in the array between two given integer values or 
     *         -1 if one or both of given two numbers are not exist in array
     */
    public static int inBetween(Integer[] nums, int smaller, int larger) {
        // get the last larger target value
        int end = binarySearch(nums, larger, true, 0, nums.length - 1);
        if (end != -1) {
            // get the first smaller target value
            int start = binarySearch(nums, smaller, false, 0, end - 1);
            if (start != -1)
                return end - start - 1;
        }
        return -1;
    }

    /**
     * Binary search algorithm
     * @param <T> item type
     * @param arr items 
     * @param target searching item
     * @param lastOcc true for last occurance of the target, false for first occurance
     * @param start start index of the array
     * @param end end index of the array
     * @return index of the target item
     */
    private static <T> int binarySearch(T[] arr, Comparable<T> target, boolean lastOcc, int start, int end) {
        if (start > end)
            return -1;
        else {
            int mid = (start + end) / 2;
            int compare = target.compareTo(arr[mid]);
            if (compare == 0) { // target found
                int index = lastOcc ? 
                    binarySearch(arr, target, lastOcc, mid + 1, end) :
                    binarySearch(arr, target, lastOcc, start, mid - 1);
                return index != -1 ? index : mid;
            } 
            else if (compare < 0) // target could be left side of the mid point
                return binarySearch(arr, target, lastOcc, start, mid - 1);
            else // target could be right side of the mid point
                return binarySearch(arr, target, lastOcc, mid + 1, end);
        }
    }

    /** Tests inBetween method */
    public static void test() {
        System.out.println("\n================ Q2 TEST ================");
        Integer[] nums1 = {0, 1, 3, 4, 5, 6, 7, 8, 9};
        Integer[] nums2 = {0, 1, 2, 3, 3, 3, 4, 5, 6, 7, 7, 7, 7, 8, 9};
        
        // T1.	Given array doesn’t contain one of the given integer values.
        System.out.println("\nT1:");
        int smaller = 2;
        int larger = 7;
        int result = inBetween(nums1, smaller, larger);
        System.out.printf("In between %d and %d, result: %d\n", smaller, larger, result);
    
        // T2.	Wrongly called method (smaller value shouldn’t be larger than larger one)
        System.out.println("\nT2:");
        smaller = 3;
        result = inBetween(nums1, larger, smaller);
        System.out.printf("In between %d and %d, result: %d\n", larger, smaller, result);

        // T3.	Given two integer value are same.
        System.out.println("\nT3:");
        result = inBetween(nums1, smaller, smaller);    // no dublication
        System.out.printf("In between %d and %d, result: %d\n", smaller, smaller, result);
        result = inBetween(nums2, smaller, smaller);    // dublication exist
        System.out.printf("In between %d and %d, result: %d\n", smaller, smaller, result);

        // T4.	Array contains no duplication.
        System.out.println("\nT4:");
        result = inBetween(nums1, smaller, larger);    // dublication exist
        System.out.printf("In between %d and %d, result: %d\n", smaller, larger, result);
        
        // T5.	Array contains duplicates values for given two integers.
        System.out.println("\nT5:");
        smaller = 3;
        larger = 7;
        result = inBetween(nums2, smaller, larger);  
        System.out.printf("In between %d and %d, result: %d\n", smaller, larger, result);
        System.out.println("================ END ================");
    }
}
