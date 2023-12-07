package src;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;

public class Q3 {
    /**
     * Finds subarray(s) that the sum of its items is equal to a given integer value
     * @param arr an integer array
     * @param targetSum sum of subarray
     * @return number of subarray
     */
    public static ArrayList<ArrayList<Integer>> subArr(int[] arr, int targetSum) {
        Deque<Integer> q = new ArrayDeque<>();
        subArr(arr, q, targetSum, 0);
        ArrayList<ArrayList<Integer>> list = new ArrayList<>();
        while (!q.isEmpty()) {
            int start = q.poll();
            int end;
            do {
                end = q.poll();
                list.add(createSubArr(arr, start, end));
            } while (!q.isEmpty() && end < q.peek());
        } 
        return list;
    }

    /**
     * Finds subarray(s) that the sum of its items is equal to a given integer value
     * @param arr an integer array
     * @param targetSum sum of subarray
     * @param index starting index of the subarray
     * @return number of subarray
     */
    private static void subArr(int[] arr, Deque<Integer> q, int targetSum, int index) {
        if (index < arr.length) {
            // offer starting position of subarray(s)
            q.offer(index);
            // if no subarray exist then pool the starting index
            if (!findSubArrays(arr, q, targetSum, 0, index))
                q.pollLast();
            // continue searching subarray with next index
            subArr(arr, q, targetSum, index + 1);
        }
    } 

    public static ArrayList<Integer> createSubArr(int arr[], int start, int end) {
        ArrayList<Integer> list = new ArrayList<>();
        createSubArr(arr, list, start, end);
        return list;
    }

    private static void createSubArr(int arr[], ArrayList<Integer> list, int start, int end) {
        if (start <= end) {
            list.add(arr[start]);
            createSubArr(arr, list, start + 1, end);
        }
    }

    /**
     * Adds last index of the proper subarrays to qiven queue
     * @param arr an integer array
     * @param q queue to hold the data
     * @param targetSum sum of all the items in subarray
     * @param sum current sum of items
     * @param index starting position of subarray
     * @return true if there exist at least one subarray, otherwise returns false
     */
    private static boolean findSubArrays(int[] arr, Deque<Integer> q, int targetSum, int sum, int index) {
        boolean r = false;
        if (index < arr.length) {
            sum += arr[index];
            if (sum == targetSum) {
                q.offer(index);
                r = true;
            }
            return r || findSubArrays(arr, q, targetSum, sum, index + 1);           
        }
        return r;
    }
    
    public static void test() {
        System.out.println("\n================ Q3 TEST ================");

        int[] arr1 = {10, 4, 5, 3, -12, 9, 2, 1, -7};        
        int targetSum = 16;
        var list = subArr(arr1, targetSum);
        System.out.println("\nT1:");
        for (var l : list)
            System.out.println(l);

        // T2.	There exists more than one sub array which start at same index.
        System.out.println("\nT2.1:");
        int[] arr2 = {1, 4, -7, 2, 0, 1, 9, 2, -13, 5 -1};
        targetSum = 3;
        list = subArr(arr2, targetSum);
        for (var l : list)
            System.out.println(l);

        System.out.println("\nT2.2:");
        targetSum = 2;
        list = subArr(arr2, targetSum);
        for (var l : list)
            System.out.println(l);

        System.out.println("================ END ================");
    }
}