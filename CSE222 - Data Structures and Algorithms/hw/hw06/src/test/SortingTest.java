package src.test;

import java.util.Random;

import src.sorting.MergeSort;
import src.sorting.NewSort;
import src.sorting.QuickSort;

public class SortingTest {
    /** Max value that random number generator can generate */
    private static int MAX_VAL = 1000;
    /** Number of data set */
    public static final int NUM_SETS = 1000;
    /** Small data set size */
    public static final int SIZE_SMALL = 100;
    /** Medium data set size */
    public static final int SIZE_MEDIUM = 1000;
    /** Large data set size */
    public static final int SIZE_LARGE = 10000;

    public static void test_validation() {
        // create a random data set and make copy of it to use same data set 
        Integer[] arr1 = generateRandomData(SIZE_SMALL);
        Integer[] arr2 = new Integer[SIZE_SMALL];
        Integer[] arr3 = new Integer[SIZE_SMALL];
        System.arraycopy(arr1, 0, arr2, 0, arr1.length);
        System.arraycopy(arr1, 0, arr3, 0, arr1.length);
        
        System.out.println("----------------- Test MergeSort -----------------");
        System.out.printf("Unsorted %s\n", toStringArray(arr1));
        MergeSort.sort(arr1);
        System.out.printf("Sorted %s\n", toStringArray(arr1));
        
        
        System.out.println("----------------- Test NewSort -----------------");
        System.out.printf("Unsorted %s\n", toStringArray(arr3));
        NewSort.sort(arr3);
        System.out.printf("Sorted %s\n", toStringArray(arr3));

        System.out.println("----------------- Test QuickSort -----------------");
        System.out.printf("Unsorted %s\n", toStringArray(arr2));
        QuickSort.sort(arr2);
        System.out.printf("Sorted %s\n", toStringArray(arr2));
    }   

    /**
     * Compares runtime performance of QuickSort, MergeSort and NewSort
     */
    public static void test_runtime() {
        // generate the data set for three different set size small, medium, large
        Integer[][][] dataSet = new Integer[3][][];
        dataSet[0] = generateRandomDataSet(NUM_SETS, SIZE_SMALL);
        dataSet[1] = generateRandomDataSet(NUM_SETS, SIZE_MEDIUM);
        dataSet[2] = generateRandomDataSet(NUM_SETS, SIZE_LARGE);

        // measure the total time to sort randomly created data
        // for three different sorting algorithms
        long[] resultMergeSort = measureMergeSort(dataSet);
        long[] resultQuickSort = measureQuickSort(dataSet);
        long[] resultNewSort = measureNewSort(dataSet);

        // System.out.printf("%-35s %-45s\n", "Sorting Algortihm", "Set Size");
        String sset = "Small(" + SIZE_SMALL + ")";
        String mset = "Medium(" + SIZE_MEDIUM + ")";
        String lset = "Large(" + SIZE_LARGE + ")";
        System.out.println("----------------------------------------------------------------------------");
        System.out.printf("\n%-20s %-20s %-20s %-20s\n\n", "Sorting Algorithm", sset, mset, lset);
        System.out.printf("%-20s %-20d %-20d %-20d\n", 
            "Merge Sort", resultMergeSort[0], resultMergeSort[1], resultMergeSort[2]);
        System.out.printf("%-20s %-20d %-20d %-20d\n", 
            "New Sort", resultNewSort[0], resultNewSort[1], resultNewSort[2]);
        System.out.printf("%-20s %-20d %-20d %-20d\n", 
            "Quick Sort", resultQuickSort[0], resultQuickSort[1], resultQuickSort[2]);
        System.out.println("----------------------------------------------------------------------------");
    }

    /**
     * Measures the total execution time for sorting given data set with MergeSort
     * @param dataSet The data set
     * @return Total execution time for 3 different set size
     * runtimes[0] = execution time for small set size
     * runtimes[1] = execution time for medium set size
     * runtimes[2] = execution time for large set size
     */
    private static long[] measureMergeSort(Integer[][][] dataSet) {
        // copy the original data set to another place
        Integer[][][] sortedSet = new Integer[3][][];
        sortedSet[0] = new Integer[NUM_SETS][SIZE_SMALL];
        sortedSet[1] = new Integer[NUM_SETS][SIZE_MEDIUM];
        sortedSet[2] = new Integer[NUM_SETS][SIZE_LARGE];
        for (int i = 0; i < dataSet.length; ++i)
            for (int j = 0; j < dataSet[i].length; ++j)
                System.arraycopy(dataSet[i][j], 0, sortedSet[i][j], 0, dataSet[i][j].length);
        // meausure the total execution time of three different data set
        long[] runTimes = new long[3];
        long startTime = 0l; 
        for (int i = 0; i < sortedSet.length; ++i) {
            startTime = System.nanoTime();
            for (int j = 0; j < sortedSet[i].length; ++j)
                MergeSort.sort(sortedSet[i][j]);
            // save the run time of the data set
            runTimes[i] = (System.nanoTime() - startTime) / (1000l * NUM_SETS);
        }
        return runTimes;
    }

    /**
     * Measures the total execution time for sorting given data set with QuickSort
     * @param dataSet The data set
     * @return Total execution time for 3 different set size
     * runtimes[0] = execution time for small set size
     * runtimes[1] = execution time for medium set size
     * runtimes[2] = execution time for large set size
     */
    private static long[] measureQuickSort(Integer[][][] dataSet) {
        // copy the original data set to another place
        Integer[][][] sortedSet = new Integer[3][][];
        sortedSet[0] = new Integer[NUM_SETS][SIZE_SMALL];
        sortedSet[1] = new Integer[NUM_SETS][SIZE_MEDIUM];
        sortedSet[2] = new Integer[NUM_SETS][SIZE_LARGE];
        for (int i = 0; i < dataSet.length; ++i)
            for (int j = 0; j < dataSet[i].length; ++j)
                System.arraycopy(dataSet[i][j], 0, sortedSet[i][j], 0, dataSet[i][j].length);
        // meausure the total execution time of three different data set
        long[] runTimes = new long[3];
        long startTime = 0l; 
        for (int i = 0; i < sortedSet.length; ++i) {
            startTime = System.nanoTime();
            for (int j = 0; j < sortedSet[i].length; ++j)
                QuickSort.sort(sortedSet[i][j]);
            // save the run time of the data set
            runTimes[i] = (System.nanoTime() - startTime) / (1000l * NUM_SETS);
        }
        return runTimes;
    }

    /**
     * Measures the total execution time for sorting given data set with QuickSort
     * @param dataSet The data set
     * @return Total execution time for 3 different set size
     * runtimes[0] = execution time for small set size
     * runtimes[1] = execution time for medium set size
     * runtimes[2] = execution time for large set size
     */
    private static long[] measureNewSort(Integer[][][] dataSet) {
        // copy the original data set to another place
        Integer[][][] sortedSet = new Integer[3][][];
        sortedSet[0] = new Integer[NUM_SETS][SIZE_SMALL];
        sortedSet[1] = new Integer[NUM_SETS][SIZE_MEDIUM];
        sortedSet[2] = new Integer[NUM_SETS][SIZE_LARGE];
        for (int i = 0; i < dataSet.length; ++i)
            for (int j = 0; j < dataSet[i].length; ++j)
                System.arraycopy(dataSet[i][j], 0, sortedSet[i][j], 0, dataSet[i][j].length);
        // meausure the total execution time of three different data set
        long[] runTimes = new long[3];
        long startTime = 0l; 
        for (int i = 0; i < sortedSet.length; ++i) {
            startTime = System.nanoTime();
            for (int j = 0; j < sortedSet[i].length; ++j)
                NewSort.sort(sortedSet[i][j]);
            // save the run time of the data set
            runTimes[i] = (System.nanoTime() - startTime) / (1000l * NUM_SETS);
        }
        return runTimes;
    }


    /**
     * Creates a randomly generated data sets
     * @param numSets The number of set
     * @param setSize Size of the each data set
     * @return Set of randomly generated values
     */
    private static Integer[][] generateRandomDataSet(int numSets, int setSize) {
        Integer[][] dataSets = new Integer[numSets][];
        // fill the array with random integer values
        for (int i = 0; i < numSets; ++i)
            dataSets[i] = generateRandomData(setSize);
        return dataSets;
    }

    /**
     * Creates a randomly generated data set
     * @param size Size of the data set
     * @return Randomly generated values
     */
    private static Integer[] generateRandomData(int size) {
        Integer[] data = new Integer[size];
        // fill the array with random integer values
        Random rand = new Random();
        for (int i = 0; i < size; ++i)
            data[i] = rand.nextInt(MAX_VAL);
        return data;
    }

    /**
     * Converts given array to string
     * @param arr Array
     * @return String format of arr "arr = {...}"
     */
    private static <E> String toStringArray(E[] arr) {
        StringBuilder sb = new StringBuilder();
        sb.append("arr = {");
        for (int i = 0; i < arr.length; ++i) {
            sb.append(String.format("%-4d ", arr[i]));
            sb.append(i + 1 != arr.length ? ", " : "}\n");
        }
        return sb.toString();
    }
}
