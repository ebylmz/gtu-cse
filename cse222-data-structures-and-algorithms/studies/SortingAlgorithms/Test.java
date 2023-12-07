import java.util.Random;

public class Test {
    private static final int MAX_VALUE = 1000;
    private static final int SET_SIZE = 100;

    public static void main(String[] args) {
        // create the data set
        Integer[] dataset = createRandomDataSet(SET_SIZE, MAX_VALUE);
        // sort and display the array content
        // BubbleSort.sort(dataset);
        // InsertionSort.sort(dataset);
        // SelectionSort.sort(dataset);
        // MergeSort.sort(dataset);
        // QuickSort.sort(dataset);
        // HeapSort.sort(dataset);
        ShellSort.sort(dataset);
        System.out.println(toStringArray(dataset));
    }

    private static Integer[] createRandomDataSet(int size, int max) {
        Integer[] dataset = new Integer[size];
        Random rand = new Random();
        for (int i = 0; i < size; ++i)
            dataset[i] = rand.nextInt(max);
        return dataset;
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
