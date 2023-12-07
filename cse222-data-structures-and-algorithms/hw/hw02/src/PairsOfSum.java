import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Scanner;

class PairsOfSum {
    public static void main(String[] args) {
        ArrayList<String> list = new ArrayList<>();
        for (int i = 10; i <= 10000; i *= 10)
            list.add(test(i));
        
        // print the result
        for (var r : list)
            System.out.println(r);
    }

    public static String test(int size) {
        int[] arr = createArray(size);
        
        long startTime = System.nanoTime();
        pairsOfSumLoop(arr, 25);
        // pairsOfSumRecursive(arr, 25, 0);
        long stopTime = System.nanoTime();

        return String.format("Running time for problem size %7d: %15.2f", size, (stopTime - startTime) / 1000.0);
    }

    public static void pairsOfSumLoop(int[] arr, int sum) {
        int count = 0;
        for (int i = 0; i < arr.length; ++i) {
            int target = sum - arr[i];
            for (int j = i + 1; j < arr.length; ++j)
                if (arr[j] == target)
                    ++count;
                    // System.out.printf("Pair (%d, %d)\n", arr[i], target);
        }
    }

    public static void pairsOfSumRecursive(int[] arr, int sum, int index) {
        int count = 0;
        if (index < arr.length - 1) {
            int target = sum - arr[index];
            for (int i = index + 1; i < arr.length; ++i)
                if (arr[i] == target)
                    ++count;
                    // System.out.printf("Pair (%d, %d)\n", arr[index], target);
            pairsOfSumRecursive(arr, sum, index + 1);
        }
    }

    public static int[] createArray(int size) {
        int[] arr = new int[size];

        try (Scanner scanner = new Scanner(new File("random.txt"))) {
            for (int i = 0; i < size && scanner.hasNext(); ++i)
                arr[i++] = scanner.nextInt();
            scanner.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }

        return arr;
    }

}
