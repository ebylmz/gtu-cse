package src.test;

import java.util.Random;
import java.util.StringTokenizer;

import src.heap.Heap;

public class TestHeap {
    /** Number of items inside the data set */
    private static final int SET_SIZE = 30;
    /** Max number can be generated with Rand.nextInt() */
    private static final int MAX_RAND = 1000;

    public static void test1() {
        Heap<Integer> heap = new Heap<>();
        // insert random integer values into the heap
        Random rand = new Random();
        for (int i = 0; i < SET_SIZE; ++i) {
            heap.insert(rand.nextInt(MAX_RAND));
        }
            // set[i] = rand.nextInt(MAX_RAND);
        // extracts and print the items
        while(! heap.isEmpty())
            System.out.println(heap.extractMin());
    }

    public static void test2() {
        Heap<String> heap = new Heap<>();
        String sentence = "The quick brown fox jumps over the lazy dog";
        StringTokenizer st = new StringTokenizer(sentence);
        // add all the words into the heap
        while(st.hasMoreTokens())
            heap.insert(st.nextToken());
        // extracts and print the items
        while(! heap.isEmpty())
            System.out.println(heap.extractMin());
    }
}
