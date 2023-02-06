package src;

import java.util.ArrayList;

    /** TEST CASES
     * 1. Insert item to the heap
     * 2. Get the min value of the heap with extractMin method
     * 3. Incerement an key priority
     * 4. Merge two heap
     */

public class TestBinaryHeap {
    public static void test1() {
        BinaryHeap<Integer> h = new BinaryHeap<>();
        int[] items = {12, 334, 53, 89, 1, 4, 54, 123, 324, 54, 12, 4, 4, 32, 23, 14, 89, 11, 789, 23, 421, 432, 67};

        System.out.println(h);
        System.out.println("Initially the heap is empty\n");
        for (var k : items) {
            System.out.printf("INSERT %d\n", k);
            h.insert(k);
            System.out.println(h);
        }
        System.out.printf("\nsize: %d\n", h.size());

        int target1 = 12; // not exist in tree
        int target2 = 25; // exist in tree
        System.out.printf("contains %-3d: %b\n", target1, h.contains(target1));
        System.out.printf("contains %-3d: %b\n", target2, h.contains(target2));

        // extract the top item of heap and add it to the arrayList to observe
        // the items ordered in increasing order (HeapSort) 
        ArrayList<Integer> list = new ArrayList<>();
        while (! h.isEmpty()) {
            System.out.printf("MinValue: %d\nExtract min\n\n", h.getMin());
            list.add(h.extractMin());
            System.out.println(h);
            System.out.printf("new size: %d\n\n", h.size());
        }

        System.out.printf("contains %-3d: %b\n", target1, h.contains(target1));
        System.out.printf("contains %-3d: %b\n", target2, h.contains(target2));

        // all the heap content
        System.out.println(list);
    }

    public static void test2() {
        // create two heap
        BinaryHeap<String> h1 = new BinaryHeap<>();
        h1.insert("meat");
        h1.insert("peanut");
        h1.insert("egg");
        h1.insert("cheese");
        h1.insert("hamburger");
        h1.insert("hazalnut");
        h1.insert("almond");
        h1.insert("milk");
        System.out.println("heap1: ");
        System.out.println(h1);

        BinaryHeap<String> h2 = new BinaryHeap<>();
        h2.insert("apple");
        h2.insert("orange");
        h2.insert("banana");
        h2.insert("avocado");
        h2.insert("blueberry");
        h2.insert("strawberry");
        h2.insert("watermelon");
        h2.insert("pineapple");
        System.out.println("heap2: ");
        System.out.println(h2);

        var merged = h1.merge(h2);
        System.out.println("Merge heap1 and heap2: ");
        System.out.println(merged);

        if (merged.incrementKey("egg", "sausage"))
            System.out.println("Key value 'egg' incremented to 'sausage'");
        else
            System.out.println("Key value 'egg' cannot be incremented to 'sausage'");
        System.out.println(merged);
        if (merged.incrementKey("blue", "purple"))
            System.out.println("Key value 'blue' incremented to 'blue'");
        else
            System.out.println("Key value 'blue' cannot be incremented to 'blue'");
        System.out.println(merged);
    }
}
