package src;

public class TestBinarySearchTree {
    /** TEST CASES
     * 1. Create an binary search tree by using add methods
     * 1.1. Try to add an item which is already exist in the tree
     * 2. Check if tree contains given item
     * 3. Search the given item in the tree
     * 4. Remove an item from the tree
     * 4.1. Remove an node which is not exist in the tree
     * 4.1. Remove an node which has no child
     * 4.2. Remove an node which has one child
     * 4.3. Remove an node which has two child
     * 5. Destroy the tree  
     */

    public static void test1() {
        BinarySearchTree<Integer> t = new BinarySearchTree<>();
        int[] vals = {36, 25, 9, 49, 61, 1, 4, 16};

        System.out.println(t);
        System.out.println("Initially tree is empty\n");
        for (var v : vals) {
            System.out.printf("ADD %d\n\n", v);
            boolean r = t.add(v);
            System.out.println(t);
            if (r)
                System.out.printf("%d is added properly\n\n", v);
            else
                System.out.printf("%d isn't added\n\n", v);
        }

        int target1 = 12; // not exist in tree
        int target2 = 25; // exist in tree
        System.out.printf("contains %-3d: %b\n", target1, t.contains(target1));
        System.out.printf("contains %-3d: %b\n", target2, t.contains(target2));

        target1 =  112; // not exist in tree
        target2 =  49; // exist in tree
        System.out.printf("find %-3d: %b\n", target1, t.find(target1));
        System.out.printf("find %-3d: %b\n", target2, t.find(target2));
    }

    public static void test2() {
        BinarySearchTree<String> t = new BinarySearchTree<>();
        t.add("H");
        t.add("D");
        t.add("B");
        t.add("A");
        t.add("C");
        t.add("F");
        t.add("E");
        t.add("G");
        t.add("L");
        t.add("J");
        t.add("I");
        t.add("K");
        t.add("N");
        t.add("M");
        t.add("O");
        t.add("P");
        
        System.out.println(t);

        // 4.1. Remove an node which is not exist in the tree
        String[] targets = {
            "X", // 4.3. Remove an node which is not in tree 
            "K", // 4.1. Remove an node which has no child 
            "O", // 4.2. Remove an node which has one child 
            "N"  // 4.2. Remove an node which has two child
        };

        for (var target : targets) {
            System.out.printf("contains %s: %b\n", target, t.contains(target));
            t.remove(target);
            System.out.printf("contains %s: %b\n", target, t.contains(target));
        }
        System.out.println();

        System.out.println(t);

        System.out.println("Destroy the tree");
        t.destroy();
        System.out.println(t);
    }
}
