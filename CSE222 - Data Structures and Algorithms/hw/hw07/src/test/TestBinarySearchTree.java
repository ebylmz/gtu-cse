package src.test;

import java.util.Random;

import src.bst.BinarySearchTree;
import src.bst.BinaryTree;

public class TestBinarySearchTree {
    /** Number of items inside the data set */
    private static final int SET_SIZE = 9;
    /** Max number can be generated with Rand.nextInt() */
    private static final int MAX_RAND = 100;

    /**
     * Tests the static method createBST
     */
    public static void test1() {
        // TEST CASE 1:
        // create a skewed binary tree
        BinarySearchTree<Integer> t1 = new BinarySearchTree<>();
        Integer[] treeset = {8, 7, 6, 5, 4, 3, 2, 1, 0};
        // add the items in the treeset to BST
        for (var e : treeset)
            t1.add(e);
        Integer[] s1 = {12, 42, 13, 65, 17, 35, 98, 5, 71};
        var bst = BinarySearchTree.createBST(t1, s1);
        
        System.out.println("\nSkewed Binary Tree");
        System.out.println("============================================================");
        System.out.println(t1);
        System.out.println("Generated Binary Search Tree");
        System.out.println("============================================================");
        System.out.println(bst);

        // TEST CASE 2:
        // create a non-skewed binary tree and data set
        BinaryTree<Integer> t2 = createRandomBinaryTree(SET_SIZE);
        Integer[] s2 = createDataSet(SET_SIZE);
        // generate the bst
        bst = BinarySearchTree.createBST(t2, s2);
        // compare binary tree and generated binary search tree 
        System.out.println("\nNon-Skewed Binary Tree");
        System.out.println("============================================================");
        System.out.println(t2);
        System.out.println("Generated Binary Search Tree");
        System.out.println("============================================================");
        System.out.println(bst);

        // TEST CASE 3:
        // create a random binary tree and data set
        BinaryTree<Integer> t3 = createRandomBinaryTree(SET_SIZE);
        Integer[] s3 = createDataSet(SET_SIZE);
        // generate the bst
        bst = BinarySearchTree.createBST(t3, s3);
        // compare binary tree and generated binary search tree 
        System.out.println("\nRandomly Generated Binary Tree");
        System.out.println("============================================================");
        System.out.println(t3);
        System.out.println("Generated Binary Search Tree");
        System.out.println("============================================================");
        System.out.println(bst);
    } 

    /**
     * Tests the public method balance
     */
    public static void test2() {
        // TEST CASE 1:
        // create a skewed BST
        Integer[] s1 = {1, 2, 3, 4, 5, 6, 7, 8, 9};
        BinarySearchTree<Integer> t1 = new BinarySearchTree<>();
        // add the items inside s1 to t1
        for (var e : s1)
            t1.add(e);
        // display the current BST
        System.out.println("\nSkewed Binary Search Tree");
        System.out.println("============================================================");
        System.out.println(t1);
        // balance and display the tree
        t1.convertAVL();
        System.out.println("\nConverted AVL Tree");
        System.out.println("============================================================");
        System.out.println(t1);

        // TEST CASE 2:
        Integer[] s2 = {4, 2, 18, 5, 12, 11, 8, 7, 17, 4};
        BinarySearchTree<Integer> t2 = new BinarySearchTree<>();
        for (var e : s2)
            t2.add(e);
        // display the current BST
        System.out.println("\nNon-Skewed Unbalanced Binary Search Tree");
        System.out.println("============================================================");
        System.out.println(t2);
        // balance and display the tree
        t2.convertAVL();
        System.out.println("\nConverted AVL Tree");
        System.out.println("============================================================");
        System.out.println(t2);

        // TEST CASE 3:
        Random rand = new Random();
        // create a bst which is filled with random integer values 
        BinarySearchTree<Integer> t3 = new BinarySearchTree<>();
        for (int i = 0; i < SET_SIZE; ++i)
            while (!t3.add(rand.nextInt(MAX_RAND))); // make sure to insert an item
        // display the current BST
        System.out.println("\nRandomly Generated Unbalanced Binary Search Tree");
        System.out.println("============================================================");
        System.out.println(t3);
        // balance and display the tree
        t3.convertAVL();
        System.out.println("\nConverted AVL Tree");
        System.out.println("============================================================");
        System.out.println(t3);
    }

    /**
     * Creates a random binary tree
     * @param n The number of item in the tree
     * @return Randomly generated binary tree
     */
    private static BinaryTree<Integer> createRandomBinaryTree(int n) {
        // keeps the number of nodes that's going to be inserted
        int[] remain = new int[1];
        Random rand = new Random();
        BinaryTree<Integer> t = null;
        do {
            remain[0] = n; 
            // create n nodes binary tree
            t = createRandomBinaryTree(rand, remain);
            // make sure the new generated tree has exactly  
            // n nodes, so there must be no remained node
        } while(remain[0] != 0); 
        return t;
    }   

    private static BinaryTree<Integer> createRandomBinaryTree(Random rand, int[] size) {
        if (size[0] == 0)
            return null;
        else {
            --size[0]; // new item is inserted
            
            return new BinaryTree<>(
                rand.nextInt(MAX_RAND),
                (size[0] > 0 && rand.nextInt(2) == 1) ? createRandomBinaryTree(rand, size) : null, 
                (size[0] > 0 && rand.nextInt(2) == 1) ? createRandomBinaryTree(rand, size) : null);
        }
    }

    /**
     * Creates an data set which is filled with random integer values
     * @param size Number of item inside the set
     * @return The data set
     */
    private static Integer[] createDataSet(int size) {
        Integer[] dataset = new Integer[size];
        Random rand = new Random();
        for (int i = 0; i < size; ++i)
            dataset[i] = rand.nextInt(MAX_RAND);
        return dataset;
    }
}
