package src.bst;

import java.io.Serializable;
import java.util.Scanner;

public class BinaryTree<E> implements Serializable {
    /** Root of the binary tree */
    protected Node<E> root;

    /** Constructs a new empty BinaryTree */
    public BinaryTree() {
        root = null;
    }

    /**
     * Construts a new BinaryTree with given root node
     * @param root Root node of the tree
     */
    protected BinaryTree(Node<E> root) {
        this.root = root;
    }

    /**
     * Constructs a new BinaryTree with given root data and it's subtrees 
     * @param data Data at root
     * @param leftTree Left child/subtree
     * @param rightTree Right child/subtree
     */
    public BinaryTree(E data, BinaryTree<E> leftTree, BinaryTree<E> rightTree) {
        root = new Node<E>(data);
        root.left = leftTree != null ? leftTree.root : null; 
        root.right = rightTree != null ? rightTree.root : null;
    }

    /**
     * Returns the left subtree
     * @return Left subtree
     * @throws NullPointerException Throws NullPointerException if the tree is empty
     */
    public BinaryTree<E> getLeftSubtree() throws NullPointerException {
        checkRoot();
        return new BinaryTree<>(root.left);
    }

    /**
     * Returns the right subtree
     * @return Right subtree
     * @throws NullPointerException Throws NullPointerException if the tree is empty
     */
    public BinaryTree<E> getRightSubtree() throws NullPointerException {
        checkRoot();
        return new BinaryTree<>(root.right);
    }

    /**
     * Modifies the left subtree of this tree
     * @param tree Left subtree
     * @return Previos left subtree
     * @throws NullPointerException Throws NullPointerException if the root of the tree is null
     */
    public BinaryTree<E> setLeftSubtree(BinaryTree<E> tree) throws NullPointerException {
        checkRoot();
        var r = new BinaryTree<>(root.left);
        root.left = tree.root;
        return r;
    }

    /**
     * Modifies the right subtree of this tree
     * @param tree Right subtree
     * @return Previos right subtree
     * @throws NullPointerException Throws NullPointerException if the root of the tree is null
     */
    public BinaryTree<E> setRightSubtree(BinaryTree<E> tree) throws NullPointerException {
        checkRoot();
        var r = new BinaryTree<>(root.right);
        root.right = tree.root;
        return r;
    }

    /**
     * The number of items that this tree contain
     * @return Number of items
     */
    public int getNumItem() {
        return getNumNodes(root);        
    }

    /**
     * Returns the number of node in the subtree
     * @param localRoot Root of the subtree
     * @return The number of nodes
     */
    private int getNumNodes(Node<E> localRoot) {
        return (localRoot == null) ?
            0 : 1 + getNumNodes(localRoot.left) + getNumNodes(localRoot.right);
    }

    /**
     * Returns the data of the tree
     * @return Data at the root
     * @throws NullPointerException Throws NullPointerException if the root of the tree is null
     */
    public E getData() throws NullPointerException {
        checkRoot();
        return root.data;
    }

    /**
     * Checks whether this tree is a leaf or not
     * @return True if this tree has no child
     */
    public boolean isLeaf() {
        checkRoot();
        return root.right == null && root.left == null;
    }

    /**
     * Checks whether the tree is empty or not
     * @return True if the tree has no item, false otherwise
     */
    public boolean isEmpty() {
        return root == null;
    }

    /*** Converts a tree to string */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        toString(root, 1, sb);
        return sb.toString();
    }

    /**
     * Converts a sub-tree to string 
     * @param localRoot Local root
     * @param depth Depth of the sub-tree
     * @param sb String builder to concatenate the string representation of all datas
     */
    private void toString(Node<E> localRoot, int depth, StringBuilder sb) {
        if (localRoot == null) {
            addMargin(sb, depth);
            // add some margin to indicate depth/level with null sign
            sb.append("\\0\n"); 
        }
        else {
            // convert right subtree to string
            toString(localRoot.right, depth + 1, sb);
            // add some margin to indicate depth/level
            addMargin(sb, depth);
            sb.append(localRoot.data);
            sb.append("\n");
            // convert left subtree to string
            toString(localRoot.left, depth + 1, sb);
        }
    }

    /**
     * Adds horizontal margin for toString method
     * @param sb String Builder
     * @param i Amount of space
     */
    private void addMargin(StringBuilder sb, int i) {
        while (i > 0) {
            if (i == 1)
                sb.append("  ••• ");
            else
                sb.append("      "); // 6 blank
            --i;
        }
    
    }

    /**
     * Construct a BinaryTree which 
     * pre: The input consists of a preorder traversal of the binary tree. 
     * The line "null" indicates a null tree.
     * @param scan Scanner opened with the input file
     * @return The binary tree 
     */
    public static BinaryTree<String> readBinaryTree(Scanner scan) {
        // read a line and trim leading and trailing spaces
        String data = scan.nextLine().trim();
        if (data.equals("null"))
            return new BinaryTree<String>(null);
        else {
            // first scan and generate the childs and then link them with their parents 
            BinaryTree<String> leftTree = readBinaryTree(scan);
            BinaryTree<String> rightTree = readBinaryTree(scan);
            return new BinaryTree<>(data, leftTree, rightTree);
        } 
    }
    
    /**
     * Checks whether root is null or not 
     * @throws NullPointerException Throws if the three is empty (root is null)
     */
    private void checkRoot() throws NullPointerException {
        if (root == null)
            throw new NullPointerException("Empty tree, root is null");
    }

    /*** Protected static class to be able to extending subclasses later */
    protected static class Node<E> {
        protected E data;
        protected Node<E> parent;
        protected Node<E> right;
        protected Node<E> left;

        /**
         * Construct an node with given parent node
         * @param data The data
         * @param parent Parent of the current node
         */
        public Node(E data, Node<E> parent) {
            this.parent = parent;
            this.data = data;
            right = null;
            left = null;
        }

        /**
         * Constructs a new Node with given data and null links
         * @param data Data of the node
         */
        public Node(E data) {
            this(data, null);
        }

        /**
         * Swaps the data of this node and other node
         * @param other Other node
         */
        public void swap(Node<E> other) {
            E tmp = this.data;
            this.data = other.data;
            other.data = tmp;            
        }

        @Override
        public String toString() {
            return data.toString();
        }
    }   
}