

public class BinarySearchTreeWithRotate<E extends Comparable<E>> extends BinarySearchTree<E> {
    /**
     * Performs a right rotation
     * pre: localRoot is the root of the binary search tree
     * post: localRoot.left is the root of the binary search tree
     * @param localRoot The root of the binary search tree to be rotated
     * @return The new root of the rotate tree
     */
    protected Node<E> rotateRight(Node<E> localRoot) {
        Node<E> temp = localRoot.left; 
        localRoot.left = temp.right;
        temp.right = localRoot;
        return temp; // return new root
    }

    /**
     * Performs a left rotation
     * pre: localRoot is the root of the binary search tree
     * post: localRoot.right is the root of the binary search tree
     * @param localRoot The root of the binary search tree to be rotated
     * @return The new root of the rotate tree
     */
    protected Node<E> rotateLeft(Node<E> localRoot) {
        Node<E> temp = localRoot.right;
        localRoot.right = temp.left;
        temp.left = localRoot;
        return temp; // return new root
    }
}
