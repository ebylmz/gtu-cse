//! method delete NOT IMPLEMENTED YET


public class RedBlackTree<E extends Comparable<E>> implements SearchTree<E> {
    /** Root of the red-black tree */
    Node<E> root;
    
    public RedBlackTree() {
        root = null;
    }

    @Override
    public boolean add(E item) {
        if (root == null) {
            root = new Node<E>(item);
            root.isRed = false;
            return true;
        } 
        else {
            Node<E> c = root; // child
            Node<E> p = root; // parent
            // find a proper place for new item
            while (c != null) {
                System.out.println("search for " + item + " ----");
                p = c;
                int comp = item.compareTo(c.data);
                if (comp < 0)
                    c = c.left;
                else if (comp > 0)
                    c = c.right;
                else // item is already placed in the tree
                    return false;
            }
            // here c is null 
            c = new Node<E>(item);
            // link child to the parent
            c.parent = p;
            // link parent to the child
            if (item.compareTo(p.data) < 0)
                p.left = c;
            else
                p.right = c;
            // check balance of the tree
            checkBalance(c);
            return true;
        }
    }

    private void checkBalance(Node<E> c) {
        // get the parent node
        Node<E> p = c.parent; 
        if (p == null) // root must be black
            c.isRed = false;
        else if (p.isRed) {
            // get the grandparent node
            //if p is red gp must be exist, o.w. root becomes red and this is invalid in rbt
            Node<E> gp = p.parent; 
            // get the sibling node
            Node<E> sb = gp.left != p ? gp.left : gp.right; 
            // first try to balance by recoloring, if it's not
            // solved the problem apply proper rotations 

            // if both parent and his sibling is red
            if (sb != null && sb.isRed) {
                // recolor parent and his sibling black, grandparent red
                p.isRed = false;
                sb.isRed = false;
                gp.isRed = true;
                // continue to balance at grandparent level
                checkBalance(gp);
            }
            else {
                // recoloring does not solve the problem
                // determine the type of rotation tree
                if (gp.left == p) { 
                    if (p.right == c) // convert LR tree to LL tree
                        p = rotateLeft(p); 
                    // recolor parent and grandparent 
                    // then rotate right around grandparent
                    p.isRed = false;
                    gp.isRed = true;
                    gp = rotateRight(gp); 
                }
                else {
                    if (p.left == c) // convert RL tree to RR tree
                        p = rotateRight(p);
                    // recolor parent and grandparent 
                    // then rotate left around grandparent
                    p.isRed = false;
                    gp.isRed = true;
                    gp = rotateLeft(gp); 
                }
                // continue rebalancing from grandparent level
                checkBalance(gp);
            }
        }

    }

    @Override
    public boolean contains(E target) {
        return find(root, target) != null;
    }

    @Override
    public E find(E target) {
        return find(root, target);
    }

    /**
     * Search the given target item in the tree
     * @param localRoot Local root
     * @param target Target item 
     * @return If the item is found returns a referance to the target item, otherwise null
     */
    private E find(Node<E> localRoot, E target) {
        if (localRoot == null)
            return null;
        else {
            int comp = target.compareTo(localRoot.data);
            if (comp < 0) // continue to search from left subtree
                return find(localRoot.left, target);
            else if (comp > 0) // continue to search from right subtree
                return find(localRoot.right, target);
            else // target is found
                return localRoot.data;
        } 
    }

    @Override
    public E delete(E target) {
        //! NOT IMPLEMENTED YET
        return null;
    }

    @Override
    public boolean remove(E target) {
        return delete(target) != null;
    }

    /**
     * Performs a right rotation
     * pre: localRoot is the root of the binary search tree
     * post: localRoot.left is the root of the binary search tree
     * @param localRoot The root of the binary search tree to be rotated
     * @return The new root of the rotate tree
     */
    private Node<E> rotateRight(Node<E> localRoot) {        
        Node<E> newRoot = localRoot.left; 
        localRoot.left = newRoot.right;
        newRoot.right = localRoot;
        // after rotation set new root as parent of previous root 
        newRoot.parent = localRoot.parent;
        localRoot.parent = newRoot;
        // parent of previous root still points previous root
        Node<E> p = newRoot.parent; 
        if (p != null)
            if (p.left == localRoot)
                p.left = newRoot;
            else
                p.right = newRoot;
        // update the root
        if (localRoot == root)
            root = newRoot;
        return newRoot; 
    }

    /**
     * Performs a left rotation
     * pre: localRoot is the root of the binary search tree
     * post: localRoot.right is the root of the binary search tree
     * @param localRoot The root of the binary search tree to be rotated
     * @return The new root of the rotate tree
     */
    private Node<E> rotateLeft(Node<E> localRoot) {
        Node<E> newRoot = localRoot.right;
        localRoot.right = newRoot.left;
        newRoot.left = localRoot;
        // after rotation set new root as parent of previous root 
        newRoot.parent = localRoot.parent;
        localRoot.parent = newRoot;
        // parent of previous root still points previous root
        Node<E> p = newRoot.parent; 
        if (p != null)
            if (p.left == localRoot)
                p.left = newRoot;
            else
                p.right = newRoot;
        // update the root
        if (localRoot == root)
            root = newRoot;

        return newRoot; 
    }

    private static class Node<E> {
        /** Data field of this node */
        private E data;
        /** Right child of this node */
        private Node<E> right;
        /** Left child of this node */
        private Node<E> left;
        /** Parent of this node */
        private Node<E> parent;
        /** Determines whether this node is red or not */
        private boolean isRed;

        /**
         * Constructs a red Node
         * @param data The data 
         */
        public Node(E data) {
            this.data = data;
            // initially all the new nodes are red
            isRed = true;
        }
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
            sb.append(localRoot.isRed ? "(R)" : "(B)");
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
}
