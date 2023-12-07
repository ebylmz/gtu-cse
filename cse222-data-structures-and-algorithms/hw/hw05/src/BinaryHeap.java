package src;

public class BinaryHeap<E extends Comparable<E>> extends BinaryTree<E> {    
    /** The number of keys that heap contains */
    private int size;

    /** Construct an empty BinaryHeap */
    public BinaryHeap() {
        size = 0;
        root = null;
    }

    /**
     * Inserts the given key to the heap
     * @param key Key value
     * @return True, if the insertion done succesfully
     */
    public boolean insert(E key) {
        if (size == 0) { // insert root
            root = new Node<E>(key);
            ++size;
            return true;
        }
        else {
            int h = isPerfect() ? height() + 1 : height();
            ++size;
            var parent = getParentOf(root, h, size);
            Node<E> newnode = new Node<E>(key);
            newnode.parent = parent;
            if (parent.left == null)
                parent.left = newnode;
            else
                parent.right = newnode;
            heapifyUp(newnode);
        }
        return true;
    }

    /**
     * Makes sure that the given node don't violate the min-heap property
     * by swapping child's key with its parent by recursively till child has 
     * higher priority (larger key value) than its parent 
     * @param child Child node
     */
    private void heapifyUp(Node<E> child) {
        var parent = child.parent;
        if (parent != null) {
            int comp = child.data.compareTo(parent.data);
            // make sure parent has smaller key value
            if (comp < 0) {
                // swap data of child and parent
                child.swap(parent);
                heapifyUp(parent);
            }
        }
    }

    /**
     * Makes sure that the given node don't violate the min-heap property
     * by swapping parent's key with its child by recursively till parent has
     * lower priority (smaller key value) than its children 
     * @param parent Parent node
     */
    private void heapifyDown(Node<E> parent) {
        Node<E> lc = parent.left;
        Node<E> rc = parent.right;
        Node<E> min = null;
        if (lc != null || rc != null) {
            // get the smaller child
            if (lc != null && rc != null) 
                min = (lc.data.compareTo(rc.data) < 0) ? lc : rc;
            else 
                min = lc != null ? lc : rc;
            // check if parent has larger key value than one of its child
            if (parent.data.compareTo(min.data) > 0) {
                // continue to heapify to maintain min-heap property
                parent.swap(min);
                heapifyDown(min); 
            }
        }
    }

    /**
     * Gets the parent of the given target position
     * Pre: given root is not an empty subtree
     * @param root Local root
     * @param h Height of the tree during insertion (if the tree is perfect, it should be h + 1)
     * @param tp Target position (according to level order traversal)
     * @return Always returns true, except out of memory
     */
    private Node<E> getParentOf(Node<E> root, int h, int tp) {
        // if the target position can be reached by root node, apply insertion 
        // if (tp == 2 || tp == 3) 
        if (tp <= 3) 
            return root;
        else {
            // find the order of the target position in current level
            // by subtracting the number of node at height h - 1 tree 
            int levelOrder = tp - totalNodeHeight(h - 1);
            // in each recursive call choose the proper direction to
            // reach last insertion point which is given as tp
            int preNodes = totalNodeHeight(h - 2);
            int levelNodes = totalNodeLevel(h); 
            if (levelOrder <= levelNodes / 2) 
                return getParentOf(root.left, h - 1, preNodes + levelOrder);
            else {
                int nextpos = preNodes;
                if (levelOrder == levelNodes)
                    nextpos += levelOrder / 2;
                else 
                    nextpos += levelOrder % (levelNodes / 2);
                return getParentOf(root.right, h - 1, nextpos);
            }
        }
    }

    /**
     * Returns the minimum key value of the heap
     * @return Minumum key value of the heap (top of the heap) 
     */
    public E getMin() throws NullPointerException {
        if (root == null)
            throw new NullPointerException("Empty heap");
        return root.data;
    }

    /**
     * Extracts and returns the minimum key value of the heap
     * @return Minumum key value of the heap (top of the heap) 
     */
    public E extractMin() throws NullPointerException {
        E retVal = getMin();
        if (size == 1) 
            root = null;
        else {
            var p = getParentOf(root, height(), size); 
            Node<E> rightest = (p.right != null) ? p.right : p.left;
            root.data = rightest.data;
            // remove the rightest node
            if (p.right == rightest) 
                p.right = null;
            else
                p.left = null;
            heapifyDown(root);
        }
        --size; // one key removed
        return retVal;
    }

    /**
     * Sets the key priority to larger priority  
     * @param oldKey Existing key
     * @param newKey  key
     * @return False if the old key is not found, otherwise true
     * @throws IllegalArgumentException If the newKey priority less than oldKey
     */
    public boolean incrementKey(E oldKey, E newKey) throws IllegalArgumentException {
        int comp = newKey.compareTo(oldKey);
        if (comp < 0)
            throw new IllegalArgumentException("New key cannot have smaller priority than the existing key");
        else if (comp == 0)
            return true;
        else {
            // get the key node and replace it with the newKey
            var node = find(root, oldKey);
            if (node == null)
                return false;
            node.data = newKey;
            // since new key value has larger priority than its parent 
            // min-heap property doesn't violated for above heap but 
            // for down heap it's required to check 
            heapifyDown(node);
            return true;
        }
    }

    /**
     * Merges this heap and other heap and returns the new created heap
     * @param other Other heap
     * @return Merged heap
     */
    public BinaryHeap<E> merge(BinaryHeap<E> other) {
        // convert two heap to 
        var h1 = toArray();
        var h2 = other.toArray();
        BinaryHeap<E> merged = new BinaryHeap<>();
        for (var key : h1)
            merged.insert(key);
        for (var key : h2)
            merged.insert(key);
        return merged;
    }

    /*** Removes all the heap content */
    public void clear() {
        clear(root);
        size = 0;
        root = null;
    }

    /**
     * Remove all the content of the given tree/subtree
     * @param root Local root of tree/subtree
     */
    private void clear(Node<E> root) {
        if (root != null) {
            clear(root.left);
            clear(root.right);
            root = null;
        }
    }

    /**
     * Size of the heap
     * @return The number of key inside the heap
     */
    public int size() {
        return size;
    }

    /**
     * Checks whether heap is empty or not
     * @return
     */
    public boolean isEmpty() {
        return size == 0;
    }

    /**
     * Checks whether given key is inside the heap or not
     * @param key 
     * @return True if key can be found in heap otherwise false
     */
    public boolean contains(E key) {
        return find(root, key) != null;
    }

    /**
     * Checks whether given key is inside the heap or not
     * @param root Local root
     * @param key Item that will be searched inside the heap
     * @return Reference for the target key
     */
    private Node<E> find(Node<E> root, E key) {
        Node<E> r = null; // return value
        if (root != null) {
            int comp = root.data.compareTo(key);
            if (comp < 0) { // root's priority smaller than key's, so continue to search 
                r = find(root.left, key);
                if (r == null) // if key is not found in left subtree, check also right subtree 
                    r = find(root.right, key);
            }
            else if (comp > 0) // root's data larger than key's data that's why key doesn't exist by the rule
                r = null;
            else // key is found
                r = root;
        }
        return r;
    }

    /**
     * Checks if current tree is perfect or not
     * @return True if the tree is perfect, otherwise false
     */
    private boolean isPerfect() {
        return size == totalNodeHeight(height());
    }

    /**
     * Calculates the total node at height h perfect binary tree
     * @param h The height of the tree
     * @return Total number of nodes in heigh h perfect binary tree
     */
    private int totalNodeHeight(int h) {
        return (int) Math.pow(2, h) - 1;
    } 

    /**
     * Calculates the total node at given level
     * pre: lvl > 0
     * @param l Level
     * @return Total node at level
     */
    private int totalNodeLevel(int l) {
        return (int) Math.pow(2, l - 1);
    } 

    /**
     * Calculates height of the tree by using the
     * number of nodes in the tree 
     * @return Height of the tree
     */
    private int height() {
        //loga(b) = loge(b) / loge(a)
        return (int) (Math.log(size) / Math.log(2) + 1);
    }

    /**
     * Converts the current heap structure to array structure
     * by level order traversing the tree 
     * @return Array representation of the heap
     */
    @SuppressWarnings("unchecked")
    private E[] toArray() {
        E[] arr = (E[]) new Comparable[size];
        // fill the array with level order traversing
        toArray(arr, 0, root);
        return arr;
    }

    /**
     * Converts the current heap structure to array structure
     * by level order traversing the tree 
     * @param arr Array that is represent the heap
     * @param i Current index at the array
     * @param node Current node at the heap
     */
    private void toArray(E[] arr, int i, Node<E> node) {
        if (i < size) {
            arr[i] = node.data;
            toArray(arr, 2 * i + 1, node.left);
            toArray(arr, 2 * i + 2, node.right);
        }
    }
}   