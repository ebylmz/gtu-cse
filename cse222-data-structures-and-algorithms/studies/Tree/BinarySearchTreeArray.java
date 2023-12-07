/** Array based binary search tree implementation */
public class BinarySearchTreeArray<E extends Comparable<E>> implements SearchTree<E> {
    /** Array represantation of the binary tree structure */
    private E[] theData;

    /** Construct a new empty BinarySearchTree  */
    @SuppressWarnings("unchecked")
    public BinarySearchTreeArray() {
        theData = (E[]) new Comparable[3];
    }

    @Override
    public boolean add(E item) {
        return add(0, item);
    }

    /**
     * Adds the given item to the tree
     * @param index Index of the local root
     * @param item Item to insert
     * @return true if insertion is succesfully done, otherwise false
     */
    private boolean add(int index, E item) {
        // add item to the array as child (may required resizing)
        if (index >= theData.length) {
            realloc();
            theData[index] = item;
            return true;
        }
        else if (theData[index] == null) {
            theData[index] = item;
            return true;
        }
        else {
            int compare = item.compareTo(theData[index]);
            if (compare < 0) // add to left subtree
                return add(2*index + 1, item);
            else if (compare > 0) // add to right subtree
                return add(2*index + 2, item);
            else // item already in the tree
                return false;
        }
    }

    /*** Doubles the current array size */
    @SuppressWarnings("unchecked")
    private void realloc() {
        var old = theData;
        // array size is always 2^n - 1 (n: the depth of the tree)
        theData = (E[]) new Comparable[(old.length + 1) * 2 - 1]; 
        // copy the data to new allocated mem space
        for (int i = 0; i < old.length; ++i)
            theData[i] = old[i];
    }

    @Override
    public boolean contains(E target) {
        return find(0, target) != -1;
    }

    @Override
    public E find(E target) {
        int i = find(0, target);
        return i != -1 ? theData[i] : null;
    }

    /**
     * Search given target in the tree
     * @param root index of local root
     * @param target target value 
     * @return If target is found returns index of it, otherwise -1
     */
    private int find(int root, E target) {
        // make sure root index is not out of array size
        if (root >= theData.length || theData[root] == null)
            return -1; // target doesn't exist in the tree
        else {
            // compare the target with local root
            int compare = target.compareTo(theData[root]);
            if (compare < 0) // target may exist in left subtree
                return find(2*root + 1, target);
            else if (compare > 0) // target may exist in right subtree
                return find(2*root + 2, target);
            else // target is found
                return root;
        }
    }

    @Override
    public E delete(E target) {
        E rv = null;   // return value
        int p = find(0, target); // target index (parent)
        if (p != -1) { // target doesn't exist in the tree
            rv = theData[p]; 
            // define index of right and left childs
            int lc = 2*p + 1;
            int rc = lc + 1;
            if (isEmpty(lc))
                set(p, rc); // assign right child to the target position
            else if (isEmpty(rc))
                set(p, lc); // assign left child to the target position            
            else { // two children exist 
                // get the largest child at left subtree (inorder predecessor)
                int predecessor = findLargestChild(lc);
                // set target node to predecessor node value
                theData[p] = theData[predecessor];
                // remove predecessor
                theData[predecessor] = null;
            }
        }
        return rv;
    }

    /**
     * Removes target subtree at it's position and add given source subtree
     * @param target The subtree which will be removed, and it's position used as target position 
     * @param source The subtree which will be inserted the position of target subtree
     */
    private void set(int target, int source) {
        // destroy target subtree
        destroy(target);
        // place new subtree
        addSubtree(target, source);
    }


    /*** Removes all the nodes of the tree */
    public void destroy() {
        destroy(0);
    }

    /**
     * Removes all the nodes of the tree (subtree) at given root index 
     * @param root Index of subtree which will be removed with all its children
     */
    private void destroy(int root) {
        // make sure index doesn't exceed the array size
        if (root < theData.length) {
            theData[root] = null;
            // destroy child trees
            destroy(2*root + 1);
            destroy(2*root + 2);
        }
    }

    /**
     * Add given subtree to the target position
     * Pre: target position should be empty (requires to call destroy)
     * @param target Target position to add subtree at source index
     * @param source The subtree which will be placed the target position
     */
    private void addSubtree(int target, int source) {
        if (source < theData.length) {
            if (target >= theData.length)
                realloc();
            theData[target] = theData[source];
            // assign childs
            addSubtree(2*target + 1, 2*source + 1);  
            addSubtree(2*target + 2, 2*source + 2);  
        }
    }

    /**
     * Checks whether if the given index contain an data (subtree) or not
     * @param root Root index of the subtree
     * @return Returns true if the subtree is empty, otherwise false
     */
    private boolean isEmpty(int root) {
        return root >= theData.length || theData[root] == null; 
    }

    @Override
    public boolean remove(E target) {
        return delete(target) != null; 
    }

    /**
     * Finds the largest child of given subtree
     * pre: given index contains a nonempty subtree 
     * @param index Index of the local root
     * @return Index of the local root
     */
    private int findLargestChild(int index) {
        int rc = 2*index + 2; // index of right child  
        if (isEmpty(rc)) // current node has no right child (most right node at given subtree)
            return index;
        else // search largest child at right subtree
            return findLargestChild(rc);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        toString(0, 1, sb);
        return sb.toString();
    }

    /**
     * Converts a sub-tree to string 
     * @param index Index of local root in the array
     * @param depth Depth of the sub-tree
     * @param sb String builder to concatenate the string representation of all datas
     */
    private void toString(int index, int depth, StringBuilder sb) {
        // add indentation to print nodes in depth order
        for (int i = 0; i < depth; ++i)
            sb.append("  ");
        
        if (index >= theData.length || theData[index] == null)
            sb.append("null\n");
        else {
            sb.append(theData[index].toString());
            sb.append("\n");
            // continue with childs
            toString(2*index + 1, depth + 1, sb);
            toString(2*index + 2, depth + 1, sb);
        }
    }

    /**
     * Checks whether the current binary tree is BST or not
     * @param root Index of local root
     * @return True if the tree is generated properly with BST properties
    */
    /**
     * 
    private boolean isBST(int root) {
        // tree is b
        if (isEmpty(root))
            return true;
        else {
            // everything in the left subtree should be smaller than root 
            int lc = 2*root + 1; 
            if (! isEmpty(lc)) {
                if (! (theData[root].compareTo(theData[lc]) > 0))
                    return false;
            }
            // everything in the right subtree should be larger than root 
            int rc = lc + 1;
            if (! isEmpty(rc)) {
                if (! (theData[root].compareTo(theData[rc]) < 0))
                    return false;
            }

            // check the childs
            return isBST(lc) & isBST(rc);
        }
    }
    */
} 