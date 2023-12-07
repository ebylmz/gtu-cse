public class AVLTree<E extends Comparable<E>> extends BinarySearchTreeWithRotate<E> {
    /** Flag to indicate the balance property is checked or not */
    private boolean checkBalance;
    
    /*** Constructs an empty AVL tree */
    public AVLTree() {
        super();
    }

    @Override
    public boolean add(E item) {
        // to use polymorphism cast root to the AVLNode
        root = add((AVLNode<E>) root, item);
        return addReturn;
    }

    private AVLNode<E> add(AVLNode<E> localRoot, E item) {
        if (localRoot == null) {
            addReturn = true;
            checkBalance = true;
            localRoot = new AVLNode<E>(item);
        }
        else {
            int comp = item.compareTo(localRoot.data);
            if (comp == 0) { 
                // item is already exist
                addReturn = false;
                checkBalance = false;
            }            
            else if (comp < 0)
                localRoot.left = add((AVLNode<E>) localRoot.left, item);
            else
                localRoot.right = add((AVLNode<E>) localRoot.right, item);
            // after insertion make sure the tree is still balanced 
            if (addReturn) {  
                localRoot.updateHeight(); // new item is inserted 
                // if the tree is not balanced yet
                if (checkBalance) {
                    System.out.println("check balance for " + localRoot);
                    localRoot = rebalance(localRoot);
                }
            }
        }
        return localRoot;
    }

    public String inOrder() {
        StringBuilder sb = new StringBuilder();
        inOrder((AVLNode<E>) root, sb);
        return sb.toString();
    }

    public void inOrder(AVLNode<E> localRoot, StringBuilder sb) {
        if (localRoot != null) {
            inOrder((AVLNode<E>) localRoot.left, sb);
            sb.append(localRoot.toString());
            sb.append(" ");
            inOrder((AVLNode<E>) localRoot.right, sb);
        }
    }

    @Override
    public E delete(E target) {
        root = delete((AVLNode<E>) root, target);
        return deleteReturn;
    }

    private AVLNode<E> delete(AVLNode<E> localRoot, E target) {
        if (localRoot == null) { 
            // target item is not found
            deleteReturn = null;
        }
        else {
            int comp = target.compareTo(localRoot.data);
            if (comp == 0) { 
                // item is found
                checkBalance = false; // set flag to check balance 
                deleteReturn = localRoot.data;
                localRoot = null;
            }
            else {
                if (comp < 0)
                    localRoot.left = delete((AVLNode<E>) localRoot.left, target);
                else
                    localRoot.right = delete((AVLNode<E>) localRoot.right, target);
                // after deletion make sure the tree is still balanced 
                if (deleteReturn != null) { 
                    localRoot.updateHeight(); // an item is removed
                    // make sure the tree is balanced
                    if (checkBalance) {
                        localRoot = rebalance(localRoot);
                        checkBalance = false; //  tree is balanced
                    }
                }
            }            
        }
        return localRoot;
    }

    private AVLNode<E> rebalance(AVLNode<E> localRoot) {
        // check the balance factor
        int bf = balanceFactor(localRoot); 
        // determine the type of tree (LL, LR, RL, RR)
        if (bf < AVLNode.LEFT_HEAVY) {
            if (balanceFactor(localRoot.left) > 0) {
                // convert Left-Right tree to Left-Left tree by rotating the right-child left
                localRoot.left = (AVLNode<E>) rotateLeft(localRoot.left);
            }
            // balance Left-Left tree by rotating right
            localRoot = (AVLNode<E>) rotateRight(localRoot);
            checkBalance = false;
        }
        else if (bf > AVLNode.RIGHT_HEAVY) {
            if (balanceFactor(localRoot.right) < 0) {
                // convert Right-Left tree to Right-Right tree by rotating the left-child right
                localRoot.right = (AVLNode<E>) rotateRight(localRoot.right);
            }
            // balance Right-Right tree by rotating left
            localRoot = (AVLNode<E>) rotateLeft(localRoot);
            checkBalance = false;
        }
        return localRoot;
    }

    /** Balance factor of a node is defined Height(LeftSubtree) - Height(RightSubtree)
     * @return Height(LeftSubtree) - Height(RightSubtree)
     */
    private int balanceFactor(Node<E> node) {
        return node == null ? 0 : height(node.left) - height(node.right);
    }

    private int height(Node<E> node) {
        return node == null ? 0 : ((AVLNode<E>) node).height;
    }

    private static class AVLNode<E> extends Node<E> {
        /** Constant to indicate left-heavy */
        public static final int LEFT_HEAVY = -1;
        /** Constant to indicate balanced */
        public static final int BALANCED = 0;
        /** Constant to indicate right-heavy */
        public static final int RIGHT_HEAVY = 1;
        /** Height of the current subtree */
        private int height; 

        /**
         * Constructs an AVL node
         * @param data The data
         */
        public AVLNode(E data) {
            super(data);
            height = 1;
        }

        public void updateHeight() {
            height = 1 + Math.max(getHeight(left), getHeight(right)); 
        }

        private AVLNode<E> higherChild() {
            return (AVLNode<E>) (getHeight(left) > getHeight(right) ? left : right);
        }

        private int getHeight(Node<E> node) {
            return (node != null) ? ((AVLNode<E>) node).height : 0;
        }
    /*
        @Override
        public String toString() {
            return height + ": " + super.toString();
        }
    */
    }
}
