package src.bst;

import java.util.Iterator;
import java.util.NoSuchElementException;

public class BinarySearchTree<E extends Comparable<E>> 
    extends BinaryTree<E> implements SearchTree<E>, Iterable<E> {
    /** Return value from the public add method */
    private boolean addReturn;
    /** Return value from the public delete method */
    private E deleteReturn;

    @Override
    public boolean add(E item) {
        root = add(root, item);
        return addReturn;
    }

    /**
     * Inserts given item to the tree (no dublication allowed)
     * @param localRoot Local root
     * @param item Item to insert
     * @return False if the item is already exist in the tree, otherwise true
     */
    private Node<E> add(Node<E> localRoot, E item) {
        if (localRoot == null) {
            addReturn = true;   
            return new Node<>(item);
        }
        else {
            int comp = item.compareTo(localRoot.data);
            if (comp < 0) { // insert left subtree
                localRoot.left = add(localRoot.left, item);
                localRoot.left.parent = localRoot;
            }
            else if (comp > 0) { // insert right subtree
                localRoot.right = add(localRoot.right, item);
                localRoot.right.parent = localRoot;
            }
            else // item is already placed to the tree
                addReturn = false;
            return localRoot;
        }
    }

    @Override
    public boolean contains(E target) {
        return find(target) != null;
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
        root = delete(root, target);
        return deleteReturn;
    }

    /**
     * Deletes the given target item from the tree
     * @param localRoot Local root
     * @param target Delete item
     * @return Returns the value of target if the deletion is done succesfully, 
     * otherwise returns null
     */
    private Node<E> delete(Node<E> localRoot, E target) {
        if (localRoot == null) { // item is not in the tree
            deleteReturn = null;
            return null;
        }
        else {
            int comp = target.compareTo(localRoot.data);
            if (comp < 0) { // target could be at left subtree
                localRoot.left = delete(localRoot.left, target);
                return localRoot;
            }
            else if (comp > 0) { // target could be at right subtree
                localRoot.right = delete(localRoot.right, target); 
                return localRoot;
            }
            else { // target found
                deleteReturn = localRoot.data; // return value
                // three deletion scenerio
                // 1. delete node has no child
                // 2. delete node has one child
                // 3. delete node has two child
                if (localRoot.left == null)
                    return localRoot.right; // return other child
                else if (localRoot.right == null) // no child
                    return null; 
                else {
                    // node being deleted has 2 children, replace the data with inorder predecessor
                    if (localRoot.left.right == null) { 
                        // the left child has no right child
                        // replace the data with the data in the left child.
                        localRoot.data = localRoot.left.data;
                        // replace the left child with its left child.
                        localRoot.left = localRoot.left.left;
                        return localRoot;
                    }
                    else {
                        // search for the inorder predecessor (ip) and replace 
                        // deleted node data with ip
                        localRoot.data = extractLargest(localRoot.left);
                        return localRoot;
                    }
                }
            }         
        }         
    }    

    /*** Destroys the tree by Removing all the items of the tree */
    public void destroy() {
        root = destroy(root);
    }

    /*** Destroys the tree by Removing all the items of the tree */
    private Node<E> destroy(Node<E> localRoot) {
        if (localRoot != null) {
            // first destroy left and right subtrees
            localRoot.left = destroy(localRoot.left);
            localRoot.right = destroy(localRoot.right);
        }
        // then destroy the tree itself by returning null (things work like this in java)
        return null;
    }

    /**
     * Removes largest child and returns the value of it
     * @param parent Parent node
     * @return Largest child
     */
    private E extractLargest(Node<E> parent) {
        // traverse to right to find largest child
        if (parent.right.right == null) {
            var v = parent.right.data;
            parent.right = parent.right.left;
            return v;
        }
        else 
            return extractLargest(parent.right);
    }

    @Override
    public boolean remove(E target) {
        return delete(target) != null;
    }

    @Override
    public Iterator<E> iterator() {
        return new TreeIterator();
    }

    private class TreeIterator implements Iterator<E> {
        private Node<E> nextNode;

        /*** Constructs an iterator which next value is most left item */
        public TreeIterator() {
            nextNode = (root != null) ? mostLeft(root) : null;
        }   

        @Override
        public boolean hasNext() {
            return nextNode != null;
        }

        @Override
        public E next() throws NoSuchElementException {
            if (!hasNext())
                throw new NoSuchElementException();
            E r = nextNode.data;
            // currently we are at the most left node
            // means there is no more left child, so continue
            // to traversing with the right child if there is
            if (nextNode.right != null)
                nextNode = mostLeft(nextNode.right);
            else { // if the nextNode is left child of his parent
                while (true) {
                    if (nextNode.parent == null) {
                        // all nodes in the tree is traversed
                        nextNode = null;
                        break;
                    }
                    else if (nextNode.parent.left == nextNode) {
                        // if the current node is left child, then next node is parent
                        nextNode = nextNode.parent;
                        break;
                    }
                    else {
                        // if the current node is right child, parent is already traversed so
                        // the next node should be ancestor of parent node 
                        nextNode = nextNode.parent;
                    }
                }
            }
            
            return r;
        }

        /**
         * Returns the node which is placed at most left of the tree 
         * @param localRoot Root of the current subtree
         * @return Most left node of the subtree
         */
        private Node<E> mostLeft(Node<E> localRoot) {
            if (localRoot.left == null)
                return localRoot;
            else
                return mostLeft(localRoot.left);
        }
    }
}