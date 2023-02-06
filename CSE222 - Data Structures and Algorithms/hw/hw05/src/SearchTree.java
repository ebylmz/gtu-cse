package src;

public interface SearchTree<E> {
    /**
     * Inserts item where it belongs in the tree
     * @param item
     * @return Returns true if item is inserted else false
     */
    boolean add(E item);

    /**
     * Checks whether given item in tree or not
     * @param target Target item
     * @return true if the target item is found
     */
    boolean contains(E target);
    
    /**
     * Searches spefic item in the tree and returns a reference to it
     * @param target Target item
     * @return If target is found in tree returns a reference to target object, 
     * otherwise returns null
     */
    E find(E target);

    /**
     * Deletes given item from the tree and returns it
     * @param target
     * @return If target exist in tree returns target object, otherwise returns null
     */
    E delete(E target);

    /**
     * Removes given item from the tree
     * @param target
     * @return If target exist in tree removes and returns true, otherwise returns false
     */
    boolean remove(E target);
}