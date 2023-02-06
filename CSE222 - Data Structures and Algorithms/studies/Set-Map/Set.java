import java.util.Collection;
import java.util.Iterator;

public interface Set<E> extends Iterable<E> {
    /**
     * Adds the item e to this set if it is not already present
     * @return True if the addition succesfull. False if the item e already exist
     */
    boolean add(E e);

    /**
     * Adds all of the elements in collection coll to this set if they’re not
     * already present. This operation can also be called union of two set. 
     * @param coll The collection
     * @return Returns true if the set is chanced.
     */
    boolean addAll(Collection<E> coll);

    /**
     * Determine whether this set contains an element that is equal to e.
     * @param e The element
     * @return True if set contains the element e.
     */
    boolean contains(E e);
    
    /**
     * Determine this set is a subset of coll.
     * @param coll
     * @return Returns true if this set contains all of the elements of collection.
     */
    boolean containsAll(Collection<E> coll);
    
    /**
     * Determine whether this set is empty or not.
     * @return Returns true if this set contains no elements.
     */
    boolean isEmpty();

    /**
     * Returns an iterator over the elements in this set.
     * @return Iterator
     */
    @Override
    Iterator<E> iterator();
    
    /**
     * Removes the set element equal to e if it is present.
     * @param e The element
     * @return True if the object was removed
     */
    boolean remove(E e);
    
    /**
     * Removes from this set all of its elements that are contained in
     * the collectiom coll. If coll is a set, performs the set difference operation.
     * @param coll The collection
     * @return Returns true if this set is changed
     */
    boolean removeAll(Collection<E> coll);


    /** */
}


/*
collection coll (optional operation). Returns true if this set is
changed. 
boolean retainAll(Collection<E> coll) Retains only the elements in this set that are contained in collection
coll (optional operation). Returns true if this set is changed. If
coll is a set, performs the set intersection operation
int size()
*/