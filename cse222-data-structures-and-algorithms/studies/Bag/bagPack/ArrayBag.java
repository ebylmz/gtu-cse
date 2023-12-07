package bagPack;

import java.util.Arrays;

public class ArrayBag<T> implements BagInterface<T> {
    private final static int DEFAULT_CAPACITY = 25;
    private final T[] bag;  // fixed sized array
    private int numEntry;

    public ArrayBag(T[] content) {
        bag = Arrays.copyOf(content, content.length);
        numEntry = content.length;
    }

    public ArrayBag(int capacity) {
        numEntry = 0;

        // the cast is safe because the new array contains null entries
        @SuppressWarnings("unchecked")   
        // Note that this instruction can precede only a method definition or a variable declaration.
        T[] tempBag = (T[]) new Object[Math.max(1, capacity)]; // unchecked cast
        bag = tempBag;
    }

    public ArrayBag() {this(DEFAULT_CAPACITY);}

    /** Sees whether this bag is full.
    @return true if the bag is full, or false if not */
    public int getCurrentSize() {return numEntry;}

    /** Sees whether this bag is empty.
        @return true if the bag is empty, or false if not */
    public boolean isFull() {return numEntry == bag.length;}

    /** Sees whether this bag is empty.
        @return true if the bag is empty, or false if not */
    public boolean isEmpty() {return numEntry == 0;}

    /** Adds a new entry to this bag.
        @param newEntry the object to be added as a new entry
        @return true if the addition is successful, or false if not */
    public boolean add(T newEntry) {
        if (!isFull()) {
            bag[numEntry] = newEntry; //! clone 
            ++numEntry;
            return true;
        }
        return false;
    }

    /** Removes one unspecified entry from this bag, if possible.
        @return either the removed entry, if the removal was successful, or null */
    public T remove() {
        T result = removeEntry(numEntry - 1);
        return result;
    }
        
    /** Removes one occurrence of a given entry from this bag, if possible.
        @param anEntry the entry to be removed
        @return true if the removal was successful, or false if not */
    public boolean remove(T anEntry) {
        int index = getIndexOf(anEntry);
        T result = removeEntry(index);
        return result != null;
    }

    private T removeEntry(int index) {
        T result = null;
        if (0 <= index && index < numEntry) {
            --numEntry;
            result = bag[index];
            bag[index] = bag[numEntry]; 
            bag[numEntry] = null; // set it as null to deallocate and recycle memory place
        }
        return result;
    }

    private int getIndexOf(T anEntry) {
        for (int i = 0; i < numEntry; ++i)
            if (anEntry.equals(bag[i]))
                return i;
        return -1;
    }

    /** Removes all entries from this bag. */
    public void clear() {
        while (remove() != null);
    }

    /** Counts the number of times a given entry appears in this bag.
        @param anEntry the entry to be counted
        @return the number of times anEntry appears in the bag */
    public int getFrequencyOf(T anEntry) {
        int freq = 0;
        for (int i = 0; i < getCurrentSize(); ++i)
            if (anEntry.equals(bag[i]))
                ++freq;
        return freq;
    }
    
    /** Tests whether this bag contains a given entry.
        @param anEntry the entry to locate
        @return true if the bag contains anEntry, or false otherwise */
    public boolean contains(T anEntry) {
        return getIndexOf(anEntry) != -1;
    }

    /** Creates an array of all entries that are in this bag.
        @return a newly allocated array of all the entries in the bag */
    public T[] toArray() {
        // the cast is safe because the new array contains null entries
        @SuppressWarnings("unchecked")   
        T[] cloneBag = (T[]) new Object[getCurrentSize()]; 
        for (int i = 0; i < getCurrentSize(); ++i)
            cloneBag[i] = bag[i]; //! clone

        return cloneBag;
    }
}