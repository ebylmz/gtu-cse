package bagPack;

import nodePack.Node;

public class LinkedBag<T> implements BagInterface<T> {
    private Node<T> head;
    private int numEntry;

    public LinkedBag() {
        head = null;
        numEntry = 0;
    }

    /** Sees whether this bag is full.
        @return true if the bag is full, or false if not */
    public int getCurrentSize() {return numEntry;}

    /** Sees whether this bag is empty.
        @return always false */
    public boolean isFull() {return false;}

    /** Sees whether this bag is empty.
        @return true if the bag is empty, or false if not */
    public boolean isEmpty() {return numEntry == 0;}

    /** Adds a new entry to this bag.
        @param newEntry the object to be added as a new entry
        @return always true */
    public boolean add(T newEntry) {
        // create new node acc. to newEntry 
        // set new node next as head of the list
        // set new node as head of the list
        Node<T> newNode = new Node<T>(newEntry, head);
        head = newNode;
        ++numEntry;
        return true;
    }

    /** Removes one unspecified entry from this bag, if possible.
        @return either the removed entry, if the removal was successful, or null */
    public T remove() {
        T result = null;
        // if list exist, remove first entry from the list
        if (head != null) {
            result = head.getData();
            head = head.getNextNode();
            --numEntry;
        }
        return result;
    }
        
    /** Removes one occurrence of a given entry from this bag, if possible.
        @param anEntry the entry to be removed
        @return true if the removal was successful, or false if not */
    public boolean remove(T anEntry) {
        // it's easier to remove first node because there is no backward connection to another node
        // since the order is not important we always remove the first node

        // first find the remove entry's node
        Node<T> nodeN = getReference(anEntry);
        
        // here nodeN points either nothing (null) or searched node
        if (nodeN != null) {
            nodeN.setData(head.getData());  // replace located entry with entry in head node
            head = head.getNextNode(); // remove head node
            --numEntry;
            return true;
        }
        return false;
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
        for (var trav = head; trav != null; trav = trav.getNextNode())
            if (anEntry.equals(trav.getData()))
                ++freq;
        return freq;
    }

    /** Tests whether this bag contains a given entry.
        @param anEntry the entry to locate
        @return true if the bag contains anEntry, or false otherwise */
    public boolean contains(T anEntry) {
        return getReference(anEntry) != null;
    }
    
    private Node<T> getReference(T anEntry) {
        Node<T> trav = head;
        while (trav != null && ! anEntry.equals(trav.getData()))
            trav = trav.getNextNode();
        
        // here trav points either nothing (null) or searched node
        return trav;
    }

    /** Creates an array of all entries that are in this bag.
        @return a newly allocated array of all the entries in the bag */
    public T[] toArray() {
        // the cast is safe because the new array contains null entries
        @SuppressWarnings("unchecked")
        T[] arr = (T[]) new Object[numEntry]; // unchecked cast

        Node<T> trav = head;   //! clone
        for (int i = 0; i < numEntry && trav != null; ++i, trav = trav.getNextNode()) 
            arr[i] = trav.getData(); //! clone
        return arr;
    }
}
