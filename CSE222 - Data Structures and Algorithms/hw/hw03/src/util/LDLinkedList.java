package src.util;

import java.util.AbstractList;
import java.util.Collection;
import java.util.Iterator;
import java.util.ListIterator;
import java.util.NoSuchElementException;

public class LDLinkedList<T> extends AbstractList<T> {
    private Node<T> removedNodeList;
    private Node<T> head;
    private Node<T> tail;
    private int size;

    public LDLinkedList() {
        removedNodeList = head = tail = null;
        size = 0;
    }

    /**
     * Returns the entry at given index
     * @param index of entry
     * @return an entry at the list
     * @throws IndexOutOfBoundsException if the index is out of range (i < 0 || i > size())
     */
    @Override
    public T get(int index) throws IndexOutOfBoundsException {
        return listIterator(index).next();
    }

    /**
     * Inserts the given entry at the beginning of the list
     * @param e entry to insert the list
     */
    public void addFirst(T e) {
        add(0, e);
    }

    /**
     * Inserts the given entry at the end of the list
     * @param e entry to insert the list
     */
    public void addLast(T e) {
        add(size, e);
    }

    /**
     * Inserts the given entry at the end of the list
     * @param e entry to insert the list
     * @return true as long as enough free memory exist 
     */
    @Override
    public boolean add(T e) {
        addLast(e);
        return true;
    }

    /**
     * Inserts the given entry at given index of the list
     * @param index index of the new inserted entry
     * @param e entry to insert the list
     * @return true as long as enough free memory exist 
     */
    @Override
    public void add(int index, T e) {
        listIterator(index).add(e);
    }

    @Override
    public boolean addAll(Collection<? extends T> c) {
        for (var entry : c)
            add(entry);
        return true;
    }

    @Override
    public T remove(int index) {
        ListIterator<T> it = listIterator(index);
        T e = it.next();  // pre requirement of remove function (calling next or prev methods)
        it.remove();
        return e;
    }

    @Override
    public boolean remove(Object o) {
        var it = listIterator();
        while (it.hasNext()) {
            if (o.equals(it.next())) {
                it.remove();
                return true;
            }
        }
        return false;
    }

    @Override
    public boolean removeAll(Collection<?> c) {
        boolean r = true;
        for (var entry : c)
            if (! remove(entry))
                r = false;
        return r;
    }

    @Override
    public boolean retainAll(Collection<?> c) {
        boolean r = false;
        for (var e : this)
            if (! c.contains(e))
                r = remove(e);
        return r;
    }

    /**
     * Removes everything inside the list
     */
    @Override
    public void clear() {
        var it = listIterator();
        while (it.hasNext()) {
            it.next();
            // since remove deletes the last returning node, it is
            // required to call next or prev before on that
            it.remove();
        }
    }

    @Override
    public boolean contains(Object o) {
        for (var e : this)
            if (e.equals(o))
                return true;
        return false;
    }

    @Override
    public boolean isEmpty() {
        return head == null; // size == 0
    }

    /**
     * Returns an Iterator to the list which begins at index 0
     * @return an Iterator to the list
     */
    @Override
    public Iterator<T> iterator() {
        return new LDListIter(0);
    }

    /**
     * Returns an ListIterator to the list which begins at index 0
     * @return an ListIterator to the list
     */
    @Override
    public ListIterator<T> listIterator() {
        return new LDListIter(0);
    }

    /**
     * Returns an ListIterator to the list which begins at given index
     * @return an ListIterator to the list
     */
    @Override
    public ListIterator<T> listIterator(int index) {
        return new  LDListIter(index);
    }

    @Override
    public T set(int index, T e) {
        var it = listIterator(index);
        var preValue = it.next();
        it.set(e);
        return preValue; 
    }



    @Override
    public int size() {
        return size;
    }

    @Override
    public Object[] toArray() {
        Object[] arr = new Object[size];

        Iterator<T> it = iterator();
        
        int i = 0;
        while (it.hasNext())
            arr[i++] = it.next();

        return arr;
    }

    @Override
    public String toString() {
        StringBuffer buffer = new StringBuffer("{");
        
        var it = listIterator();
        while (it.hasNext()) {
            buffer.append(it.next());
            if (it.nextIndex() < size)
                buffer.append(", "); 
        }
        buffer.append("}");
        return buffer.toString();
    }

    @Override
    public int hashCode() {
        int hashCode = 0;
        for (var e : this)
            hashCode += 31 * e.hashCode();
        return hashCode;
    }

    @Override
    public LDLinkedList<T> clone() {
        try {
            @SuppressWarnings("unchecked")
            LDLinkedList<T> newList = (LDLinkedList<T>) super.clone();

            newList.size = 0;
            newList.head = null;
            newList.tail = null;
            newList.removedNodeList = null;

            var it = iterator();
            while (it.hasNext())
                newList.add(it.next());
            return newList;
        }
        catch (CloneNotSupportedException e) {
            // this will never happen            
            return null;
        }
    }

    @Override
    public boolean equals(Object obj) {
        boolean r = false;

        if (obj == this)
            r = true;
        else if (obj != null && (obj instanceof LDLinkedList)) {
            @SuppressWarnings("unchecked")
            var other = (LDLinkedList<T>) obj;
            if (other.size() == this.size()) {
                var it1 = this.iterator();
                var it2 = other.iterator();
                r = true;
                // one hasNext is enough because they have same number of entry
                while (it1.hasNext() && it2.hasNext() && r) 
                    if (! it1.next().equals(it2.next()))
                        r = false;
            } 
        }
        return r;
    }

    private static class Node<T> {
        private T data;
        private Node<T> next;
        private Node<T> prev;

        public Node(T data, Node<T> next, Node<T> prev) {
            this.data = data;
            this.next = next;
            this.prev = prev;
        }

        public Node(T data) {
            this(data, null, null);
        }
        
        public Node() {
            this(null, null, null);
        }
    }

    private class LDListIter implements ListIterator<T> {
        /** Next entry in the list */
        private Node<T> nextEntry;
        /** last returned (jumper over) item */
        private Node<T> lastEntryReturned;
        /** Index of the current entry */
        private int index; 

        public LDListIter(int index) {
            if (index < 0 || index > size)
                throw new IndexOutOfBoundsException();
            this.index = index;
            // end of the list
            if (index == size) {
                nextEntry = null; 
            }
            else { 
                nextEntry = head;
                for (int i = 0; i < index; ++i)
                    nextEntry = nextEntry.next;
            }
        }

        /**
         * Checks if there is a next entry
         * @return true if next wouldn't throw exception
         */
        @Override
        public boolean hasNext() {
            return nextEntry != null; // index < size
        }

        /**
         * Checks if there is a previous entry
         * @return true if prev wouldn't throw exception
         */
        @Override
        public boolean hasPrevious() {
            // nextEntry is null when list is empty or iterator located at the end of the list
            return (nextEntry == null && size != 0) ||
                    (nextEntry.prev != null); 
        }
        
        /**
         * Moves iterator forward and return the next entry
         * @return next entry
         * @throws NoSuchElementException if there is no next entry exist
         */
        @Override
        public T next() throws NoSuchElementException {
            if (! hasNext())
                throw new NoSuchElementException();
            
            lastEntryReturned = nextEntry;
            nextEntry = nextEntry.next;
            ++index;
            return lastEntryReturned.data;
        }

        // 0   1   2   3   4   5   6    // iterator position index
        // | A | B | C | D | E | F |    

        /**0,,
         * Returns the index of the next entry that would be returned by next
         * @return the index of the next entry
         */
        @Override
        public int nextIndex() {
            return index;
        }

        /**
         * Moves iterator backward and return the previous entry
         * @throws NoSuchElementException if there is no previous entry exist
         */
        @Override
        public T previous() throws NoSuchElementException {
            if (! hasPrevious())
                throw new NoSuchElementException();
            
            if (nextEntry == null) // iterator at the end of the list
                nextEntry = tail;   
            else
                nextEntry = nextEntry.prev;
            // nextEntry is also last returned entry during backward iteration
            lastEntryReturned = nextEntry;
            --index;
            return lastEntryReturned.data;
        }

        /**
         * Returns the index of the prev entry that would be returned by prev
         * @return the index of the prev entry
         */
        @Override
        public int previousIndex() {
            return index - 1;
        }

        /** Removes the last entry returned.
         *  @throws IllegalStateException if there is no returned entry 
         * (next or prev methods doesn't called before calling this method)
         */
        @Override
        public void remove() {
            if (lastEntryReturned == null)
                throw new IllegalStateException();
                
            // unlink this entry from its lastEntryReturned.next
            if (lastEntryReturned.next == null) { // (lastEntryReturned == tail)
                tail = lastEntryReturned.prev;
                if (tail != null)
                    tail.next = null;
                // else
                    // head = null; //! is neeed
            }
            else 
                lastEntryReturned.next.prev = lastEntryReturned.prev;

            // unlink this entry from its lastEntryReturned.prev
            if (lastEntryReturned.prev == null) { // (lastEntryReturned == head)
                head = lastEntryReturned.next;
                if (head != null)
                    head.prev = null;
                // else
                    // tail = null; //! is neeed
            }
            else
                lastEntryReturned.prev.next = lastEntryReturned.next;
            
            // keep the removed node in the list to later use (lazy deletion)
            lastEntryReturned.next = removedNodeList;
            removedNodeList = lastEntryReturned;

            // since lastEntryReturned not in list any more, set lastEntryReturned as null 
            lastEntryReturned = null; 
            --size;
            --index; //!  
        }

    /**
     * Add a new entry between the next and prev entries
     * If previous is called after add, the element added is returned.
     * @param e The entry to be inserted
     */
        @Override
        public void add(T e) {
            // check if removed node exist
            Node<T> newNode = null;
            if (removedNodeList != null) {
                newNode = removedNodeList;
                removedNodeList = removedNodeList.next;
            }
            else
                newNode = new Node<T>();
            newNode.data = e;
            
            // there are 4 addition scenerio exist
            if (head == null) {
                head = tail = newNode;
                newNode.next = newNode.prev = null;
            }
            else if (nextEntry == head) { // add new entry as head
                newNode.next = head;
                newNode.prev = null;
                head.prev = newNode;
                head = newNode;
                // after adding new entry if there exist only
                // one entry it's became both head and tail
            }
            else if (nextEntry == null) { // add new entry as tail
                newNode.next = null;
                newNode.prev = tail;
                tail.next = newNode;
                tail = newNode;
            }
            else { // add the middle
                // link with nextItem.prev
                newNode.prev = nextEntry.prev;
                nextEntry.prev.next = newNode;
                // link with nextItem
                newNode.next = nextEntry;
                nextEntry.prev = newNode;
            }
            // update the size and index
            ++size;
            ++index;   //!
            lastEntryReturned = null;
        }

    /** Sets the value of last returned item
     *  @param e New entry value
     *  @throws IllegalStateException if next or previous was not called prior to calling this method
     */
        @Override
        public void set(T e) {
            if (lastEntryReturned == null)
                throw new IllegalStateException();
            lastEntryReturned.data = e;
        }
    }
}