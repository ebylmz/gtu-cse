/**
 * 
 * Implementation for Double Ended Queue by Using Circular Array
 * 
 */

import java.util.Iterator;
import java.util.ListIterator;
import java.util.NoSuchElementException;

public class ArrayDeque<E> implements Iterable<E> {
    private static final int INITIAL_CAPACITY = 10;
    private E[] theData;
    private int front;
    private int rear;
    private int size;
    private int capacity;

    @SuppressWarnings("unchecked")
    public ArrayDeque() {
        theData = (E[]) new Object[INITIAL_CAPACITY];
        capacity = INITIAL_CAPACITY;
        size = 0;
        front = 0;
        rear = capacity - 1;
    }

    public boolean offerFirst(E e) {
        if (size == capacity)
            realloc();
        // set the front index (<--) 
        front = (front == 0) ? capacity - 1 : front - 1;
        theData[front] = e;
        ++size;
        return true;
    }

    public boolean offerLast(E e) {
        if (size == capacity)
            realloc();
        // set the rear index (-->) 
        rear = (rear == capacity - 1) ? 0 : rear + 1; 
        theData[rear] = e;
        ++size;
        return true;
    }

    public E poolFirst() throws NoSuchElementException {
        if (size == 0)
            throw new NoSuchElementException();      
        var retVal = theData[front];
        theData[front] = null; 
        // set the front index (-->) 
        front = (front == capacity - 1) ? 0 : front + 1;
        --size;
        return retVal;
    }

    public E poolLast() throws NoSuchElementException {
        if (size == 0)
            throw new NoSuchElementException();        
        var retVal = theData[rear];
        theData[rear] = null; 
        // set the rear index (<--) 
        rear = (rear == 0) ? capacity - 1 : rear - 1;
        --size;
        return retVal;
    }

    public E peekFirst() throws NoSuchElementException {
        if (size == 0)
            throw new NoSuchElementException();
        return theData[front];
    }

    public E peekLast() throws NoSuchElementException {
        if (size == 0)
            throw new NoSuchElementException();
        return theData[rear];
    }

    public void clear() {
        // set array references as null to free those allocated spaces
        for (int i = 0; i < size; ++i)
            theData[(front + i) % capacity] = null;
        // reset the deque as its initial status
        front = 0;
        rear = capacity - 1;
        size = 0;
    }

    public int size() {
        return size;
    }

    public boolean isEmpty() {
        return size == 0;
    }

    @SuppressWarnings("unchecked")
    private void realloc() {
        var oldData = theData;
        // double the array size
        capacity *= 2;
        theData = (E[]) new Object[capacity]; 
        // copy old data to the new allocated memory space
        for (int i = 0; i < size; ++i)
            theData[i] = oldData[(front + i) % capacity];
    }

    @Override
    public Iterator<E> iterator() {
        return new  DequeIter(0);
    }

    public ListIterator<E> listIterator(int index) {
        return new  DequeIter(index);
    }

    @Override
    public String toString() {
        StringBuffer buffer = new StringBuffer();

        var it = iterator();
        while (it.hasNext()) {
            buffer.append(it.next());
            buffer.append(" ");
        }

        return buffer.toString();
    }

    private class DequeIter implements ListIterator<E> {
        /**
         * add, remove, set throw UnsupportedOperationException 
         * because inserting or removing item by iterator
         * can violate the Deque contract 
         */

        private int count;
        private int index; 

        public DequeIter(int index) {
            count = index;
            this.index = (front + index) % capacity;
        }

        @Override
        public void add(E e) {
            throw new UnsupportedOperationException();            
        }

        @Override
        public boolean hasNext() {
            return count < size;
        }

        @Override
        public boolean hasPrevious() {
            return count > 0;
        }

        @Override
        public E next() {
            if (!hasNext())
                throw new NoSuchElementException();
            var retVal = theData[index];
            // set index forward
            index = (index == capacity - 1) ? 0 : index + 1;
            ++count;
            return retVal;
        }

        @Override
        public int nextIndex() {
            return index;
        }

        @Override
        public E previous() {
            if (!hasPrevious())
                throw new NoSuchElementException();
            var retVal = theData[index];
            // set index backward
            index = (index == 0) ? capacity - 1 : index - 1;
            --count;
            return retVal;
        }

        @Override
        public int previousIndex() {
            return index - 1;
        }

        @Override
        public void remove() {
            throw new UnsupportedOperationException();            
        }

        @Override
        public void set(E e) {
            throw new UnsupportedOperationException();            
        }        
    }
}