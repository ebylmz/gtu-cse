import java.util.Iterator;
import java.util.NoSuchElementException;

public class ArrayQueue<E> implements Iterable<E> {
    private final int INITIAL_CAPACITY = 10; 
    private E[] theData;
    private int front;
    private int rear;
    private int size;
    private int capacity;

    public ArrayQueue() {
        size = 0;
        capacity = INITIAL_CAPACITY;
        front = 0;
        rear = INITIAL_CAPACITY - 1;
        @SuppressWarnings("unchecked")
        E[] a = (E[]) new Object[capacity];
        theData = a;
    }

    public int size() {
        return size;
    }

    public boolean isEmpty() {
        return size == 0;
    }

    public boolean offer(E e) {
        if (size == capacity)
            resize();
        rear = (rear + 1) % capacity;
        theData[rear] = e;
        ++size;
        return true;
    }

    private void resize() {
        @SuppressWarnings("unchecked")
        E[] tmp = (E[]) new Object[capacity * 2];
        
        for (int i = 0; i < size; ++i)
            tmp[i] = theData[(front + i) % capacity];
        front = 0;
        rear = capacity - 1;
        theData = tmp;
    }

    public E pool() throws NoSuchElementException {
        if (size == 0)
            throw new NoSuchElementException();
        E data = theData[front];
        front = (front + 1) % capacity;        
        --size;
        return data;
    }


    public E peek() throws NoSuchElementException {
        if (size == 0)
            throw new NoSuchElementException();
        return theData[front];
    }

    public void clear() {
        front = 0;
        rear = capacity - 1;
        for (int i = 0; i < size; ++i)
            theData[i] = null;
        size = 0;
    }

    @Override
    public Iterator<E> iterator() {
        return new QueueIter();
    }

    private class QueueIter implements Iterator<E> {
        private int index;
        private int count;

        public QueueIter() {
            index = front;
            count = 0;
        }

        @Override
        public boolean hasNext() {
            return count < size;
        }

        @Override
        public E next() {
            if (!hasNext())
                throw new NoSuchElementException();
            E returnValue = theData[index];
                index = (index + 1) % capacity;
            ++count;
            return returnValue;
        }

        @Override
        public void remove() {
            throw new UnsupportedOperationException();            
        }
    }
}