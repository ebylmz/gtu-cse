package stack;

import java.util.NoSuchElementException;

public class ArrayStack<E> implements StackInterface<E> {
    private E[] theData;
    private int size;

    public ArrayStack() {
        theData = allocateMem(10);  
        size = 0;        
    }

    @Override
    public void push(E newEntry) {
        if (size == theData.length)
            resize();
        theData[size] = newEntry; 
    }

    @Override
    public E pop() {
        if (isEmpty())
            throw new NoSuchElementException();
        --size;
        var data = theData[size];   // last item
        theData[size] = null;
        return data;
    }

    @Override
    public E peek() {
        if (isEmpty())
            throw new NoSuchElementException();
        return theData[size - 1];
    }

    @Override
    public boolean isEmpty() {
        return size == 0;
    }

    @Override
    public int size() {
        return size;
    }

    @Override
    public void clear() {
        for (int i = 0; i < size; ++i)
            theData[i] = null;
        size = 0;        
    }

    private void resize() {
        E[] oldData = theData;
        theData = allocateMem(oldData.length * 2);
        for (int i = 0; i < oldData.length; ++i)
            theData[i] = oldData[i];
    }

    private E[] allocateMem(int size) {
        @SuppressWarnings("unchecked")
        var newData = (E[]) new Object[theData.length * 2];
        return newData;
    }


}
