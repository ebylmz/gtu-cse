package src.heap;

import java.util.NoSuchElementException;

/** Min-Heap Array Implementation */
public class Heap<E extends Comparable<E>> {
    private static int INIT_SIZE = 7;
    /** Array represantation of heap structure */
    private E[] theData;
    /** Number of item in the heap */
    private int size;

    /**
     * Constructs an empty min-heap
     */
    @SuppressWarnings("unchecked")
    public Heap() {
        theData = (E[]) new Comparable[INIT_SIZE];
        size = 0;
    }

    /**
     * Adds the given item to the heap
     * @param item The item to be inserted
     */
    public void insert(E item) {
        if (size == theData.length)
            resize();
        theData[size] = item;
        // make sure heap property is still maintaining  
        heapifyUp(size);
        ++size;
    }

    /**
     * Returns the minimum item on the current heap without modifying it
     * @return Minimum item on current heap
     * @throws NoSuchElementException If the heap does not contain any item that exception will be thrown
     */
    public E getMin() throws NoSuchElementException {
        if (size == 0)
            throw new NoSuchElementException();
        return theData[0];
    }

    /**
     * Removes and returs the min item at the top of heap
     * @return Minimum item on current heap
     * @throws NoSuchElementException If the heap does not contain any item that exception will be thrown
     */
    public E extractMin() throws NoSuchElementException {
        if (size == 0)
            throw new NoSuchElementException();
        --size;
        E min = theData[0];
        theData[0] = theData[size];
        // make sure heap property is still maintaining 
        heapifyDown(0);        
        return min;
    }

    /**
     * Removes all the items that heap contains
     * post: heap.isEmpty() true
     */
    public void destroy() {
        for (int i = 0; i < size; ++i)
            theData[i] = null;
        size = 0;
    }

    /**
     * The number of item that heap contains
     * @return The number of item
     */
    public int size() {
        return size;
    }

    /**
     * Determine whether the heap is empty or not
     * @return True for an empty heap
     */
    public boolean isEmpty() {
        return size == 0;
    }

    /**
     * Readjust min-heap property by swapping parent and childs
     * @param parent Index of current item
     */
     private void heapifyDown(int parent) {
        int child = parent * 2 + 1;
        if (child < size) {
            // if right child smaller than left child, take the right child
            if (child + 1 < size && theData[child + 1].compareTo(theData[child]) < 0)
                ++child;
            // compare smaller child with parent
            if (theData[child].compareTo(theData[parent]) < 0) {
                swap(theData, parent, child);
                heapifyDown(child); // continue heapify from child level
            }
        }
    }


    /**
     * Readjust min-heap property by swapping parent and childs
     * @param child Index of current item
     */
    private void heapifyUp(int child) {
        int parent = (child - 1) / 2;
        if (parent >= 0) {
            // make sure parent is smaller than child
            if(theData[child].compareTo(theData[parent]) < 0) {
                swap(theData, parent, child);
                heapifyUp(parent); // continue heapify from parent level
            }
        }
    }

    /**
     * Swaps given two array item
     * @param arr The array
     * @param i Index of an item
     * @param j Index of other item
     */
    private static <E> void swap(E[] arr, int i, int j) {
        var temp = arr[i];
        arr[i] = arr[j];
        arr[j] = temp;
    }

    /**
     * Doubles the current heap capacity
     */
    @SuppressWarnings("unchecked")
    private void resize() {
        E[] oldData = theData;
        theData = (E[]) new Comparable[oldData.length * 2];
        for (int i = 0; i < size; ++i)
            theData[i] = oldData[i];
    }
}
