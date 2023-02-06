package stack;

import java.util.NoSuchElementException;

public class LListStack<E> implements StackInterface<E> {   
   private Node<E> top;    // head of the linkedList
   private int size;

   public LListStack() {
       top = null;
       size = 0; 
   }

    @Override
    public void push(E newEntry) {
        top = new Node<E>(newEntry, top);
    }

    @Override
    public E pop() {
        if (isEmpty())
            throw new NoSuchElementException();
        var data = top.data;
        top = top.next;
        return data;
    }

    @Override
    public E peek() {
        if (isEmpty())
            throw new NoSuchElementException();
        return top.data;
    }

    @Override
    public boolean isEmpty() {
        return top == null; // size == 0
    }

    @Override
    public int size() {
        return size;
    }

    @Override
    public void clear() {
        while (top != null)
            top = top.next;
        size = 0;
    }

    private static class Node<E> {
        private E data;
        private Node<E> next;

        public Node(E data, Node<E> next) {
            this.data = data;
            this.next = next;
        }
    }
}
