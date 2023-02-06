import java.util.AbstractList;
import java.util.Iterator;
import java.util.ListIterator;

public class SingleLinkedList<E> extends AbstractList<E> {
    private Node<E> head;
    private Node<E> tail;
    private int size;

    public SingleLinkedList() {
        head = tail = null;
        size = 0;
    } 

    @SuppressWarnings("unchecked")
    public boolean add(Object e) {
        add(size, (E) e);
        return true;
    }

    public void addFirst(E e) {
        add(0, e);
    }

    public void addLast(E e) {
        add(size, e);
    } 

    @Override
    public void add(int index, E e) {
        if (index < 0 || index > size)
            throw new IndexOutOfBoundsException();

        var newItem = new Node<E>(e);        
        // add the first
        if (index == 0) {
            newItem.next = head;
            head = newItem;
            if (size == 0) // if the list was empty
                tail = head;
        }
        else if (index == size) { // add the end 
            // size > 0 
            tail.next = newItem;
            tail = newItem;
        }
        else { // add the middle
            var prev = getNode(index - 1);
            newItem.next = prev.next; 
            prev.next = newItem;
        }
        ++size;
    }

    @Override
    public boolean remove(Object o) {
        var it = iterator();
        while (it.hasNext()) {
            var data = it.next();
            if (data.equals(o)) {
                it.remove();
                return true;
            }
        }
        return false;
    }

    @Override
    public E remove(int index) {
        if (index < 0 || index >= size)
            throw new IndexOutOfBoundsException();
        
        E data = null;
        if (index == 0) {
            data = head.data;
            head = head.next;
        }
        else if (index == size - 1) { // remove from tail
            tail = getNode(index - 1);
            data = tail.next.data;
            tail.next = null;
        } 
        else { // remove middle (at least 3 node exist)
            // prev.next is not null because we are middle of the list
            var prev = getNode(index - 1);
            data = prev.next.data;
            prev.next = prev.next.next;
        }
        --size;
        return data;
    }

    @Override
    public E set(int index, E e) throws IndexOutOfBoundsException {
        var item = getNode(index);
        var oldData = item.data;
        item.data = e;
        return oldData;
    }

    @Override
    public E get(int index) throws IndexOutOfBoundsException {
        if (index < 0 || index > size)
            throw new IndexOutOfBoundsException();

        var it = iterator();
        for (int i = 0; i < index; ++i) // iterator().hasNext()
            it.next();
        return it.next(); //!!
    }

    private Node<E> getNode(int index) throws IndexOutOfBoundsException {
        if (index < 0 || index > size)
            throw new IndexOutOfBoundsException();
        
        var trav = head;
        for (int i = 0; i < index; ++i)
            trav = trav.next;
        return trav; 
    }

    @Override
    public void clear() {
        var it = iterator();
        while (it.hasNext()) {
            it.next();
            it.remove();
        }
        size = 0;
    }

    @Override
    public boolean contains(Object o) {
        for (var item : this)
            if (item.equals(o))
                return true;
        return false;
    }

    @Override
    public int size() {
        return size;
    }

    @Override
    public boolean isEmpty() {
        return head == null;
    }

    @Override
    public ListIterator<E> listIterator() throws UnsupportedOperationException {
        throw new UnsupportedOperationException();
    }

    @Override
    public ListIterator<E> listIterator(int index) throws UnsupportedOperationException {
        throw new UnsupportedOperationException();
    }

    @Override
    public Iterator<E> iterator() {
        return new SingleLinkedListIter();
    }

    private class SingleLinkedListIter implements Iterator<E> {
        private Node<E> nextItem;
        private Node<E> lastItemReturned;
        private Node<E> lastItemReturnedPrev;

        public SingleLinkedListIter() {
            nextItem = head;
            lastItemReturned = null; 
            lastItemReturnedPrev = null;
        }
        
        @Override
        public boolean hasNext() {
            return nextItem != null;
        }
        @Override
        public E next() {
            lastItemReturnedPrev = lastItemReturned;
            lastItemReturned = nextItem;
            nextItem = nextItem.next;
            return lastItemReturned.data;
        }
        
        @Override
        public void remove() throws IllegalStateException {
            if (lastItemReturned == null)
                throw new IllegalStateException();

            if (lastItemReturned == head) {
                head = head.next;
                if (size == 1)
                    tail = head; // head and tail become null
            }
            else {
                lastItemReturnedPrev.next = nextItem;
                if (nextItem == null) // update the tail
                    tail = lastItemReturned.next;
            }
            lastItemReturned = null;
            --size;
        }
    }

    private static class Node<E> {
        private E data;
        private Node<E> next;

        public Node(E data) {
            this.data = data;
            next = null;
        }

        public Node(E data, Node<E> next) {
            this.data = data;
            this.next = next;
        }
    }
}