import java.util.AbstractList;
import java.util.Collection;
import java.util.Iterator;
import java.util.ListIterator;
import java.util.NoSuchElementException;

public class LinkedList<E> extends AbstractList<E> {
    private Node<E> head;
    private Node<E> tail;
    private int size;
    
    public LinkedList() {
        head = tail = null;
        size = 0;
    }

    public void addFirst(E e) {
        add(0, e);
    }
    
    public void addLast(E e) {
        add(size(), e);
    }
    
    public E getFirst(E e) {
        return head.data;
    }
    
    public E getLast(E e) {
        return tail.data;
    }

    @Override
    public Iterator<E> iterator() {
        return new LDListIter();
    }

    @Override
    public ListIterator<E> listIterator() {
        return new LDListIter();
    }

    @Override
    public ListIterator<E> listIterator(int index) {
        return new LDListIter(index);
    }

    @Override
    public boolean add(E e) {
        listIterator(size()).add(e);
        return true;
    }

    @Override
    public void add(int index, E e) {
        listIterator(index).add(e);
    }

    @Override
    public boolean addAll(Collection<? extends E> c) {
        for (var item : c)
            add(item);
        return true;
    }

    @Override
    public boolean remove(Object o) {
        var it = listIterator();
        while (it.hasNext()) {
            // pre requirement of remove function (calling next or prev methods)
            var item = it.next();
            if (item.equals(o)) {
                it.remove();
                return true;
            }
        }        
        return false;
    }

    @Override
    public E remove(int index) {
        var it = listIterator();
        // pre requirement of remove function (calling next or prev methods)
        E data = it.next();
        it.remove();
        return data;
    }

    @Override
    public boolean removeAll(Collection<?> c) {
        boolean isListChange = false;
        for (var item : c)
            if (remove(item))
                isListChange = true;
        return isListChange;
    }

    @Override
    public void clear() {
        var it = listIterator();
        while (it.hasNext()) {
            it.next();
            it.remove();
        }
    }

    @Override
    public boolean retainAll(Collection<?> c) {
        boolean isListChange = false;
        for (var item : c)
            if (!contains(item)) {
                remove(item);
                isListChange = true;
            }
        return isListChange;
    }

    @Override
    public boolean contains(Object o) {
        // this class uses LinkeListIter for foreach loop
        for (var item : this)
            if (item.equals(o))
                return true;
        return false;
    }

    @Override
    public boolean isEmpty() {
        return head == null;
    }

    @Override
    public E get(int index) {
        return listIterator(index).next();
    }

    @Override
    public E set(int index, E e) {
        var it = listIterator(index);
        E data = it.next();
        it.set(e);
        return data; 
    }


    @Override
    public int size() {
        return size;
    }

    @Override
    public Object[] toArray() {
        Object[] arr = new Object[size];
        Iterator<E> it = iterator();
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
    public LinkedList<E> clone() {
        try {
            @SuppressWarnings("unchecked")
            LinkedList<E> newList = (LinkedList<E>) super.clone();
            newList.size = 0;
            newList.head = null;
            newList.tail = null;

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

    private class LDListIter implements ListIterator<E> {
        /** A reference to the next item (next item can be return by next() or previous()). */        
        private Node<E> nextItem;
        /** A reference to the last item returned. */
        private Node<E> lastItemReturned;
        /** Index of the current item */
        private int index;

        /**
         * Construct a ListIter that will reference the ith item.
         * @param i The index of the item to be referenced
        */
        public LDListIter(int index) {
            if (index < 0 || index > size())
                throw new IndexOutOfBoundsException();
            this.index = index;
            lastItemReturned = null;
            // iterator at the end of the list
            if (index == size) 
                nextItem = null;
            else { // start at the beginning
                nextItem = head;
                for (int i = 0; i < index; ++i)
                    nextItem = nextItem.next;
            }
        }

        public LDListIter() {
            this(0);
        }

        /** 
         * Add a new item between the item that will be returned
         * by next and the item that will be returned by previous.
         * If previous is called after add, the element added is
         * returned.
         * @param obj The item to be inserted
        */
        @Override
        public void add(E e) {
            Node<E> newItem = new Node<E>(e);
            // add at the beginning of the list (update head)
            if (nextItem == head) {                    
                newItem.next = head;
                newItem.prev = null;
                // if the list was empty, new item become both head and tail
                if (head == null)
                    tail = newItem;
                else   
                    head.prev = newItem;
                head = newItem;
            }
            else if (nextItem == null) { // add at the end of the list (update tail)
                newItem.next = null;
                newItem.prev = tail;
                tail.next = newItem;
                tail = newItem;
            }
            else { // add middle of the list
                newItem.next = nextItem;
                newItem.prev = nextItem.prev;
                nextItem.prev.next = newItem;
                nextItem.prev = newItem;
            }
            // increase size and index and invalidate lastItemReturned
            ++size;
            ++index;
            lastItemReturned = null;
        }   

        /**
         * Indicate whether movement forward is defined.
         * @return true if call to next will not throw an exception
         */
        @Override
        public boolean hasNext() {
            return nextItem != null;
        }


        /** 
         * Indicate whether movement backward is defined.
         * @return true if call to previous will not throw an exception
         */
        @Override
        public boolean hasPrevious() {
            //! nextItem became null if the iterator at the end of the list
            return index > 0; //!
        }

        /** Move the iterator forward and return the next item.
         * @return The next item in the list
         * @throws NoSuchElementException if there is no such object
         */
        @Override
        public E next() throws NoSuchElementException {
            if (!hasNext())
                throw new NoSuchElementException();
            lastItemReturned = nextItem;
            nextItem = nextItem.next;
            ++index;
            return lastItemReturned.data;
        }

        /** 
         * Return the index of the next item to be returned by next
         * @return the index of the next item to be returned by next
        */
        @Override
        public int nextIndex() {
            return index; 
        }

        /**
         * Move the iterator backward and return the previous item.
         * @return The previous item in the list
         * @throws NoSuchElementException if there is no such object
        */
        @Override
        public E previous() {
            if (!hasPrevious())
                throw new NoSuchElementException();
            
            // iterator iterator past the last element during forward iteration
            if (nextItem == null) 
                nextItem = tail;
            else
                nextItem = nextItem.prev;
            lastItemReturned = nextItem;
            --index;
            return lastItemReturned.data;
        }

        /** 
         * Return the index of the next item to be returned by previous
         * @return the index of the next item to be returned by previous
        */
        @Override
        public int previousIndex() {
            return index - 1;
        }

        /**
         *  Remove the last item returned. This can only be
         *  done once per call to next or previous.
         *  @throws IllegalStateException if next or previous
         *  was not called prior to calling this method
         */
        @Override
        public void remove() {
            if (lastItemReturned == null)
                throw new IllegalStateException();
            
            if (lastItemReturned.next == null) { // item is the tail 
                tail = lastItemReturned.prev;
                if (tail != null)
                    tail.next = null;
            }

            if (lastItemReturned.prev == null) { // item is the head
                head = head.next;
                if (head != null)
                    head.prev = null;
            }
            else 
                lastItemReturned.prev.next = lastItemReturned.next;
            // decrement size and index and invalidate lastItemReturned
            --size;
            --index;
            lastItemReturned = null;
        }

        /** Replace the last item returned with a new value
         *  @param item The new value
         *  @throws IllegalStateException if next or previous
         *  was not called prior to calling this method
         */
        @Override
        public void set(E e) throws IllegalStateException {
            if (lastItemReturned == null)
                throw new IllegalStateException();
            lastItemReturned.data = e;            
        }
        
    }
    
    private static class Node<E> {
        private E data;
        private Node<E> next;
        private Node<E> prev;

        public Node(E data, Node<E> next, Node<E> prev) {
            this.data = data;
            this.next = next;
            this.prev = prev;
        }

        public Node(E data) {
            this(data, null, null);
        }
    }
}