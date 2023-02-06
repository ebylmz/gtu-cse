import java.util.AbstractList;

/**
 * Recursive implementation of the Class Linked List
 * This is not the most efficient way of implementing linked list,
 * just it's a suitable application for recursion
 */
public class RecLinkedList<E> extends AbstractList<E> {
    private Node<E> head;

    public RecLinkedList() {
        head = null;
    }

    @Override
    public int size() {
        return size(head);
    }

    private int size(Node<E> head) {
        return head == null ? 0 : 1 + size(head.next);
    }

    @Override
    public boolean add(E e) {
        if (head == null)
            head = new Node<E>(e);
        else 
            add(head, e);
        return true;
    }

    /**
     * pre: head is not null
     * @param head head of the list
     * @param e item that will be inserted at the end
     */
    private void add(Node<E> head, E e) {
        if (head.next == null)
            head.next = new Node<E>(e);
        else
            add(head.next, e);
    }

    @Override
    public void add(int index, E e) throws IndexOutOfBoundsException {
        if (index < 0 || index > size())
            throw new IndexOutOfBoundsException();
        if (index == 0)
            head = new Node<E>(e, head);
        else
            add(head, index, e);
    }

    private void add(Node<E> head, int index, E e) {
        if (index == 1)
            head.next = new Node<E>(e, head.next);
        else
            add(head.next, index - 1, e);
    }

    public void replace(E oldData, E newData) {
        replace(head, oldData, newData);
    }

    private void replace(Node<E> head, E oldData, E newData) {
        if (head != null) {
            if (head.data.equals(oldData))
                head.data = newData;
            // continue to replace other occurances by head.next
            replace(head.next, oldData, newData);
        }
    }

    @Override
    public boolean remove(Object o) {
        if (head == null)
            return false;
        else if (head.data.equals(o)) {
            head = head.next;
            return true;
        }
        else
            return remove(head, o);
    }

    private boolean remove(Node<E> head, Object o) {
        if (head.next == null)
            return false;
        else {
            if (head.next.data.equals(o)) {
                head.next = head.next.next;
                return true;
            }
            else 
                return remove(head.next, o);
        }
    }

    @Override
    public E remove(int index) throws IndexOutOfBoundsException {
        if (index < 0 || index >= size())
            throw new IndexOutOfBoundsException();
        if (index == 0) {
            var data = head.data;
            head = head.next;
            return data;
        }
        else
            return remove(head, index);
    }

    private E remove(Node<E> head, int index) {
        if (index == 1) {
            var data = head.next.data;
            head.next = head.next.next;
            return data;
        }
        else 
            return remove(head.next, index - 1);
    }

    @Override
    public void clear() {
        // this will be different than C, C++ because it's enough to just use free/delete
        // but in java to free an object (done by gc), there must be no owner of that object means 
        // nobody referancing it. So here remove all the links between nodes to make it
        if (head != null) {
            clear(head);
            head = null;
        }
    }

    /**
     * pre: list is non empty (head is not null)
     * @param head head of the list
     */
    private void clear(Node<E> head) {
        if (head.next != null) {
            clear(head.next);
            head.next = null;
        }
    }

    private static class Node<E> {
        private E data;
        private Node<E> next;

        public Node(E data, Node<E> next) {
            this.data = data;
            this.next = next;
        }

        public Node(E data) {
            this.data = data;
            next = null;
        }
    }

    @Override
    public String toString() {
        if (head == null)
            return null;
        else {
            StringBuffer buffer = new StringBuffer();
            toString(head, buffer);
            return buffer.toString();
        }
    }

    /**
     * pre: head is not null
     * @param head hea of the nonempty list
     * @param buffer string buffer to store nodes data
     */
    private void toString(Node<E> head, StringBuffer buffer) {
        buffer.append(head. data);
        if (head.next != null) {
            buffer.append(" ==> ");
            toString(head.next, buffer);
        }
    }

    @Override
    public E get(int index) throws IndexOutOfBoundsException {
        try {
            return getNode(head, index).data;
        }
        catch (IndexOutOfBoundsException e) {
            // rethrow exception with more explanation
            throw new IndexOutOfBoundsException("Index out of bounds: " + index);
        }
    }

    private Node<E> getNode(Node<E> head, int index) throws IndexOutOfBoundsException {
        if (head == null)
            throw new IndexOutOfBoundsException();
        else if (index == 0)
            return head;
        else
            return getNode(head.next, index);      
    }
}
