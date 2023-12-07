public class Stack<T> {
    private int _size;
    private Node<T> _head;
    private Node<T> _tail;

    Stack () {
        _size = 0;
    }

    Stack (Node<T> head) {
        _tail = _head = head;
        _size = 0;

        if (_tail != null)
            while (_tail.next() != null)
                _tail = _tail.next(); 
    }

    public String toString () {
        String content = new String();

        for (var trav = _head; trav != null; trav = trav.next())
            content += (trav == _head) ? trav.data() : ", " + trav.data();

        return String.format("{%s} (size: %d)", content, size());
    }

    public Stack<T> clone () {
        Stack<T> newstack = new Stack<T>();

        for (var trav = _head; trav != null; trav = trav.next())
            newstack.push(trav.clone());    //!!!
        return newstack;
    }

    public int size () {return _size;}

    public void push (Node<T> newnode) {
        if (_head == null)
            _head = _tail = newnode; 
        else {
            _tail.setNext(newnode);
            _tail = newnode;
        }
        ++_size;
    }

    public void push (T data) {
        push(new Node<T>(data, _tail, null));
    }

    public void pop () {
        if (size() > 0) {
            _tail = _tail.prev();        
            --_size;

            if (size() == 0)  
                _head = null;
        }
    }

    public T peek () throws NullPointerException {
        if (size() == 0)
            throw new NullPointerException();
        return _tail.data();
    }

    public void destroy () {
        // free the memory by forgetting addresses            
        while (_head != null) {
            var tmp = _head;
            _head = _head.next();
            //! NOT IMPLEMENTED YET
        }
        
        _tail = null;
        _size = 0;
    }

    public void display () {
        for (Node<T> trav = _head; trav != null; trav = trav.next())
            System.out.printf("%s ", trav.data());
        System.out.println();
    }

    public void displayReverse () {
        for (Node<T> trav = _tail; trav != null; trav = trav.prev())
            System.out.printf("%s ", trav.data());
        System.out.println();
    }
}
