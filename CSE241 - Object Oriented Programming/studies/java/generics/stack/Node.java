public class Node<T> {
    private T _data;
    private Node<T> _next;
    private Node<T> _prev;

    public Node (T data, Node<T> prev, Node<T> next) {
        //! shallow copy 
        _data = data; // _data = data.clone();
        _prev = prev;
        _next = next;    
    }

    public Node (T data) {
        this(data, null, null);
    }

    public Node () {}

    public Node<T> clone () {
        return new Node<T>(data(), prev(), next());
    }
    
    public void setData (T data) {_data = data;}
    public void setNext (Node<T> next) {_next = next;}
    public void setPrev (Node<T> prev) {_prev = prev;}

    public T data () {return _data;}
    public Node<T> next () {return _next;}
    public Node<T> prev () {return _prev;}

}
