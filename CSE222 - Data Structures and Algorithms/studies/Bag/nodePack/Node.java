package nodePack;

public class Node<T> implements Cloneable { 
    private T data;
    private Node<T> next;

    public Node(T data, Node<T> next) {
        this.data = data;
        this.next = next;
    }

    public Node(T data) {this(data, null);}
    
    public Node() {this(null, null);}

    public void setData(T data) {this.data = data;}

    public void setNextNode(Node<T> next) {this.next = next;}

    public T getData() {return data;}

    public Node<T> getNextNode() {return next;}

    @Override
    public Node<T> clone() {
        //! shallow copy
        try {
            @SuppressWarnings("unchecked")   
            Node<T> r = (Node<T>) super.clone();
            return r;
        }
        catch (CloneNotSupportedException e) {
            // this will never happen
            return null;
        }
    }
} 