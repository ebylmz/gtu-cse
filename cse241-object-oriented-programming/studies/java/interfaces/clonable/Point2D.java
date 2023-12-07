// must look this code
// http://www.javapractices.com/topic/TopicAction.do?Id=71

// Oracle explanation Object.clone() method 
// https://docs.oracle.com/javase/6/docs/api/java/lang/Object.html#clone()

// https://stackoverflow.com/questions/11905630/java-super-clone-method-and-inheritance

package clonable;

public class Point2D implements Cloneable {
    private int _x;  // x axis 
    private int _y;  // y axis

    public Point2D (int x, int y) {
       _x = x;
       _y = y;
    }

    public Point2D () {
        this(0, 0);
    }

    public void setX (int x) {_x = x;}
    
    public void setY (int y) {_y = y;}

    public int x () {return _x;}

    public int y () {return _y;}

    public double distance (Point2D other) {
        int h = y() - other.y();    // height
        int w = x() - other.x();    // widht
        return Math.sqrt(h*h + w*w);
    }

    // Object.equals() cheks if they are in the same memory location 
    // Object.equals() is same as operator==
    public boolean equals(Point2D other) {
        return x() == other.x() && y() == other.y();
    }

    public String toString () {
        return String.format("(%d, %d)", x(), y());
    }
    
    // implement Cloneable and downcast the result of super.clone() to your class. 
    // Another burden is that the call to super.clone() 
    // can throw a CloneNotSupportedException that you have to catch, even
    // though you know it won't happen (since your class implements Cloneable).
    public Point2D clone () {
        try {
            Point2D copy = (Point2D) super.clone();
            return copy; 
        }
        catch (CloneNotSupportedException e) {
            // this will never happen because exception throwns
            // only if Clonable interface isn't implemented 
            return null;
        }
    }  
}
