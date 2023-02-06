package clonable;

public class Point3D extends Point2D {
    private int _z; // z axis

    public Point3D (int x, int y, int z) {
        super(x, y);
        _z = z;
    }

    public Point3D () {
        this(0, 0, 0);
    }

    public void setZ (int z) {_z = z;}

    public int z () {return _z;}

    public double distance (Point3D other) {
        double d = z() - other.z(); // depth
        double v = super.distance(other);   
        return Math.sqrt(v*v + d*d);
    }

    public boolean equals(Point3D other) {
        return x() == other.x() && y() == other.y() && z() == other.z();
    }

    public String toString () {
        return String.format("(%d, %d, %d)", x(), y(), z());
    }

    public Point3D clone () {
        Point3D copy = (Point3D) super.clone();
        return copy;
    }
}