package src.structure;

public class Land implements Cloneable {
    private int _location; // center location of land
    private int _width;
    private int _heigth;

    /**
     * Initializes Land with given location, width and height
     * @param location
     * @param width
     * @param heigth
     */
    public Land(int location, int width, int heigth) {
        setWidth(width);
        setLoc(location);
        setHeight(heigth);
    }

    /**
     * Initializes Land with given location, width and 0 height
     * @param location
     * @param width
     */
    public Land(int location, int width) {
        this(location, width, 0);   // empty land
    }

    /**
     * Initializes Land with 0 height and width, at location 0
     * 
     */
    public Land() {
        this(0, 0, 0);   // point
    }

    /**
     * Sets location of the land
     * @param loc location
     */
    public void setLoc(int loc) {
        _location = loc;
    }

    /**
     * Location of the land
     * @return location of the land
     */
    public int getLoc() {return _location;}

    /**
     * Set width
     * @param width
     * @throws IllegalArgumentException in case of negative values for width 
     */
    public void setWidth(int width) throws IllegalArgumentException {
        if (width >= 0)
            _width = width;
        else
            throw new IllegalArgumentException();
    }

    /**
     * Width of the land
     * @return width
     */
    public int getWidth() {return _width;}

    /**
     * Set height
     * @param heigth
     * @throws IllegalArgumentException in case of negative values for height
     */
    public void setHeight(int heigth) throws IllegalArgumentException {
        if (heigth >= 0)
            _heigth = heigth;
        else
            throw new IllegalArgumentException();
    }

    /**
     * Height of the land
     * @return height
     */
    public int getHeight() {return _heigth;}

    /**
     * Returns location, width and height as formatted string
     * @return string
     */
    public String getDimensionInfo() {
        return String.format("(loc: %+3d\tw: %3d\th: %3d)", _location, _width, _heigth);
    }

    @Override
    public String toString() {
        return String.format(
            "Location: %d\nWidth: %d\nHeight: %d\n", 
            _location, _width, _heigth);
    }

    @Override
    public int hashCode() {
        return 31 * (_location + _width + _heigth);
    }

    @Override
    public Land clone() {
        try {
            // shallow copy
            return (Land) super.clone();
        }
        catch (CloneNotSupportedException e) {
            // this will never happen            
            return null;
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        else if (obj == null || this.getClass() != obj.getClass())
            return false;
        Land other = (Land) obj;
        return  _location == other._location &&
                _heigth == other._heigth &&
                _width == other._width;            
    }
}
