package src.structure;

public class House extends Building {
    private int _numRooms;
    private String _color;    
    
    /**
     * Initializes the house with given parameters
     * @param location
     * @param width
     * @param heigth
     * @param owner 
     * @param numRooms number of rooms of house
     * @param color color of house
     */
    public House(int location, int width, int heigth, String owner, int numRooms, String color) {
        super(location, width, heigth, owner);
        setNumRooms(numRooms);
        setColor(color);
    }

    /**
     * Initializes the house with given parameters
     * @param location
     * @param width
     * @param heigth
     * @param owner
     */
    public House(int location, int width, int heigth, String owner) {
        this(location, width, heigth, owner, 1, null);
    }

    /**
     * Initialized house with default values
     */
    public House() {
        this(0, 0, 0, null, 0, null);
    }

    /**
     * Set number of rooms of house
     * @param numRooms
     */
    public void setNumRooms(int numRooms) {
        _numRooms = numRooms > 0 ? numRooms : 1; 
    }

    /**
     * Number of rooms of house
     * @return number of room
     */
    public int getNumRooms() {return _numRooms;}

    /**
     * Set color of the house
     * @param color house color
     */
    public void setColor(String color) {_color = color;}

    /**
     * House color
     * @return house color
     */
    public String getColor() {return _color;}
    
    @Override
    public String toString() {
        return String.format("%sNumber of Rooms: %d\nColor: %s\n", super.toString(), _numRooms, _color);
    }

    public int hashCode() {
        return 31 *(super.hashCode() + 
            _numRooms + ((_color != null && _color.length() > 0) ? _color.charAt(0) : 0));
    }

    @Override
    public boolean equals(Object obj) {
        if (super.equals(obj)) {
            House other = (House) obj;
            
            if (_numRooms == other._numRooms) {
                if (_color == null)
                    return other._color == null;
                else
                    return _color.equals(other._color);
            }
        }
        return false;
    } 

    @Override
    public House clone() {
        House r = (House) super.clone();
        r._color = new String(_color);
        return r;
    }
}