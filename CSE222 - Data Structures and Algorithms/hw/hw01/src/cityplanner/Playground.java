package src.cityplanner;

public class Playground extends Land {
    /** Fix height of the playground */
    public static final int HEIGHT = 1;

    /**
     * Initializes the Playground with given parameters
     * @param location
     * @param width
     */
    public Playground(int location, int width) {
        super(location, width, HEIGHT); // fixed height
    }

    /**
     * Initializes the Playground with given location
     * @param location
     */
    public Playground(int location) {
        this(location, 1); // fixed height
    }

    public Playground() {
        super(0, 0, HEIGHT);
    }

    /** hight cannot be change */
    public void setHeight(int heigth) {
        super.setHeight(HEIGHT);
    }

    // toString already overriden

    public int hashCode() {
        return 31 *(super.hashCode() + HEIGHT); 
    }

    @Override
    public boolean equals(Object obj) {
        return super.equals(obj);
    } 

    @Override
    public Playground clone() {
        return (Playground) super.clone();
    }
}
