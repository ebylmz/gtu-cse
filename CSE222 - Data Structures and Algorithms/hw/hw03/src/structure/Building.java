package src.structure;

public class Building extends Land {
    // each building has an owner
    private String _owner;

    /**
     * Initializes the building with given parameters
     * @param location
     * @param width
     * @param heigth
     * @param owner
     */
    public Building(int location, int width, int heigth, String owner) {
        super(location, width, heigth);
        setOwner(owner);
    }

    /**
     * Initializes the building with given parameters
     * @param location
     * @param width
     * @param heigth
     */
    public Building(int location, int width, int heigth) {
        this(location, width, heigth, null);
    }

    /**
     * Initializes the building as default value (0)
     */
    public Building() {
        this(0, 0, 0, null); 
    }

    /**
     * Sets owner of the build
     * @param owner
     */
    public void setOwner(String owner) {_owner = owner;}

    /**
     * Owner of the build
     * @return owner
     */
    public String getOwner() {return _owner;}

    @Override
    public String toString() {
        return String.format(
            "%sOwner: %s\n", super.toString(), _owner);
    }

    @Override
    public int hashCode() {
        if (_owner != null && _owner.length() > 0)
            return 31 *(super.hashCode() + _owner.charAt(0));
        else
            return super.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (super.equals(obj)) {
            Building other = (Building) obj;

            if (_owner == null)
                return (other._owner == null);
            else
                return _owner.equals(other._owner);
        }
        return false;
    } 

    @Override
    public Building clone() {
        Building r = (Building) super.clone();
        r._owner = new String(_owner);
        return r;
    }
}
