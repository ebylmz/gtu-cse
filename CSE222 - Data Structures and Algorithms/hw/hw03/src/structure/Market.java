package src.structure;

import src.util.Time;

public class Market extends Building {
    private Time _openingTime;
    private Time _closingTime;
    
    /**
     * Initializes the Market with given parameters
     * @param location
     * @param width
     * @param heigth
     * @param owner
     * @param openingTime opening time of the market
     * @param closingTime closing time of the market
     */
    public Market(int location, int width, int heigth, String owner, Time openingTime, Time closingTime) {
        super(location, width, heigth, owner);
        setOpeningTime(openingTime);
        setClosingTime(closingTime);
    }

    /**
     * Initializes the Market with given parameters
     * @param location
     * @param width
     * @param heigth
     * @param owner
     */
    public Market(int location, int width, int heigth, String owner) {
        this(location, width, heigth, owner, null, null);
    }

    /**
     * Initializes the öarket witdh default values
     */
    public Market() {
        this(0, 0, 0, null, null, null);
    }

    /**
     * Set opening time of the market
     * @param time opening time
     */
    public void setOpeningTime(Time time) {_openingTime = time;}
    
    /**
     * Opening time of the market
     * @return opening time
     */
    public Time getOpeningTime() {return _openingTime;}

    /**
     * Set closing time of the market
     * @return closing time
     */
    public Time getClosingTime() {return _closingTime;}

    /**
     * Closing time of the market
     * @param time closing time
     */
    public void setClosingTime(Time time) {_closingTime = time;}

    // The market presents its closing time.
    @Override
    public String toString() {
        return String.format(
            "%sOpening Time: %s\nClosing Time: %s\n", 
            super.toString(), _openingTime.toString(), _closingTime.toString());
    }

    public int hashCode() {
        return 31 *(super.hashCode() + _openingTime.hashCode() + _closingTime.hashCode());
    }

    @Override
    public boolean equals(Object obj) {
        if (super.equals(obj)) {
            Market other = (Market) obj;
            if (_openingTime != null ? _openingTime.equals(other._openingTime) : other._openingTime == null) 
                return 
                    _closingTime != null ? _closingTime.equals(other._closingTime) : other._closingTime == null;
        }
        return false;
    } 

    @Override
    public Market clone() {
        Market r = (Market) super.clone();
        r._openingTime = _openingTime.clone();
        r._closingTime = _closingTime.clone();
        return r;
    }
}