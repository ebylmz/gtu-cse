package src.street;

import src.structure.Land;

public interface StreetInterface {
    /** the top margin value for the highest structure between screen canvas */
    int TOP_MARGIN = 5;    
    
    /*** Length of the street */
    int getLength();
    
    /**
     * Sets the length of the street 
     * @param newLength
     * @throws IllegalArgumentException in case of negative value for length
     */
    void setLength(int newLength) throws IllegalArgumentException;

    /**
     * Length of the remaining space that can be added new buildings
     * @return remaining space
     */
    int getRemainingSpace();
    
    /**
     * the number of placed land to the street
     * @return land count
     */
    int getLandCount();

    /**
     * Returns the land which is indicated by its index  
     * @param index
     * @return desired land
     * @throws IndexOutOfBoundsException in case of invalid index values
     */
    Land get(int index) throws IndexOutOfBoundsException;

    /**
     * Adds given building to street if there is a place for that
     * @param land
     * @return false if no enough place, else true
     */
    boolean add(Land land);
    
    /**
     * Removes the placed land 
     * @param land
     */
    boolean remove(Land land);
    
    /**
     * Removes the structured land at given index 
     * @param index
     * @return
     * @throws IndexOutOfBoundsException if (index < 0 || index >= landCount())
     */
    Land remove(int index) throws IndexOutOfBoundsException;
    
    /** Removes all the placed lands in the street */
    void clear();
    
    /**
     * Finds the land at the given location
     * @param location land location
     * @return if empty land returns null, else land itself
     */
    Land find(int location);
    
    /**
     * Checks if given land exist in the street
     * @param land
     * @return if given land does not exist returns -1 else its index
     */
    int find(Land land);

    /**
     * Finds the total length and number of instance of given land type
     * @param totalLand
     * @return number of instance at given land type (Market, Office etc.)
     */
    int getAnalysis(Land totalLand);

    /*** Lists all the placed lands in the street with its general informations */
    void list();

    /**
     * Display specific informations of the land (if exist) at given location
     * @param location land location
     */
    void focus(int location);
    
    /*** Displays the skyline silhouette of the street */
    void displaySkylineSilhouette();

    StreetInterface clone();
}