package src.street;

import java.util.Collection;

import src.structure.*;
import src.util.AnsiEscape;

public class StreetArray implements StreetInterface, Cloneable {
    private Land[] _landList; // keeps all the filled lands in the street
    private int _landCount; 
    private int[] _posSide;
    private int[] _negSide;
    private int _maxLandHeight;

    /**
     * Initializes the street with given length
     * @param streetLength length of the street
     */
    public StreetArray(int streetLength) {
        setLength(streetLength);
        _maxLandHeight =_landCount = 0;
        _landList = new Land[1];
    }

    /**
     * Initializes the street with given lands
     * @param c collection that contain lands which will be added to the street
     * @param streetLength length of the street
     */
    public StreetArray(Collection<? extends Land> c, int streetLength) {
        this(streetLength);
        for (var land : c)
            add(land);
    }
    
    /*** Initializes an empty street with defualt length 64 */
    public StreetArray() {
        this(64); 
    }

    /*** Length of the street */
    public int getLength() {return _posSide == null ? 0 : _posSide.length;}
    
    /**
     * Sets the length of the street 
     * @param newLength
     * @throws IllegalArgumentException in case of negative value for length
     */
    public void setLength(int newLength) throws IllegalArgumentException {
        if (newLength < 0)
            throw new IllegalArgumentException();

        // since remove method always removed from at the end, apply reverse iteration
        if (newLength < getLength()) {
            for (int i = _landCount - 1; i >= 0; --i) {
                int[] locs = getSilhouetteLoc(_landList[i]);
                if (locs[1] >= newLength)
                    remove(_landList[i]);
            }
        }
        _posSide = resize(_posSide, newLength);
        _negSide = resize(_negSide, newLength);
    }

    /**
     * Returns the land which is indicated by its index  
     * @param index
     * @return desired land
     * @throws IndexOutOfBoundsException in case of invalid index values
     */
    public Land get(int index) throws IndexOutOfBoundsException {
        if (0 > index || index >= _landCount)
            throw new IndexOutOfBoundsException();
        return _landList[index];
    }

    /**
     * Length of the remaining space that can be added new buildings
     * @return remaining space
     */
    public int getRemainingSpace() {
        return getEmptyLandLength(_negSide) + getEmptyLandLength(_posSide);
    }

    /**
     * Returns the remained empty land length
     * @param streetSide
     * @return empty land length
     */
    private int getEmptyLandLength(int[] streetSide) {
        int len = 0;
        for (var spaceHeigth : streetSide)
            if (spaceHeigth == 0)
                ++len;
        return len;
    }

    /**
     * the number of placed land to the street
     * @return land count
     */
    public int getLandCount() {
        return _landCount;
    }

    /*** Lists all the placed lands in the street with its general informations */
    public void list() {
        System.out.printf("\n%-3s%-20s %-10s %-10s %-10s\n\n", "", "Structure Type", "Location", "Width", "Height");
        // displays placed lands with their location, width, height properties
        for (int i = 0; i < _landCount ; ++i) {
            String structureType = "";
            Land land = _landList[i];
            if (land instanceof Market)
                structureType = "Market";   
            else if (land instanceof House)
                structureType = "House";
            else if (land instanceof Office)
                structureType = "Office";
            else if (land instanceof Playground)
                structureType = "Playground";
            else if (land instanceof Building)            
                structureType = "Building";
            else            
                structureType = "Land";
            System.out.printf("%-3d%-20s %-+10d %-10d %-10d\n",
                i + 1, structureType, land.getLoc(), land.getWidth(), land.getHeight());  
        }
    }

    /**
     * Adds given building to street if there is a place for that
     * @param land
     * @return false if no enough place, else true
     */
    public boolean add(Land land) {
        if (land != null && canBePlaced(land)) {
            if (_landCount == _landList.length)
                _landList = resize(_landList, _landList.length * 2);
            // set the heigth of new land place to indicate an filled land place
            setSilhouette(land);

            if (land.getHeight() > _maxLandHeight)
                _maxLandHeight = land.getHeight();

            // add the given land at the end of the array
            _landList[_landCount++] = land;
            return true;
        }
        return false; 
    }

    /**
     * Returns the side of the street
     * @param location
     * @return the side of the street
     */
    private int[] getStreetSide(int location) {
        return (location > 0) ? _posSide : _negSide;
    }

    /**
     * sets silhouette of the land
     * @param land land information
     * @param landHeigth new height of the land part
     */
    private void setSilhouette(Land land, int landHeigth) {
        var streetSide = getStreetSide(land.getLoc());
        int[] locs = getSilhouetteLoc(land);

        // locs[0]: start location, locs[1] end location of the land
        for (int i = locs[0]; i < locs[1]; ++i)
            streetSide[i] = landHeigth;
    } 
    
    /**
     * sets silhouette of the land
     * @param land
     */
    private void setSilhouette(Land land) {
        setSilhouette(land, land.getHeight());
    } 

    /**
     * Gets the Sihouette location of the given land
     * Silhouette locations are always positive and between 0 to length of the stree
     * @param land
     * @return arr, start(arr[0]) and end (arr[1]) point of the land
     */
    private int[] getSilhouetteLoc(Land land) {
        int width = land.getWidth();
        int centre = land.getLoc();  

        // make sure silhouette locations are positive
        if (centre < 0) centre *= -1;
        int start = centre - width/2;
        int end = start + width;

        return new int[]{start, end};
    }

    /**
     * Checks if given land can be placed without any superposition with other lands.
     * A building can be placed at a land on the street only 
     * if there is enough land for that building in terms of length.
     * @param land
     * @return
     */
    private boolean canBePlaced(Land land) {
        var streetSide = getStreetSide(land.getLoc());
        int[] locs = getSilhouetteLoc(land);
        
        // locs[0]: start location, locs[1] end location of the land
        if (locs[1] >= getLength())
            return false;
        for (int i = locs[0]; i < locs[1]; ++i)
            // if street location is full
            if (streetSide[i] > 0)
                return false;
        return true;
    }

    /**
     * Removes the placed land 
     * @param land
     */
    public boolean remove(Land land) {
        int index = find(land);
        if (index != -1) {
            remove(index);
            return true; 
        }
        return false;
    }

    /**
     * Removes the structured land at given index 
     * @param index
     * @throws IndexOutOfBoundsException if (index < 0 || index >= landCount())
     */
    public Land remove(int index) throws IndexOutOfBoundsException {
        if (index < 0 || _landCount <= index)
            throw new IndexOutOfBoundsException();
        // set the heigth of removed land place 0 from the street silhouette to indicate empty land
        setSilhouette(_landList[index], 0);

        // remove land specific information which are kept in Land array
        --_landCount;
        Land removedLand = _landList[index];
        // swap removed element with last element
        _landList[index] = _landList[_landCount];
        _landList[_landCount] = null;

        // update the max land height
        _maxLandHeight = 0;
        for (int i = 0; i < _landCount; ++i)
            if (_landList[i].getHeight() > _maxLandHeight)
                _maxLandHeight = _landList[i].getHeight();

        return removedLand;
    }

    /** Removes all the placed lands in the street */
    public void clear() {
        // remove all the lands 
        while (getLandCount() > 0)
            remove(0);         
    }

    /**
     * Finds the land at the given location
     * @param location land location
     * @return if empty land returns null, else land itself
     */
    public Land find(int location) {
        for (int i = 0; i < _landCount; ++i) {
            // for simplicity convert locations to silhouette location (positive)
            // before that make sure two location are on the same street side 
            if (location * _landList[i].getLoc() > 0) { 
                int targetLoc = (location < 0) ? -location : location;
                // get start and end silhouette locations of the land 
                int[] locs = getSilhouetteLoc(_landList[i]);
                
                // check if our target lies between lands bound
                if (locs[0] <= targetLoc && targetLoc <= locs[1]) 
                    return _landList[i];
            }
        }
        return null;
    }

    /**
     * Checks if given land exist in the street
     * @param land
     * @return if given land does not exist returns -1 else its index
     */
    public int find(Land land) {
        if (land != null)
            for (int i = 0; i < _landList.length; ++i)
                if (land.equals(_landList[i])) 
                    return i;
        return -1;
    }

    /**
     * Finds the total length and number of instance of given land type
     * @param totalLand
     * @return number of instance at given land type (Market, Office etc.)
     */
    public int getAnalysis(Land totalLand) {
        totalLand.setHeight(0);
        totalLand.setWidth(0);
        totalLand.setLoc(0);

        int n = 0;
        for (int i = 0; i < _landCount; ++i) {
            var land = _landList[i];
            if (land.getClass().equals(totalLand.getClass())) {
                totalLand.setWidth(totalLand.getWidth() + land.getWidth());
                ++n;
            }
        }
        return n;
    }


    /**
     * Display specific informations of the land (if exist) at given location
     * @param location land location
     */
    public void focus(int location) {
        Land land = find(location);
        if (land != null) {
            if (land instanceof House) 
                System.out.printf("House owner: %s\n", ((House) land).getOwner());
            else if (land instanceof Office)
                System.out.printf("Office job Type: %s\n", ((Office) land).getJobType());
            else if (land instanceof Market)
                System.out.printf("Closing time: %s\n", ((Market) land).getClosingTime());
            else if (land instanceof Playground)
                System.out.printf("Length: %d\n", land.getWidth());
        }
    }

    /*** Displays the skyline silhouette of the street */
    public void displaySkylineSilhouette() {
        AnsiEscape.clearScreen();
        drawSkylineSilhouette(_negSide, AnsiEscape.Color.WHITE);
        drawSkylineSilhouette(_posSide, AnsiEscape.Color.BLACK);
        
        for (int i = 0; i <= getLength(); i+= 4) {
            AnsiEscape.setCursor(_maxLandHeight + TOP_MARGIN, i);
            System.out.print(i);
        }

        AnsiEscape.setBGColor(AnsiEscape.Color.WHITE); System.out.printf("\n\n  "); AnsiEscape.setBGColor(AnsiEscape.Color.DEFAULT);
        System.out.printf(" : negative location\t");
        AnsiEscape.setBGColor(AnsiEscape.Color.BLACK); System.out.printf("  "); AnsiEscape.setBGColor(AnsiEscape.Color.DEFAULT);
        System.out.printf(" : positive location\n\n");
    }

    /**
     * Draws skyline silhoutte of the street
     * @param streetSide side of the street
     * @param color structure color
     */
    private void drawSkylineSilhouette(int[] streetSide, AnsiEscape.Color color) {
        AnsiEscape.setBGColor(color);
        for (int i = 0; i < _maxLandHeight; ++i) {
            for (int j = 0; j < streetSide.length; ++j) {
                if (streetSide[j] >= _maxLandHeight - i) {
                    AnsiEscape.setCursor(i + TOP_MARGIN, j);
                    System.out.printf(" ");
                }
            }
        }
        AnsiEscape.setBGColor(AnsiEscape.Color.DEFAULT);
    }

    private Land[] resize(Land[] arr, int newsize) {
        var old = arr;
        arr = new Land[newsize];
        if (old != null) {
            int cloneindex = old.length < newsize ? old.length : newsize;
            for (int i = 0; i < cloneindex; ++i)
                arr[i] = old[i];
            old = null;
        }
        return arr;
    }

    private int[] resize(int[] arr, int newsize) {
        var old = arr;
        arr = new int[newsize];
        if (old != null) {
            int cloneindex = old.length < newsize ? old.length : newsize;
            for (int i = 0; i < cloneindex; ++i)
                arr[i] = old[i];
            old = null;
        } 
        return arr;
    }

    @Override
    public String toString() {
        return String.format("Length: %d, land count: %d\n", getLength(), getLandCount());
    }

    @Override
    public int hashCode() {
        int h = 0;
        for (int i = 0; i < _landCount; ++i)
            h += _landList[i].hashCode();
        return h;
    }

    @Override
    public StreetArray clone() {
        try {
            StreetArray r = (StreetArray) super.clone();

            r._landList = new Land[_landList.length];
            for (int i = 0; i < _landCount; ++i)
                r._landList[i] = _landList[i].clone();
            
            // negside and posside length are same
            r._negSide = new int[getLength()];
            r._posSide = new int[getLength()];
            for (int i = 0; i < getLength(); ++i) {
                r._negSide[i] = _negSide[i];
                r._posSide[i] = _posSide[i];
            }
            return r;
        }
        catch (CloneNotSupportedException e) {
            // this will never happen
            return null;
        }
    }
}