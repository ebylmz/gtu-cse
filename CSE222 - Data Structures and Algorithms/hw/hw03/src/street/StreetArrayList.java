package src.street;

import java.util.ArrayList;
import java.util.Collection;

import src.structure.*;
import src.util.AnsiEscape;

public class StreetArrayList implements StreetInterface, Cloneable {
    private ArrayList<Land> _landList;
    private int _streetLength;
    private ArrayList<Integer> _posSide;
    private ArrayList<Integer> _negSide;
    private int _maxLandHeight;

    /**
     * Initializes the street with given length
     * @param streetLength length of the street
     */
    public StreetArrayList(int streetLength) {
        _landList = new ArrayList<>();
        _negSide = new ArrayList<>();
        _posSide = new ArrayList<>();
        _maxLandHeight = 0;
        setLength(streetLength);
    }

    /**
     * Initializes the street with given lands
     * @param c collection that contain lands which will be added to the street
     * @param streetLength length of the street
     */
    public StreetArrayList(Collection<? extends Land> c, int streetLength) {
        this(streetLength);
        for (var land : c)
            add(land);
    }

    /*** Initializes an empty street with defualt length 64 */
    public StreetArrayList() {
        this(64); 
    }


    /*** Length of the street */
    @Override
    public int getLength() {
        return _streetLength;
    }

    /**
     * Sets the length of the street 
     * @param newLength
     * @throws IllegalArgumentException in case of negative value for length
     */
    @Override
    public void setLength(int newLength) throws IllegalArgumentException {
        if (newLength < 0)
            throw new IllegalArgumentException();

        // since remove method always removed from at the end, apply reverse iteration
        if (newLength < getLength()) {
            for (int i = _landList.size() - 1; i >= 0; --i) {
                int[] locs = getSilhouetteLoc(_landList.get(i));
                if (locs[1] >= newLength)
                    remove(_landList.get(i));
            }
            
            // remove the last n item
            int n = _streetLength - newLength;
            for (int i = 0; i < n; ++i) {
                _posSide.remove(_streetLength - 1 - i);
                _negSide.remove(_streetLength - 1 - i);
            }
        }
        else {
            // add new empty n item
            int n = newLength - _streetLength;
            for (int i = 0; i < n; ++i) {
                _posSide.add(0);
                _negSide.add(0);
            }
        }
        _streetLength = newLength;
    }

    /**
     * Length of the remaining space that can be added new buildings
     * @return remaining space
     */
    @Override
    public int getRemainingSpace() {
        return getEmptyLandLength(_negSide) + getEmptyLandLength(_posSide);
    }

    /**
     * Returns the remained empty land length
     * @param streetSide
     * @return empty land length
     */
    private int getEmptyLandLength(ArrayList<Integer> streetSide) {
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
    @Override
    public int getLandCount() {
        return _landList.size();
    }

    /**
     * Returns the land which is indicated by its index  
     * @param index
     * @return desired land
     * @throws IndexOutOfBoundsException in case of invalid index values
     */
    @Override
    public Land get(int index) throws IndexOutOfBoundsException {
        return _landList.get(index);
    }

    /**
     * Adds given building to street if there is a place for that
     * @param land
     * @return false if no enough place, else true
     */
    @Override
    public boolean add(Land land) {
        // make sure given land does not cause superposition        
        if (land != null && canBePlaced(land)) {
            // set the heigth of new land place to indicate an nontempty land place
            setSilhouette(land);

            if (land.getHeight() > _maxLandHeight)
                _maxLandHeight = land.getHeight();

            // add the given land at the end of the array
            _landList.add(land);
            return true;
        }
        return false; 
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
            if (streetSide.get(i) > 0)
                return false;
        return true;
    }

    @Override
    /**
     * Removes the placed land 
     * @param land
     * @throws IndexOutOfBoundsException if (index < 0 || index >= landCount())
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
     */
    @Override
    public Land remove(int index) throws IndexOutOfBoundsException {
        if (index < 0 || _landList.size() <= index)
            throw new IndexOutOfBoundsException();
        
        // set the heigth of removed land place 0 from the street silhouette to indicate empty land
        setSilhouette(_landList.get(index), 0);

        // remove land specific information which are kept in Land array
        Land removedLand = _landList.get(index);
        _landList.remove(index);

        // update the max land height
        _maxLandHeight = 0;
        for (int i = 0; i < _landList.size(); ++i)
            if (_landList.get(i).getHeight() > _maxLandHeight)
                _maxLandHeight = _landList.get(i).getHeight();

        return removedLand;      
    }

    @Override
    /** Removes all the placed lands in the street */
    public void clear() {
        while (getLandCount() > 0)
            remove(0);
    }

    /**
     * Finds the land at the given location
     * @param location land location
     * @return if empty land returns null, else land itself
     */
    @Override
    public Land find(int location) {
        for (int i = 0; i < _landList.size(); ++i) {
            // for simplicity convert locations to silhouette location (positive)
            // before that make sure two location are on the same street side 
            if (location * _landList.get(i).getLoc() > 0) { 
                int targetLoc = (location < 0) ? -location : location;
                // get start and end silhouette locations of the land 
                int[] locs = getSilhouetteLoc(_landList.get(i));
                
                // check if our target lies between lands bound
                if (locs[0] <= targetLoc && targetLoc <= locs[1]) 
                    return _landList.get(i);
            }
        }
        return null;
    }

    /**
     * Checks if given land exist in the street
     * @param land
     * @return if given land does not exist returns -1 else index of it
     */
    @Override
    public int find(Land land) {

        if (land != null)
            for (int i = 0; i < _landList.size(); ++i)
                if (land.equals(_landList.get(i))) 
                    return i;
        return -1;
    }

    /**
     * Finds the total length and number of instance of given land type
     * @param totalLand
     * @return number of instance at given land type (Market, Office etc.)
     */
    @Override
    public int getAnalysis(Land totalLand) {
        totalLand.setHeight(0);
        totalLand.setWidth(0);
        totalLand.setLoc(0);

        int n = 0;
        for (int i = 0; i < _landList.size(); ++i) {
            var land = _landList.get(i);
            if (land.getClass().equals(totalLand.getClass())) {
                totalLand.setWidth(totalLand.getWidth() + land.getWidth());
                ++n;
            }
        }
        return n;    
    }

    /*** Lists all the placed lands in the street with its general informations */
    @Override
    public void list() {
        System.out.printf("\n%-3s%-20s %-10s %-10s %-10s\n\n", "", "Structure Type", "Location", "Width", "Height");
        // displays structures with their location, width, height properties
        for (int i = 0; i < _landList.size() ; ++i) {
            Land land = _landList.get(i);
            String structureType = "";
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
                i + 1, structureType, land.getLoc(), land.getWidth(), _landList.get(i).getHeight());  
        }
    }

    /**
     * Display specific informations of the land (if exist) at given location
     * @param location land location
     */
    @Override
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
     * Returns the side of the street
     * @param location
     * @return the side of the street
     */
    private ArrayList<Integer> getStreetSide(int location) {
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
            streetSide.set(i, landHeigth);
    } 
    
    /**
     * sets silhouette of the land
     * @param land
     */
    private void setSilhouette(Land land) {
        setSilhouette(land, land.getHeight());
    } 


    /*** Displays the skyline silhouette of the street */
    @Override
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
     * @param color land color
     */
    private void drawSkylineSilhouette(ArrayList<Integer> streetSide, AnsiEscape.Color color) {
        AnsiEscape.setBGColor(color);
        for (int i = 0; i < _maxLandHeight; ++i) {
            for (int j = 0; j < streetSide.size(); ++j) {
                if (streetSide.get(j) >= _maxLandHeight - i) {
                    AnsiEscape.setCursor(i + TOP_MARGIN, j);
                    System.out.printf(" ");
                }
            }
        }
        AnsiEscape.setBGColor(AnsiEscape.Color.DEFAULT);
    }

    @Override
    public String toString() {
        return String.format("Length: %d, land count: %d\n", getLength(), getLandCount());
    }

    @Override
    public int hashCode() {
        int h = 0;
        for (int i = 0; i < _landList.size(); ++i)
            h += _landList.get(i).hashCode();
        return h;
    }

    @Override
    public StreetArrayList clone() {
        try {
            StreetArrayList r = (StreetArrayList) super.clone();
            
            @SuppressWarnings("unchecked")
            var tmp1 = (ArrayList<Land>) _landList.clone();
            r._landList = tmp1;

            @SuppressWarnings("unchecked")
            var tmp2 = (ArrayList<Integer>) _negSide.clone();
            r._negSide = tmp2;

            @SuppressWarnings("unchecked")
            var tmp3 = (ArrayList<Integer>) _posSide.clone();
            r._posSide = tmp3;
            return r;
        }
        catch (CloneNotSupportedException e) {
            // this will never happen
            return null;
        }
    }
}
