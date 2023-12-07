package src.street;

import src.util.LDLinkedList;

import java.util.Collection;
import java.util.ListIterator;

import src.structure.*;
import src.util.AnsiEscape;

public class StreetLDLinkedList implements StreetInterface, Cloneable {
    private LDLinkedList<Land> _landList;
    private int _streetLength;
    private LDLinkedList<Integer> _posSide;
    private LDLinkedList<Integer> _negSide;
    private int _maxLandHeight;

    /**
     * Initializes the street with given length
     * @param streetLength length of the street
     */
    public StreetLDLinkedList(int streetLength) {
        _landList = new LDLinkedList<>();
        _negSide = new LDLinkedList<>();
        _posSide = new LDLinkedList<>();
        _maxLandHeight = 0;
        setLength(streetLength);
    }

    /**
     * Initializes the street with given lands
     * @param c collection that contain lands which will be added to the street
     * @param streetLength length of the street
     */
    public StreetLDLinkedList(Collection<? extends Land> c, int streetLength) {
        this(streetLength);
        for (var land : c)
            add(land);
    }

    /*** Initializes an empty street with defualt length 64 */
    public StreetLDLinkedList() {
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

        if (newLength < getLength()) {
            ListIterator<Land> it1 = _landList.listIterator();            
            while (it1.hasNext()) {
                var e = it1.next();
                int[] locs = getSilhouetteLoc(e);
                if (locs[1] >= newLength)
                    remove(e);
            }
            // get list iterator which are begins at the end of the list
            ListIterator<Integer> it2 = _posSide.listIterator(_streetLength);            
            ListIterator<Integer> it3 = _negSide.listIterator(_streetLength);            
            // remove the last n item
            int n = _streetLength - newLength;
            while (n > 0 && it2.hasPrevious() && it3.hasPrevious()) {
                it2.previous();
                it3.previous();
                it2.remove();
                it3.remove();
                --n;
            }
        }
        else if (newLength > getLength()) {
            // get list iterator which are begins at the end of the list
            ListIterator<Integer> it2 = _posSide.listIterator(_streetLength);
            ListIterator<Integer> it3 = _negSide.listIterator(_streetLength);            
            
            // add new empty n item
            int n = newLength - _streetLength;
            while (n > 0) {
                it2.add(0);
                it3.add(0);
                --n;
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
    private int getEmptyLandLength(LDLinkedList<Integer> streetSide) {
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
        
        var it = streetSide.listIterator(locs[0]);
        while (it.hasNext() && it.nextIndex() < locs[1])
            // if street location is full
            if (it.next() > 0)
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
        Land removedLand = _landList.remove(index);
        
        // update the max land height
        _maxLandHeight = 0;
        for (var e : _landList)
            if (e.getHeight() > _maxLandHeight)
                _maxLandHeight = e.getHeight();
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
        var it = _landList.listIterator();
        while (it.hasNext()) {
            var land = it.next();
            // for simplicity convert locations to silhouette location (positive)
            // before that make sure two location are on the same street side 
            if (location * land.getLoc() > 0) { 
                int targetLoc = (location < 0) ? -location : location;
                // get start and end silhouette locations of the land 
                int[] locs = getSilhouetteLoc(land);
                
                // check if our target lies between lands bound
                if (locs[0] <= targetLoc && targetLoc <= locs[1]) 
                    return land;
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
        if (land != null) {
            var it = _landList.listIterator();
            while (it.hasNext())
                if (land.equals(it.next()))
                    return it.previousIndex();
        }
        return -1;
    }

    /**
     * Finds the total length and number of instance of given land type
     * @param totalLand
     * @return instance count as given land type (Market, Office etc.)
     */
    @Override
    public int getAnalysis(Land totalLand) {
        totalLand.setHeight(0);
        totalLand.setWidth(0);
        totalLand.setLoc(0);

        int n = 0; // instance count as given land type
        var it = _landList.iterator();
        while (it.hasNext()) {
            var land = it.next();
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
        var it = _landList.listIterator();
        while (it.hasNext()) {
            String structureType = "";
            Land land = it.next();
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
                it.nextIndex(), structureType, land.getLoc(), land.getWidth(), land.getHeight());  
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
    private LDLinkedList<Integer> getStreetSide(int location) {
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
        var it = streetSide.listIterator(locs[0]);
        while (it.hasNext() && it.nextIndex() < locs[1]) {
            it.next();
            it.set(landHeigth);
        }
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
    private void drawSkylineSilhouette(LDLinkedList<Integer> streetSide, AnsiEscape.Color color) {
        AnsiEscape.setBGColor(color);
        for (int i = 0; i < _maxLandHeight; ++i) {
            var it = streetSide.listIterator();
            while (it.hasNext()) {
                int height = it.next();
                if (height >= _maxLandHeight - i) {
                    AnsiEscape.setCursor(i + TOP_MARGIN, it.nextIndex() - 1);
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
        var it = _landList.iterator();
        while (it.hasNext())
            h += it.next().hashCode();
        return h;
    }

    @Override
    public StreetLDLinkedList clone() {
        try {
            StreetLDLinkedList r = (StreetLDLinkedList) super.clone();
            var tmp1 = (LDLinkedList<Land>) _landList.clone();
            r._landList = tmp1;
            var tmp2 = (LDLinkedList<Integer>) _negSide.clone();
            r._negSide = tmp2;
            var tmp3 = (LDLinkedList<Integer>) _posSide.clone();
            r._posSide = tmp3;
            return r;
        }
        catch (CloneNotSupportedException e) {
            // this will never happen
            return null;
        }
    }
}
