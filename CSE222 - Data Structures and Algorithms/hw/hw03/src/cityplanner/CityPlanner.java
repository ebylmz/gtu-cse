package src.cityplanner;

import java.util.InputMismatchException;
import java.util.NoSuchElementException;
import java.util.Scanner;

import src.street.*;
import src.structure.*;
import src.util.*;

public class CityPlanner implements Cloneable {
    private StreetInterface _street;
    private Scanner _scanner;
    
    /**
     * Initializes the CityPlanner with given street
     * @param street
     */
    public CityPlanner(StreetInterface street) {
        // _street = street != null ? street : new Street();
        // _street = street != null ? street : new StreetArrayList();
        _street = street != null ? street : new StreetArray();
        _scanner = new Scanner(System.in);
    }

    /**
     * No parameter constructor for CityPlanner
     */
    public CityPlanner() {
        this(new StreetArray());
    }

    /*** Starts CityPlanner */
    public void start() {
        boolean repeat = true;

        String[] options = {"Edit Mode", "View Mode", "Exit"};

        do {
            printMenu("CityPlanner", options, true);
            switch (getInt(">>", 0, 2)) {
                case 1: 
                    editMode(); break;
                case 2: 
                    viewMode(); break;
                case 0:
                    repeat = false; break;
            }
        } while (repeat); 

        // if System.in is closed then it cannot be open in the single program execution
        // so don't close scanner if it is used later for another execution 
        // _scanner.close();
    }

    /** Displays edit menu for adding/removing structures */
    private void editMode() {
        String[] mainOptions = {"Add building", "Remove building", "Set street size", "Back"};
        
        // add/delete a building on a land in the street.
        boolean repeat = true;
        do {
            printMenu("EDIT MODE", mainOptions, true);
            switch (getInt(">>", 0, mainOptions.length - 1)) {
                case 1:
                    addMenu();
                    break;
                case 2:
                    removeMenu();
                    break;
                case 3:
                    try {
                        _street.setLength(getInt("Street Length:"));
                        System.out.printf("New street length is %d\n", _street.getLength());
                    } 
                    catch (IllegalArgumentException e) {
                        System.out.println("Street length cannot be negative");
                    }
                    enterToContinue();
                    break;
                case 0:
                    repeat = false;
                    break;
            }
        } while (repeat);
    }

    /** Displays view menu for analysing street */
    private void viewMode() {
        String[] options = {
            "Total remaining length of lands on the street",
            "List of buildings on the street",
            "Skyline silhouette of the street",
            "Total length of street occupied by the structures",
            "Back"
        };
        
        boolean repeat = true;
        do {
            printMenu("VIEW MODE", options, true);
            int select = getInt(">>", 0, options.length - 1);
            switch (select) {
                case 1:
                    System.out.printf("Remaining length of lands: %d\n", _street.getRemainingSpace());
                    break;
                case 2:
                    AnsiEscape.clearScreen(); AnsiEscape.setCursor(0, 0);
                    _street.list();
                    String[] subOptions = {"Focus", "Back"};
                    printMenu("Detailed View", subOptions, false);
                    select = getInt(">>", 0, 1);
                    if (select == 1) {
                        select = getInt("Structure:", 1, _street.getLandCount());
                        _street.focus(_street.get(select - 1).getLoc());
                    }
                    break;
                case 3:
                    _street.displaySkylineSilhouette();                    
                    break;
                case 4:
                    House totalHouse = new House();
                    Market totalMarket = new Market();
                    Office totalOffice = new Office();
                    Playground totalPlayground = new Playground();
                    
                    String headerFormat = "\n%-18s %-18s %s\n\n"; 
                    String listElementFormat = "%-18s %-18d %.2f%%\n";
                     
                    System.out.printf(headerFormat, "Structure Type", "Instance Number", "Occupy Ratio");
                    System.out.printf(listElementFormat,
                        "House", _street.getAnalysis(totalHouse), calcOccupyRatio(totalHouse));
                    System.out.printf(listElementFormat, 
                        "Market", _street.getAnalysis(totalMarket), calcOccupyRatio(totalMarket));
                    System.out.printf(listElementFormat, 
                        "Office", _street.getAnalysis(totalOffice), calcOccupyRatio(totalOffice));
                    System.out.printf(listElementFormat, 
                        "Playground", _street.getAnalysis(totalPlayground), calcOccupyRatio(totalPlayground));
                    break;
                case 0:
                    repeat = false;
                    break;
                }
            if (select != 0)
                enterToContinue();
        } while (repeat);
    }

    /** Add menu for adding new structures */
    private void addMenu() {
        String[] addOptions = {"Add house", "Add office", "Add market", "Add playground", "Back"};

        printMenu("ADD", addOptions, true);
        int select = getInt(">>", 0, addOptions.length - 1); 
        if (select != 0) {
            boolean addedSuccessfuly = false;
            
            // display street view before addition process
            _street.displaySkylineSilhouette();
            System.out.println();
            switch (select) {
                case 1:
                    addedSuccessfuly = addHouse(); break;
                case 2:
                    addedSuccessfuly = addOffice(); break;
                case 3:
                    addedSuccessfuly = addMarket(); break;
                case 4:
                    addedSuccessfuly = addPlayground(); break;
            }
            // display street view after addition process
            _street.displaySkylineSilhouette();
            if (addedSuccessfuly)
                System.out.printf("\nAdded succesfully. ");
            else
                System.out.printf("\nCannot add. ");
            enterToContinue();
        }
    }

    /** Remove menu for removing structures */
    private void removeMenu() {
        // list all the structures in the street to get selection of user
        _street.list();
        int select = getInt(">> ", 0, _street.getLandCount());
        if (select != 0) {
            if (_street.remove(select - 1) != null) 
                System.out.printf("\nRemoved succesfully. ");
            else
               System.out.printf("\nCannot remove. ");
            enterToContinue();
        }
    }


    private boolean addHouse() {
        var build = new House();
        boolean repeat = true;
        do {
            try {
                setBuilding(build);
                System.out.print("Room number: ");
                build.setNumRooms(_scanner.nextInt());
                // consume rest of the line after integer value
                _scanner.nextLine(); // consume rest of the line            

                System.out.print("Color: ");
                build.setColor(_scanner.nextLine());

                repeat = false;
            } 
            catch(InputMismatchException e) {
                System.out.println("Please enter proper values");
                _scanner.nextLine(); // consume rest of the line            
            }
        } while (repeat);
        return _street.add(build);
    }

    private boolean addOffice() {
        var build = new Office();
        boolean repeat = true;
        
        do {
            try {
                setBuilding(build);
                System.out.printf("Job type: ");
                build.setJobType(_scanner.nextLine());
                repeat = false;
            } 
            catch(InputMismatchException e) {
                System.out.println("Please enter proper values");
                _scanner.nextLine(); // consume rest of the line            
            }
        } while (repeat);
        return _street.add(build);
    }

    private boolean addMarket() {
        var build = new Market();
        boolean repeat = true;
        
        do {
            try {
                setBuilding(build);
                Time openingTime = new Time();
                Time closingTime = new Time();
                System.out.printf("Opening time (hh : mm): ");
                openingTime.readTime(_scanner);
                System.out.printf("Closing time (hh : mm): ");
                closingTime.readTime(_scanner);
                build.setOpeningTime(openingTime);
                build.setClosingTime(closingTime);
                repeat = false;
            } 
            catch(InputMismatchException e) {
                System.out.println("Please enter proper values");
                _scanner.nextLine(); // consume rest of the line            
            }
        } while (repeat);
        return _street.add(build);
    }

    private boolean addPlayground() {
        var build = new Playground();
        boolean repeat = true;
        
        do {
            try {
                System.out.print("Location: ");
                build.setLoc(_scanner.nextInt());
                System.out.print("Width: ");
                build.setWidth(_scanner.nextInt());
                
                _scanner.nextLine(); // consume rest of the line            
                repeat = false;
            } 
            catch(InputMismatchException e) {
                System.out.println("Please enter proper values");
                _scanner.nextLine(); // consume rest of the line            
            }
        } while (repeat);
        return _street.add(build);
    }

    void setBuilding(Building build) {
        boolean repeat = true;
        do {
            try {
                setLand(build);
                System.out.print("Owner: ");
                build.setOwner(_scanner.nextLine());
                repeat = false;
            }
            catch (NoSuchElementException e) {
                // just try again
            }
        } while (repeat);
    }

    void setLand(Land land) {
        boolean repeat = true;
        do {
            try {
                System.out.print("Location: ");
                land.setLoc(_scanner.nextInt());
                System.out.print("Width: ");
                land.setWidth(_scanner.nextInt());
                System.out.print("Height: ");
                land.setHeight(_scanner.nextInt());
             
                _scanner.nextLine(); // consume rest of the line            
                repeat = false;
            } 
            catch(InputMismatchException e) {
                System.out.println("Please enter proper values");
                _scanner.nextLine(); // consume rest of the line            
            }
        } while (repeat);
    }

    /**
     * Gets an integer value in given range inclusively  
     * @param min
     * @param max
     * @return r, (min <= r <= max)
     */
    private int getInt(String prompt, int min, int max) {
        boolean repeat = true;
        int r = 0;

        do {
            System.out.printf("%s ", prompt);
            try {
                r = _scanner.nextInt();                
                if (min <= r && r <= max) 
                    repeat = false;
            }
            catch (NoSuchElementException e) {
                System.err.println("Please make a proper choose");
                enterToContinue();
                _scanner.next();
            }
        } while (repeat);
        _scanner.nextLine(); // consume rest of the line            

        return r;
    }

    /**
     * Takes an integer from System In
     * @param prompt prompt before taking input
     * @return
     */
    private int getInt(String prompt) {
        boolean repeat = true;
        int r = 0;

        do {
            System.out.printf("%s ", prompt);
            try {
                r = _scanner.nextInt();                
                repeat = false;
            }
            catch (NoSuchElementException e) {
                System.err.println("Please make a proper choose");
                enterToContinue();
                _scanner.next();
            }
        } while (repeat);
        _scanner.nextLine(); // consume rest of the line            

        return r;
    }

    /** To stop the execution and give user enough time to see the screen */
    private void enterToContinue() {
        System.out.printf("Enter to continue ");
        try {
            System.in.read();
        }
        catch(Exception e) {
            // just continue
        }
    }

    /**
     * Prints menu layout
     * @param menuHeader
     * @param options options that will be listed for user
     * @param printTop clear the screen and print at the top of the screen
     */
    private void printMenu(String menuHeader, String[] options, boolean printTop) {
        // clear the screen and print the menu at the top of the terminal
        if (printTop) {
            AnsiEscape.clearScreen();
            AnsiEscape.setCursor(0, 0);
        }

        System.out.println();
        System.out.println(menuHeader);
        System.out.println("=============================================");
        for (int i = 0; i < options.length - 1; ++i)
            System.out.printf("%d- %s\n", i + 1, options[i]);
        System.out.printf("0- %s\n", options[options.length - 1]);    }

    private double calcOccupyRatio(Land land) {
        return ((double) land.getWidth() / (_street.getLength() * 2)) * 100;
    }

    @Override
    public String toString() {
        return _street.toString();
    }

    @Override
    public int hashCode() {
        return _street.hashCode();
    }

    @Override
    public CityPlanner clone() {
        try {
            CityPlanner r = (CityPlanner) super.clone();
            r._street = _street.clone();
            return r;
        }
        catch (CloneNotSupportedException e) {
            // this will never happen
            return null;
        }     
    }
}