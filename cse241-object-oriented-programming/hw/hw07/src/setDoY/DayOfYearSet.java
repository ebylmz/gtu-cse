/**
 * @file    DayOfYearSet.java
 * @author  Emirkan Burak Yılmaz 
 * @brief   DayOfYearSet Implementation   
 * @version 0.1
 * @date    2022-01-04
 * 
 * @copyright Copyright (c) 2021
 */

package setDoY;

import java.util.ArrayList;
import java.util.Scanner;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;

public class DayOfYearSet implements Cloneable {
    // Nested classes are divided into two categories: 
    // static and non-static. Nested classes that are  
    // declared static are simply called static nested classes. 
    // Non-static nested classes are called inner classes.
    // https://stackoverflow.com/questions/70324/java-inner-class-and-static-nested-class

    public static class DayOfYear implements Cloneable {
        private int _day;
        private int _month;

        /**
         * Initializes DayOfYear object with specific month and date values.
         * If given value is invalid, then it's converted to 1
         * @param m month 
         * @param d day
         */
        public DayOfYear (int m, int d) {
            _month = (1 <= m && m <= 12) ? m : 1;
            _day = (1 <= d && d <= dayInMonth()) ? d : 1;
        }
        
        /**
         * Initializes DayOfYear object with 1sth day of given month.
         * If given value is invalid, then it's converted to 1
         * @param m
         */
        public DayOfYear (int m) {this(m, 1);}
        
        /**
         * Initializes DayOfYear object with 1sth day of January
         */
        public  DayOfYear () {this(1, 1);}

        @Override
        public DayOfYear clone () {
            try {
                // shallow copy works
                return (DayOfYear) super.clone();
            }
            catch (CloneNotSupportedException e) {
                // this will never happen
                return null;
            }
        }

        /**
         * Returns the total number of day in given month
         * @param m month
         * @return total day of month
         * @throws IllegalAccessError if month doesn't in range [1, 12]
         */
        public static int dayInMonth (int m) throws IllegalAccessError {
            switch (m) {
                case  1: return 31;
                case  2: return 28;
                case  3: return 31;
                case  4: return 30;
                case  5: return 31;
                case  6: return 30;
                case  7: return 31;
                case  8: return 31;
                case  9: return 30;
                case 10: return 31;
                case 11: return 30;
                case 12: return 31;
                default:
                    throw new IllegalArgumentException();
            }        
        }

        /**
         * Returns the number of day in current month
         * @return day in current month
         */
        public int dayInMonth () {
            return dayInMonth(month());
        }

        /**
         * Returns the number of day passed from first day of year to today
         * @return 
         */
        public int daySoFar () {
            int dayMonth = 0;
            for (int i = 1; i < month(); ++i)
                dayMonth += dayInMonth(i);
            return dayMonth + day(); 
        }

        /**
         * Returns the number of day between two date
         * @param other
         * @return the day difference between two date
         */
        public int dayBetween (DayOfYear other) {
            if (preceding(other))
                return other.daySoFar() - daySoFar();
            else
                return daySoFar() - other.daySoFar();
        }

        // if next month has less day than current day value, then day is setted as 1
        private void nextMonth () {
            _month = (month() == 12) ? 1 : _month + 1; 
            // check if day is suitable for new month
            if (day() < dayInMonth())
                _day = 1;
        }

        // if next month has less day than current day value, then day is setted as 1
        private void preMonth () {
            _month = (month() == 1) ? 12 : _month - 1; 
            // check if day is suitable for new month
            if (day() < dayInMonth())
                _day = 1;
        }

        /**
         * gets the day of date
         * @return day
         */
        public int day () {return _day;}

        /**
         * gets the month of date
         * @return month
         */
        public int month () {return _month;}
        
        /**
         * @param d day
         */
        public void setDay (int d) {
            if (d <= dayInMonth())
                _day = d;
        }

        /**
         * @param m month
         */
        public void setMonth (int m) {
            if (1 <= m && m <= 12) {
                _month = m;
                // be sure day is still valid
                if (day() > dayInMonth())
                    _day = 1;
            }
        }

        /**
         * sets the day to next day
         */
        public void next () {
            if (day() == dayInMonth()) {
                nextMonth();
                _day = 1;
            }
            else
                ++_day;
        }

        /**
         * sets the day to previous day
         */
        public void pre () {
            if (day() == 1) {
                preMonth();
                _day = dayInMonth();
            }
            else
                --_day;
        }

        /**
         * @return the DayOfYear object which is comes after current date
         */
        public DayOfYear nextDay () {
            var newday = clone();
            newday.next();
            return newday;   
        }

        /**
         * @return the DayOfYear object which is comes before current date
         */
        public DayOfYear preDay () {
            var newday = clone();
            newday.pre();
            return newday;
        }

        /**
         * Cheks if today is comes first than given other day
         * @param other 
         * @return true for this day comes first other day
         */
        public boolean preceding (DayOfYear other) {
            return daySoFar() < other.daySoFar();
        }

        /**
         * Checks if two days are same
         * @param other
         * @return true if two day is equal
         */
        @Override
        public boolean equals (Object o) {
            if (this == o)
                return true;
            else if (!(o instanceof DayOfYear)) 
                return false;
            else {
                DayOfYear other = (DayOfYear) o;
                return day() == other.day() && month() == other.month();
            }
        }
        
        /**
         * Returns date as string in format "mm/dd"
         */
        @Override
        public String toString () {
            return String.format("%d/%d", _month, _day);
        }
    }

    /** Total day in year */
    static final int DAY_IN_YEAR = 365;
    private DayOfYear[] _set;   // dynamic array keeps set elements
    private int _size;          // keeps the filled array size
    private static int _total;  // total number of DayOfYear objects alive in all the sets
    
    /**
     * Initialize the set with the values from given ArrayList.
     * Dublicated values does not added in the set
     * @param arr 
     */
    public DayOfYearSet (ArrayList<DayOfYear> arr) {        
        reserve(arr.size());  
        for (var e : arr)
            add(e);
    }

    /**
     * Initialize the set by reading set from given the file
     * @param filename name of the file which contains set element
     */
    public DayOfYearSet (String filename) {
        this(); 
        read(filename);
    }
    
    /**
     * Initialize the set with empty set
     */
    public DayOfYearSet () {
        _size = 0;
        _set = null;
    }

    @Override
    public DayOfYearSet clone () {
        // inherited version: protected Object clone () throws CloneNotSupportedException
        // handle the exception
        // change the return type (Covariant return type)
        try {
            DayOfYearSet newset = (DayOfYearSet) super.clone();
            if (_set != null) {
                newset._set = new DayOfYear[_set.length];
                for (int i = 0; i < size(); ++i) 
                    newset._set[i] = _set[i].clone();
                _total += size(); // new DayOfYear object created;
            }
            else
                newset._set = null;
            return newset;
        }
        catch (CloneNotSupportedException e) {
            // this will never happen
            return null;
        }
    }

    /**
     * Checks if two set are equal
     * @param other
     * @return true equal if two sets elements are equal regardless of the keeping order
     */
    @Override
    public boolean equals (Object o) {
        if (this == o)
            return true;
        else if (!(o instanceof DayOfYearSet))
            return false;
        else {
            var other = (DayOfYearSet) o;
            for (int i = 0; i < size(); ++i)
                if (! other.inside(at(i)))
                    return false;
            return true;
        }
    }

    /**
     * Returns the total number of DayOfYear objects alive in all the sets
     * @return the total number of DayOfYear objects
     */
    public static int total () {
        System.gc();
        return _total;
    }

    /**
     * Checks if given element is inside of the set
     * @param element
     * @return true if element is inside of the set
     */
    public boolean inside (DayOfYear element) {
        return find(element) != -1;
    }

    // o.w. returns the size of the set
    /** 
     * Returns the index of the element if it's in set
     * 
     * @param element
     * @return if given element inside of the set returns the index of element inside the set,
     * o.w. returns -1
     */
    public int find (DayOfYear element) {
        for (int i = 0; i < size(); ++i)
            if (at(i).equals(element))
                return i;
        return -1;
    }

    /**
     * number of element in the set
     * @return number of element in the set
     */
    public int size () {return _size;}
    
    /**
     * Returns the element at given position
     * @param position
     * @return final DayOfYear object at given position in set
     * @throws IllegalAccessError throws for invalid position values. 
     * Position should be in range [0, setsize - 1]
     */
    public final DayOfYear at (int position) throws IllegalAccessError {
        if (_set == null || position < 0 || position >= size())
            throw new IllegalAccessError();
        return _set[position];
    }

    /**
     * Modify the element at given position with the given element.
     * Modification only happens if newElement is unique in the set
     * @param position 
     * @param newElement 
     * @throws IllegalAccessError throws for invalid position values
     */
    public void modify (int position, DayOfYear newElement) throws IllegalAccessError {
        if (_set == null || position < 0 || position >= size())
            throw new IllegalAccessError();
        // be sure given new element does not cause an dubliceted values
        if (!inside(newElement))
            _set[position] = newElement.clone();
    }

    /**
     * Modify the element with given month and day values.
     * @param position
     * @param month
     * @param day
     * @throws IllegalAccessError
     */
    public void modify (int position, int month, int day) throws IllegalAccessError {
        var v = at(position);
        v.setDay(day);
        v.setMonth(month);
    }

    /**
     * Sorts the set as increasing order (1/1 to 12/31)
     */
    public void sort () {
        for (int i = 1; i < size(); ++i) {
            DayOfYear min = _set[i].clone();
            
            int j = i;
            while (j > 0 && min.preceding(_set[j - 1])) {
                _set[j] = _set[j - 1];
                --j;
            }
            _set[j] = min;
        }
    }

    /**
     * Adds given element to the set. No duplication allowed
     * @param element
     */
    public void add (DayOfYear element) {
        if (!inside(element)) {
            // make sure set has capacity to adding new elements
            if (_set == null)
                reserve(1);
            else if (size() == _set.length)
                reserve(2 * _set.length);
            
            _set[_size] = element.clone(); 
            ++_size;
            ++_total;   // new DayOfYear object created
        }
    }

    /**
     * Removes the given element if it's inside of the set
     * @param element
     */
    public void remove (DayOfYear element) {
        // find returns the position of given element
        remove(find(element));  
    }

    /**
     * Removes the element at given position in set
     * @param position
     */
    public void remove (int position) {
        if (0 <= position && position < size()) {
            // if remove element is located at last position, 
            // then no copy-paste needed. Just decrease the set size 
            for (int i = position + 1; i < size(); ++i)
                _set[i - 1] = _set[i];
            _set[_size - 1] = null;
            --_size;
            --_total;   // a DayOfYear object killed
        }
    }

    /**
     * sets the set as empty set 
     */
    public void empty () {
        _total -= _size;
        _size = 0;
    }

    /**
     * Writes the set element to the file
     * @param filename destination file
     */
    public void write (String filename) {
        try {
            // File file = new File(filename);
            FileWriter writer = new FileWriter(filename);
            if (size() > 1) {
                writer.write(at(0).toString());
                for (int i = 1; i < size(); ++i) 
                    writer.write("\n" + at(i).toString());
            }
            writer.close();
        }
        catch (IOException e) {
            System.out.println("Something went wrong.");
            e.printStackTrace();
        }
    }

    /**
     * adds new elements from given file 
     * @param filename
     */
    public void read (String filename) {
        try {
            File file = new File(filename);
            Scanner reader = new Scanner(file);
            while (reader.hasNextLine()) {
                String str = reader.nextLine();
                // parse an DayOfYear Object from given string
                for (int i = 0; i < str.length(); ++i) {
                    String m = "";  // month
                    // read month value
                    while (i < str.length() && Character.isDigit(str.charAt(i))) {
                        m += str.charAt(i);
                        ++i;
                    }
                    
                    if (m.length() > 0 && str.charAt(i) == '/') {
                        String d = "";  // day
                        ++i;
                        // read day value
                        while (i < str.length() && Character.isDigit(str.charAt(i))) {
                            d += str.charAt(i);
                            ++i;
                        }
                        // add scanned element to the set
                        add(new DayOfYear(Integer.parseInt(m), Integer.parseInt(d)));
                    }
                }
            }
            reader.close();
        }   
        catch (FileNotFoundException e) {
            System.out.println("Something went wrong.");
            e.printStackTrace();
        }
    }

    // private function because memory manipulation doesn't concern the user 
    private void reserve (int newcapacity) {
        if (newcapacity >= 0) {
            if (_size > newcapacity)
                _size = newcapacity;
            
            DayOfYear[] pre = _set;
            _set = new DayOfYear[newcapacity]; 

            for (int i = 0; i < size(); ++i)
                _set[i] = pre[i];   // no need to clone
        }
    }

    /**
     * Returns the union of sets
     * @param other
     * @return union of two set
     */
    public DayOfYearSet union (DayOfYearSet other) {
        DayOfYearSet newset = this.clone();

        for (int i = 0; i < other.size(); ++i)
            newset.add(other.at(i)); // add function does not allow dublicated elements
        return newset;
    }

    /**
     * Returns the set difference two set
     * @param other
     * @return difference of this from other set
     */
    public DayOfYearSet difference (DayOfYearSet other) {
        DayOfYearSet newset = new DayOfYearSet();

        // add the elements which are only in this set
        for (int i = 0; i < size(); ++i)
            if (!other.inside(at(i)))
                newset.add(at(i));
        return newset;
    } 

    /**
     * Returns intersection of two set
     * @param other
     * @return intersection of two set
     */
    public DayOfYearSet intersection (DayOfYearSet other) {
        DayOfYearSet newset = new DayOfYearSet();

        // add the elements which are in both sets
        for (int i = 0; i < size(); ++i)
            if (other.inside(at(i)))
                newset.add(at(i));
        return newset;
    } 
    
    /**
     * Returns complement of the set
     * @return complement set of current set
     */
    public DayOfYearSet complement () {
        DayOfYearSet newset = new DayOfYearSet();
        DayOfYear e = new DayOfYear();  // e is 1 January by default
        
        // try each day 
        for (int i = 0; i < DAY_IN_YEAR; ++i, e.next())
            if (!inside(e))
                newset.add(e);
        return newset;    
    } 

    /**
     * Returns set as string in format "{mm/dd, mm/dd}"
     */
    public String toString () {
        String strSet = "";
        if (size() > 0) {
            strSet += at(0);
            for (int i = 1; i < size(); ++i)
                strSet += ", " + at(i);
        }
        return String.format("{%s}", strSet);
    }
}