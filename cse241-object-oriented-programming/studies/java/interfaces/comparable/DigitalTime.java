package comparable;

import java.util.InputMismatchException;

public class DigitalTime implements Comparable<DigitalTime>, Cloneable {
    private int __hour; // 0 - 23
    private int __min; // 0 - 59

    public DigitalTime (int h, int m) {
        try {
            setHour(h);
            setMinute(m);
        } 
        catch (InputMismatchException e) {
            System.out.printf("%s \nDigitalTime initialized as 00:00\n");
            setHour(0);
            setMinute(0);
        }
    }

    public DigitalTime (int h) {
        this(h, 0);
    }

    public DigitalTime () {
        this(0, 0);
    }

    public void setHour (int h) throws IllegalArgumentException {
        if (0 <= h && h <= 23)
            __hour = h;
        else
            throw new IllegalArgumentException("hour must be in range [0, 23]");
    }

    public void setMinute (int m) throws IllegalArgumentException {
        if (0 <= m && m <= 59)
            __min = m;
        else
            throw new IllegalArgumentException("minute must be in range [0, 59]");
    }

    public int hour () {return __hour;}

    public int minute () {return __min;}

    public int totalMinute () {return hour() * 60 + minute();}

    public DigitalTime clone () {
        try {
            return (DigitalTime) super.clone();
        }
        catch (CloneNotSupportedException e) {
            // this will never happen since Cloneable interface implemented
            return null;
        }
    }

    public boolean equals(Object o) {
        if (this == o)
            return true;
        else if (o instanceof DigitalTime) {
            // downcasting from Object to DigitalTime
            // return compareTo((DigitalTime) o) == 0;
            DigitalTime other = (DigitalTime) o;
            return compareTo(other) == 0; 
        }
        else
            return false;
    }

    public int compareTo (DigitalTime other) {
        return totalMinute() - other.totalMinute(); 
    }

    /**
     * prints the DigitalTime in 24 hour format
     */
    public String toUniversalString () {
        return String.format("%02d:%02d", hour(), minute());
    }

    /**
     * prints the DigitalTime in AM/PM format
     */
    public String toString () {
        return String.format("%d:%02d %s", 
           (hour() == 12 || hour() == 0 ? 12 : hour() % 12), 
           minute(), (hour() > 12 ? "PM" : "AM"));
    }
}