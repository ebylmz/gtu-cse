public class Time {
    private int hour; // 0 - 23
    private int min; // 0 - 59
    private int sec; // 0 - 59
    
    public Time (int h, int m, int s) {
        set(h, m, s);
    }

    public Time (int h, int m) {this(h, m, 0);}

    public Time (int h) {this(h, 0, 0);}


    public void set (int h, int m, int s) {
        setHour(h);
        setMinute(m);
        setSecond(s);
    }

    public void setHour (int h) {hour = (0 <= h && h <= 23) ? h : 0;}
    public void setMinute (int m) {min = (0 <= m && m <= 59) ? m : 0;}
    public void setSecond (int s) {sec = (0 <= s && s <= 59) ? s : 0;}

    public int getHour () {return hour;}
    public int getMinute () {return min;}
    public int getSecond () {return sec;}
    
    // overrides Object.toString method
    public String toString () {
        return String.format("%d:%02d:%02d %s", 
            (getHour() == 0 || getHour() == 12 ? 12 : getHour() % 12), 
            getMinute(), getSecond(),
            (hour < 12 ? "AM" : "PM"));
    }

    public String toUniversalString () {
        return String.format("%02d:%02d:%02d", getHour(), getMinute(), getSecond());
    }

    // driver code
    public static void main (String[] args) {
        Time t1 = new  Time(1, 20, 34);
        Time t2 = new  Time(12, 42, 59);
        Time t3 = new  Time(16, 2, 19);
        Time t4 = new  Time(0, 1, 46);
        
        t1.getClass().getName();
        
        System.out.printf("%s - %11s\n", t1.toUniversalString(), t1);
        System.out.printf("%s - %11s\n", t2.toUniversalString(), t2);
        System.out.printf("%s - %11s\n", t3.toUniversalString(), t3);
        System.out.printf("%s - %11s\n", t4.toUniversalString(), t4);
    }
}