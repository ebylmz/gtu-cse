package employee;

public class HourlyEmployee extends Employee {
    private double _hourlyWage;   // wage per hour 
    private double _hoursWorked;  // hours wored for week

    public HourlyEmployee (String fname, String lname, String ssn, double hourlyWage, double hoursWorked) {
        super(fname, lname, ssn);
        setHourlyWage(hourlyWage);
        setHoursWorked(hourlyWage);
    }

    public void setHourlyWage (double hourlyWage) {_hourlyWage = hourlyWage < 0.0 ? 0.0 : hourlyWage;}
 
    public void setHoursWorked (double hoursWorked) {_hoursWorked = hoursWorked < 0.0 ? 0.0 : hoursWorked;}

    public double hourlyWage () {return _hourlyWage;}
 
    public double hoursWorked () {return _hoursWorked;}

    public double earnings () {
        if (hoursWorked() <= 40) // no overtime 
            return hourlyWage() * hoursWorked(); 
        else 
            return 40 * hourlyWage() + (hoursWorked() - 40) * hourlyWage() * 1.5;
    }

    public String toString () {
        return String.format("hourly employee: %s\n%s: $%,.2f; %s: %,.2f", 
            super.toString(), "hourly wage", hourlyWage(), "hours worked", hoursWorked());
    }
}