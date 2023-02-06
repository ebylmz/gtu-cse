package payable;

public class SalariedEmployee extends Employee {
    private double _weeklySalary;

    public SalariedEmployee (String fname, String lname, String ssn, double weeklySalary) {
        super(fname, lname, ssn);
        setWeeklySalary(weeklySalary);
    }

    public void setWeeklySalary (double salary) {_weeklySalary = salary < 0.0 ? 0.0 : salary;}
    
    public double weeklySalary () {return _weeklySalary;}

    public String toString () {
        return String.format("salaried employee: %s \nweekly salary: $%,.2f", 
            super.toString(), weeklySalary());
    }

    public double paymentAmount () {
        return weeklySalary();
    }
}