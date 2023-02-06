package employee;

public class SalariedEmployee extends Employee {
    private double _weeklySalary;

    public SalariedEmployee (String fname, String lname, String ssn, double salary) {
        super(fname, lname, ssn);
        _weeklySalary = salary;
    }

    public double weeklySalary () {return _weeklySalary;}
    
    public void setWeeklySalary (double salary) {_weeklySalary = salary < 0.0 ? 0.0 : salary;}

    public double earnings() {return weeklySalary();}

    public String toString () {
        return String.format("salaried employee: %s\n%s: $%,.2f", 
            super.toString(), "weekly salary", weeklySalary());
    }
}