package employee;

public class BasePlusCommissionEmployee extends CommissionEmployee {
    private double _baseSalary;

    public BasePlusCommissionEmployee (String fname, String lname, String ssn, double sales, double rate, double baseSalary) {
        super(fname, lname, ssn, sales, rate);
        setBaseSalary(baseSalary);    
    }

    public void setBaseSalary (double baseSalary) {_baseSalary = (baseSalary < 0.0) ? 0.0 : baseSalary;}    

    public double baseSalary () {return _baseSalary;}

    public double earnings () {return super.earnings() + baseSalary();}

    public String toString () {
        return String.format("%s %s; %s: $%,.2f", 
            "base-salalied", super.toString(), "base salary", baseSalary());
    } 
}
