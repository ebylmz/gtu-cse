package employee;

public class CommissionEmployee extends Employee {
    protected double _grossSales;
    protected double _commissionRate;

    public CommissionEmployee (String fname, String lname, String ssn, double sales, double rate) {
        super(fname, lname, ssn);
        setGrossSales(sales); 
        setCommissionRate(rate); 
    }

    public void setGrossSales (double sales) {_grossSales = (sales < 0.0) ? 0.0 : sales;}

    public void setCommissionRate (double rate) {_commissionRate = (0.0 < rate && rate < 1.0) ? rate : 0.0;}

    public double grossSales () {return _grossSales;}

    public double commissionRate () {return _commissionRate;}

    public double earnings () {return commissionRate() * grossSales();}

    public String toString () {
        return String.format("%s: %s\n%s: $%,.2f; %s: %.2f", 
            "commission employee", super.toString(), 
            "gross sales", grossSales(),
            "commission rate", commissionRate() );
    }
}
