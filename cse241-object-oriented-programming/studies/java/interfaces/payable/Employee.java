package payable;

//! be careful Comparable<Employee>, otherwise it become Comparable<Object>
public abstract class Employee implements Payable, Comparable<Employee> {
    private String _fname;
    private String _lname;
    private String _ssn;

    public Employee (String fname, String lname, String ssn) {
        _fname = fname;
        _lname =lname;
        _ssn = ssn;
    }

    public void setFirstName (String name) {_fname = name;}

    public void setLastName (String name) {_lname = name;}

    public void setSsn (String ssn) {_ssn = ssn;}

    public String firstName () {return _fname;}

    public String lastName () {return _lname;}

    public String ssn () {return _ssn;}

    public String toString () {
        return String.format("%s %s \nsocial security number: %s", firstName(), lastName(), ssn());
    }

    public int compareTo (Employee other) {
        if (paymentAmount() > other.paymentAmount())
            return 1;
        else if (paymentAmount() < other.paymentAmount())
            return -1;
        else 
            return 0;
    }

    // Note: We do not implement Payable method paymentAmount here so 
    // this class must be declared abstract to avoid a compilation error.
}