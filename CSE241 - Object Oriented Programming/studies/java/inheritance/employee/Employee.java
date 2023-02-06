// https://www.geeksforgeeks.org/can-we-overload-or-override-static-methods-in-java/

package employee;

public abstract class Employee {
    private String _fname;
    private String _lname;
    private String _ssn;

    public Employee (String fname, String lname, String ssn) {
        _fname = fname; 
        _lname = lname;
        _ssn = ssn;
    }
    
    public void setFirstName (String fname) {_fname = fname;}
    
    public void setLastName (String lname) {_lname = lname;}
    
    public void setSocialSecurityNumber (String ssn) {_ssn = ssn;}

    public String firstName () {return _fname;}
    
    public String lastName () {return _lname;}

    public String socialSecurityNumber () {return _ssn;}

    public String toString () {
        return String.format("%s %s\nsocial security number: %s", 
            firstName(), lastName(), socialSecurityNumber());
    }

    public abstract double earnings ();
}
