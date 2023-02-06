package payable;

//! interfaces doesn't implements other interfaces
// implements denotes defining an implementation for the methods of an interface. 
// However interfaces have no implementation so that's not possible.
// https://stackoverflow.com/questions/3921412/why-an-interface-can-not-implement-another-interface

public interface Payable {
    // All fields are implicitly public , static and final
    // All methods are implicitly public abstract methods
    double paymentAmount ();
    // implicitly public abstract paymentAmount ();
}
