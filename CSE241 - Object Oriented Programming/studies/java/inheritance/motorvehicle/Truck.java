package motorvehicle;

public class Truck extends MotorVehicle {
    private double _loadCapa;
    
    public Truck (double engineSize, int wheels, int passengers, double loadCapa) {
        super(engineSize);
        setWheels(wheels);
        setPassengerCap(passengers);
        setLoadCapa(loadCapa);
    }

    public void setWheels (int n) {_wheels = n > 4 ? n : 5;}
    
    public void setPassengerCap (int n) {_passengerCapa = n >= 0 && n < 4 ? n : 0;}

    public void setLoadCapa (double loadCapa) {_loadCapa = loadCapa >= 0 ? loadCapa : 0.0;} 

    public double loadCapa () {return _loadCapa;} 

    public double taxAmount () {return loadCapa() * engineSize();}

    public String toString () {
        return String.format("%s\n%s\n%-15s: %.1f",
            "Truck", 
            super.toString(), 
            "Load Cap", loadCapa());
    }
}