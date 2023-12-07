package motorvehicle;

public abstract class MotorVehicle {
    protected double _engineSize;
    protected int _wheels;
    protected int _passengerCapa; // passenger capacity of vehicle

    public MotorVehicle (double engineSize) {
        setEngineSize(engineSize);
    }

    public void setEngineSize (double size) {_engineSize = size >= 0.0 ? size : 0.0;}
    
    public double engineSize () {return _engineSize;}
    
    public int wheels () {return _wheels;}
    
    public int passengerCap () {return _passengerCapa;}

    public abstract double taxAmount ();

    public String toString () {
        return String.format("%-15s: %.1f\n%-15s: %d\n%-15s: %d", 
            "Engine Size", engineSize(),
            "Wheels Number", wheels(),
            "Passenger Capa", passengerCap());
    }
}