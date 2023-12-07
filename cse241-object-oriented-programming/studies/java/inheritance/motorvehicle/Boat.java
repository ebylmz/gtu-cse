package motorvehicle;

public class Boat extends MotorVehicle {
    private double _weight;

    public Boat (double engineSize, int passengers, double weight) {
        super(engineSize);
        _wheels = 0;
        setPassengerCap(passengers);
        setWeight(weight);
    }

    public void setPassengerCap (int n) {_passengerCapa = (n >= 0 && n <= 12) ? n : 0;}

    public void setWeight (double weight) {_weight = (weight >= 0.0) ? weight : 0.0;}
    
    public double weight () {return _weight;}
    
    public double taxAmount () {return weight() * 100;}

    public String toString () {
        return String.format("%s\n%s\n%-15s: %.1f",
            "Boat", 
            super.toString(), 
            "Load Cap", weight());
    }
}