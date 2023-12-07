package motorvehicle;

public class Automobile extends MotorVehicle {
    private double _maxSpeed;
    private String _color;

    public Automobile (double engineSize, int passengers, double maxSpeed, String color) {
        super(engineSize);
        _wheels = 4;
        setPassengerCap(passengers);
        setMaxSpeed(maxSpeed);
        setColor(color);
    }

    public void setPassengerCap (int n) {_passengerCapa = n >= 0 && n < 4 ? n : 0;}

    public void setMaxSpeed (double maxSpeed) {_maxSpeed = (maxSpeed >= 0.0) ? maxSpeed : 0.0;}
    
    public void setColor (String color) {_color = color;}

    public double maxSpeed () {return _maxSpeed;}
    
    public String color () {return _color;}

    public double taxAmount () {return maxSpeed() * engineSize();}

    public String toString () {
        return String.format("%s\n%s\n%-15s: %.1f",
            "Automobile", 
            super.toString(), 
            "Max Speed", maxSpeed());
    }
}