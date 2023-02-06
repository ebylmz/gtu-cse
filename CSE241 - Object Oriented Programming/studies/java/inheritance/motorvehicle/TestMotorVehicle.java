package motorvehicle;

public class TestMotorVehicle {
    public static void main (String[] args) {
        MotorVehicle[] vehicle = new MotorVehicle[4];
        vehicle[0] = new Truck(4.0, 8, 2, 1000.0);
        vehicle[1] = new Truck(3.6, 6, 2, 750.0);
        vehicle[2] = new Boat(3.2, 10, 800);
        vehicle[3] = new Automobile(1.6, 4, 170, "White"); 

        // total number of passengers that all the vehicles can hold. 
        int passengerCapa = 0;
        // total load capacity of all the vehicles 
        double loadCapa = 0.0;
        for (var v : vehicle) {
            System.out.printf("%s\n\n", v);
            passengerCapa += v.passengerCap();

            // only the trucks have load capacity defined
            if (v instanceof Truck) {
                // downcast MotorVehicle reference to Truck reference
                Truck truck = (Truck) v;
                loadCapa += truck.loadCapa();
            }
        }

        System.out.printf("%-20s: %d\n%-20s: %.2f\n\n", "Passenger Capacity", passengerCapa, "Load Capacity", loadCapa);
    
        for (int i = 0; i < vehicle.length; ++i)
            System.out.printf("Vehicle[%d] is a %s\n", i, vehicle[i].getClass().getName());
    }
}
