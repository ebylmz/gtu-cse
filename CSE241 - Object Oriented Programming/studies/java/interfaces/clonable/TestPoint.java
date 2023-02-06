package clonable;

public class TestPoint {
    public static void main (String[] args) {
        Point2D p1 = new Point2D(0, 3);
        Point2D p2 = new Point2D(4, 0);
        System.out.printf("p1: %s \np2: %s \ndistance: %.2f\n\n", p1, p2, p1.distance(p2));

        Point3D p3 = new Point3D(12, 9, 3);
        Point3D p4 = new Point3D(7, 8, 15);
        System.out.printf("p3: %s \np4: %s \ndistance: %.2f\n\n", p3, p4, p3.distance(p4));

        Point3D p5 = p3.clone();
        System.out.printf("p3: %s \np5: %s\n", p3, p5);

        System.out.printf("%-30s : %s\n", "p3 == p5", p3 == p5);
        System.out.printf("%-30s : %s\n", "p3.getClass() == p5.getClass()", p3.getClass() == p5.getClass());
        System.out.printf("%-30s : %s\n", "p3.equals(p5)", p3.equals(p5));
    }
}
