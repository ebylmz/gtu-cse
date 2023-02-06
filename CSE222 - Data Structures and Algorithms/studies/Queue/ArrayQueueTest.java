public class ArrayQueueTest {
    public static void main(String[] args) {
        ArrayQueue<String> q = new ArrayQueue<>();
        q.offer("Alice");
        q.offer("Mary");
        q.offer("Bruce");
        q.offer("David");
        q.offer("Rocky");

        while (!q.isEmpty())
            System.out.println(q.pool());
    }
}
