public class ArrayDequeTest {
    public static void main(String[] args) {
        ArrayDeque<Integer> deque = new ArrayDeque<>();
        deque.offerFirst(4);
        deque.offerFirst(25);
        deque.offerFirst(49);
        deque.offerFirst(64);
        deque.offerFirst(36);
        deque.offerFirst(12);
        deque.offerFirst(81);
        deque.offerFirst(100);

        while (!deque.isEmpty()) {
            deque.poolLast();
            deque.poolFirst();
            System.out.println(deque);
        }

        deque.peekFirst();

        System.out.println(deque);
    }
}
