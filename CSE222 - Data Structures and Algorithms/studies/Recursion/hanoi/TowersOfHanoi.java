package hanoi;

// Towers of Hanoi
// Only the top disk on a peg can be moved to another peg.
// A larger disk cannot be placed on top of a smaller disk.

// 1. Move the top n - 1 disks from peg L to peg M.
// 2. Move the bottom disk from peg L to peg R.
// 3. Move the top n - 1 disks from peg M to peg R.

// First locate larger disk to destination peg. After that
// any remaning disk can be placed to destionation peg
// without violating game rule. So after that we can assume 
// destionation peg is empty and our new problem became n - 1 disk problem

public class TowersOfHanoi {
    public static void main(String[] args) {
        System.out.print(TowersOfHanoi.ShowMoves(3, 'L', 'R', 'M'));
    }

    // Since strings are immutable in java each string concenation takes 
    // linear time. So use StringBuffer in a private function and use 
    // that function in a public wrapper function
    public static String ShowMoves(int n, char startPeg, char destPeg, char tempPeg) {
        if (n == 1)
            return "Move disk 1 from peg " + startPeg + " to peg " + destPeg + "\n";
        else 
            return 
                ShowMoves(n - 1, startPeg, tempPeg, destPeg) +
                "Move disk " + n + " from peg " + startPeg + " to peg " + destPeg + "\n" +
                ShowMoves(n - 1, tempPeg, destPeg, startPeg);
    }
}