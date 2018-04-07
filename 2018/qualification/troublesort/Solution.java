/**
 * Google Code Jam 2018
 *
 * Trouble Sort
 *
 * Run the following in this directory:
 * $ javac Solution.java
 * $ java Solution
 */
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Scanner;


public class Solution {

    public static void main(String[] args) {
        Scanner in = new Scanner(new BufferedReader(new InputStreamReader(System.in)));
        int T = in.nextInt();
        for (int i = 1; i <= T; ++i) {
            int N = in.nextInt();

            List<Integer> ints = new ArrayList<>();
            while (N-- > 0) {
                ints.add(in.nextInt());
            }

            int sortedIndex = troubleSort(ints);
            if (sortedIndex == ints.size() - 1) {
                System.out.println("Case #" + i + ": OK");
            } else {
                System.out.println("Case #" + i + ": " + (sortedIndex + 1));
            }
        }
    }

    // Attempt to sort, returning the index up to which
    // the array is sorted.
    public static int troubleSort(List<Integer> ints) {
        boolean done = false;

        while (!done) {
            done = true;
            for(int i = 0; i < ints.size() - 2; i++) {
                if (ints.get(i) > ints.get(i + 2)) {
                    done = false;
                    int first = ints.get(i);
                    ints.set(i, ints.get(i + 2));
                    ints.set(i + 2, first);
                }
            }
        }

        for (int i = 0; i < ints.size(); i++) {
            boolean isLast = (i + 1) == ints.size();
            if (isLast) {
                continue;
            }
            if (ints.get(i) > ints.get(i + 1)) {
                return i - 1;
            }
        }
        return ints.size() - 1;
    }
}
