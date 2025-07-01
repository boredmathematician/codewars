package java_solutions;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class InfiniteDigitalString {

    public static long findPosition(final String s) {
        List<Long> indexes = cycle(s); // cycle through rotations and find index
        indexes.add(specialCase(s)); // check for special cases
        indexes.add(breakDown(s, 1, 0)); // try breaking down in smaller consecutive numbers

        // pick the minimum out of all
        long min = indexes.get(0);
        for (int i = 1; i < indexes.size(); i++) {
            if (min > indexes.get(i)) min = indexes.get(i);
        }
        return min;
    }

    private static long specialCase(final String s) {
        Matcher carry = Pattern.compile("(.)(9+)(.+?)(.)(0*)").matcher(s);
        Matcher nines = Pattern.compile("(9+)").matcher(s);
        Matcher flanked = Pattern.compile("(0+)(.*?)(0*)").matcher(s); // NOTE: the second group MUST be lazy, to maximize the number of zeros in the third group

        if (carry.matches()) {
            String a = carry.group(1);
            String leading9 = carry.group(2);
            String middle = carry.group(3);
            String b = carry.group(4);
            String trailing0 = carry.group(5);

            // ...[middle][a][9..9]|[middle][b][0..0]...
            //             ^ [a][9..9][middle][b][0..0]
            //             if b = a+1 and there are more 9s than 0s
            // NOTE: the third group (middle) MUST be lazy, so that the following cases still matches:
            //             v [8][9..9][middle][9][0..0] // here a greedy third group would steal from the fourth group
            // ...[middle][8][9..9]|[middle][9][0..0]...
            //             v [9][9..9][middle][1][0..0] // here a greedy third group would steal from the second group
            // ...[middle][9][9..9]|[middle][1][0..0]...

            if (leading9.length() >= trailing0.length() && Long.parseLong(b) == Long.parseLong(a) + 1)
                return indexOf(Long.parseLong(middle + a + leading9)) + middle.length();
        }
        if (nines.matches())
            // ...8[9..9]|9[0..0]...
            //      ^ first occurrence of [9..9]9
            return indexOf(Long.parseLong("8" + tail(nines.group(1)))) + 1;

        if (flanked.matches()) {
            String leading = flanked.group(1);
            String middle = flanked.group(2);
            String trailing = flanked.group(3);

            if (middle.isEmpty())
                // n was all zeros
                // ...1[0..0]...
                //      ^ first occurrence of [0..0]
                return indexOf(Long.parseLong("1" + leading)) + 1;
            else if (leading.length() > trailing.length())
                // ...[middle][leading]|[middle][trailing]..1...
                //             ^ when leading > trailing
                return indexOf(Long.parseLong(middle + leading)) + middle.length();
            else
                // ...[middle]0..[leading]|[middle][trailing]1...
                //                ^ when leading <= trailing
                return indexOf(Long.parseLong(middle + trailing + "1")) - leading.length();

        }

        return Long.MAX_VALUE;
    }

    /* Cycles through all 'rotations' of s, ignoring rotations that start with '0'.
     * When rotating, tries to 'squish' same digits at the ends of the string.
     * Example: when rotating abcwxyzabc, we form yzabcwx, and not yzabcabcwx
     */
    private static List<Long> cycle(String s) {
        List<Long> indexes = new ArrayList<>();
        for (int i = 1; i < s.length(); i++) {
            String left = take(s, i);
            String right = drop(s, i);
            if (!right.startsWith("0")) {
                int squish = 0;
                for (int j = Math.min(left.length(), right.length()); j > 0; j--) {
                    String overlap = take(left, j);
                    String rest = drop(left, j);
                    boolean allNines = true;
                    for (int k = 0; k < rest.length(); k++) {
                        allNines &= rest.charAt(k) == '9';
                    }
                    if (overlap.equals(takeRight(right, j)) && !allNines) {
                        squish = j;
                        break;
                    }
                }
                /*
                 * For each such rotation, find the index
                 * ...yzabcwx|yzabcw(x+1)...
                 *      ^ abcwxyzabc
                 */
                indexes.add(indexOf(Long.parseLong(right + drop(left, squish))) + right.length() - squish);
            }
        }
        return indexes;
    }

    /**
     * Starting at `from`, build the string [from][from+1][from+2]... till it's length exceed the desired length Then return the
     * string truncated to the desired length
     *
     * @param from   The starting point
     * @param length The desired length
     */
    public static String grow(long from, int length, String acc) {
        if (acc.length() >= length) return acc.substring(0, length);
        else return grow(from + 1, length, acc + from);
    }

    /**
     * Tries to find a number `start` that is `check` digit long, such that [start-1][start][start+1]... can fit into n
     *
     * @param check The length of `start`
     * @param skip  The number of digits to ignore from the start. The ignored digits should fit into the end of [start-1]
     * @return The index of `start` - the number of digits skipped
     */
    private static long breakDown(String n, int check, int skip) {
        String lead = take(n, skip);
        String rest = drop(n, skip);
        long start = Long.parseLong(take(rest, check));
        String prev = "" + (start - 1);

        if (start != 0 && grow(start, rest.length(), "").equals(rest) && prev.endsWith(lead))
            return indexOf(start) - skip;
        else if (check == n.length()) return Long.MAX_VALUE; // stop searching when you reach the string length
        else if (check == skip + 1) return breakDown(n, check + 1, 0);
        else return breakDown(n, check, skip + 1);

    }

    /**
     * The sum of number of digits of all numbers less than a given number
     *
     * @param n The given number
     */
    public static long indexOf(long n) {

        String asString = "" + n;
        int digit = asString.length();
        String nine = "9" + ("0".repeat(digit - 1));
        long offset = 0;
        for (int i = 1; i < digit; i++) {
            offset += i * Long.parseLong(take(nine, i));
        }
        return (n - Long.parseLong(nine) / 9) * digit + offset;
    }

    ///////////////////////////////
    // Scala like String helpers //
    ///////////////////////////////

    /**
     * The rest of the string without its first char.
     */
    private static String tail(String s) {
        return slice(s, 1, s.length());
    }

    /**
     * A string containing the last `n` chars of this string.
     */
    private static String takeRight(String s, int n) {
        return drop(s, s.length() - Math.max(n, 0));
    }

    /**
     * The rest of the string without its `n` first chars.
     */
    private static String drop(String s, int n) {
        return slice(s, Math.min(n, s.length()), s.length());
    }

    /**
     * A string containing the first `n` chars of this string.
     */
    private static String take(String s, int n) {
        return slice(s, 0, n);
    }

    /**
     * Selects an interval of elements.  The returned string is made up
     * of all elements `x` which satisfy the invariant:
     * {{{
     * from <= indexOf(x) < until
     * }}}
     *
     * @param s     the String
     * @param from  the lowest index to include from this string.
     * @param until the lowest index to EXCLUDE from this string.
     * @return a string containing the elements greater than or equal to
     * index `from` extending up to (but not including) index `until`
     * of this string.
     */
    private static String slice(String s, int from, int until) {
        int start = Math.max(from, 0);
        int end = Math.min(until, s.length());
        if (start >= end) return "";
        else return s.substring(start, end);
    }
}