package b4b_solution;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class InfiniteDigitalString {

    public static long getIndex(long n) {

        String asString = "" + n;
        int digit = asString.length();
        String nine = "9" + ("0".repeat(digit - 1));
        long offset = 0;
        for (int i = 1; i < digit; i++) {
            offset += i * Long.parseLong(take(nine, i));
        }
        return (n - Long.parseLong(nine) / 9) * digit + offset;
    }


    public static long findPosition(final String s) {

        if (Long.parseLong(s) == 0L) return getIndex(Long.parseLong("1" + s)) + 1;

        List<Long> poss = null;
        for (int l = 1; l < s.length() + 1; l++) {
            poss = new ArrayList<Long>();
            for (int i = 0; i < l + 1; i++) {
                String sdt = s.substring(0, l - i), end = s.substring(l - i, l);

                List<String> toTest = end.length() != 0 && Long.parseLong(end) != 0 ? Arrays.asList(end + sdt, (Long.parseLong(end) - 1) + sdt) : List.of(end + sdt);
                for (String c : toTest) {
                    if (c.charAt(0) == '0') continue;
                    String ds = c;
                    long n = Long.parseLong(c);
                    while (ds.length() < s.length() + l) ds += "" + ++n;
                    int idx = ds.indexOf(s);
                    if (idx != -1) poss.add(getIndex(Long.parseLong(c)) + idx);
                }
            }
            if (poss.size() > 0) break;
        }
        return Collections.min(poss);
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