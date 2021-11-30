package org.funz.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import org.apache.commons.lang.ArrayUtils;

/**
 *
 * @author richet
 */
public class Data {

    public final static char ARRAY_BEG = '[', ARRAY_END = ']', ARRAY_SEP = ',';
    public final static char MAP_BEG = '{', MAP_END = '}', MAP_SEP = ',';
    public final static char STR_BEG = '\"', STR_END = '\"';
    public final static char NUL = '\u00D8';

    public static String[] asStringArray(Object[] array) {
        String[] sarray = new String[array.length];
        for (int i = 0; i < sarray.length; i++) {
            sarray[i] = asString(array[i]);
        }
        return sarray;
    }

    public static Object[] asObjectArray(String[] array) {
        Object[] oarray = new Object[array.length];
        for (int i = 0; i < oarray.length; i++) {
            oarray[i] = asObject(array[i]);
        }
        return oarray;
    }

    public static String asString(Object o) {
        return asString(o, false, "=");
    }

    public static String asString(Object o, boolean str_mark, String eq) {
        String str_beg = str_mark ? "" + STR_BEG : "";
        String str_end = str_mark ? "" + STR_END : "";

        if (o == null) {
            return null;//"" + NUL;
        }

        if (o instanceof Map) {
            Map map = (Map) o;
            StringBuilder buf = new StringBuilder();
            buf.append(MAP_BEG);
            for (Object oo : map.keySet()) {
                buf.append(str_beg + oo.toString() + str_end + eq + asString(map.get(oo), str_mark, eq));
                buf.append(MAP_SEP);
            }
            if (map.size() > 0) {
                buf.delete(buf.length() - 1, buf.length());
            }
            buf.append(MAP_END);
            return buf.toString();
        }

        if (!o.getClass().isArray()) {
            try {
                Integer.parseInt(o.toString());
                return o.toString();
            } catch (Exception e) {

                try {
                    Double.parseDouble(o.toString());
                    return o.toString();
                } catch (Exception ee) {
                    return str_beg + o.toString().replace("\n", "\\n") + str_end;
                }
            }
        }

        try {
            int[] cast = (int[]) o;
            if (cast == null || cast.length == 0) {
                return "" + ARRAY_BEG + ARRAY_END;
            }
            StringBuilder buf = new StringBuilder();
            buf.append(ARRAY_BEG);
            for (int i = 0; i < cast.length; i++) {

                buf.append(cast[i]);

                if (i < cast.length - 1) {
                    buf.append(ARRAY_SEP);
                }
            }

            buf.append(ARRAY_END);
            return buf.toString();

        } catch (ClassCastException c) {
            //System.err.println("Cannot cast to (int[]):" + array);
        } catch (NullPointerException c) {
        }

        try {
            double[] cast = (double[]) o;
            if (cast == null || cast.length == 0) {
                return "" + ARRAY_BEG + ARRAY_END;
            }
            StringBuilder buf = new StringBuilder();
            buf.append(ARRAY_BEG);
            for (int i = 0; i < cast.length; i++) {

                buf.append(cast[i]);

                if (i < cast.length - 1) {
                    buf.append(ARRAY_SEP);
                }
            }

            buf.append(ARRAY_END);
            return buf.toString();

        } catch (ClassCastException c) {
            //System.err.println("Cannot cast to (double[]):" + array);
        } catch (NullPointerException c) {
        }

        Object[] cast = null;
        try {
            cast = (Object[]) o;
        } catch (ClassCastException c) {
            System.err.println("Cannot cast to (Object[]):" + o);
        }
        if (cast == null || cast.length == 0) {
            return "" + ARRAY_BEG + ARRAY_END;
        }
        StringBuilder buf = new StringBuilder();
        buf.append(ARRAY_BEG);
        for (int i = 0; i < cast.length; i++) {
            if (cast[i] != null) {
                if (cast[i].getClass().isArray()) {
                    buf.append(asString(cast[i], str_mark, eq));
                } else {

                    try {
                        Double.parseDouble(cast[i].toString());
                        buf.append(cast[i]);
                    } catch (Exception e) {
                        buf.append(asString(cast[i], str_mark, eq));
                    }

                    //buf.append(cast[i]);
                }
            }
            if (i < cast.length - 1) {
                buf.append(ARRAY_SEP);
            }
        }
        buf.append(ARRAY_END);
        return buf.toString();
    }
    static String sysoutprefix = "";

    public static Map asMapObject(String map_string) {
        Map m = new HashMap();
        if (map_string.charAt(0) != '{' || map_string.charAt(map_string.length() - 1) != '}') {
            throw new IllegalArgumentException("String " + map_string + " is not a map.");
        }
        String in = map_string.substring(1, map_string.length() - 1);
//        System.err.println("Map: "+in);
        int o_quote = 0;
        int o_squote = 0;
        int o_bracket = 0;
        int o_sqbracket = 0;
        String k = "";
        boolean k2v = false;
        String v = "";
        for (int i = 0; i < in.length(); i++) {
//            System.err.println("k: " + k + " v:" + v + " [" + o_sqbracket + " {" + o_bracket);
            char c = in.charAt(i);
            if ((c == ':' || c == '=') && !k2v) { //should support JSON ':' or '='
                k2v = true;
                c = ' ';
            } else if (c == '[') {
                o_sqbracket++;
            } else if (c == ']') {
                o_sqbracket--;
            } else if (c == '{') {
                o_bracket++;
            } else if (c == '}') {
                o_bracket--;
            } else if (c == '"') {
                o_quote++;
            } else if (c == '\'') {
                o_squote++;
            }

            if (o_bracket == 0 && o_sqbracket == 0 && o_quote % 2 == 0 && o_squote % 2 == 0 && (c == ',' || i == in.length() - 1)) {
                if (c != ',') {
                    v = v + c;
                }
                m.put(k.trim(), asObject(v.trim()));
                k = "";
                k2v = false;
                v = "";
            } else {
                if (k2v) {
                    v = v + c;
                } else {
                    k = k + c;
                }
            }
        }
        return m;
    }

    public static Object asArrayObject(String array_string) {
        List l = new LinkedList();
        if (array_string.charAt(0) != '[' || array_string.charAt(array_string.length() - 1) != ']') {
            throw new IllegalArgumentException("String " + array_string + " is not an array.");
        }
        String in = array_string.substring(1, array_string.length() - 1);
//        System.err.println("Array: "+in);
        int o_bracket = 0;
        int o_sqbracket = 0;
        String v = "";
        Object instance = null;
        boolean same_class = true;
        for (int i = 0; i < in.length(); i++) {
//            System.err.println("v:" + v + " [" + o_sqbracket + " {" + o_bracket);
            char c = in.charAt(i);
            if (c == '[') {
                o_sqbracket++;
            } else if (c == ']') {
                o_sqbracket--;
            } else if (c == '{') {
                o_bracket++;
            } else if (c == '}') {
                o_bracket--;
            }

            if (o_bracket == 0 && o_sqbracket == 0 && (c == ',' || i == in.length() - 1)) {
                if (c != ',') {
                    v = v + c;
                }
                Object o = asObject(v.trim());
                if (instance == null) {
                    instance = o;
                } else {
                    if (!o.getClass().equals(instance.getClass())) {
                        same_class = false;
//                        System.err.println(o.getClass()+" != "+instance.getClass());
                    }
                }
                l.add(o);
                v = "";
                c = ' ';
            } else {
                v = v + c;
            }
        }

        if (same_class) {
            if (instance instanceof Double) {
//                System.err.println("Double");
                return ArrayUtils.toPrimitive((Double[]) (l.toArray(new Double[0])));
            } else if (instance instanceof double[]) {
//                System.err.println("double[]");
                return (double[][]) l.toArray(new double[0][]);
            } else if (instance instanceof double[][]) {
//                System.err.println("double[][]");
                return (double[][][]) l.toArray(new double[0][][]);
            } else {
//                  System.err.println("?");
                return l.toArray();
            }
        } else {
            return l.toArray();
        }
    }

    public static Object asObject(String string) {
        //System.out.println(sysoutprefix + "  ? " + string);
        if (string == null || string.length() == 0 || string.equals("null")) {
            return null;
        }

        if (string.charAt(0) == ARRAY_BEG && string.charAt(string.length() - 1) == ARRAY_END) {
            return asArrayObject(string);
        } else if (string.charAt(0) == MAP_BEG && string.charAt(string.length() - 1) == MAP_END) {
            return asMapObject(string);
        } else if (string.length()>1 && string.charAt(0) == STR_BEG && string.charAt(string.length() - 1) == STR_END) {
            String element = string.substring(1, string.length() - 1);
            //System.err.println(">" + element);
            return element;
        } else {
            //System.out.println(sysoutprefix + "   dim=0");
            Object val = null;
            /*try {
             val = new Integer(Integer.parseInt(string));
             System.out.println(sysoutprefix + "     is Integer");
             } catch (NumberFormatException nfe) {
             }*/
            //if (val == null) {
            try {
                val = Double.parseDouble(string);
                //System.out.println(sysoutprefix + "     is Double");
            } catch (NumberFormatException nfe) {
            }
            //} else {
            //    return val;
            //}
            if (val == null) {
                //System.out.println(sysoutprefix + "     is String");
                return string;
            } else {
                return val;
            }
        }
    }

    public static Map<String, Object[]> transpose(Map<String, Object[]> m, String by_key) {
        if (!m.containsKey(by_key)) {
            return null;
        }

        String[] new_keys = asStringArray(m.get(by_key));
        Set<String> keys = m.keySet();
        keys.remove(by_key);

        Map<String, Object[]> t = new HashMap<String, Object[]>();
        for (int i = 0; i < new_keys.length; i++) {
            t.put(new_keys[i], asArray(get(m, i), (new LinkedList<String>(keys)).toArray(new String[keys.size()])));
        }
        return t;
    }

    /**
     * Conveniency static method to get a list of keys in a HashMap. Useful
     * outside Java.
     *
     * @param h HashMap
     * @return list of keys of h
     */
    public static String[] keys(Map h) {
        if (h == null) {
            return null;
        }
        String[] keys = new String[h.size()];
        int i = 0;
        for (Iterator it = h.keySet().iterator(); it.hasNext();) {
            keys[i++] = it.next().toString();
        }
        return keys;
    }

    public static Map newMap(Object... o) {
        Map m = new HashMap();
        for (int i = 0; i < o.length / 2; i++) {
            m.put(o[2 * i], o[2 * i + 1]);
        }
        return m;
    }

    public static Map mergeMapArray(Map[] maparray) {
        Map mergedmap = new TreeMap();
        if (maparray == null || maparray.length == 0) {
            return mergedmap;
        }
        List keys = new LinkedList();
        for (Map result : maparray) {
            if (result != null && !result.isEmpty()) {
                keys.addAll(result.keySet());
            }
        }
        for (Object key : keys) {
            List values = new ArrayList(maparray.length);
            int ii = 0;
            for (int i = 0; i < maparray.length; i++) {
                if (maparray[i] != null) {
                    if (maparray[i].containsKey(key)) {
                        values.add(ii++, maparray[i].get(key));
                    } else {
                        values.add(ii++, null);
                    }
                }
            }
            mergedmap.put(key, values.toArray(new Object[values.size()]));
        }
        return mergedmap;
    }

    public static Map<String, String[]> mergeStringArrayMap(Map<String, ?>... maparray) {
        Map<String, String[]> mergedmap = new HashMap();
        if (maparray == null || maparray.length == 0) {
            return mergedmap;
        }
        List<String> keys = new LinkedList();
        for (Map<String, ?> result : maparray) {
            if (result != null && !result.isEmpty()) {
                keys.addAll(result.keySet());
            }
        }

        for (String key : keys) {
            List<String> values = new ArrayList(maparray.length);
            int ii = 0;
            for (int i = 0; i < maparray.length; i++) {
                if (maparray[i] != null) {
                    if (maparray[i].containsKey(key)) {
                        values.add(ii++, asString(maparray[i].get(key)));
                    } else {
                        values.add(ii++, "?");
                    }
                }
            }
            mergedmap.put(key, values.toArray(new String[values.size()]));
        }
        return mergedmap;
    }

    public static Map<String, Object[]> mergeArrayMap(Map<String, ?>... maparray) {
        Map<String, Object[]> mergedmap = new HashMap();
        if (maparray == null || maparray.length == 0) {
            return mergedmap;
        }
        List<String> keys = new LinkedList();
        for (Map<String, ?> result : maparray) {
            if (result != null && !result.isEmpty()) {
                keys.addAll(result.keySet());
            }
        }

        for (String key : keys) {
            List<Object> values = new ArrayList(maparray.length);
            int ii = 0;
            for (int i = 0; i < maparray.length; i++) {
                if (maparray[i] != null) {
                    if (maparray[i].containsKey(key)) {
                        values.add(ii++, maparray[i].get(key));
                    } else {
                        values.add(ii++, "?");
                    }
                }
            }
            mergedmap.put(key, values.toArray(new Object[values.size()]));
        }
        return mergedmap;
    }

    public static int[] indexOf(Map<String, Object[]> map, Map<String, Object> what) {
        if (map == null || what == null) {
            return null;
        }

        List<Integer> all = null;
        for (String w : what.keySet()) {
            List<Integer> which = new LinkedList<Integer>();
            Object o = what.get(w);
            Object[] os = map.get(w);
            if (all == null) {
                all = new LinkedList<Integer>();
                for (int i = 0; i < os.length; i++) {
                    all.add(i);
                }
            }
            for (int i = 0; i < os.length; i++) {
                if (os[i].equals(o)) {
                    which.add(i);
                }
            }
            List<Integer> whichnot = new LinkedList<Integer>();
            for (Integer i : all) {
                if (!which.contains(i)) {
                    whichnot.add(i);
                }
            }
            for (Integer i : whichnot) {
                all.remove(i);
            }
        }
        int[] allint = new int[all.size()];
        if (all.size() > 0) {
            for (int i = 0; i < allint.length; i++) {
                allint[i] = all.get(i);
            }
        }
        return allint;
    }

    public static Map<String, Object> get(Map<String, Object[]> map, int i) {
        if (map == null) {
            return null;
        }
        Map<String, Object> res = new HashMap<String, Object>();
        for (String k : map.keySet()) {
            res.put(k, map.get(k)[i]);
        }
        return res;
    }

    public static Map<String, Object> remove(Map<String, Object> m, String regexp) {
        if (m == null) {
            return null;
        }
        Map<String, Object> filtered = new HashMap(m.size());
        for (String k : m.keySet()) {
            if (!k.matches(regexp)) {
                filtered.put(k, m.get(k));
            }
        }
        return filtered;
    }

    public static Map<String, Object[]> remove_array(Map<String, Object[]> m, String regexp) {
        if (m == null) {
            return null;
        }
        Map<String, Object[]> filtered = new HashMap(m.size());
        for (String k : m.keySet()) {
            if (!k.matches(regexp)) {
                filtered.put(k, m.get(k));
            }
        }
        return filtered;
    }

    public static Map<String, Object> keep(Map<String, Object> m, String regexp) {
        if (m == null) {
            return null;
        }
        Map<String, Object> filtered = new HashMap(m.size());
        for (String k : m.keySet()) {
            if (k.matches(regexp)) {
                filtered.put(k, m.get(k));
            }
        }
        return filtered;
    }

    public static Map<String, Object[]> keep_array(Map<String, Object[]> m, String regexp) {
        if (m == null) {
            return null;
        }
        Map<String, Object[]> filtered = new HashMap(m.size());
        for (String k : m.keySet()) {
            if (k.matches(regexp)) {
                filtered.put(k, m.get(k));
            }
        }
        return filtered;
    }

    public static Object[] asArray(Map<String, Object> map, String... keys) {
        Object[] res = new Object[keys.length];
        for (int i = 0; i < keys.length; i++) {
            String k = keys[i];
            res[i] = map.get(k);
        }
        return res;
    }

    public static Map<String, String[]> MapArrayToMapStringArray(Map<String, Object[]> am) {
        if (am == null) {
            return null;
        }
        Map<String, String[]> sam = new TreeMap();
        for (String k : am.keySet()) {
            sam.put(k, asStringArray(am.get(k)));
        }
        return sam;
    }

    public static void main(String[] args) throws InterruptedException {
        System.err.println("'" + NUL + "'");

        Map map = new HashMap();
        map.put("a", 123);
        map.put("b", new double[]{1, 2, 3});
        map.put("bb", new String[]{"aa", "bb", "cc"});
        System.err.println(map.get("bb"));
        map.put("bbb", Arrays.asList(new String[]{"aaa", "bbb", "ccc"}));
        System.err.println(map.get("bbb"));
        Map c = new HashMap();
        c.put("b1", new double[]{10, 20, 30});
        c.put("b2", Arrays.asList(new String[]{"bbb", "ccc"}));
        map.put("c", c);
        String str_map = asString(map);
        System.err.println(str_map);

        System.err.println(asString(asObject(str_map)));

        /*System.err.println(":" + (new String[1]).getClass().isArray());
        System.err.println(asString(null));

        System.err.println("asObject(\"\")= " + asObject(""));
        System.err.println("asObject(\"[]\")= " + asObject("[]"));

        Map mNULL = new HashMap();
        mNULL.put("a1", null);
        mNULL.put("b", null);
        mNULL.put("c", null);
        mNULL.put("d", null);
        System.err.println("m0= " + mNULL);

        String mNULLAsString = asString(mNULL);
        System.err.println("asString(mNULL)= " + mNULLAsString);
        System.err.println("asObject(asString(mNULL))= " + asObject(mNULLAsString));
        //System.err.println(((Object[])((Map)asObject(mAsString)).get("d")).length);
        System.out.println("ArrayMapToMDString(mNULL)=\n" + ArrayMapToMDString(mNULL));
        System.out.println("ArrayMapToCSVString(mNULL)=\n" + ArrayMapToCSVString(mNULL));
        System.out.println("ArrayMapToXMLString(mNULL)=\n" + ArrayMapToXMLString(mNULL));
        System.out.println("ArrayMapToJSONString(mNULL)=\n" + ArrayMapToJSONString(mNULL));

        Map m0 = new HashMap();
        m0.put("a1", new double[]{});
        m0.put("b", new double[]{});
        m0.put("c", null);
        m0.put("d", new String[]{});
        System.err.println("m0= " + m0);

        String m0AsString = asString(m0);
        System.err.println("asString(m0)= " + m0AsString);
        System.err.println("asObject(asString(m0))= " + asObject(m0AsString));
        //System.err.println(((Object[])((Map)asObject(mAsString)).get("d")).length);
        System.out.println("ArrayMapToMDString(m0)=\n" + ArrayMapToMDString(m0));
        System.out.println("ArrayMapToCSVString(m0)=\n" + ArrayMapToCSVString(m0));
        System.out.println("ArrayMapToXMLString(m0)=\n" + ArrayMapToXMLString(m0));
        System.out.println("ArrayMapToJSONString(m0)=\n" + ArrayMapToJSONString(m0));

        Map m = new HashMap();
        m.put("a1", 132);
        m.put("b", "cdef");
        m.put("c", null);
        m.put("d", new String[]{"a", "b"});
        m.put("d2", new String[]{"a", "b", "c"});
        System.err.println("m= " + m);

        String mAsString = asString(m);
        System.err.println("asString(m)= " + mAsString);
        System.err.println("asObject(asString(m))= " + asObject(mAsString));
        //System.err.println(((Object[])((Map)asObject(mAsString)).get("d")).length);
        System.out.println("ArrayMapToMDString(m)=\n" + ArrayMapToMDString(m));
        System.out.println("ArrayMapToCSVString(m)=\n" + ArrayMapToCSVString(m));
        System.out.println("ArrayMapToXMLString(m)=\n" + ArrayMapToXMLString(m));
        System.out.println("ArrayMapToJSONString(m)=\n" + ArrayMapToJSONString(m));*/
    }
}
