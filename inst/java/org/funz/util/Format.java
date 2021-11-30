package org.funz.util;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.apache.commons.lang.StringEscapeUtils;
import org.apache.commons.lang.StringUtils;
import static org.funz.util.Data.ARRAY_BEG;
import static org.funz.util.Data.ARRAY_END;
import static org.funz.util.Data.MAP_BEG;
import static org.funz.util.Data.MAP_END;
import static org.funz.util.Data.MAP_SEP;
import static org.funz.util.Data.asString;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 *
 * @author richet
 */
public class Format {
//    public static void main(String[] args) {
//        System.err.println(repeat(138,"+","123"));
//        System.err.println(repeat(138,"","123"));
//    }

    public static String MapToCSVString(Map<String, ?> X) {
        if (X == null) {
            return null;
        }
        return MapToCSVString(X, X.keySet());
    }

    public static String MapToCSVString(Map<String, ?> X, Collection<String> order) {
        StringBuilder sb = new StringBuilder();
        for (String o : order) {
            sb.append("\"").append(o).append("\"").append(";");
        }
        if (order.size() > 0) {
            sb.delete(sb.length() - 2, sb.length() - 1);
        }
        sb.append("\n");
        for (String o : order) {
            sb.append(X.get(o)).append(";");
        }
        if (order.size() > 0) {
            sb.delete(sb.length() - 2, sb.length() - 1);
        }

        return sb.toString();
    }

    public static String ArrayMapToCSVString(Map<String, ?> X) {
        if (X == null) {
            return null;
        }
        return ArrayMapToCSVString(X, X.keySet());
    }

    public static String ArrayMapToCSVString(Map<String, ?> X, Collection<String> order) {
        if (X == null) {
            return null;
        }

        if (X.size() == 0) {
            return "";
        }

        StringBuilder sb = new StringBuilder();
        String n = null;
        int l = 0;
        for (String k : order) {
            sb.append("\"").append(k).append("\"").append(";");
            n = k;
            if (X.get(k) != null) {
                if (X.get(k).getClass().isArray()) {
                    l = Math.max(l, Array.getLength(X.get(k)));
                } else {
                    l = Math.max(l, 1);
                }
            }
        }
        sb.append("\n");
        sb.delete(sb.length() - 2, sb.length() - 1);

        if (l > 0) {
            for (int i = 0; i < l; i++) {
                for (String k : order) {
                    if (X.get(k) != null) {
                        if (X.get(k).getClass().isArray()) {
                            sb.append(asString(Array.get(X.get(k), i % Array.getLength(X.get(k)))));
                        } else {
                            sb.append(asString(X.get(k)));
                        }
                    }
                    sb.append(";");
                }
                sb.append("\n");
                sb.delete(sb.length() - 2, sb.length() - 1);
            }
        }

        return sb.deleteCharAt(sb.length() - 1).toString();
    }

    public static String MapToJSONString(Map<String, ?> X) {
        return asString(X, true, " : ");
    }

    /*static String JSONify(Object o) {
     String str = null;
     if (o instanceof Map) {
     str = MapToJSONString((Map) o);
     } else {
     str = asString(o, true, " : ");
     }
     System.err.println(o + " (" + o.getClass() + ") -> " + str);
     return str;

     }*/
    public static String ArrayMapToJSONString(Map<String, ?> X/*, Collection<String> order*/) {
        if (X == null) {
            return null;
        }

        if (X.size() == 0) {
            return "";
        }

        return asString(X, true, " : ").replace("" + MAP_BEG, MAP_BEG + "\n").replace("" + MAP_END, "\n" + MAP_END).replace("" + MAP_SEP, MAP_SEP+"\n");
    }

    /*public static String MapToXMLString(Map<String, ?> X) {
     return MapToXMLString(X);
     }*/
    public static String MapToXMLString(Map<String, ?> X, boolean xmlheader) {
        if (X == null) {
            return null;
        }
        return MapToXMLString(X, X.keySet(), xmlheader);
    }

    public static String MapToXMLString(Map<String, ?> X, Collection<String> order, boolean xmlheader) {
        StringBuilder sb = new StringBuilder();
        for (String o : order) {
            String Xo = asString(X.get(o),false,"=");
            sb.append("<").append(o).append(">").append(Xo).append("</").append(o).append(">").append("\n");
        }

        return (!xmlheader ? "" : "<?xml version=\"1.0\"?>\n<data>\n") + sb.toString() + (!xmlheader ? "" : "</data>");
    }

    public static String ArrayMapToXMLString(Map<String, ?> X) {
        return ArrayMapToXMLString(X, true);

    }

    public static String ArrayMapToXMLString(Map<String, ?> X, boolean xmlheader) {
        if (X == null) {
            return null;
        }
        return ArrayMapToXMLString(X, X.keySet(), xmlheader);
    }

    public static String ArrayMapToXMLString(Map<String, ?> X, Collection<String> order, boolean xmlheader) {
        if (X == null) {
            return null;
        }

        if (X.size() == 0) {
            return !xmlheader ? "" : "<?xml version=\"1.0\"?>\n<data></data>";
        }

        StringBuilder sb = new StringBuilder();
        for (String o : order) {
            Object Xo = X.get(o);
            String sXo = asString(Xo,false,"=");
            if (Xo != null) {
                if (Xo instanceof Map) {
                    sXo = ArrayMapToXMLString((Map<String, ?>) Xo, false);
                } else {
                    sXo = sXo.replace("" + ARRAY_BEG, "").replace("" + ARRAY_END, "");
                }
            } else {
                sXo = "";
            }

            sb.append("<").append(o).append(">").append(sXo).append("</").append(o).append(">").append("\n");
        }

        return (!xmlheader ? "" : "<?xml version=\"1.0\"?>\n<data>\n") + sb.toString() + (!xmlheader ? "" : "</data>");
    }

    public static String MapToMDString(Map<String, ?> X) {
        if (X == null) {
            return null;
        }
        return MapToMDString(X, X.keySet());
    }

    public static String MapToMDString(Map<String, ?> X, Collection<String> order) {
        if (X == null) {
            return null;
        }
        if (order == null) {
            order = X.keySet();
        }

        StringBuilder sb = new StringBuilder();
        int m = 0;
        for (String o : order) {
            if (o != null) {
                m = Math.max(m, o.length());
            }
        }
        for (String o : order) {
            if (o != null) {
                String Xo = asString(X.get(o),false,":");
                sb.append("  * ").append(StringUtils.rightPad(o + ": ", m + 2)).append(Xo).append("\n");
            }
        }
        if (sb.length() > 0) {
            return sb.deleteCharAt(sb.length() - 1).toString();
        } else {
            return sb.toString();
        }
    }

    public static String ArrayMapToMDString(Map<String, ?> X) {
        if (X == null) {
            return null;
        }
        
        return ArrayMapToMDString(X, X.keySet());
    }

    public static String ArrayMapToMDString(Map<String, ?> X, Collection<String> order) {
        if (X == null) {
            return null;
        }

        if (X.size() == 0) {
            return "";
        }

        if (order == null) {
            order = X.keySet();
        }
        
        StringBuilder sb = new StringBuilder();
        int l = 0;
        Map<String, Integer> maxsize = new HashMap<String, Integer>();
        for (String k : order) {
            int m = k.length();
            if (X.get(k) != null) {
                if (X.get(k).getClass().isArray()) {
                    for (int i = 0; i < Array.getLength(X.get(k)); i++) {
                        if (Array.getLength(X.get(k)) > i && Array.get(X.get(k), i) != null) {
                            m = Math.max(m, asString(Array.get(X.get(k), i),false,":").length());
                        }
                    }
                    l = Math.max(l, Array.getLength(X.get(k)));
                } else {
                    m = Math.max(m, asString(X.get(k),false,":").length());
                    l = Math.max(l, 1);
                }

                maxsize.put(k, m);
                sb.append("| ").append(trimToSize(k, maxsize.get(k))).append(" ");
            } else {
                maxsize.put(k, m);
                sb.append("| ").append(trimToSize(k, maxsize.get(k))).append(" ");
//                l = Math.max(l,0); not needed
            }
        }
        sb.append("|\n");

        for (String k : order) {
            sb.append("|-").append(repeat(maxsize.get(k), "", "-")).append("-");
        }
        sb.append("|\n");

        if (l > 0) {
            for (int i = 0; i < l; i++) {
                for (String k : order) {
                    if (X.get(k) != null) {
                        if (X.get(k).getClass().isArray()) {
                            Object Xk = Array.get(X.get(k), i % Array.getLength(X.get(k)));
                            String s = Xk==null ? ""+Data.NUL:asString(Xk,false,":");
                            sb.append("| ").append(trimToSize(s.replace('\t', ' '), maxsize.get(k))).append(" ");
                        } else {
                            Object Xk = X.get(k);
                            String s = Xk==null ? ""+Data.NUL:asString(Xk,false,":");
                            sb.append("| ").append(trimToSize(s.replace('\t', ' '), maxsize.get(k))).append(" ");
                        }
                    } else {
                        sb.append("| ").append(repeat(maxsize.get(k), "", " ")).append(" ");
                    }
                }
                sb.append("|\n");
            }
        }

        if (sb.length() > 0) {
            return sb.deleteCharAt(sb.length() - 1).toString();
        } else {
            return sb.toString();
        }
    }
    
    public static String trimToSize(String s, int t) {
        if (s == null) {
            return trimToSize("" + Data.NUL, t);
        }
        if (t > 0) {
            return StringUtils.rightPad(s, t, " ").substring(0, t);//StringUtils.rightPad(s.replace('\n', ';'), t, " ").substring(0, t);
        } else {
            return s;//s.replace('\n', ';');
        }

    }
    
    public static String repeat(int times, String separator, String torepeat) {
        if (times <= 0) {
            return "";
        }
        StringBuilder o = new StringBuilder((separator.length() + torepeat.length()) * times);
        for (int i = 0; i < times; i++) {
            o = o.append(separator).append(torepeat);
        }

        return o.substring(separator.length(), o.length());
    }

    public static String fromHTML(String src) {
        if (src == null) {
            return src;
        }
        src = StringEscapeUtils.unescapeHtml(src);
        return src.replace("<br/>", "\n");
        /*src = src.replace("&lt;", "<");
         src = src.replace("&quot;", "\"");
         src = src.replace("&apos;", "'");
         src = src.replace("&gt;", ">");
         return src.replace("&amp;", "&");*/
    }

    public static String toHTML(String src) {
        if (src == null) {
            return src;
        }
        src = StringEscapeUtils.escapeHtml(src);
        return src.replace("\n", "<br/>");
        /*src = src.replace("&", "&amp;");
         src = src.replace("\"", "&quot;");
         src = src.replace("'", "&apos;");
         src = src.replace(">", "&gt;");
         return src.replace("<", "&lt;");*/
    }

    /**
     * Conveniency static method to convert a html text in plain format.
     *
     * @param html String of HTML formatted text
     * @return plain text
     */
    public static String HTMLToString(String html) {
        int i = html.indexOf("<table");
        while (i >= 0) {
            int e = html.indexOf("</table>", i);
            if (e > i) {
                html = html.substring(0, i) + HTMLTableToString(html.substring(i, e)) + html.substring(e + 8);
            } else {
                html = html.substring(0, i);
                break;
            }
            i = html.indexOf("<table", i + 1);
        }
        return fromHTML(html);
    }

    /**
     * Conveniency static method to convert a html table in plain format (using
     * tabs and '|')
     *
     * @param html_table String of HTML formatted table
     * @return plain formatted table
     */
    public static String HTMLTableToString(String html_table) {
        html_table = html_table.replace("\n", "");
        html_table = html_table.replace("</td>", "" + '\u0009' + "|");
        html_table = html_table.replace("<td>", " ");
        html_table = html_table.replace("<tr>", "\n|");
        html_table = html_table.replace("<table>", "");
        html_table = html_table.replace("</table>", "");
        return html_table;
    }

    public static class XML {

        public static String merge(String sep, List<String> s) {
            if (s == null || s.size() == 0) {
                return "";
            }
            List<Element> le = new ArrayList<Element>(s.size());
            for (int i = 0; i < s.size(); i++) {
                try {
                    le.add(i, fromString(s.get(i)));
                } catch (Exception ex) {
                    le.add(i, null);
                }
            }
            Element root = le.get(0);
            StringBuilder content = new StringBuilder(root.getTextContent());
            for (int i = 1; i < le.size(); i++) {
                content.append(sep);
                content.append(le.get(i).getTextContent());
            }
            root.setTextContent(content.toString());
            try {
                return toString(root);
            } catch (Exception ex) {
                ex.printStackTrace();
                return null;
            }
        }

        public static String toString(Node e) throws TransformerConfigurationException, TransformerException {
            return fromHTML(getText(e));
        }

        public static String getText(Node e) throws TransformerConfigurationException, TransformerException {
            TransformerFactory tf = TransformerFactory.newInstance();
            Transformer trans = tf.newTransformer();
            StringWriter sw = new StringWriter();
            trans.transform(new DOMSource(e), new StreamResult(sw));
            String out = sw.toString();
            if (out.indexOf("?>") >= 0) {
                return out.substring(out.indexOf("?>") + 2);
            } else {
                return out;
            }
        }

        public static Element fromString(String s) throws ParserConfigurationException, IOException {
            if (!s.startsWith("<?")) {
                s = "<?xml version=\"1.0\"?>" + s;
            }
            Element e = null;
            try {
                e = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new InputSource(new StringReader(s))).getDocumentElement();
            } catch (SAXException se) {
                System.err.println("Impossible to instanciate as XML Element :\n" + s);
            }
            return e;
        }
    }

}
