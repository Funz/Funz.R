/*
 * Created on 1 juin 06 by richet
 */
package org.funz.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Iterator;
import java.util.Map;
import org.apache.commons.io.FileUtils;
import static org.funz.util.Data.asString;

public class ASCII {

    public static final String CHARSET;

    static {
        String charset = System.getProperty("charset");
        CHARSET = (charset != null && charset.length() > 0) ? charset : "ISO-8859-15";
    }

    public static String removeAccents(String s) {
        s = s.replaceAll("[ÃšÃ©ÃªÃ«]", "e");
        s = s.replaceAll("[Ã»Ã¹]", "u");
        s = s.replaceAll("[Ã¯Ã®]", "i");
        s = s.replaceAll("[Ã Ã¢]", "a");
        s = s.replaceAll("Ã", "o");
        s = s.replaceAll("Ã§", "c");
        s = s.replaceAll("[ÃÃÃÃ]", "E");
        s = s.replaceAll("[ÃÃ]", "U");
        s = s.replaceAll("[ÃÃ]", "I");
        s = s.replaceAll("[ÃÃ]", "A");
        s = s.replaceAll("Ã", "O");
        s = s.replaceAll("Ã", "C");
        return s;
    }

    public static String InputStreamToString(InputStream is) throws IOException {
        String out = "";
        if (is != null) {
            Writer writer = new StringWriter();
            Reader reader = null;
            char[] buffer = new char[1024];
            try {
                reader = new BufferedReader(new InputStreamReader(is, CHARSET));
                int n;
                while ((n = reader.read(buffer)) != -1) {
                    writer.write(buffer, 0, n);
                }
                out = writer.toString();
            } finally {
                is.close();
                writer.close();
                if (reader != null) {
                    reader.close();
                }
            }
        }
        return out;
    }

    public static String cat(Map hash) {
        if (hash == null) {
            return null;
        }
        StringBuilder sb = new StringBuilder();
        Iterator s = hash.keySet().iterator();
        while (s.hasNext()) {
            Object key = s.next();
            sb.append(key).append(" = ").append(asString(hash.get(key))).append("\n");
        }
        if (hash.size() > 0) {
            sb.setLength(sb.length() - 1);
        }
        return sb.toString();
    }

    public static String cat(String separator, Map hash) {
        if (hash == null) {
            return null;
        }
        StringBuilder sb = new StringBuilder();
        Iterator s = hash.keySet().iterator();
        while (s.hasNext()) {
            Object key = s.next();
            sb.append(key).append(" = ").append(asString(hash.get(key))).append(separator);
        }
        if (hash.size() > 0 && sb.length() > separator.length()) {
            sb.setLength(sb.length() - separator.length());
        }
        return sb.toString();
    }

    public static String cat(String separator, double[] array) {
        if (array == null) {
            return null;
        }
        if (array.length == 0) {
            return "";
        }

        String o = array[0] + "";
        if (array.length > 1) {
            for (int i = 1; i < array.length; i++) {
                o += (separator + (array[i] + ""));
            }
        }
        return o;
    }

    public static String cat(String separator, int[] array) {
        if (array == null) {
            return null;
        }
        if (array.length == 0) {
            return "";
        }

        String o = array[0] + "";
        if (array.length > 1) {
            for (int i = 1; i < array.length; i++) {
                o += (separator + (array[i] + ""));
            }
        }
        return o;
    }

    public static String cat(String separator, double[][] array) {
        if (array == null) {
            return null;
        }
        if (array.length == 0) {
            return "";
        }

        String o = cat(separator, array[0]);
        if (array.length > 1) {
            for (int i = 1; i < array.length; i++) {
                o += "\n" + cat(separator, array[i]);
            }
        }
        return o;
    }

    public static String cat(String separator, int[][] array) {
        if (array == null) {
            return null;
        }
        if (array.length == 0) {
            return "";
        }

        String o = cat(separator, array[0]);
        if (array.length > 1) {
            for (int i = 1; i < array.length; i++) {
                o += "\n" + cat(separator, array[i]);
            }
        }
        return o;
    }

    public static String cat(String separator, Object[] array) {
        if (array == null) {
            return null;
        }
        if (array.length == 0) {
            return "";
        }

        String o = (array[0] == null ? "" : asString(array[0]));
        if (array.length > 1) {
            for (int i = 1; i < array.length; i++) {
                o += (separator + (array[i] == null ? "" : asString(array[i])));
            }
        }

        return o;
    }

    public static String cat(String separator, Object[][] array) {
        if (array == null) {
            return null;
        }
        if (array.length == 0) {
            return "";
        }

        String o = cat(separator, array[0]);
        if (array.length > 1) {
            for (int i = 1; i < array.length; i++) {
                o += "\n" + cat(separator, array[i]);
            }
        }
        return o;
    }

    public static String cat(String separator, String[] array) {
        if (array == null) {
            return null;
        }
        if (array.length == 0) {
            return "";
        }

        String o = (array[0] == null ? "" : array[0]);
        if (array.length > 1) {
            for (int i = 1; i < array.length; i++) {
                o += (separator + (array[i] == null ? "" : array[i]));
            }
        }

        return o;
    }

    public static String cat(String separator, String[][] array) {
        if (array == null) {
            return null;
        }
        if (array.length == 0) {
            return "";
        }

        String o = cat(separator, array[0]);
        if (array.length > 1) {
            for (int i = 1; i < array.length; i++) {
                o += "\n" + cat(separator, array[i]);
            }
        }
        return o;
    }

    /*public static String cat(String separator, List array) {
     if (array == null || array.size() == 0 || array.get(0) == null) {
     return "";
     }
    
     String o = array.get(0).toString();
     if (array.size() > 1) {
     for (int i = 1; i < array.size(); i++) {
     o += (separator + (array.get(i) == null ? "" : array.get(i).toString()));
     }
     }
    
     return o;
     }*/
    public static void saveFile(File file, String content) {

        /*OutputStreamWriter osw = null;
         FileOutputStream fos = null;
         try {
         fos = new FileOutputStream(file, false);
         osw = new OutputStreamWriter(fos, CHARSET);
         osw.write(content);
         osw.flush();
         } catch (Exception e) {
         e.printStackTrace();
         } finally {
         try {
         fos.close();
         osw.close();
         } catch (Exception ee) {
         ee.printStackTrace();
         }
         }*/
        try {
            FileUtils.writeStringToFile(file, content);
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }

    public static void appendFile(File file, String content) {
        if (!file.isFile()) {
            System.err.println("" + file + " is not a regular file. Failed to append text.");
            return;
        }
        /*OutputStreamWriter osw = null;
         FileOutputStream fos = null;
         try {
         fos = new FileOutputStream(file, true);
         osw = new OutputStreamWriter(fos, CHARSET);
         osw.write(content);
         osw.flush();
         } catch (Exception e) {
         e.printStackTrace();
         } finally {
         try {
         fos.close();
         osw.close();
         } catch (Exception ee) {
         ee.printStackTrace();
         }

         }*/
        try {
            FileUtils.writeStringToFile(file, content, true);
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }

}
