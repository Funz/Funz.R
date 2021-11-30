/*
 * Created on 1 juin 06 by richet
 */
package org.funz.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.io.FileUtils;
import static org.funz.util.ASCII.CHARSET;

public class ParserUtils {

    public static boolean ASCIIFilesAreIdentical(File a, File b) {
        if (!a.isFile() || !b.isFile()) {
            return false;
        }
        if (a.length() != b.length()) {
            return false;
        }
        BufferedReader areader = null;
        BufferedReader breader = null;
        boolean ret = true;
        try {
            areader = new BufferedReader(new InputStreamReader(new FileInputStream(a), CHARSET));
            breader = new BufferedReader(new InputStreamReader(new FileInputStream(b), CHARSET));
            String aline = null, bline = null;

            while ((aline = areader.readLine()) != null) {
                if ((bline = breader.readLine()) != null) {
                    if (!bline.equals(aline)) {
                        ret = false;
                        break;
                    }
                } else {
                    ret = false;
                    break;
                }
            }

            if (bline != null && breader.readLine() != null) {
                ret = false;
            }
        } catch (IOException e1) {
            e1.printStackTrace();
        } finally {
            try {
                areader.close();
                breader.close();
            } catch (Exception ee) {
                ee.printStackTrace();
            }
        }

        return ret;
    }
    
    
    public static boolean contains(File file, String str, boolean caseSensitive) {
        return contains(file, new String[]{str}, caseSensitive);
    }

    public static boolean contains(File file, String[] str, boolean caseSensitive) {
        return containsAnd(file, str, caseSensitive);
    }

    public static boolean containsAnd(File file, String[] str, boolean caseSensitive) {
        if (!file.isFile()) {
            return false;
        }
        BufferedReader in = null;
        InputStreamReader isr = null;
        FileInputStream fis = null;
        String tmp;

        boolean[] found = new boolean[str.length];
        for (int i = 0; i < found.length; i++) {
            found[i] = false;
        }

        try {
            fis = new FileInputStream(file);
            isr = new InputStreamReader(fis, CHARSET);
            in = new BufferedReader(isr);
            while ((tmp = in.readLine()) != null) {
                if (!caseSensitive) {
                    for (int i = 0; i < str.length; i++) {
                        if (tmp.toLowerCase().indexOf(str[i].toLowerCase()) >= 0) {
                            found[i] = true;
                            break;

                        }
                    }
                } else {
                    for (int i = 0; i < str.length; i++) {
                        if (tmp.indexOf(str[i]) >= 0) {
                            found[i] = true;
                            break;

                        }
                    }
                }
            }
        } catch (Exception e1) {
            e1.printStackTrace();
        } finally {
            try {
                fis.close();
                isr.close();
                in.close();
            } catch (Exception ee) {
                ee.printStackTrace();
            }

        }

        boolean b = true;
        for (int i = 0; i < found.length; i++) {
            b = b && found[i];
        }

        return b;
    }

    public static boolean containsOr(File file, String[] str, boolean caseSensitive) {
        if (!file.isFile()) {
            return false;
        }
        BufferedReader in = null;
        InputStreamReader isr = null;
        FileInputStream fis = null;
        String tmp;

        /*boolean[] found = new boolean[str.length];
         for (int i = 0; i < found.length; i++) {
         found[i] = false;
         }*/
        try {
            fis = new FileInputStream(file);
            isr = new InputStreamReader(fis, CHARSET);
            in = new BufferedReader(isr);
            while ((tmp = in.readLine()) != null) {
                if (!caseSensitive) {
                    for (int i = 0; i < str.length; i++) {
                        if (tmp.toLowerCase().indexOf(str[i].toLowerCase()) >= 0) {
                            return true;
                            //found[i] = true;
                            //break;
                        }
                    }
                } else {
                    for (int i = 0; i < str.length; i++) {
                        if (tmp.indexOf(str[i]) >= 0) {
                            return true;
                            //found[i] = true;
                            //break;
                        }
                    }
                }
            }
        } catch (Exception e1) {
            e1.printStackTrace();
        } finally {
            try {
                fis.close();
                isr.close();
                in.close();
            } catch (Exception ee) {
                ee.printStackTrace();
            }

        }

        return false;
        /*boolean b = true;
         for (int i = 0; i < found.length; i++) {
         b = b && found[i];
         }
         return b;*/
    }

    public static boolean contains(String str, String[] in) {
        return containsAnd(str, in);
    }

    public static boolean containsAnd(String str, String[] in) {
        return containsAnd(str, in, true);
    }

    public static boolean contains(String str, String[] in, boolean caseS) {
        return containsAnd(str, in, caseS);
    }

    public static boolean containsAnd(String str, String[] in, boolean caseS) {
        for (int i = 0; i < in.length; i++) {
            if (caseS) {
                if (str.indexOf(in[i]) < 0) {
                    return false;
                } else if (str.toLowerCase().indexOf(in[i].toLowerCase()) < 0) {
                    return false;
                }
            }
        }
        return true;
    }

    public static boolean containsOr(String str, String[] in, boolean caseS) {
        for (int i = 0; i < in.length; i++) {
            if (caseS) {
                if (str.indexOf(in[i]) >= 0) {
                    return true;
                } else if (str.toLowerCase().indexOf(in[i].toLowerCase()) >= 0) {
                    return true;
                }
            }
        }
        return false;
    }

    public static int countLines(File file, String str, boolean startsWith) {
        if (!file.isFile()) {
            return -1;
        }
        int ret = 0;
        BufferedReader reader = null;
        InputStreamReader isr = null;
        FileInputStream fis = null;
        try {
            fis = new FileInputStream(file);
            isr = new InputStreamReader(fis, CHARSET);
            reader = new BufferedReader(isr);
            String line;

            while ((line = reader.readLine()) != null) {
                if (startsWith) {
                    if (line.startsWith(str)) {
                        ret++;
                    }

                } else if (line.indexOf(str) >= 0) {
                    ret++;
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            ret = -1;
        } finally {
            try {
                fis.close();
                isr.close();
                reader.close();
            } catch (Exception ee) {
                ee.printStackTrace();
            }

        }

        return ret;
    }

/// calcule le nombre de lignes commencant par str
    public static int countLinesContaining(File file, String str) {
        return countLines(file, str, false);
    }

    public static int countLinesStarting(File file, String str) {
        return countLines(file, str, true);
    }

    public static int[] getPositionsContaining(String in, String grep) {
        ArrayList lines = new ArrayList();

        int p = 0;
        while ((p = in.indexOf(grep, p + grep.length())) > -1) {
            lines.add(new Integer(p));
        }

        int[] lines_num = new int[lines.size()];
        for (int i = 0; i < lines_num.length; i++) {
            lines_num[i] = ((Integer) (lines.get(i))).intValue();
        }

        return lines_num;
    }

    private static int[] getPositionsContaining(BufferedReader in, String grep) {
        ArrayList lines = new ArrayList();
        String tmp;

        try {
            int i = 0;
            int p = 0;
            while ((tmp = in.readLine()) != null) {
                if ((i = tmp.indexOf(grep)) > -1) {
                    lines.add(new Integer(i + p));
                }

                p += tmp.length() + 1;
            }

        } catch (IOException e) {
            e.printStackTrace();
        }

        int[] lines_num = new int[lines.size()];
        for (int i = 0; i < lines_num.length; i++) {
            lines_num[i] = ((Integer) (lines.get(i))).intValue();
        }

        return lines_num;
    }

    private static int getPositionOfLine(BufferedReader in, int l) {
        String tmp;

        try {
            int i = 0;
            int p = 0;
            while (i < l) {
                tmp = in.readLine();
                p += tmp.length() + 1;
                i++;
            }
            return p;
        } catch (IOException e) {
            e.printStackTrace();
        }

        return -1;
    }

    public static int[] getPositionsContaining(File file, String grep) {
        if (!file.isFile()) {
            return null;
        }
        BufferedReader in = null;
        InputStreamReader isr = null;
        FileInputStream fis = null;
        int[] ret = null;
        try {
            fis = new FileInputStream(file);
            isr = new InputStreamReader(fis, CHARSET);
            in = new BufferedReader(isr);
            ret = getPositionsContaining(in, grep);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            try {
                fis.close();
                isr.close();
                in.close();
            } catch (Exception ee) {
                ee.printStackTrace();
            }
        }
        return ret;
    }

    public static int getPositionOfLine(File file, int l) {
        if (!file.isFile()) {
            return -1;
        }
        BufferedReader in = null;
        InputStreamReader isr = null;
        FileInputStream fis = null;
        int ret = -1;
        try {
            fis = new FileInputStream(file);
            isr = new InputStreamReader(fis, CHARSET);
            in = new BufferedReader(isr);
            ret = getPositionOfLine(in, l);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            try {
                fis.close();
                isr.close();
                in.close();
            } catch (Exception ee) {
                ee.printStackTrace();
            }
        }
        return ret;
    }

    private static int[] getLineNumbersContaining(BufferedReader in, String grep) {
        ArrayList lines = new ArrayList();
        String tmp;

        try {
            int i = 0;
            while ((tmp = in.readLine()) != null) {
                if (tmp.indexOf(grep) > -1) {
                    lines.add(new Integer(i));
                }
                i++;
            }
        } catch (IOException e) {
            e.printStackTrace();
        } /*finally {
         try {
         in.close();
         } catch (Exception ee) {
         ee.printStackTrace();
         }
         }*/

        int[] lines_num = new int[lines.size()];
        for (int i = 0; i < lines_num.length; i++) {
            lines_num[i] = ((Integer) (lines.get(i))).intValue();
        }

        return lines_num;
    }

    public static int[] getLineNumbersContaining(File file, String grep) {
        if (!file.isFile()) {
            return null;
        }
        BufferedReader in = null;
        InputStreamReader isr = null;
        FileInputStream fis = null;
        int[] ret = null;
        try {
            fis = new FileInputStream(file);
            isr = new InputStreamReader(fis, CHARSET);
            in = new BufferedReader(isr);
            //in = new BufferedReader(new InputStreamReader(new FileInputStream(file), CHARSET));
            ret = getLineNumbersContaining(in, grep);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            try {
                fis.close();
                isr.close();
                in.close();
            } catch (Exception ee) {
                ee.printStackTrace();
            }

        }
        return ret;
    }

    private static String[] getAllLinesStarting(BufferedReader in, String start) {
        ArrayList lines = new ArrayList();
        String tmp;

        try {
            while ((tmp = in.readLine()) != null) {
                if (tmp.startsWith(start)) {
                    lines.add(tmp);
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        } /*finally {
         try {
         in.close();
         } catch (Exception ee) {
         ee.printStackTrace();
         }
         }*/


        String[] lines_string = new String[lines.size()];
        for (int i = 0; i < lines_string.length; i++) {
            lines_string[i] = (String) (lines.get(i));
        }

        return lines_string;
    }

    public static String[] getAllLinesStarting(File file, String start) {
        if (!file.isFile()) {
            return null;
        }
        BufferedReader in = null;
        InputStreamReader isr = null;
        FileInputStream fis = null;
        String[] ret = null;
        try {
            fis = new FileInputStream(file);
            isr = new InputStreamReader(fis, CHARSET);
            in = new BufferedReader(isr);
            //in = new BufferedReader(new InputStreamReader(new FileInputStream(file), CHARSET));
            ret = getAllLinesStarting(in, start);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            try {
                fis.close();
                isr.close();
                in.close();
            } catch (Exception ee) {
                ee.printStackTrace();
            }

        }
        return ret;
    }

    private static String[] getAllLinesContaining(BufferedReader in, String grep) {
        ArrayList lines = new ArrayList();
        String tmp;

        try {
            while ((tmp = in.readLine()) != null) {
                if (tmp.indexOf(grep) > -1) {
                    lines.add(tmp);
                }

            }
        } catch (IOException e) {
            e.printStackTrace();
        } /*finally {
         try {
         in.close();
         } catch (Exception ee) {
         ee.printStackTrace();
         }
         }*/


        String[] lines_string = new String[lines.size()];
        for (int i = 0; i < lines_string.length; i++) {
            lines_string[i] = (String) (lines.get(i));
        }

        return lines_string;
    }

    public static String[] getAllLinesContaining(File file, String grep) {
        if (!file.isFile()) {
            return null;
        }
        BufferedReader in = null;
        InputStreamReader isr = null;
        FileInputStream fis = null;
        String[] ret = null;
        try {
            fis = new FileInputStream(file);
            isr = new InputStreamReader(fis, CHARSET);
            in = new BufferedReader(isr);
            //in = new BufferedReader(new InputStreamReader(new FileInputStream(file), CHARSET));
            ret = getAllLinesContaining(in, grep);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            try {
                fis.close();
                isr.close();
                in.close();
            } catch (Exception ee) {
                ee.printStackTrace();
            }

        }
        return ret;
    }

    public static String getASCIIFileContent(File file) {

        /*if (!file.isFile()) {
         return null;
         }
         StringBuilder sb = new StringBuilder();
         BufferedReader reader = null;
         InputStreamReader isr = null;
         FileInputStream fis = null;
         String line;
            
         try {
         fis = new FileInputStream(file);
         isr = new InputStreamReader(fis, CHARSET);
         reader = new BufferedReader(isr);
         while ((line = reader.readLine()) != null) {
         sb.append(line);
         sb.append("\n");
         }
         } catch (Exception e) {
         e.printStackTrace();
         } finally {
         try {
         fis.close();
         isr.close();
         reader.close();
         } catch (Exception ee) {
         ee.printStackTrace();
         }

         }
         return sb.toString();*/
        try {
            return FileUtils.readFileToString(file);
        } catch (IOException ex) {
            ex.printStackTrace();
            return null;
        }
    }

    public static String[] getASCIIFileLines(File file) {
        if (!file.isFile()) {
            return null;
        }
        /*LinkedList linesVector = new LinkedList();
         FileReader fr = null;
         BufferedReader b = null;
         try {
         fr = new FileReader(file);
         b = new BufferedReader(fr);
         boolean eof = false;
         while (!eof) {
         String line = b.readLine();
         if (line == null) {
         eof = true;
         } else {
         linesVector.add(line);
         }

         }
         } catch (Exception e) {
         e.printStackTrace();
         } finally {
         try {
         b.close();
         fr.close();
         } catch (Exception ee) {
         ee.printStackTrace();
         }

         }

         String[] lines = new String[linesVector.size()];
         for (int i = 0; i < lines.length; i++) {
         lines[i] = (String) (linesVector.get(i));
         }

         return lines;*/
        try {
            List<String> lines = FileUtils.readLines(file);
            return lines.toArray(new String[lines.size()]);
        } catch (IOException ex) {
            ex.printStackTrace();
            return null;
        }
    }

    private static String getFirstLineContaining(BufferedReader in, String grep) {
        String line = "", tmp;

        try {
            while ((tmp = in.readLine()) != null) {
                if (tmp.indexOf(grep) > -1) {
                    line = tmp;
                    break;
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        } /*finally {
         try {
         in.close();
         } catch (Exception ee) {
         ee.printStackTrace();
         }
         }*/

        return line;
    }

    public static String getFirstLineContaining(File file, String grep) {
        if (!file.isFile()) {
            return null;
        }
        BufferedReader in = null;
        InputStreamReader isr = null;
        FileInputStream fis = null;
        String ret = null;
        try {
            fis = new FileInputStream(file);
            isr = new InputStreamReader(fis, CHARSET);
            in = new BufferedReader(isr);
            //in = new BufferedReader(new InputStreamReader(new FileInputStream(file), CHARSET));
            ret = getFirstLineContaining(in, grep);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            try {
                fis.close();
                isr.close();
                in.close();
            } catch (Exception ee) {
                ee.printStackTrace();
            }

        }
        return ret;
    }

    private static String getFirstLineStarting(BufferedReader in, String start) {
        String line = "", tmp;

        try {
            while ((tmp = in.readLine()) != null) {
                if (tmp.startsWith(start)) {
                    line = tmp;
                    break;
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        } /*finally {
         try {
         in.close();
         } catch (Exception ee) {
         ee.printStackTrace();
         }
         }*/


        return line;
    }

    public static String getFirstLineStarting(File file, String start) {
        if (!file.isFile()) {
            return null;
        }
        BufferedReader in = null;
        InputStreamReader isr = null;
        FileInputStream fis = null;
        String ret = null;
        try {
            fis = new FileInputStream(file);
            isr = new InputStreamReader(fis, CHARSET);
            in = new BufferedReader(isr);
            //in = new BufferedReader(new InputStreamReader(new FileInputStream(file), CHARSET));
            ret = getFirstLineStarting(in, start);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            try {
                fis.close();
                isr.close();
                in.close();
            } catch (Exception ee) {
                ee.printStackTrace();
            }

        }
        return ret;
    }

    private static String getLastFullLine(BufferedReader in) {
        String line = "", tmp, line_before = "?";

        try {
            while ((tmp = in.readLine()) != null) {
                //System.out.println("tmp " + tmp);
                //if (tmp.startsWith(start)) {
                if (line.length() > 0) {
                    line_before = line;
                }
                line = tmp;
                //System.out.println("line " + line);
                //}
            }
//System.out.println(" > " + line);

        } catch (IOException e) {
            //do nothing
            e.printStackTrace();
        } /*finally {
         try {
         in.close();
         } catch (Exception ee) {
         ee.printStackTrace();
         }
         }*/
//System.out.println(" => " + line_before);


        return line_before;
    }

    public static String getLastFullLine(File file) {
        if (!file.isFile()) {
            return null;
        }
        BufferedReader in = null;
        InputStreamReader isr = null;
        FileInputStream fis = null;
        String l = null;
        try {
            fis = new FileInputStream(file);
            isr = new InputStreamReader(fis, CHARSET);
            in = new BufferedReader(isr);
            //in = new BufferedReader(new InputStreamReader(new FileInputStream(file), CHARSET));
            l = getLastFullLine(in);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            try {
                fis.close();
                isr.close();
                in.close();
            } catch (Exception ee) {
                ee.printStackTrace();
            }

        }
        return l;
    }

    private static String getLastLine(BufferedReader in) {
        String line = null, tmp;
        try {
            while ((tmp = in.readLine()) != null) {
                line = tmp;
            }
//System.out.println(" > "+line);

        } catch (IOException e) {
            //do nothing
            e.printStackTrace();
        } /*finally {
         try {
         in.close();
         } catch (Exception ee) {
         ee.printStackTrace();
         }
         }*/
//System.out.println(" => "+line);


        return line;
    }

    public static String getLastLine(File file) {
        if (!file.isFile()) {
            return null;
        }
        BufferedReader in = null;
        InputStreamReader isr = null;
        FileInputStream fis = null;
        String l = null;
        try {
            fis = new FileInputStream(file);
            isr = new InputStreamReader(fis, CHARSET);
            in = new BufferedReader(isr);
            //in = new BufferedReader(new InputStreamReader(new FileInputStream(file), CHARSET));
            l = getLastLine(in);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            try {
                fis.close();
                isr.close();
                in.close();
            } catch (Exception ee) {
                ee.printStackTrace();
            }

        }
        return l;
    }

    private static String getLastLineContaining(BufferedReader in, String grep) {
        String line = "", tmp;

        try {
            while ((tmp = in.readLine()) != null) {
                //System.out.println("tmp " + tmp);
                if (tmp.indexOf(grep) > -1) {
                    line = tmp;
                    //System.out.println("line " + line);
                }
            }
            //System.out.println(" > "+line);
        } catch (IOException e) {
            e.printStackTrace();
            //do nothing
        } /*finally {
         try {
         in.close();
         } catch (Exception ee) {
         ee.printStackTrace();
         }
         }*/
//System.out.println(" => "+line);


        return line;
    }

    public static String getLastLineContaining(File file, String grep) {
        if (!file.isFile()) {
            return null;
        }
        BufferedReader in = null;
        InputStreamReader isr = null;
        FileInputStream fis = null;
        String ret = null;
        try {
            fis = new FileInputStream(file);
            isr = new InputStreamReader(fis, CHARSET);
            in = new BufferedReader(isr);
            //in = new BufferedReader(new InputStreamReader(new FileInputStream(file), CHARSET));
            ret = getLastLineContaining(in, grep);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            try {
                fis.close();
                isr.close();
                in.close();
            } catch (Exception ee) {
                ee.printStackTrace();
            }

        }
        return ret;
    }

    private static String getLastLineStarting(BufferedReader in, String start) {
        String line = "", tmp;

        try {
            while ((tmp = in.readLine()) != null) {
                //System.out.println("tmp " + tmp);
                if (tmp.startsWith(start)) {
                    line = tmp;
                    //System.out.println("line " + line);
                }

            }
            //System.out.println(" > "+line);
        } catch (IOException e) {
            e.printStackTrace();
            //do nothing
        } /*finally {
         try {
         in.close();
         } catch (Exception ee) {
         ee.printStackTrace();
         }
         }*/
//System.out.println(" => "+line);


        return line;
    }

    public static String getLastLineStarting(File file, String start) {
        if (!file.isFile()) {
            return null;
        }
        BufferedReader in = null;
        InputStreamReader isr = null;
        FileInputStream fis = null;
        String ret = null;
        try {
            fis = new FileInputStream(file);
            isr = new InputStreamReader(fis, CHARSET);
            in = new BufferedReader(isr);
            //in = new BufferedReader(new InputStreamReader(new FileInputStream(file), CHARSET));
            ret = getLastLineStarting(in, start);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            try {
                fis.close();
                isr.close();
                in.close();
            } catch (Exception ee) {
                ee.printStackTrace();
            }

        }
        return ret;
    }

    private static String[] getMultipleNextLineContaining(BufferedReader in, String first_line_content, String next_line_content) {
        ArrayList lines = new ArrayList();
        String tmp;

        try {
            while ((tmp = in.readLine()) != null) {
                if (tmp.indexOf(first_line_content) > -1) {
                    while ((tmp = in.readLine()) != null) {
                        if (tmp.indexOf(next_line_content) > -1) {
                            lines.add(tmp);
                            break;
                        }
                    }
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        } /*finally {
         try {
         in.close();
         } catch (Exception ee) {
         ee.printStackTrace();
         }
        
         }*/

        String[] lines_string = new String[lines.size()];
        for (int i = 0; i
                < lines_string.length; i++) {
            lines_string[i] = (String) (lines.get(i));
        }

        return lines_string;
    }

    public static String[] getMultipleNextLineContaining(File file, String first_line_content, String next_line_content) {
        if (!file.isFile()) {
            return null;
        }
        BufferedReader in = null;
        InputStreamReader isr = null;
        FileInputStream fis = null;
        String[] ret = null;
        try {
            fis = new FileInputStream(file);
            isr = new InputStreamReader(fis, CHARSET);
            in = new BufferedReader(isr);
            //in = new BufferedReader(new InputStreamReader(new FileInputStream(file), CHARSET));
            ret = getMultipleNextLineContaining(in, first_line_content, next_line_content);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            try {
                fis.close();
                isr.close();
                in.close();
            } catch (Exception ee) {
                ee.printStackTrace();
            }

        }
        return ret;
    }

    private static String[] getNextLinesContaining(BufferedReader in, String first_line_content, String next_line_content) {
        ArrayList lines = new ArrayList();
        String tmp;

        try {
            while ((tmp = in.readLine()) != null) {
                if (tmp.indexOf(first_line_content) > -1) {
                    while ((tmp = in.readLine()) != null) {
                        if (tmp.indexOf(next_line_content) > -1) {
                            lines.add(tmp);
                        }
                    }
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        } /*finally {
         try {
         in.close();
         } catch (Exception ee) {
         ee.printStackTrace();
         }
        
         }*/


        String[] lines_string = new String[lines.size()];
        for (int i = 0; i < lines_string.length; i++) {
            lines_string[i] = (String) (lines.get(i));
        }

        return lines_string;
    }

    public static String[] getNextLinesContaining(File file, String first_line_content, String next_line_content) {
        if (!file.isFile()) {
            return null;
        }
        BufferedReader in = null;
        InputStreamReader isr = null;
        FileInputStream fis = null;
        String[] ret = null;
        try {
            fis = new FileInputStream(file);
            isr = new InputStreamReader(fis, CHARSET);
            in = new BufferedReader(isr);
            //in = new BufferedReader(new InputStreamReader(new FileInputStream(file), CHARSET));
            ret = getNextLinesContaining(in, first_line_content, next_line_content);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            try {
                fis.close();
                isr.close();
                in.close();
            } catch (Exception ee) {
                ee.printStackTrace();
            }

        }
        return ret;
    }

    private static String[] getNextLinesStarting(BufferedReader in, String first_line_content, String next_line_content) {
        ArrayList lines = new ArrayList();
        String tmp;

        try {
            while ((tmp = in.readLine()) != null) {
                if (tmp.startsWith(first_line_content)) {
                    while ((tmp = in.readLine()) != null) {
                        if (tmp.startsWith(next_line_content)) {
                            lines.add(tmp);
                        }
                    }
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        } /*finally {
         try {
         in.close();
         } catch (Exception ee) {
         ee.printStackTrace();
         }
        
         }*/


        String[] lines_string = new String[lines.size()];
        for (int i = 0; i < lines_string.length; i++) {
            lines_string[i] = (String) (lines.get(i));
        }

        return lines_string;
    }

    public static String[] getNextLinesStarting(File file, String first_line_content, String next_line_content) {
        if (!file.isFile()) {
            return null;
        }
        BufferedReader in = null;
        InputStreamReader isr = null;
        FileInputStream fis = null;
        String[] ret = null;
        try {
            fis = new FileInputStream(file);
            isr = new InputStreamReader(fis, CHARSET);
            in = new BufferedReader(isr);
            //in = new BufferedReader(new InputStreamReader(new FileInputStream(file), CHARSET));
            ret = getNextLinesStarting(in, first_line_content, next_line_content);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            try {
                fis.close();
                isr.close();
                in.close();
            } catch (Exception ee) {
                ee.printStackTrace();
            }

        }
        return ret;
    }

    private static String getNextLineContaining(BufferedReader in, String first_line_content, String next_line_content) {
        return getNextLineContaining(in, new String[]{first_line_content}, new String[]{next_line_content});
    }

    private static String getNextLineContaining(BufferedReader in, String[] first_line_content, String[] next_line_content) {
        String line = "", tmp;

        try {
            while ((tmp = in.readLine()) != null) {
                if (contains(tmp, first_line_content)) {
                    while ((tmp = in.readLine()) != null) {
                        if (contains(tmp, next_line_content)) {
                            line = tmp;
                            return line;
                        }

                    }
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        } /*finally {
         try {
         in.close();
         } catch (Exception ee) {
         ee.printStackTrace();
         }
        
         }*/


        return line;
    }

    public static String getNextLineContaining(File file, String first_line_content, String next_line_content) {
        if (!file.isFile()) {
            return null;
        }
        BufferedReader in = null;
        InputStreamReader isr = null;
        FileInputStream fis = null;
        try {
            fis = new FileInputStream(file);
            isr = new InputStreamReader(fis, CHARSET);
            in = new BufferedReader(isr);
            //in = new BufferedReader(new InputStreamReader(new FileInputStream(file), CHARSET));
            return getNextLineContaining(in, first_line_content, next_line_content);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            try {
                fis.close();
                isr.close();
                in.close();
            } catch (Exception ee) {
                ee.printStackTrace();
            }

        }
        return null;
    }

    public static String getNextLineContaining(File file, String[] first_line_content, String[] next_line_content) {
        if (!file.isFile()) {
            return null;
        }
        BufferedReader in = null;
        InputStreamReader isr = null;
        FileInputStream fis = null;
        try {
            fis = new FileInputStream(file);
            isr = new InputStreamReader(fis, CHARSET);
            in = new BufferedReader(isr);
            //in = new BufferedReader(new InputStreamReader(new FileInputStream(file), CHARSET));
            return getNextLineContaining(in, first_line_content, next_line_content);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            try {
                fis.close();
                isr.close();
                in.close();
            } catch (Exception ee) {
                ee.printStackTrace();
            }

        }
        return null;
    }

    private static String getNextLineStarting(BufferedReader in, String first_line_start, String next_line_start) {
        String line = "", tmp;

        try {
            while ((tmp = in.readLine()) != null) {
                if (tmp.startsWith(first_line_start)) {
                    while ((tmp = in.readLine()) != null) {
                        if (tmp.startsWith(next_line_start)) {
                            line = tmp;
                            return line;
                        }

                    }
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        } /*finally {
         try {
         in.close();
         } catch (Exception ee) {
         ee.printStackTrace();
         }
        
         }*/


        return line;
    }

    public static String getNextLineStarting(File file, String first_line_start, String next_line_start) {
        if (!file.isFile()) {
            return null;
        }
        BufferedReader in = null;
        InputStreamReader isr = null;
        FileInputStream fis = null;
        try {
            fis = new FileInputStream(file);
            isr = new InputStreamReader(fis, CHARSET);
            in = new BufferedReader(isr);
            //in = new BufferedReader(new InputStreamReader(new FileInputStream(file), CHARSET));
            return getNextLineStarting(in, first_line_start, next_line_start);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            try {
                fis.close();
                isr.close();
                in.close();
            } catch (Exception ee) {
                ee.printStackTrace();
            }

        }
        return null;
    }

    public static boolean startsWith(File file, String str, boolean caseSensitive) {
        if (!file.isFile()) {
            return false;
        }
        BufferedReader in = null;
        InputStreamReader isr = null;
        FileInputStream fis = null;
        String tmp;

        try {
            fis = new FileInputStream(file);
            isr = new InputStreamReader(fis, CHARSET);
            in = new BufferedReader(isr);
            //in = new BufferedReader(new InputStreamReader(new FileInputStream(file), CHARSET));
            tmp = in.readLine();
            //System.out.println("tmp=" + tmp + " ?= " + str);
            if (!caseSensitive) {
                if (tmp.toLowerCase().startsWith(str.toLowerCase())) {
                    return true;
                }

            } else {
                if (tmp.startsWith(str)) {
                    return true;
                }

            }
        } catch (Exception e1) {
            e1.printStackTrace();
        } finally {
            try {
                fis.close();
                isr.close();
                in.close();
            } catch (Exception ee) {
                ee.printStackTrace();
            }

        }
        return false;
    }
}
