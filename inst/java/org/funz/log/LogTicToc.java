package org.funz.log;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import org.apache.commons.lang.StringUtils;

/**
 *
 * @author richet
 */
public class LogTicToc {

    public static final String HOUR_FORMAT = "HH:mm", SEC_FORMAT = ":ss", DAY_FORMAT = "yyyy/MM/dd";

    static Map<String, Long> tic = new HashMap<String, Long>();

    private static LogCollector console = new LogConsole();

    public static void tic(String key) {
        tic.put(key, Calendar.getInstance().getTimeInMillis());
    }

    public static void toc(String key, LogCollector log) {
        try {
            double toc = Calendar.getInstance().getTimeInMillis();
            double dt = toc - tic.get(key);
            String mess = (dt<10 ? 
            StringUtils.rightPad(key + ":", 100) + "time elapsed= " + ((toc - tic.get(key)) )+ " ms" : 
            StringUtils.rightPad(key + ":", 100) + "time elapsed= " + ((toc - tic.get(key)) / 1000.0 )+ " s");
            if (dt < 10)
            if (log != null) {
                log.logMessage(LogCollector.SeverityLevel.INFO, true, mess);
            } else {
                System.err.println(mess);
            }
            tic.remove(key);
        } catch (Exception e) {
            System.err.println("[PANIC] Cannot toc " + key + " on " + log);
            e.printStackTrace();
        }
    }

    public static void toc(String key) {
        toc(key, console);
    }

    static long T0 = Calendar.getInstance().getTimeInMillis();

    public static long T() {
        return Calendar.getInstance().getTimeInMillis() - T0;
    }
    static long T1 = Calendar.getInstance().getTimeInMillis();

    public static long DT() {
        long t = Calendar.getInstance().getTimeInMillis();
        long dt = t - T1;
        T1 = t;
        return dt;
    }

    public static String HMS() {
        Date date = new Date();
        SimpleDateFormat sdf = new SimpleDateFormat(DAY_FORMAT);
        sdf.applyPattern(HOUR_FORMAT + SEC_FORMAT);
        return sdf.format(date);
    }

}
