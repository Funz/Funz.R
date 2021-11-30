package org.funz.log;

import java.io.PrintWriter;
import java.io.StringWriter;
import org.funz.log.LogCollector.SeverityLevel;

/**
 * Implements InfoCollector as text console.
 */
public class LogConsole implements LogCollector {

    public void logException(boolean sync, final Exception ex) {
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        ex.printStackTrace(pw);
        logMessage(SeverityLevel.PANIC, sync, sw.toString());
        pw.close();
    }
    String lastmessage = "";
    int repeated = 0;

    public void logMessage(final SeverityLevel severity, boolean sync, final String message) {
        if (message.equals(lastmessage) && repeated < 100) {
            repeated++;
            return;
        } else {
            if (repeated > 0) {
                _logMessage(severity, sync, "    Repeated " + repeated + " times.");
                repeated = 0;
                lastmessage = message;
                _logMessage(severity, sync, message);
            } else {
                lastmessage = message;
                _logMessage(severity, sync, message);
            }
        }
    }

    public void _logMessage(final SeverityLevel severity, boolean sync, final String message) {
        //System.out.println(message);
        if (message == null) {
            return;
        }
        if (severity == SeverityLevel.ERROR) {
            System.out.println("[ERROR]   " + message);
        } else if (severity == SeverityLevel.WARNING) {
            System.out.println("[WARNING] " + message);
        } else if (severity == SeverityLevel.PANIC) {
            System.out.println("[PANIC]   " + message);
        } else {
            System.out.println("          " + message);
        }
    }

    public void resetCollector(boolean sync) {
        System.out.println("[RESET]  ");
    }

    public void close() {
        System.out.println("[CLOSE]  ");
    }

    @Override
    public String toString() {
        return "LogConsole";
    }
}
