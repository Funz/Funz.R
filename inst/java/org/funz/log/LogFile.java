package org.funz.log;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintStream;

/**
 * Implements a text logger.
 */
public class LogFile implements LogCollector {

    private File logfile;
    PrintStream log;

    @Override
    public void close() {
        log.close();
    }

    public PrintStream printStream() {
        return log;
    }

    public LogFile(String title) {
        logfile = new File(title);
        try {
            log = new PrintStream(new FileOutputStream(logfile, true));
        } catch (FileNotFoundException e) {
            e.printStackTrace(System.err);
            log = System.out;
        }
    }

    public LogFile(File logfile) {
        this.logfile = logfile;
        try {
            log = new PrintStream(new FileOutputStream(logfile, true));
        } catch (FileNotFoundException e) {
            e.printStackTrace(System.err);
            log = System.out;
        }
    }

    public void logException(boolean sync, Exception ex) {
        log.println("[EXCEPTION] " + ex.getMessage());
    }

    public void logMessage(SeverityLevel l, boolean sync, String message) {
        if (l == SeverityLevel.ERROR) {
            log.println("[ERROR]   " + message);
        } else if (l == SeverityLevel.WARNING) {
            log.println("[WARNING] " + message);
        } else if (l == SeverityLevel.PANIC) {
            log.println("[PANIC]   " + message);
        } else {
            log.println("          "+message);
        }
    }

    public void resetCollector(boolean sync) {
    }

    @Override
    public String toString() {
        return "LogFile " + logfile.getAbsolutePath();
    }
}
