package org.funz.log;

/**
 * Implements a text logger.
 */
public class LogNull implements LogCollector {

    @Override
    public void close() {
    }

    public LogNull(String title) {

    }

    public void logException(boolean sync, Exception ex) {
    }

    public void logMessage(SeverityLevel l, boolean sync, String message) {

    }

    public void resetCollector(boolean sync) {
    }

    @Override
    public String toString() {
        return "LogNull";
    }
}
