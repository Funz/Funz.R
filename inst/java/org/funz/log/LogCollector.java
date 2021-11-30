package org.funz.log;

public interface LogCollector {
    /// Severity level
    public enum SeverityLevel {
        /// error
        ERROR,
      
        /// simple infomation
        INFO,
       
        /// critical message or exception
        PANIC,
      
        /// warning or important information
        WARNING
    }
    
    /** Logs or shows an exception.
     * @param sync says whether the message comes from the GUI thread or not
     * @param ex exception to log
     */
    public void logException(boolean sync, final Exception ex);

    /** Logs or shows a message.
     * @param severity severity level
     * @param sync says whether the message comes from the GUI thread or not
     * @param message message
     */
    public void logMessage(final SeverityLevel severity, boolean sync, final String message);

    /** Resets the collector.
     * @param sync says whether the message comes from the GUI thread or not
     */
    public void resetCollector(boolean sync);

    public void close();
    
}
