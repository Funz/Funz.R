package org.funz;

/**
 * Network protocol headers and constants
 */
public interface Protocol {

    public static final String METHOD_RESERVE = "RESERVE",
            METHOD_UNRESERVE = "UNRESERVE",
            METHOD_GET_CODES = "GETCODES",
            METHOD_NEW_CASE = "NEWCASE",
            METHOD_EXECUTE = "EXECUTE",
            METHOD_INTERRUPT = "INTERUPT",
            METHOD_PUT_FILE = "PUTFILE",
            METHOD_ARCH_RES = "ARCHIVE",
            METHOD_GET_ARCH = "GETFILE",
            METHOD_KILL = "KILL",
            METHOD_GET_INFO = "GETINFO",
            METHOD_GET_ACTIVITY = "GETACTIVITY",
            RET_YES = "Y",
            RET_ERROR = "E",
            RET_NO = "N",
            RET_SYNC = "S",
            RET_INFO = "I",
            RET_FILE = "F",
            RET_HEARTBEAT = "H",
            END_OF_REQ = "/",
            ARCHIVE_FILE = "results.zip",
            ARCHIVE_FILTER = "ARCHIVEFILTER",
            UNAVAILABLE_STATE = "unavailable",
            ALREADY_RESERVED = "already reserved",
            IDLE_STATE = "idle",
            PRIVATE_KEY = "to be or not to be?";
    public static final int SOCKET_BUFFER_SIZE = 1024;
    public static int PING_PERIOD = 5000;
}
