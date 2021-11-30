package org.funz.util;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Calendar;
import java.util.LinkedList;
import java.util.List;
import org.apache.commons.io.FileUtils;
import org.funz.Protocol;
import java.nio.charset.Charset;

public class Disk {

    public static interface CopyProgressObserver {

        public void copyingFile(File file);
    }

    public static interface ProgressObserver {

        public void newDataBlock(int size);

        public void setTotalSize(long total);
    }
    /*public static final String CHARSET;
    
     static {
     String charset = System.getProperty("charset");
     CHARSET = (charset != null && charset.length() > 0) ? charset : "ISO-8859-5";
     }*/

    public static File[] listRecursiveFiles(File root) {
        LinkedList list = new LinkedList();
        listRecursiveFiles(root, list);
        File[] array = new File[list.size()];
        for (int i = 0; i < array.length; i++) {
            array[i] = (File) list.get(i);
        }
        return array;
    }

    public static void listRecursiveFiles(File root, List l) {
        if (root==null || !root.exists()) return;
        File[] filesndirs = root.listFiles();
        for (int i = 0; i < filesndirs.length; i++) {
            if (filesndirs[i].isFile()) {
                l.add(filesndirs[i]);
            } else {
                listRecursiveFiles(filesndirs[i], l);
            }
        }
    }

    public static void copyDir(File src, File trg) throws IOException {
        copyDir(src, trg, null, null);
    }

    public static void copyDir(File src, File trg, CopyProgressObserver observer, FileFilter filter) throws IOException {
        if (observer != null) {
            observer.copyingFile(src);
        }
        if (filter != null) {
            org.apache.commons.io.FileUtils.copyDirectory(src, trg, filter, true);
        } else {
            org.apache.commons.io.FileUtils.copyDirectory(src, trg, true);
        }
    }

    protected static boolean isStringChar(char ch) {
        if (Character.isWhitespace(ch)) {
            return true;
        }
        if (Character.isLetterOrDigit(ch)) {
            return true;
        }
        if (Character.isSpaceChar(ch)) {
            return true;
        }
        switch (ch) {
            case ' ':
            case '\t':
            case '\r':
            case '\n':
            case '/':
            case '+':
            case '*':
            case '-':
            case ':':
            case ';':
            case '.':
            case ',':
            case '_':
            case '`':
            case '!':
            case '?':
            case '&':
            case '|':
            case '@':
            case '#':
            case '\'':
            case '~':
            case '$':
            case '%':
            case '(':
            case ')':
            case '{':
            case '}':
            case '[':
            case ']':
            case '<':
            case '>':
            case '^':
            case '=':
            case '"':
            case '\\':
                return true;
        }
        //System.out.println(""+ch+"");
        return false;
    }

    public static boolean isBinary(File f) {
        for (Charset cs: new Charset[]{Charset.forName("UTF-8"),Charset.forName("ISO-8859-1"),Charset.forName("ISO-8859-15")}) {
            // Consider as binary if no charset get it as ASCII...
            if (!isBinary(f, cs)) return false;
        }
        return true;
    }

    public static boolean isBinary(File f, Charset inputCharset) {
        boolean isbin = false;
        java.io.InputStream in = null;

        try {
            in = new FileInputStream(f);
            BufferedReader r = new BufferedReader(new InputStreamReader(in, inputCharset));

            int sample = (int) Math.min(1024, f.length());

            char[] cc = new char[sample]; //do a peek
            r.read(cc, 0, sample);

            double prob_bin = 0;

//            int word_size = 10; // will consider word as binary if >50% chars are binary
//            for (int i = 0; i < cc.length-word_size; i=i+word_size) {
//                double bins = 0;
//                for (int j=0; j<word_size; j++) {
//                    if (!isStringChar((char) cc[i+j])) bins++;
//                }
//                if (bins/word_size > 0.5) {
//                    String b = ">";
//                    for (int j=0; j<word_size; j++) b=b+" "+(int)cc[i+j];
//                    System.err.println(b+"<");
//                }
//            }
            for (int i = 0; i < cc.length; i++) {
                char j = (char) cc[i];
                if (!isStringChar(j)) {
                    prob_bin++;
                }
            }

            double pb = prob_bin / sample;
            if (pb > 0.05) {// System.out.println("probably binary at "+pb);
                isbin = true;
            }

            in.close();
        } catch (Exception ee) {
            isbin = false; //error - likely broken link - so return false
        }

        try {
            in.close();
        } catch (Exception E) {
        }

        //System.err.println("File " + f + " is " + (isbin ? "binary" : "ASCII"));
        return isbin;
    }

    public static boolean isLink(File file) {
        try {
            if (!file.exists()) {
                return true;

            } else {
                String cnnpath = file.getCanonicalPath();
                cnnpath = cnnpath.substring(cnnpath.lastIndexOf(File.separator));

                String abspath = file.getAbsolutePath();
                abspath = abspath.substring(abspath.lastIndexOf(File.separator));

                boolean equals = abspath.equals(cnnpath);
                //if (!equals) {
                //    System.out.println("    AbsolutePath= " + abspath + "\n != CanonicalPath= " + cnnpath);
                //}
                return !equals;
            }
        } catch (IOException ex) {
            ex.printStackTrace();
            try {
                return FileUtils.isSymlink(file);
            } catch (IOException ex1) {
                ex1.printStackTrace();
                return true;
            }
        }
    }

    public static void main(String[] args) throws IOException {
        final File in = new File("bigfile");

        System.err.println("Starting ");
        long tic = Calendar.getInstance().getTimeInMillis();

        new Thread(new Runnable() {
            public void run() {
                try {
                    final Socket server = new ServerSocket(23456).accept();
                    System.err.println("Created server");
                    server.setTcpNoDelay(true);
                    server.setTrafficClass(0x04);
                    //server.setReceiveBufferSize(Protocol.SOCKET_BUFFER_SIZE);
                    //server.setSendBufferSize(SOCKET_BUFFER_SIZE);

                    final DataInputStream _dis = new DataInputStream(new BufferedInputStream(server.getInputStream()));

                    Disk.deserializeFile(_dis, "/tmp/file", in.length(), new ProgressObserver() {

                        public void newDataBlock(int size) {
                            System.err.println("< " + size);
                        }

                        public void setTotalSize(long total) {
                            System.err.println("deserializeFile setTotalSize " + total);
                        }
                    });

                    System.err.println("Finished deserializeFile");
                    _dis.close();

                    if (Digest.matches(Digest.getSum(in), Digest.getSum(new File("/tmp/file")))) {
                        System.err.println("File is OK.");
                    } else {
                        System.err.println("!!! File is CORRUPTED ");
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }).start();

        final Socket client = new Socket((String) null, 23456);
        System.err.println("Created client");
        client.setTcpNoDelay(true);
        client.setTrafficClass(0x04);
        //client.setSendBufferSize(Protocol.SOCKET_BUFFER_SIZE);
        //client.setReceiveBufferSize(SOCKET_BUFFER_SIZE);

        final DataOutputStream _dos = new DataOutputStream(new BufferedOutputStream(client.getOutputStream()));

        Disk.serializeFile(_dos, in, in.length(), new ProgressObserver() {

            public void newDataBlock(int size) {
                System.err.println("> " + size);
            }

            public void setTotalSize(long total) {
                System.err.println("serializeFile setTotalSize " + total);
            }
        });

        System.err.println("Finished serializeFile");
        _dos.close();

        long toc = Calendar.getInstance().getTimeInMillis();
        System.err.println("Achieved in " + (toc - tic) / 1000.0 + " s.");

        /*File src_main_java = new File("src/main/java");
         File[] lrs = listRecursiveFiles(src_main_java);
         for (int i = 0; i < lrs.length; i++) {
         System.out.println(lrs[i]);
         }*/

        /*File test = new File("test");
         System.out.println(getHumanFileSizeString(getDirSize(test)));
         File[] files = test.listFiles();
         for (int i = 0; i < files.length; i++) {
         File f = files[i];
         if (isLink(f)) {
         System.err.println(f + " is a link:");
         System.err.println("     " + f.getCanonicalPath());
         System.err.println("  != " + f.getAbsolutePath());
         }
         }*/

        /*File dir = new File("test/dirtoemptytest");
         System.out.println("empty " + dir + ": " + emptyDir(dir));
        
         File dir2 = new File("/tmp/sec-11/4657/spool");
         System.out.println("empty " + dir2 + ": " + emptyDir(dir2));*/
    }

    public static void copyFile(File in, File out) throws IOException {
        org.apache.commons.io.FileUtils.copyFile(in, out);
    }

    public static void moveDir(File src, File trg) throws IOException {
        moveDir(src, trg, null);
    }

    public static void moveDir(File src, File trg, CopyProgressObserver observer) throws IOException {
        if (observer != null) {
            observer.copyingFile(src);
        }
        if (trg.exists()) {
            for (File f : src.listFiles()) {
                if (f.isFile())
                org.apache.commons.io.FileUtils.moveToDirectory(f, trg, trg.isDirectory());
                else moveDir(f,new File(trg,f.getName()),observer);
            }
        } else {
            org.apache.commons.io.FileUtils.moveDirectory(src, trg);
        }
    }

    public static void moveFile(File in, File out) throws IOException {
        org.apache.commons.io.FileUtils.moveFile(in, out);
    }

    public static void copyFilesIn(File[] src, File trg) throws IOException {
        for (int i = 0; i < src.length; i++) {
            org.apache.commons.io.FileUtils.copyFileToDirectory(src[i], trg);
        }
    }

    public static void deserializeEncryptedFile(InputStream is, String file, long size, byte key[], ProgressObserver observer) {
        long bufSize = Math.min(Protocol.SOCKET_BUFFER_SIZE, size);
        byte buf[] = new byte[(int) bufSize];
        FileOutputStream os = null;
        //BufferedInputStream bis = null;

        try {
            os = new FileOutputStream(file);

            //bis = new BufferedInputStream(is, (int) bufSize);
            long read = 0L;
            int pos = 0;
            if (observer != null) {
                observer.setTotalSize(size);
            }
            for (long toRead = size; toRead > 0L;) {
                read = Math.min(toRead, bufSize);
                read = /*b*/ is.read(buf, 0, (int) read);
                if (read == -1L) {
                    throw new IOException("broken pipe");
                }
                if (observer != null) {
                    observer.newDataBlock((int) read);
                }
                pos = xor(buf, key, (int) read, pos);
                os.write(buf, 0, (int) read);
                os.flush();
                toRead -= read;
            }
        } catch (IOException ioe) {
            ioe.printStackTrace();
        } finally {
            try {
                os.close();
            } catch (Exception ee) {
                ee.printStackTrace();
            }
        }
        //return bis;

    }

    public static void deserializeFile(DataInputStream is, String file, long size, ProgressObserver observer) {
        long bufSize = Math.min(Protocol.SOCKET_BUFFER_SIZE, size);
        byte buf[] = new byte[(int) bufSize];
        FileOutputStream os = null;
        //BufferedInputStream bis = null;
        try {
            os = new FileOutputStream(file);
            //bis = new BufferedInputStream(is, (int) bufSize);

            long read = 0L;
            if (observer != null) {
                observer.setTotalSize(size);
            }
            for (long toRead = size; toRead > 0L;) {
                read = Math.min(toRead, bufSize);
                read = /*b*/ is.read(buf, 0, (int) read);
                if (read == -1L) {
                    throw new IOException("broken pipe");
                }

                if (observer != null) {
                    observer.newDataBlock((int) read);
                }

                os.write(buf, 0, (int) read);
                os.flush();
                toRead -= read;
            }
            os.flush();
        } catch (IOException ioe) {
            ioe.printStackTrace();
        } finally {
            try {
                os.close();
            } catch (Exception ee) {
                ee.printStackTrace();
            }
        }
        //return bis;
    }

    public static void emptyDir(File dir) throws IOException {
        org.apache.commons.io.FileUtils.cleanDirectory(dir);
    }

    public static long getDirSize(File dir) {
        return org.apache.commons.io.FileUtils.sizeOfDirectory(dir);
    }

    public static String getHumanFileSizeString(long size) {
        return org.apache.commons.io.FileUtils.byteCountToDisplaySize(size);
    }

    public static boolean setWritable(File dir, boolean w) throws IOException {
        if (dir == null || !dir.exists()) {
            return false;
        }
        boolean success = true;
        if (dir.isDirectory()) {
            File[] files = dir.listFiles();
            if (files != null) {
                for (int i = 0; i < files.length; i++) {
                    success = success & setWritable(files[i], w);
                }
            }
        } else {
            success = success & dir.setWritable(w);
        }
        return success;
    }

    public static boolean setReadable(File dir, boolean r) throws IOException {
        if (dir == null || !dir.exists()) {
            return false;
        }
        boolean success = true;
        if (dir.isDirectory()) {
            File[] files = dir.listFiles();
            if (files != null) {
                for (int i = 0; i < files.length; i++) {
                    success = success & setReadable(files[i], r);
                }
            }
        } else {
            success = success & dir.setReadable(r);
        }
        return success;
    }

    public static boolean setExecutable(File dir, boolean x) throws IOException {
        if (dir == null || !dir.exists()) {
            return false;
        }
        boolean success = true;
        if (dir.isDirectory()) {
            File[] files = dir.listFiles();
            if (files != null) {
                for (int i = 0; i < files.length; i++) {
                    success = success & setExecutable(files[i], x);
                }
            }
        } else {
            success = success & dir.setExecutable(x);
        }
        return success;
    }

    public static void removeDir(File dir) throws IOException {
        org.apache.commons.io.FileUtils.deleteDirectory(dir);
    }

    public static int removeEmptyDirs(File dir, int depth) throws IOException {
        int n = 0;
        File dirs[] = dir.listFiles();
        if (dirs != null) {
            for (int i = 0; i < dirs.length; i++) {
                if (dirs[i].isDirectory() && depth > 0) {
                    n = Math.max(n, removeEmptyDirs(dirs[i], depth - 1));
                } else {
                    n++;
                    break;
                }
            }
        }
        if (n == 0) {
            removeDir(dir);
            return 0;
        }

        return n;
    }

    public static void serializeEncryptedFile(DataOutputStream os, File file, long size, byte key[], ProgressObserver observer) {
        long bufSize = Math.min(Protocol.SOCKET_BUFFER_SIZE, size);
        byte buf[] = new byte[(int) bufSize];
        FileInputStream is = null;
        try {
            is = new FileInputStream(file);
            long sent = 0L;
            int pos = 0;
            if (observer != null) {
                observer.setTotalSize(size);
            }
            for (long toSend = size; toSend > 0L; toSend -= sent) {
                sent = Math.min(toSend, bufSize);
                sent = is.read(buf, 0, (int) sent);
                if (sent == -1) {
                    os.flush();
                    break;
                }
                if (observer != null) {
                    observer.newDataBlock((int) sent);
                }
                pos = xor(buf, key, (int) sent, pos);
                os.write(buf, 0, (int) sent);
                os.flush();
            }
        } catch (IOException ioe) {
            ioe.printStackTrace();
        } finally {
            try {
                is.close();
            } catch (Exception ee) {
                ee.printStackTrace();
            }
        }
    }

    public static void serializeFile(DataOutputStream os, File file, long size) {
        serializeFile(os, file, size, null);
    }

    public static void serializeFile(DataOutputStream os, File file, long size, ProgressObserver observer) {
        long bufSize = Math.min(Protocol.SOCKET_BUFFER_SIZE, size);
        byte buf[] = new byte[(int) bufSize];
        FileInputStream is = null;
        try {
            is = new FileInputStream(file);
            long sent = 0L;
            if (observer != null) {
                observer.setTotalSize(size);
            }
            for (long toSend = size; toSend > 0L; toSend -= sent) {
                sent = Math.min(toSend, bufSize);
                sent = is.read(buf, 0, (int) sent);
                if (sent == -1) {
                    os.flush();
                    break;
                }
                if (observer != null) {
                    observer.newDataBlock((int) sent);
                }
                os.write(buf, 0, (int) sent);
                os.flush();
            }
        } catch (IOException ioe) {
            ioe.printStackTrace();
        } finally {
            try {
                is.close();
            } catch (Exception ee) {
                ee.printStackTrace();
            }
        }
    }

    private static int xor(byte buf[], byte key[], int len, int pos) {
        for (int i = 0; i < len; i++) {
            buf[i] = (byte) (buf[i] ^ key[pos]);
            pos = (pos + 1) % key.length;
        }
        return pos;
    }
}
