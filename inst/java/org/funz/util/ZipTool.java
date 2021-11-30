package org.funz.util;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Calendar;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;
import org.funz.log.LogCollector;
import static org.funz.log.LogCollector.SeverityLevel.INFO;

public class ZipTool {

    public static interface Filter {

        public boolean isToBeIncluded(File file);
    }

    public static interface ProgressObserver {

        public void nextEntry(String name);
    }
    private final static int BUFSIZE = 10240;
    static double tic;

    public static void tic() {
        tic = Calendar.getInstance().getTimeInMillis();
    }

    public static double toc() {
        return Calendar.getInstance().getTimeInMillis();
    }

    public static void main(String[] args) {
        File archive = new File("bigfile.zip");
        FileInputStream is = null;
        try {
            tic();
            is = new FileInputStream(archive);

            final StringBuffer unzip_informations = new StringBuffer();
            ZipTool.unzipWithinDirectory(is, new File("/tmp"), new ZipTool.ProgressObserver() {
                public void nextEntry(String name) {
                    System.err.println(name);
                    unzip_informations.append(name);
                    unzip_informations.append('\n');
                }
            });
            System.err.println(unzip_informations.toString());
            System.err.println("  size: " + (archive.length() / 1024) + " kb.\n  time elapsed: " + ((toc() - tic) / 1000) + " s.");

        } catch (Exception e) {
            System.err.println("Failed to unzip: " + e.getMessage());
        }
    }

    protected static void insertFile(ZipOutputStream out, String prefix, File f, LogCollector logger, ProgressObserver observer, Filter filter) throws IOException {
        if (filter != null && !filter.isToBeIncluded(f)) {
            return;
        }
        if (f.isDirectory()) {
            prefix = prefix + (prefix.length() > 0 ? "/" : "") + f.getName();
            ZipEntry e = new ZipEntry(prefix + "/");
            if (observer != null) {
                observer.nextEntry(e.getName());
            }
            out.putNextEntry(e);
            out.closeEntry();
            if (logger != null) {
                logger.logMessage(INFO,false,"added dir " + e.getName());
            }
            File ch[] = f.listFiles();
            if (ch != null) {
                for (int i = 0; i < ch.length; i++) {
                    insertFile(out, prefix, ch[i], logger, observer, filter);
                }
            }
        } else {
            byte buf[] = new byte[BUFSIZE];
            FileInputStream fis = new FileInputStream(f);
            BufferedInputStream in = new BufferedInputStream(fis, BUFSIZE);
            ZipEntry e = new ZipEntry(prefix + "/" + f.getName());
            if (observer != null) {
                observer.nextEntry(e.getName());
            }
            out.putNextEntry(e);
            int count = 0;
            while ((count = in.read(buf, 0, BUFSIZE)) != -1) {
                out.write(buf, 0, count);
            }
            in.close();
            fis.close();
            out.closeEntry();
            if (logger != null) {
                logger.logMessage(INFO,false,"added file " + e.getName());
            }
        }

    }

    public static void unzipWithinDirectory(InputStream is, File path, ProgressObserver observer) throws IOException {
        byte buf[] = new byte[BUFSIZE];
        BufferedInputStream bis = new BufferedInputStream(is, BUFSIZE);
        ZipInputStream in = new ZipInputStream(bis);
        ZipEntry e = null;
        while ((e = in.getNextEntry()) != null) {

            String name = path.getPath() + File.separatorChar + e.getName();

            if (observer != null) {
                observer.nextEntry(e.getName());
            }
            if (e.isDirectory()) {
                new File(name).mkdir();
            } else {
                BufferedOutputStream bos = new BufferedOutputStream(new FileOutputStream(name), BUFSIZE);
                int len = 0;
                while ((len = in.read(buf, 0, BUFSIZE)) != -1) {
                    bos.write(buf, 0, len);
                }
                bos.flush();
                bos.close();
            }
        }
        in.close();
        bis.close();
    }

    public static void zipDirectory(File path, boolean pathIsRoot, OutputStream os, LogCollector logger, ProgressObserver observer, Filter filter) throws IOException {
        ZipOutputStream out = new ZipOutputStream(os);

        if (logger != null) {
            logger.logMessage(INFO,false,"Zipping directory " + path.getPath());
        }
        if (!pathIsRoot) {
            File f[] = path.listFiles();
            if (f != null) {
                for (int i = 0; i < f.length; i++) {
                    insertFile(out, "", f[i], logger, observer, filter);
                }
            }
        } else {
            insertFile(out, "", path, logger, observer, filter);
        }

        out.finish();
        out.flush();
    }

    public static void zipDirectory(File path, OutputStream os, LogCollector logger) throws IOException {
        zipDirectory(path, false, os, logger, null, null);
    }
}
