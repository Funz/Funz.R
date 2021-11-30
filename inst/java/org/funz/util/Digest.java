/*
 * Created on 1 mars 07 by richet
 */
package org.funz.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class Digest {

    public static boolean equals(byte[] b1, byte[] b2) {
        if (b1.length != b2.length) {
            return false;
        }

        for (int i = 0; i < b1.length; i++) {
            if (b1[i] != b2[i]) {
                return false;
            }
        }

        return true;
    }

    public static boolean matches(byte[] b1, byte[] b2) {
        if (b1.length != b2.length) {
            return false;
        }

        for (int i = 0; i < b1.length; i++) {
            boolean found = false;
            for (int j = 0; j < b2.length; j++) {
                if (b1[i] == b2[j]) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                return false;
            }
        }

        return true;
    }

    public static void main(String[] args) throws Exception {
        byte[] d0 = Digest.getSum(new File("src/main/java/org/funz/util/ASCII.java"));
        System.err.println(d0.length + ": ");
        for (int i = 0; i < d0.length; i++) {
            System.err.print("" + d0[i]);
        }

        //byte[] d = org.apache.commons.codec.digest.DigestUtils.md5(new FileInputStream("src/main/java/org/funz/util/ASCII.java"));
        //System.err.println("\n" + d.length + ": ");
        //for (int i = 0; i < d.length; i++) {
        //    System.err.print("" + d[i]);
        //}

    }

    public static byte[] getSum(File file) {
        MessageDigest md5 = null;
        byte[] buffer = new byte[4096];
        try {
            md5 = MessageDigest.getInstance("MD5");
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        }

        md5.reset();
        InputStream fis = null;
        try {
            fis = new FileInputStream(file);
            int bytes = fis.read(buffer);
            while (bytes > 0) {
                md5.update(buffer, 0, bytes);
                bytes = fis.read(buffer);
            }

        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            try {
                fis.close();
            } catch (IOException ex) {
                ex.printStackTrace();
            }
        }
        return md5.digest();
    }

    public static byte[] getSum(URL url) {
        MessageDigest md5 = null;
        byte[] buffer = new byte[4096];
        try {
            md5 = MessageDigest.getInstance("MD5");
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        }

        md5.reset();
        InputStream fis = null;
        try {
            fis = url.openStream();
            int bytes = fis.read(buffer);
            while (bytes > 0) {
                md5.update(buffer, 0, bytes);
                bytes = fis.read(buffer);
            }

        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            try {
                fis.close();
            } catch (IOException ex) {
                ex.printStackTrace();
            }
        }
        return md5.digest();
    }
}
