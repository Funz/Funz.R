package org.funz.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLClassLoader;
import java.net.URLConnection;
import java.util.jar.JarEntry;
import java.util.jar.JarInputStream;
import org.funz.util.TimeOut.TimeOutException;

public class URLMethods {

    public static class OpenURLClassLoader extends URLClassLoader {

        public OpenURLClassLoader(URL[] urls, ClassLoader parent) {
            super(urls, parent);
        }

        @Override
        public void addURL(URL url) {
            super.addURL(url);
        }

        public void addURLs(URL... urls) {
            for (URL url : urls) {
                addURL(url);
            }
        }

        public String URLstoString() {
            StringBuilder sb = new StringBuilder();
            for (URL u : getURLs()) {
                sb.append(u);
                sb.append('\n');
            }
            return sb.toString();
        }
    }

    public static boolean preloadDependency(String className, URLMethods.OpenURLClassLoader cl, String... jarurl) {
        //System.err.println("Loading " + className + " in " + Arrays.asList(jarurl));
        JarInputStream jarIStream = null;
        try {
            URL[] urls = new URL[jarurl.length];
            URL[] jurls = new URL[jarurl.length];
            for (int i = 0; i < urls.length; i++) {
                urls[i] = new URL(jarurl[i]);
                jurls[i] = new URL("jar:" + jarurl[i] + "!/");
            }
            cl.addURLs(jurls);

            for (int i = 0; i < urls.length; i++) {
                URL url = urls[i];
                System.err.println("url=" + url);
                jarIStream = new JarInputStream(url.openStream());
                JarEntry entry;
                while ((entry = jarIStream.getNextJarEntry()) != null) {
                    String entryName = entry.getName();
                    if (entryName.endsWith(".class")) {
                        String name = entryName.substring(0, entryName.length() - 6);
                        name = name.replace('/', '.');
                        if (name.equals(className)) {
                            Class clsRef = Class.forName(name, true, cl);
                            return clsRef != null;
                        }
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            try {
                jarIStream.close();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        return false;
    }

    public static Object scanURLJar(String jarurl, String interfaceName) {
        //System.err.println("jar: " + jarurl + " / " + interfaceName);
        JarInputStream jarIStream = null;
        try {
            if (!jarurl.contains(":"))
                jarurl="file:"+jarurl; //should solve when missing protocol

            URL url = new URL(jarurl);
            OpenURLClassLoader ucl = new OpenURLClassLoader(new URL[]{new URL("jar:" + jarurl + "!/")}, URLMethods.class.getClassLoader());

            jarIStream = new JarInputStream(url.openStream());

            JarEntry entry;
            while ((entry = jarIStream.getNextJarEntry()) != null) {
                String entryName = entry.getName();
                if (entryName.endsWith(".class")) {
                    String name = entryName.substring(0, entryName.length() - 6);
                    name = name.replace('/', '.');
                    Class clsRef = Class.forName(name, true, ucl);

                    //System.out.println("  clsRef "+clsRef.getCanonicalName());
                    for (Class cls = clsRef; cls != null; cls = cls.getSuperclass()) {

                        //System.out.println("   class "+cls.getCanonicalName());
                        //if (!Modifier.isAbstract(cls.getModifiers())) {
                        Class[] classArray = cls.getInterfaces();

                        for (int i = 0; i < classArray.length; i++) {
                            //System.out.println("    interface " + classArray[i].getName());

                            if (classArray[i].getName().equals(interfaceName)) {
                                cls = null;
                                try {
                                    Object o = clsRef.newInstance();
                                    //System.err.println("Class " + clsRef.getName() + " is usable.");
                                    return o;
                                } catch (InstantiationException ie) {
                                    System.err.println("Class " + clsRef.getName() + " could not be instanciated.");
                                    //ie.printStackTrace();
                                }
                            }
                        }
                        if (cls == null) {
                            break;
                        }
                        //}
                    }
                }
            }
        } catch (Exception e) {
            System.err.println("Cannot instanciate class " + interfaceName + ": " + e.getMessage());
            //e.printStackTrace();
        } finally {
            try {
                jarIStream.close();
            } catch (Exception e) {
                System.err.println("Cannot close jar " + jarIStream + ": " + e.getMessage());
                //e.printStackTrace();
            }
        }
        return null;
    }
    public final static String NOSTREAM = "No stream";

    public static String readURL(final String urlstr, final long timeout) {
        TimeOut t = new TimeOut("readURL") {

            protected Object defaultResult() {
                return NOSTREAM;
            }

            protected Object command() {
                return readURL(urlstr);
            }
        };
        try {
            t.execute(timeout);
        } catch (TimeOutException e) {
        }
        return t.getResult().toString();

    }

    public static String readURL(String urlstr) {

        BufferedReader in = null;
        StringBuffer str = new StringBuffer();
        URLConnection conn = null;
        try {
            URL url = new URL(urlstr);

            conn = url.openConnection();
            //conn.setConnectTimeout(TIMEOUT);
            in = new BufferedReader(new InputStreamReader(conn.getInputStream()));

            String inputLine;

            while ((inputLine = in.readLine()) != null) {
                str.append(inputLine);
                str.append("\n");
            }
        } catch (IOException e) {
            str.append("Failed to access " + urlstr);
            //System.err.println("Failed to access " + urlstr);
            //str = null;
        } finally {
            try {
                if (in != null) {
                    in.close();
                }
            } catch (Exception e) {
                //e.printStackTrace();
            }
            try {
                if (conn.getInputStream() != null) {
                    conn.getInputStream().close();
                }
            } catch (Exception e) {
            }
            try {
                if (conn.getOutputStream() != null) {
                    conn.getOutputStream().close();
                }
            } catch (Exception e) {
            }
        }
        if (str == null) {
            return null;
        } else {
            return str.toString();
        }
    }

    static int TIMEOUT = 10000;

    public static File downloadFile(final String urlstr, final File dest, final long timeout) {
        TimeOut t = new TimeOut("downloadFile") {

            protected Object defaultResult() {
                return null;
            }

            protected Object command() {
                return downloadFile(urlstr, dest);
            }
        };
        try {
            t.execute(timeout);
        } catch (TimeOutException e) {
        }
        return (File) t.getResult();
    }

    public static File downloadFile(String adresse, File dest) {
        BufferedReader reader = null;
        FileOutputStream fos = null;
        InputStream in = null;
        URLConnection conn = null;
        try {

            // crÃ©ation de la connection
            URL url = new URL(adresse);
            conn = url.openConnection();
            //System.out.println("Downloading from : " + adresse);
            //conn.setConnectTimeout(TIMEOUT);

            String FileType = conn.getContentType();
            //System.out.println("FileType : " + FileType);

            //int FileLenght = conn.getContentLength();
            //if (FileLenght == -1) {
            //throw new IOException("File not valid."); NOT reliable
            //}
            // lecture de la rÃ©ponse
            in = conn.getInputStream();
            reader = new BufferedReader(new InputStreamReader(in));
            if (dest == null) {
                String FileName = url.getFile();
                FileName = FileName.substring(FileName.lastIndexOf('/') + 1);
                dest = new File(FileName);
            }
            //System.out.println("Destination : " + dest);

            fos = new FileOutputStream(dest);
            byte[] buff = new byte[1024];
            int l = in.read(buff);
            while (l > 0) {
                fos.write(buff, 0, l);
                l = in.read(buff);
            }
            //System.out.println("done.");
            return dest;
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        } finally {
            try {
                if (fos != null) {
                    fos.flush();
                    fos.close();
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
            try {
                if (reader != null) {
                    reader.close();
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
            try {
                if (in != null) {
                    in.close();
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
            try {
                if (conn.getInputStream() != null) {
                    conn.getInputStream().close();
                }
            } catch (Exception e) {
            }
            try {
                if (conn.getOutputStream() != null) {
                    conn.getOutputStream().close();
                }
            } catch (Exception e) {
            }
        }
    }
}
