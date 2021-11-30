package org.funz.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.exec.OS;
import static org.funz.util.ASCII.cat;
import static org.funz.util.ASCII.saveFile;

/**
 *
 * @author richet
 */
public class Process {

    public static void main(String[] args) throws Exception {
        //System.out.println(cat(",", splitSpacesNoQuote("C:\\\"Program Files\"\\totot")));
        //System.out.println(cat(",", splitSpacesNoQuote("\"C:\\Program Files\"\\totot")));

        //System.out.println(new Process("/bin/bash -c ls.sh", null, null).runCommand());
        //System.out.println(new Process("./ls.sh", null, null).runCommand());
        //System.out.println(new Process("./exit1.sh", new File("tmp"), null).runCommand(System.out, System.err, System.err));
        //System.out.println(new Process("./crash.sh", new File("tmp"), null).runCommand(System.out, System.err, System.err));
        /*Process fail = new Process("ThisIsABadCommand", new File("tmp"), null);
         System.err.println("runCommand");
         System.err.println(fail.runCommand(System.out, System.err, System.err));
         System.err.println("getFailReason");
         System.err.println(fail.getFailReason());*/
//System.out.println(new Process("echo 1", null, null).runCommand(new FileOutputStream("out1.txt"), new FileOutputStream("err1.txt"), new FileOutputStream("log1.txt")));
        //System.out.println(new Process("echo 2", null, null).runCommand());
        //System.out.println(new Process("echo 3", null, null).runCommand(new FileOutputStream("out3.txt"), new FileOutputStream("err3.txt"), new FileOutputStream("log3.txt")));
        //System.out.println(new Process("./ls.sh", null, null).runCommand(new FileOutputStream("out4.txt"), new FileOutputStream("err4.txt"), new FileOutputStream("log4.txt")));
        //System.out.println(new Process("dir", null, null).runCommand());
        //System.out.println(new Process("dir.bat", null, null).runCommand());
        System.out.println(new Process("./src/test/resources/mult.sh .2 .1", new File("tmpp"), null).runCommand(System.out, System.err, System.err));
    }
    public File _dir = new File(".");
    public String _cmd, _reason = "";
    public Map _env;
    protected java.lang.Process _proc;

    public void over(int ret) {
        // callback method
    }

    public class ErrorOutputReader extends Thread {

        PrintWriter _err;
        StringBuffer buffer = new StringBuffer();

        public ErrorOutputReader(PrintWriter err) {
            _err = err;
        }

        public String read() {
            return buffer.toString();
        }

        @Override
        public void run() {
            if (_proc != null) {
                BufferedReader reader = new BufferedReader(new InputStreamReader(_proc.getErrorStream()));
                String line;
                try {
                    while ((line = reader.readLine()) != null) {
                        if (_err != null) {
                            _err.println(line);
                            _err.flush();
                            buffer.append(line).append("\n");
                        }
                    }
                    reader.close();
                } catch (IOException e) {
                    System.err.println("read stderr failed " + e.getMessage());
                }
            }
        }
    }

    public Process(String cmd, File dir, Map env) {
        _cmd = cmd;
        if (dir != null) {
            _dir = dir;
            if (!_dir.exists()) {
                boolean created = _dir.mkdir();
                if (!created) {
                    throw new IllegalArgumentException("Could not create process dir: " + _dir);
                }
            }
        }
        if (env == null) {
            _env = new HashMap();
        } else {
            _env = env;
        }
        _env.put("DIR", _dir.getAbsolutePath());
        _env.put("CMD", _cmd);
    }

    public static String[] splitSpacesNoQuote(String str) {
        List<String> matchList = new ArrayList<String>();
        Pattern regex = Pattern.compile("[^\\s\"']+|\"[^\"]*\"|'[^']*'");
        Matcher regexMatcher = regex.matcher(str);
        while (regexMatcher.find()) {
            matchList.add(regexMatcher.group());
        }
        return matchList.toArray(new String[matchList.size()]);
    }

    public void stop() {
        if (_proc != null) {
            _reason = "killed";
            _proc.destroy();
            try {
                _proc.getErrorStream().close();
                _proc.getOutputStream().close();
                _proc.getInputStream().close();
            } catch (Exception e) {
                System.err.println("could not close error stream: " + e);
            }
        }
    }

    public int runCommand() throws Exception {
        return runCommand(System.out, System.err, null);
    }

    static final String[] envartag = {"$$", "__", "@@", "//"};

    public int runCommand(OutputStream out, OutputStream err, OutputStream log) throws Exception {
        PrintWriter outwriter = new PrintWriter(out);
        PrintWriter errwriter = new PrintWriter(err);
        PrintWriter logwriter = null;
        BufferedReader outreader = null;
        int ret = -666;
        try {
            if (log != null) {
                logwriter = new PrintWriter(log);
            } else {
                logwriter = new PrintWriter(File.createTempFile("log", "txt"));
            }

            logwriter.println("cmd: " + _cmd);
            logwriter.println("dir: " + _dir.getAbsolutePath() + " : " + _dir.exists());
            logwriter.println("+env: " + _env);
            logwriter.flush();
            //System.err.println("Running command "+cmd+" in "+_dir.getAbsolutePath());

            if (_cmd == null) {
                throw new NullPointerException("Command to execute is null.");
            }
            if (_cmd.length() == 0) {
                throw new IllegalArgumentException("Command to execute is empty.");
            }

            if (OS.isFamilyWindows()) {
                if (_cmd.contains("/")) {
                    _cmd = _cmd.replace('/', '\\');
                }
            }
            String[] split_cmd = splitSpacesNoQuote(_cmd);
            if (split_cmd[0].startsWith("." + File.separator)) {
                String path = new java.io.File(".").getCanonicalPath();
                if (path.contains(" ")) {
                    split_cmd[0] = "\"" + path + File.separator + split_cmd[0].substring(2) + "\"";
                } else {
                    split_cmd[0] = path + File.separator + split_cmd[0].substring(2);
                }
            }

            ProcessBuilder processBuilder = new ProcessBuilder(split_cmd);
            if (OS.isFamilyUnix() || OS.isFamilyMac()) {
                if (Disk.isBinary(new File(split_cmd[0].replace("\"", "")))) {// binary
                    processBuilder = new ProcessBuilder(split_cmd);
                } else if (split_cmd[0].trim().endsWith(".py") | split_cmd[0].trim().endsWith(".py\"")) { // python
                    String python = "python";
                    try {
                        String auto_bin = ParserUtils.getFirstLineContaining(new File(split_cmd[0].replace("\"", "")), "#!");
                        if (auto_bin != null) {
                            auto_bin = auto_bin.substring(auto_bin.indexOf("#!") + 2).replace("/usr/bin/env ", "").trim();
                        }
                        if (auto_bin != null && auto_bin.startsWith("python")) {
                            python = auto_bin;
                        } else {
                            if (ParserUtils.contains(new File(split_cmd[0].replace("\"", "")), "print ", true)) {// python2
                                python = "python2";
                            } else if (ParserUtils.contains(new File(split_cmd[0].replace("\"", "")), "print(", true)) {// python3
                                python = "python3";
                            }
                        }
                    } catch (Exception e) {
                    }
                    List<String> args = new ArrayList(Arrays.asList(split_cmd));
                    args.add(0, python);
                    processBuilder = new ProcessBuilder(args);
                } else { // anything else, including .sh files
                    processBuilder = new ProcessBuilder("/bin/bash", "-c", cat(" ", split_cmd));
                }
            } else if (OS.isFamilyWindows()) {
                if (Disk.isBinary(new File(split_cmd[0].replace("\"", ""))) || split_cmd[0].trim().endsWith(".exe") || split_cmd[0].trim().endsWith(".exe\"")) {// binary file
                    processBuilder = new ProcessBuilder(split_cmd);
                } else if (split_cmd[0].trim().endsWith(".py") | split_cmd[0].trim().endsWith(".py\"")) {// python
                    List<String> args = new ArrayList(Arrays.asList(split_cmd));
                    args.add(0, "python");
                    processBuilder = new ProcessBuilder(args);
                } else {// anything else, including .bat files
                    processBuilder = new ProcessBuilder("cmd.exe", "/C", cat(" ", split_cmd));
                }
            }
            logwriter.println("process: " + processBuilder.command());
            logwriter.flush();

            Map env = processBuilder.environment();
            if (_env != null) {
                env.putAll(_env);
            }
            logwriter.println("env: " + env);
            logwriter.flush();
            try {
                if (_dir.exists()) {
                    for (File f : _dir.listFiles()) {
                        for (Object e : env.keySet()) {
                            for (String t : envartag) {
                                if (!t.equals(e.toString()) && !Disk.isBinary(f) && ParserUtils.contains(f, t + e.toString() + t, true)) {
                                    logwriter.println("Replacing " + e.toString() + " by " + env.get(e).toString() + " found in " + f);
                                    logwriter.flush();
                                    Disk.copyFile(f, new File(_dir, "orig." + f.getName() + ".orig"));
                                    saveFile(f, ParserUtils.getASCIIFileContent(f).replace(t + e.toString() + t, env.get(e).toString()));
                                }
                            }

                        }
                    }
                } else {
                    throw new IOException("Cannot access directory " + _dir);
                }
            } catch (Exception e) {
                logwriter.println("Error replacing tags: " + e.getMessage());
                logwriter.flush();
            }

            processBuilder = processBuilder.directory(_dir);

            try {
                _proc = processBuilder.start();
            } catch (Exception e) {
                _reason = e.getMessage();
                return -666;
            }

            ErrorOutputReader eor = new ErrorOutputReader(errwriter);
            eor.start();

            outreader = new BufferedReader(new InputStreamReader(_proc.getInputStream()));
            String line;
            try {
                while ((line = outreader.readLine()) != null) {
                    outwriter.println(line);
                    outwriter.flush();
                }
            } catch (IOException e) {
                logwriter.println("Stream broken: " + e);
                logwriter.flush();
                _reason = e.getMessage();
            }

            while (true) {
                try {
                    Thread.sleep(100);
                    eor.join();
                    break;
                } catch (InterruptedException ie) {
                    logwriter.println("join ErrorOutputReader thread interrupted " + ie.getMessage());
                }
            }

            Thread.sleep(100); //patch pour attendre le lancement du thread systeme de calcul
            ret = _proc.waitFor();

            if (ret != 0) {
                _reason = eor.read();
                logwriter.println("Error message: " + _reason);
                logwriter.flush();

            }
            logwriter.println("Exit status " + ret);
            logwriter.flush();

            over(ret);
        } catch (Exception e) {
            e.printStackTrace(errwriter);
        } finally {
            if (outreader != null) {
                outreader.close();
            }

            if (errwriter != null) {
                errwriter.flush();
                if (err != null && err != System.err && err != System.out) {
                    errwriter.close();
                    err.close();
                }
            }

            if (outwriter != null) {
                outwriter.flush();
                if (out != null && out != System.out && out != System.err) {
                    outwriter.close();
                    out.close();
                }
            }

            if (logwriter != null) {
                logwriter.flush();
                if (log != null && log != System.out && log != System.err) {
                    logwriter.close();
                    log.close();
                }
            }
            if (_proc != null) {
                if (_proc.getErrorStream() != null) {
                    _proc.getErrorStream().close();
                }
                if (_proc.getInputStream() != null) {
                    _proc.getInputStream().close();
                }
                if (_proc.getOutputStream() != null) {
                    _proc.getOutputStream().close();
                }
            }

            return ret;
        }
    }

    public String getFailReason() {
        return _reason;
    }
}
