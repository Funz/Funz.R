package org.funz.util;

import java.util.logging.Level;
import java.util.logging.Logger;

public abstract class TimeOut {

    /**
     * @return the result
     */
    public Object getResult() {
        return result;
    }

    public static class TimeOutException extends Exception {

        public TimeOutException(String why) {
            super(why);
        }
    }

    private class TimeoutThread implements Runnable {

        Object res;

        public void run() {
            res = command();
            end();
        }

        public void end() {
            if (TimeOut.this != null) { // this object is still in stack
                //synchronized (TimeOut.this) {
                if (!timedOut) {// ...and not (yet) timed out
                    if (res != null) {// command() give a result
                        result = res;//... so pass it to parent in "result" object
                        if (TimeOut.this != null) {// this is (still) alive...
                            synchronized (TimeOut.this) {// notify that "this" has finished
                                TimeOut.this.notify();
                            }
                        }
                    }
                } else { // timout have been reached
                    if (TimeOut.this != null) {// this object is still in stack
                        TimeOut.this.doAfterTimeOut(res);// do something if needed
                    }
                }
                //}
            }
        }
    }
    private volatile boolean timedOut = false;
    private volatile Object result = null;
    String name;

    public String getName() {
        return name;
    }

    Thread shutdown = new Thread(new Runnable() {

        public void run() {
            interrupt();
            join();
        }
    }, "TimeOut " + name + " hook interrupt " + hashCode());

    protected TimeOut(String name) {
        this.name = name;
        Runtime.getRuntime().addShutdownHook(shutdown);
    }

    Thread t;

    public void join() {
        if (t != null) {
            try {
                t.join();
            } catch (InterruptedException ex) {
            }
            t = null;
        }
        Runtime.getRuntime().removeShutdownHook(shutdown); // Do not forget otherwise stack will be charged...
        if (interrupt != null) {
            try {
                interrupt.join();
            } catch (InterruptedException ex) {
            }
        }
    }

    public void interrupt() {
        //System.err.println("interrupt()");
        if (!interrupted && !break_command()) {
            System.err.println("TimeOut " + name + " interrupted but failed to break command !");
        }

        interrupted = true;
        end();
    }
    
    Thread interrupt;
    public void end() {
        if (t != null && interrupt==null) {
            interrupt = new Thread(new Runnable() {

                public void run() {
                    try {
                        if (t != null) {
                            t.interrupt();
                            t = null;
                        }
                    } catch(Exception e){}//just to avoid raising npe when t is destroyed
                }
            }, "TimeOut " + name + " interrupt");
            interrupt.start();
        }
        //System.err.println("interrupt() done");
        //t = null;
    }

    volatile boolean interrupted = false;

    public synchronized void execute(final long timeout) throws TimeOutException {
        timedOut = false;
        result = null;
        join();

        TimeoutThread to = new TimeoutThread();
        t = new Thread(to, "TimeOut " + name + " internal run thread");
        t.start();

        try {
            this.wait(timeout);
        } catch (InterruptedException e) {
            interrupted = true;
            //interrupt();
        }
        to.end();
        to = null;
        end();

        if (interrupted) {
            if (result == null) {
                timedOut = false;
                //result = defaultResult();
                t = null;
                throw new TimeOutException(name + " interrupted without result");
            } else {
                t = null;
                return;
            }
        }

        if (result != null) {
            end();
            return;
        } else {
            timedOut = true;
            result = defaultResult();
            end();
            throw new TimeOutException("timed out " + name);
        }
    }

    /**
     * Command si on a un resultat ok mais que le timeout est eteind
     *
     * @param result
     */
    protected abstract Object defaultResult();

    /**
     * Command a executer
     *
     * @return
     */
    protected abstract Object command();

    // to overload for shortcut to end (socket closing for instance...)
    protected boolean break_command() {
        return false;
    }

    protected void doAfterTimeOut(Object res) {
    }

    public static void main(String[] args) {

        TimeOut t = new TimeOut("t") {

            protected Object defaultResult() {
                return new Integer(-1);
            }

            protected Object command() {
                try {
                    for (int i = 0; i < 3; i++) {
                    System.err.println(".");
                    Thread.sleep(1000);
                    }
                } catch (Exception e) {
                }
                return new Integer(1);
            }
        };

        try {
            t.execute(10000);
        } catch (TimeOutException e) {
            e.printStackTrace();
        }
        System.err.println(t.getResult());

        /////////////////////////////
        /*t = new TimeOut("t new") {

            protected Object defaultResult() {
                return new Integer(-1);
            }

            protected Object command() {
                try {
                    Thread.sleep(10);
                } catch (Exception e) {
                }
                return new Integer(1);
            }
        };

        try {
            t.execute(100);
        } catch (TimeOutException e) {
            e.printStackTrace();
        }
        System.err.println(t.getResult());

        ////////////////////////////
        final TimeOut tt = new TimeOut("tt") {

            protected Object defaultResult() {
                return new Integer(-1);
            }

            protected Object command() {
                try {
                    Thread.sleep(1000);
                } catch (Exception e) {
                }
                return new Integer(1);
            }
        };

        new Thread(new Runnable() {

            public void run() {

                try {
                    tt.execute(1000);
                } catch (TimeOutException e) {
                    e.printStackTrace();
                }
            }
        }).start();

        try {
            Thread.sleep(500);
        } catch (InterruptedException ex) {
        }

        System.err.println("(!)");
        tt.interrupt();

        System.err.println(tt.getResult());*/
    }
}
