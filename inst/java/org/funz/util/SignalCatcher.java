package org.funz.util;

import sun.misc.Signal;
import sun.misc.SignalHandler;

public class SignalCatcher implements SignalHandler {

    private SignalHandler oldHandler;
    private boolean chain = true;

    public static SignalCatcher install(String signalName, boolean chain) {
        Signal diagSignal = new Signal(signalName);
        SignalCatcher instance = new SignalCatcher();
        instance.oldHandler = Signal.handle(diagSignal, instance);
        instance.chain = chain;
        return instance;
    }

    public void handle(Signal signal) {
        try {
            
            signalAction(signal);

            if (chain) // Chain back to previous handler, if one exists
            if (oldHandler != SIG_DFL && oldHandler != SIG_IGN) {
                oldHandler.handle(signal);
            }

        } catch (Exception e) {
            System.out.println("Signal catcher failed: " + e.getMessage());
        }
    }

    public void signalAction(Signal signal) {
        System.out.println("Catching " + signal.getName());
    }

    public static void main(String[] args) {
        SignalCatcher.install("TERM",true);
        SignalCatcher.install("INT",true);
        SignalCatcher.install("ABRT",true);
// Not working because handle by OS: SignalCatcher.install("KILL");
        SignalCatcher.install("PIPE",true);

        System.out.println("Signal handling example.");
        try {
            Thread.sleep(50000);
        } catch (InterruptedException e) {
            System.out.println("Interrupted: " + e.getMessage());
        }

    }
}
