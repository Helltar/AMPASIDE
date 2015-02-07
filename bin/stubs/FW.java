/*
 * This is the framework file for the MIDLetPascal generated MIDlets
 * The Pascal code is compiled into java class called M.
 * */

import javax.microedition.midlet.*;
import javax.microedition.lcdui.*;

public class FW extends MIDlet implements CommandListener {

    // command & forms support
    public static Form F;
    public static Displayable CD;
    public static Command LC;
    public static List L;
    public static Alert A;
    public static TextBox TB;

    public static FW fw = null;
    public M m = null;
    public boolean threadStarted = false;
    public Display display;
    public static int MP; // MidletPaused

    public void _mp() {
    }

    public void commandAction(Command c, Displayable item) {
        LC = c;
    }

    public void pauseApp() {
        MP = -1;
    }

    public void startApp() {
        MP = 0;

        display = Display.getDisplay(this);

        fw = this;

        LC = null;

        if (m == null) {
            m = new M();

            M.T = m;
            M.I = Image.createImage(m.getWidth(), m.getHeight());
            M.G = M.I.getGraphics();
            M.KC = 0;
            L = new List("", List.IMPLICIT);
            A = new Alert("", "", null, AlertType.INFO);
            TB = new TextBox("", "", 2, TextField.ANY);
            display.setCurrent(m);
            CD = m;
            try {
                m.setCommandListener(this);
            } catch (Exception e) {
                // do nothing, this catches the exception for NokiaAPI FullCanvas 
            }
            F = new Form("");
            F.setCommandListener(this);
        } else {
            m.repaint();
            m.serviceRepaints();
        }

        if (!threadStarted) {
            new Thread(m).start();
            threadStarted = true;
        }
    }

    public void destroyApp(boolean unconditional) {
        m = null;
        M.I = null;
        M.G = null;
        CD = null;
        TB = null;
        F = null;
        A = null;
        L = null;
        fw = null;
        LC = null;
        notifyDestroyed();
    }
}
