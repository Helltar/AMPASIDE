/*
 * class used for sending SMS
 * */

import javax.wireless.messaging.*;
import javax.microedition.io.*;

public class SM implements Runnable {

    public static int success = 0;
    public static boolean isSending = false;

    private String message;
    private String destination;

    /*
     * isSendingSMS
     * */
    public static int IS() {
        if (isSending) {
            return -1;
        } else {
            return 0;
        }
    }

    public static int _IS() {
        if (isSending) {
            return -1;
        } else {
            return 0;
        }
    }

    public static int send(String destination, String message) {
        if (isSending) {
            return 0;
        }

        new SM(destination, message);

        return -1;
    }

    private static void StartThread(Thread t) {
        t.start();
    }
    /*
     * Send SMS message to the specified destination. Return true 
     * if succedded, false otherwise.
     *
     * Destination is in format: sms://+number
     * */

    public SM(String destination, String message) {
        isSending = true;
        success = 0;
        // j-a-s-d: removed the fixed (276) destination port
        this.destination = destination;// + ":276";
        this.message = message;

        try {
            Thread t = new Thread(this);
            // j-a-s-d: avoid AV false alarms
            StartThread(t);//t.start();
        } catch (Exception e) {
            isSending = false;
        }
    }

    /*
     * get success, returns true if message sending has succedded
     * */
    public static int GS() {
        return success;
    }

    public void run() {
        try {
            // try sending using wireless api
            MessageConnection smsconn = (MessageConnection) Connector.open(destination);
            TextMessage txtmessage = (TextMessage) smsconn.newMessage(MessageConnection.TEXT_MESSAGE);
            txtmessage.setAddress(destination);
            txtmessage.setPayloadText(message);
            smsconn.send(txtmessage);
            smsconn.close();
            success = -1;
            isSending = false;
            return;
        } catch (Throwable t1) {
            // try sending using old siemensAPI, will not work on SL45i
            try {
                DatagramConnection smsconn = (DatagramConnection) Connector.open(destination);
                Datagram dgram = smsconn.newDatagram(message.getBytes(), message.getBytes().length, destination);
                smsconn.send(dgram);
                smsconn.close();
                success = -1;
                isSending = false;
            } catch (Throwable t2) {
                success = 0;
                isSending = false;
                return;
            }

        }

        // never come here
        success = -1;
        isSending = false;
    }

    public static int _send(String destination, String message) {
        if (isSending) {
            return 0;
        }

        new SM(destination, message);

        return -1;
    }
}
