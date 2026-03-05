package labs.lab1;

import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.util.Arrays;

public class JavaUdpServer4 {

    public static void main(String args[])
    {
        System.out.println("JAVA UDP SERVER");
        DatagramSocket socket = null;
        int portNumber = 9011;

        try{
            socket = new DatagramSocket(portNumber);
            byte[] receiveBuffer = new byte[1024];

            while(true) {
                Arrays.fill(receiveBuffer, (byte)0);
                DatagramPacket receivePacket = new DatagramPacket(receiveBuffer, receiveBuffer.length);
                socket.receive(receivePacket);
                String msg = new String(receivePacket.getData());
//                System.out.println("received msg: " + msg.trim());

                byte[] responseBuffer = null;
                if (msg.contains("Java")) {
                    responseBuffer = "Pong Java".getBytes();
                }
                else {
                    responseBuffer = "Pong Python".getBytes();
                }
                DatagramPacket responsePacket = new DatagramPacket(responseBuffer, responseBuffer.length,
                        receivePacket.getAddress(), receivePacket.getPort());
                socket.send(responsePacket);
            }
        }
        catch(Exception e){
            e.printStackTrace();
        }
        finally {
            if (socket != null) {
                socket.close();
            }
        }
    }
}
