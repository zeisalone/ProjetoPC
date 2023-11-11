import javax.swing.*;
import java.io.*;
import java.net.Socket;

public class Main {
    public static void main(String[] args) throws IOException {
        int serverPort = 1234; // Server port

        Socket socket = new Socket("localhost", serverPort);

        ConnectionManager connectionManager = ConnectionManager.start(socket);

        int readThreads = 20;
        for (int i = 0; i < readThreads; i++) {
            Thread readThread = new Thread(() -> {
                while (connectionManager.isOpen()) {
                    try {
                        connectionManager.read();
                    } catch (IOException e) {
                        Thread.currentThread().interrupt();
                    }
                }
            });
            readThread.start();
        }

        JFrame frame = new JFrame();
        MainMenu mainMenu = new MainMenu(frame, connectionManager);
        mainMenu.show_MainMenu();
    }
}