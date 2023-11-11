import javax.swing.*;
import java.awt.*;
import java.io.IOException;

public class ConfigMenu {
    private JFrame frame;
    private ConnectionManager connectionManager;
    private String username;
    private JLabel confiMenuLabel = new JLabel("Config Menu");
    private JButton deleteAccountButton = new JButton("Delete Account");
    private JButton logoutButton = new JButton("Logout");
    private JButton backButton = new JButton("Back");

    public ConfigMenu(JFrame frame, ConnectionManager connectionManager, String username) {
        this.frame = frame;
        this.connectionManager = connectionManager;
        this.username = username;
    }

    public void start_button_listeners() {
        deleteAccountButton.addActionListener(e -> action_delete());
        logoutButton.addActionListener(e -> action_logout());
        backButton.addActionListener(e -> action_back());
    }

    public void show_configMenu() {
        frame.getContentPane().removeAll();

        confiMenuLabel.setBounds(130, 15, 250, 25);
        confiMenuLabel.setFont(new Font(null,Font.PLAIN, 25));

        deleteAccountButton.setBounds(138, 100, 150, 30);
        deleteAccountButton.setFocusable(false);

        logoutButton.setBounds(138, 150, 150, 30);
        logoutButton.setFocusable(false);

        backButton.setBounds(138, 200, 150, 30);
        backButton.setFocusable(false);

        start_button_listeners();

        frame.add(confiMenuLabel);
        frame.add(deleteAccountButton);
        frame.add(logoutButton);
        frame.add(backButton);

        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setSize(420,420);
        frame.setLayout(null);
        frame.setVisible(true);

        frame.repaint();
    }


    public void action_delete() {
        Delete delete = new Delete(frame, connectionManager, username);
        delete.show_delete();
    }

    public void action_logout() {

        try {
            connectionManager.send("logout", "ok");
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        MainMenu mainMenu = new MainMenu(frame, connectionManager);
        mainMenu.show_MainMenu();
    }

    public void action_back(){
        Lobby lobby = new Lobby(frame, connectionManager, username);
        lobby.show_lobby();
    }
}
