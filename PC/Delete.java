import javax.swing.*;
import java.awt.*;
import java.io.IOException;

public class Delete {
    private JFrame frame;
    private ConnectionManager connectionManager;
    private String username;
    private JLabel confirmationLabel = new JLabel("If you want to delete your account, insert password");
    private JPasswordField passwordField = new JPasswordField();
    private JButton deleteButton = new JButton("Delete");
    private JButton backButton = new JButton("Back");

    public Delete(JFrame frame, ConnectionManager connectionManager, String username) {
        this.frame = frame;
        this.connectionManager = connectionManager;
        this.username = username;
    }

    public void show_delete() {
        frame.getContentPane().removeAll();

        confirmationLabel.setBounds(10, 25, 400, 25);
        confirmationLabel.setFont(new Font(null,Font.PLAIN, 15));

        passwordField.setBounds(100, 60, 200, 25);

        deleteButton.setBounds(50, 110, 100, 25);
        deleteButton.setFocusable(false);
        deleteButton.addActionListener(e -> action_delete());

        backButton.setBounds(250, 110, 100, 25);
        backButton.setFocusable(false);
        backButton.addActionListener(e -> action_back());

        this.frame.add(confirmationLabel);
        this.frame.add(passwordField);
        this.frame.add(deleteButton);
        this.frame.add(backButton);

        this.frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        this.frame.setSize(400,250);
        this.frame.setLayout(null);
        this.frame.setVisible(true);

        frame.repaint();
    }

    private void show_delete_with_error() {
        JLabel tryagainLabel = new JLabel("Try again");

        tryagainLabel.setBounds(150, 150, 100, 25);
        tryagainLabel.setForeground(Color.RED);
        tryagainLabel.setFont(new Font(null, Font.PLAIN, 20));

        this.frame.add(tryagainLabel);

        this.frame.repaint();
    }

    public void action_delete() {
        String password = passwordField.getText();

        try {
            System.out.println("Entrou");
            connectionManager.send("delete_account", password);
            String response = connectionManager.receive("account_deletion");
            System.out.println(response);
            if (response.equals("ok")) {
                MainMenu mainMenu = new MainMenu(frame, connectionManager);
                mainMenu.show_MainMenu();
            } else if (response.equals("denied")) {
                show_delete_with_error();
            }
        } catch (IOException | InterruptedException e) {
            e.printStackTrace();
        }
    }

    public void action_back(){
        ConfigMenu configMenu = new ConfigMenu(frame, connectionManager, username);
        configMenu.show_configMenu();
    }
}
