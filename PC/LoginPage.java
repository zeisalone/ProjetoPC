import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;

public class LoginPage implements ActionListener {

    private ConnectionManager connectionManager;
    private JFrame frame;
    private MainMenu mainMenu;
    private Lobby lobby;
    JLabel loginLabel = new JLabel("Login");
    JLabel usernameLabel = new JLabel("Username");
    JLabel userPasswordLabel = new JLabel("Password");
    JTextField usernameField = new JTextField();
    JPasswordField userPasswordField = new JPasswordField();
    JButton loginButton = new JButton("Login");
    JButton resetButton = new JButton("Reset");
    JButton backButton = new JButton("Back");

    LoginPage(JFrame frame, ConnectionManager connectionManager) {
        this.frame = frame;
        this.connectionManager = connectionManager;
    }

    public void start_button_listeners() {
        loginButton.addActionListener(e->action_login(frame, connectionManager));
        resetButton.addActionListener(this);
        backButton.addActionListener(e -> back_action(frame, connectionManager));
    }

    public void show_loginPage() {
        frame.getContentPane().removeAll();

        start_button_listeners();

        loginLabel.setBounds(170, 15, 150, 35);
        loginLabel.setFont(new Font(null,Font.PLAIN,25));

        usernameLabel.setBounds(50,100,100,25);
        userPasswordLabel.setBounds(50,150,100,25);

        usernameField.setBounds(125, 100, 200, 25);
        userPasswordField.setBounds(125, 150, 200, 25);

        loginButton.setBounds(90,200,100,25);
        loginButton.setFocusable(false);

        resetButton.setBounds(225,200,100,25);
        resetButton.setFocusable(false);

        backButton.setBounds(158, 250, 100, 25);
        backButton.setFocusable(false);

        frame.add(loginLabel);
        frame.add(usernameLabel);
        frame.add(userPasswordLabel);
        frame.add(usernameField);
        frame.add(userPasswordField);
        frame.add(loginButton);
        frame.add(resetButton);
        frame.add(backButton);

        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setSize(420,420);
        frame.setLayout(null);
        frame.setVisible(true);

        frame.repaint();
    }

    public void show_loginPage_with_error () {
        show_loginPage();
        JLabel errorMessage = new JLabel("Username or password incorrect");

        errorMessage.setForeground(Color.RED);
        errorMessage.setBounds(50, 310, 400, 25);
        errorMessage.setFont(new Font(null, Font.PLAIN, 20));

        frame.add(errorMessage);

        frame.repaint();
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        if(e.getSource()==resetButton){
            usernameField.setText("");
            userPasswordField.setText("");
        }
    }

    public void action_login(JFrame frame, ConnectionManager connectionManager) {
        String username = usernameField.getText();
        String password = userPasswordField.getText();

        try {
            connectionManager.send("login", username + "," + password);
            String response = connectionManager.receive("login");
            String[] aux = response.split(",");
            if (aux[0].equals("ok")) {
                Lobby lobby = new Lobby(frame, connectionManager, aux[1]);
                lobby.show_lobby();
            } else if (response.equals("denied")) {
                LoginPage loginPage = new LoginPage(frame, connectionManager);
                loginPage.show_loginPage_with_error();
            }
        } catch (IOException | InterruptedException e) {
            e.printStackTrace();
        }
    }

    public void back_action(JFrame frame, ConnectionManager connectionManager) {
        MainMenu mainMenu = new MainMenu(frame, connectionManager);
        mainMenu.show_MainMenu();
    }
}
