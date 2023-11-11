import javax.swing.*;
import java.awt.*;
import java.io.IOException;

public class MainMenu {

    private JFrame frame;
    private ConnectionManager connectionManager;
    private JLabel mainMenuLabel = new JLabel("Main Menu");
    private JButton signupButton = new JButton("Sign up");
    private JButton loginButton = new JButton("Login");
    private JButton leaveButton = new JButton("Leave");

    public MainMenu(JFrame frame, ConnectionManager connectionManager) {
        this.frame = frame;
        this.connectionManager = connectionManager;
    }

    public void show_MainMenu() {
        frame.getContentPane().removeAll();

        mainMenuLabel.setBounds(140, 15, 150, 25);
        mainMenuLabel.setFont(new Font(null,Font.PLAIN, 25));

        signupButton.setBounds(153, 100, 100, 25);
        signupButton.setFocusable(false);
        signupButton.addActionListener(e -> action_signUp(frame, connectionManager));

        loginButton.setBounds(153, 150, 100, 25);
        loginButton.setFocusable(false);
        loginButton.addActionListener(e -> action_login(frame, connectionManager));

        leaveButton.setBounds(153, 200, 100, 25);
        leaveButton.setFocusable(false);
        leaveButton.addActionListener(e -> leave(frame));

        frame.add(mainMenuLabel);
        frame.add(signupButton);
        frame.add(loginButton);
        frame.add(leaveButton);

        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setSize(420,420);
        frame.setLayout(null);
        frame.setVisible(true);

        frame.repaint();
    }

    public void action_signUp(JFrame frame, ConnectionManager connectionManager) {
        SignupPage signUpPage = new SignupPage(frame, connectionManager);
        signUpPage.show_singUpPage();
    }

    public void action_login(JFrame frame, ConnectionManager connectionManager) {
        LoginPage loginPage = new LoginPage(frame, connectionManager);
        loginPage.show_loginPage();
    }

    public void leave(JFrame frame){
        try {
            connectionManager.close();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        frame.dispose();
        System.exit(0);
    }
}
