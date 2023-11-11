import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;

public class SignupPage implements ActionListener {

    private JFrame frame;
    private ConnectionManager connectionManager;
    JLabel signupLabel = new JLabel("Sign up");
    JLabel usernameLabel = new JLabel("Username");
    JLabel userPasswordLabel = new JLabel("Password");
    JTextField usernameField = new JTextField();
    JPasswordField userPasswordField = new JPasswordField();
    JButton signupButton = new JButton("Sign up");
    JButton resetButton = new JButton("Reset");
    JButton backButton = new JButton("Back");

    public SignupPage(JFrame frame, ConnectionManager connectionManager) {
        this.frame = frame;
        this.connectionManager = connectionManager;
    }

    public void start_button_listeners() {
        signupButton.addActionListener(e -> action_signup());
        resetButton.addActionListener(this);
        backButton.addActionListener(e -> back_action(frame, connectionManager));
    }

    public void show_singUpPage() {
        frame.getContentPane().removeAll();

        start_button_listeners();

        signupLabel.setBounds(165, 15, 150, 25);
        signupLabel.setFont(new Font(null,Font.PLAIN, 25));

        usernameLabel.setBounds(50, 100, 100, 25);
        userPasswordLabel.setBounds(50, 150, 100, 25);

        usernameField.setBounds(125, 100, 200, 25);
        userPasswordField.setBounds(125, 150, 200, 25);

        signupButton.setBounds(90,200,100,25);
        signupButton.setFocusable(false);

        resetButton.setBounds(225,200,100,25);
        resetButton.setFocusable(false);

        backButton.setBounds(158, 250, 100, 25);
        backButton.setFocusable(false);
        backButton.addActionListener(e -> back_action(frame, connectionManager));

        frame.add(signupLabel);
        frame.add(usernameLabel);
        frame.add(userPasswordLabel);
        frame.add(usernameField);
        frame.add(userPasswordField);
        frame.add(signupButton);
        frame.add(resetButton);
        frame.add(backButton);

        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setSize(420,420);
        frame.setLayout(null);
        frame.setVisible(true);

        frame.repaint();
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        if(e.getSource()==resetButton){
            usernameField.setText("");
            userPasswordField.setText("");
        }
    }

    public void action_signup() {
        String username = usernameField.getText();
        String password = userPasswordField.getText();

        try {
            connectionManager.send("create_account", username + "," + password);
            String response = connectionManager.receive("registration");
            if (response.equals("ok")) {
                System.out.println("Registrado");
            } else if (response.equals("denied")) {
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
