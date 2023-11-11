import javax.swing.*;
import java.awt.*;
import java.io.IOException;

public class Lobby{

    private JFrame frame;
    private ConnectionManager connectionManager;
    private String username;
    String[] columnNames = {"Username", "Wins"};
    JLabel welcomeLabel = new JLabel();
    JLabel leaderboardLabel = new JLabel("LeaderBoard");
    JButton playButton = new JButton("Play");
    JButton configButton = new JButton("Config");
    JButton leaveButton = new JButton("Leave");
    JButton updateButton = new JButton("Update");

    Lobby(JFrame frame, ConnectionManager connectionManager, String username) {
        this.connectionManager = connectionManager;
        this.frame = frame;
        this.username = username;
    }

    public void start_button_listeners() {
        playButton.addActionListener(e->action_play());
        configButton.addActionListener(e-> action_config());
        leaveButton.addActionListener(e->action_leave());
        updateButton.addActionListener(e->action_update());
    }

    void show_lobby() {
        String[][] leaderboard = new String[10][2];

        try {
            connectionManager.send("leaderboard", "request");
        } catch (IOException e) {
            e.printStackTrace();
        }

        Thread[] leaderboardThreads = new Thread[10];
        int numberOfThreads = 10;
        for (int i = 0; i < numberOfThreads; i++) {
            int top_number = i+1;
            leaderboardThreads[i] = new Thread(() -> {
                try {
                    String response = connectionManager.receive("top" + top_number);
                    String[] aux = response.split(",");
                    leaderboard[top_number - 1][0] = aux[0];
                    leaderboard[top_number - 1][1] = aux[1];
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
            });
            leaderboardThreads[i].start();
        }

        for (int i = 0; i < numberOfThreads; i++) {
            try {
                leaderboardThreads[i].join();
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        }

        frame.getContentPane().removeAll();

        start_button_listeners();

        JTable leaderboardTable = new JTable(leaderboard, columnNames);

        welcomeLabel.setBounds(15,25,500,35);
        welcomeLabel.setFont(new Font(null, Font.PLAIN, 25));
        welcomeLabel.setText("Welcome, " + username);

        leaderboardLabel.setBounds(415, 150, 200, 25);
        leaderboardLabel.setFont(new Font(null, Font.PLAIN, 20));

        playButton.setBounds(35, 200, 150, 45);
        playButton.setFocusable(false);

        configButton.setBounds(35, 300, 150, 45);
        configButton.setFocusable(false);

        leaveButton.setBounds(35, 400, 150, 45);
        leaveButton.setFocusable(false);

        updateButton.setBounds(395, 465, 150, 45);
        updateButton.setFocusable(false);

        leaderboardTable.setBounds(325, 200, 300, 250);
        leaderboardTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
        leaderboardTable.setRowHeight(25);

        frame.add(welcomeLabel);
        frame.add(leaderboardLabel);
        frame.add(playButton);
        frame.add(configButton);
        frame.add(leaveButton);
        frame.add(updateButton);
        frame.add(leaderboardTable);

        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setSize(700, 700);
        frame.setLayout(null);
        frame.setVisible(true);

        frame.repaint();
    }

    public void action_play() {
        try {
            connectionManager.send("play", "ok");
            String response = connectionManager.receive("queue");
            if (response.equals("match_found")) {
                System.out.println("partida encontrada");
                response = connectionManager.receive("initial_setup");
                System.out.println(response);
                String[] aux = response.split(",");
                Circle c1 = new Circle("player", 30, Integer.parseInt(aux[0]), Integer.parseInt(aux[1]), Float.parseFloat(aux[2]), Float.parseFloat(aux[3]));
                Circle c2 = new Circle("enemy", 30, Integer.parseInt(aux[4]), Integer.parseInt(aux[5]), Float.parseFloat(aux[6]), Float.parseFloat(aux[7]));
                Circle[] c = new Circle[2];
                c[0] = c1;
                c[1] = c2;
                Match match = new Match(frame, connectionManager, c, username);
                match.show_match();
                match.handle_match_input();
            }
        } catch (IOException | InterruptedException e) {
            e.printStackTrace();
        }
    }

    public void action_config() {
        ConfigMenu configMenu = new ConfigMenu(frame, connectionManager, username);
        configMenu.show_configMenu();
    }

    public void action_leave() {
        try {
            connectionManager.send("leave", "ok");
            connectionManager.close();
            System.exit(0);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        frame.dispose();
    }

    public void action_update() {
        Lobby lobby = new Lobby(frame, connectionManager, username);
        lobby.show_lobby();
    }
}
