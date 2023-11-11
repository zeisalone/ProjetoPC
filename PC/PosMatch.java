import javax.swing.*;
import java.awt.*;

public class PosMatch {
    private JFrame frame;
    private ConnectionManager connectionManager;
    private String username;
    private JLabel result = new JLabel();
    private JLabel pointsP = new JLabel();
    private JLabel pointsE = new JLabel();
    private JButton backLobbyButton = new JButton("Back to Lobby");

    public PosMatch(JFrame frame, ConnectionManager connectionManager, String username) {
        this.frame = frame;
        this.connectionManager = connectionManager;
        this.username = username;
    }

    public void show_PosMatch(String res, int playerPoints, int enemyPoints) { //vê se é assim q queres. Ajusta como quiseres
        frame.getContentPane().removeAll();

        if(res.equals("win")) {
            this.result.setForeground(Color.GREEN); //Eu vou morrer com esta cor, mas infelizmente acho q é a mais correta
            this.result.setBounds(180, 50, 200, 50);
            this.result.setFont(new Font(null, Font.PLAIN, 40)); //Deve ser preciso diminuir
            this.result.setText("Victory");
        } else{
            this.result.setForeground(Color.RED);
            this.result.setBounds(180, 50, 200, 50);
            this.result.setFont(new Font(null, Font.PLAIN, 40)); //Deve ser preciso diminuir
            this.result.setText("Defeat");
        }

        this.pointsP.setBounds(190, 120, 200, 25);
        this.pointsP.setFont(new Font(null, Font.PLAIN, 25));
        this.pointsP.setText("Player " + String.valueOf(playerPoints));

        this.pointsE.setBounds(190, 145, 200, 25);
        this.pointsE.setFont(new Font(null, Font.PLAIN, 25));
        this.pointsE.setText("Enemy " + String.valueOf(enemyPoints));

        backLobbyButton.setBounds(120, 350, 250, 45);
        backLobbyButton.setFocusable(false);
        backLobbyButton.addActionListener(e->backLobby());

        frame.add(result);
        frame.add(backLobbyButton);
        frame.add(pointsP);
        frame.add(pointsE);

        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setSize(500, 500); //Ver que tamanho da janela é que ele tem em mente
        frame.setLayout(null);
        frame.setVisible(true);

        frame.repaint();
    }

    public void backLobby() {
        Lobby lobby = new Lobby(frame, connectionManager, username);
        lobby.show_lobby();
    }
}
