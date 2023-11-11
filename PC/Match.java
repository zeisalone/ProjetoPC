import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class Match extends JComponent {

    private JFrame frame;
    private ConnectionManager connectionManager;
    private String username;
    private Circle[] players;
    private HashMap<PairPosition, List<CircleBody>> items;

    private JLabel pointsLabel = new JLabel();
    private JLabel timerLabel = new JLabel("120");
    private int playerPoints;
    private int enemyPoints;
    private boolean aKeyPressed;
    private boolean dKeyPressed;
    private boolean spaceKeyPressed;
    private boolean matchEnded;
    private int timerSeconds;

    public Match(JFrame frame, ConnectionManager connectionManager, Circle[] players, String username) {
        this.frame = frame;
        this.connectionManager = connectionManager;
        this.username = username;
        this.players = players;
        this.aKeyPressed = false;
        this.dKeyPressed = false;
        this.spaceKeyPressed = false;
        this.timerSeconds = 120;
        this.matchEnded = false;
        this.playerPoints = 0;
        this.enemyPoints = 0;
        this.items = new HashMap<>();
    }
    public void show_match() {
        Thread game = new Thread(() -> {
            while (!matchEnded) {
                frame.getContentPane().removeAll();

                frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

                for (Circle circle : players) {
                    JLabel circleBody = new CircleBody(circle);
                    JLabel circleEye = new CircleEye(circle);
                    frame.add(circleBody, JLayeredPane.DEFAULT_LAYER);
                    frame.add(circleEye, JLayeredPane.DEFAULT_LAYER);
                }

                List<List<CircleBody>> allItems = new ArrayList<>(items.values());

                for (List<CircleBody> itemList : allItems) {
                    for (CircleBody item : itemList) {
                        frame.add(item);
                    }
                }

                if (timerSeconds >= 100)
                    timerLabel.setBounds(325, 30, 30, 30);

                else if (timerSeconds >= 10)
                    timerLabel.setBounds(329, 30, 30, 30);

                else
                    timerLabel.setBounds(333, 30, 30, 30);

                timerLabel.setText(String.valueOf(timerSeconds));
                frame.add(timerLabel);

                this.pointsLabel.setBounds(250, 15, 300, 30);
                this.pointsLabel.setFont(new Font(null, Font.PLAIN, 20));
                this.pointsLabel.setText("Player " + playerPoints + " X "  + enemyPoints + " Enemy");
                frame.add(pointsLabel);

                frame.setSize(700, 700);
                frame.setLayout(null);
                frame.setVisible(true);

                frame.repaint();
                try {
                    Thread.sleep(1000/60);
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
            }
        });
        game.start();
    }

    public void handle_match_input() {
        MyKeyListener myKeyListener = new MyKeyListener();
        frame.addKeyListener(myKeyListener);

        PosMatch posMatch = new PosMatch(frame, connectionManager, username);

        Thread t0 = new Thread(() -> {
            while (!Thread.currentThread().isInterrupted()) {
                try {
                    Thread.currentThread().sleep(1000);
                    if (timerSeconds > 0)
                        timerSeconds--;
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
        });
        t0.start();

        Thread t1 = new Thread(() -> {
            while (!Thread.currentThread().isInterrupted()) {
                try {
                    connectionManager.send("input_move", "move");
                    Thread.currentThread().sleep(100);
                } catch (IOException e) {
                    throw new RuntimeException(e);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
        });
        t1.start();

        Thread t2 = new Thread(() -> {
            while (!Thread.currentThread().isInterrupted()) {
                try {
                    String msg = connectionManager.receive("new_pos_player");
                    String[] aux = msg.split(",");
                    players[0].setPos_center_x((int) Float.parseFloat(aux[0]));
                    players[0].setPos_center_y((int) Float.parseFloat(aux[1]));

                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
        });
        t2.start();

        Thread t3 = new Thread(() -> {
            while (!Thread.currentThread().isInterrupted()) {
                try {
                    String msg = connectionManager.receive("new_direction_player");
                    String[] aux = msg.split(",");
                    players[0].setDirecao_x((int) Float.parseFloat(aux[0]));
                    players[0].setDirecao_y((int) Float.parseFloat(aux[1]));

                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
        });
        t3.start();

        Thread t4 = new Thread(() -> {
            while (!Thread.currentThread().isInterrupted()) {
                try {
                    String msg = connectionManager.receive("new_pos_enemy");
                    String[] aux = msg.split(",");
                    players[1].setPos_center_x((int) Float.parseFloat(aux[0]));
                    players[1].setPos_center_y((int) Float.parseFloat(aux[1]));

                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
        });
        t4.start();

        Thread t5 = new Thread(() -> {
            while (!Thread.currentThread().isInterrupted()) {
                try {
                    String msg = connectionManager.receive("new_direction_enemy");
                    String[] aux = msg.split(",");
                    players[1].setDirecao_x((int) Float.parseFloat(aux[0]));
                    players[1].setDirecao_y((int) Float.parseFloat(aux[1]));

                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
        });
        t5.start();

        Thread t6 = new Thread(() -> {
            while (!Thread.currentThread().isInterrupted()) {
                try {
                    String msg = connectionManager.receive("new_item_red");
                    String[] aux = msg.split(",");
                    System.out.println("new_item_red: " + msg);
                    int item_pos_x = Integer.parseInt(aux[0]);
                    int item_pos_y = Integer.parseInt(aux[1]);
                    int item_radius = Integer.parseInt(aux[2]);
                    CircleBody newItem = new CircleBody("itemVermelho", item_pos_x, item_pos_y, item_radius);
                    PairPosition pos = new PairPosition(item_pos_x, item_pos_y);

                    synchronized (items) {
                        List<CircleBody> getItem = items.get(pos);

                        if (getItem == null) {
                            getItem = new ArrayList<>();
                            items.put(pos, getItem);
                        }

                        getItem.add(newItem);
                    }

                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
        });
        t6.start();

        Thread t7 = new Thread(() -> {
            while (!Thread.currentThread().isInterrupted()) {
                try {
                    String msg = connectionManager.receive("new_item_blue");
                    String[] aux = msg.split(",");
                    System.out.println("new_item_blue: " + msg);
                    int item_pos_x = Integer.parseInt(aux[0]);
                    int item_pos_y = Integer.parseInt(aux[1]);
                    int item_radius = Integer.parseInt(aux[2]);
                    CircleBody newItem = new CircleBody("itemAzul", item_pos_x, item_pos_y, item_radius);
                    PairPosition pos = new PairPosition(item_pos_x, item_pos_y);

                    synchronized (items) {
                        List<CircleBody> getItem = items.get(pos);

                        if (getItem == null) {
                            getItem = new ArrayList<>();
                            items.put(pos, getItem);
                        }

                        getItem.add(newItem);
                    }

                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
        });
        t7.start();

        Thread t8 = new Thread(() -> {
            while (!Thread.currentThread().isInterrupted()) {
                try {
                    String msg = connectionManager.receive("new_item_green");
                    String[] aux = msg.split(",");
                    System.out.println("new_item_green: " + msg);
                    int item_pos_x = Integer.parseInt(aux[0]);
                    int item_pos_y = Integer.parseInt(aux[1]);
                    int item_radius = Integer.parseInt(aux[2]);
                    CircleBody newItem = new CircleBody("itemVerde", item_pos_x, item_pos_y, item_radius);
                    PairPosition pos = new PairPosition(item_pos_x, item_pos_y);

                    synchronized (items) {
                        List<CircleBody> getItem = items.get(pos);

                        if (getItem == null) {
                            getItem = new ArrayList<>();
                            items.put(pos, getItem);
                        }

                        getItem.add(newItem);

                    }

                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
        });
        t8.start();

        Thread t9 = new Thread(() -> {
            while (!Thread.currentThread().isInterrupted()) {
                try {
                    String msg = connectionManager.receive("delete_item");
                    String[] aux = msg.split(",");
                    int item_pos_x = Integer.parseInt(aux[0]);
                    int item_pos_y = Integer.parseInt(aux[1]);
                    PairPosition pos = new PairPosition(item_pos_x, item_pos_y);

                    synchronized (items) {
                        items.remove(pos);
                    }

                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
        });
        t9.start();

        Thread t11 = new Thread(() -> {
            while (!Thread.currentThread().isInterrupted()) {
                try {
                    String msg = connectionManager.receive("point_player");
                    String[] aux = msg.split(",");
                    playerPoints++;
                    System.out.println("Player points: " + playerPoints);

                    players[0].setPos_center_x((int) Float.parseFloat(aux[0]));
                    players[0].setPos_center_y((int) Float.parseFloat(aux[1]));
                    players[0].setDirecao_x((int) Float.parseFloat(aux[2]));
                    players[0].setDirecao_y((int) Float.parseFloat(aux[3]));

                    players[1].setPos_center_x((int) Float.parseFloat(aux[4]));
                    players[1].setPos_center_y((int) Float.parseFloat(aux[5]));
                    players[1].setDirecao_x((int) Float.parseFloat(aux[6]));
                    players[1].setDirecao_y((int) Float.parseFloat(aux[7]));

                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
        });
        t11.start();

        Thread t12 = new Thread(() -> {
            while (!Thread.currentThread().isInterrupted()) {
                try {
                    String msg = connectionManager.receive("point_enemy");
                    String[] aux = msg.split(",");
                    enemyPoints++;
                    System.out.println("Enemy points: " + enemyPoints);

                    players[0].setPos_center_x((int) Float.parseFloat(aux[0]));
                    players[0].setPos_center_y((int) Float.parseFloat(aux[1]));
                    players[0].setDirecao_x((int) Float.parseFloat(aux[2]));
                    players[0].setDirecao_y((int) Float.parseFloat(aux[3]));

                    players[1].setPos_center_x((int) Float.parseFloat(aux[4]));
                    players[1].setPos_center_y((int) Float.parseFloat(aux[5]));
                    players[1].setDirecao_x((int) Float.parseFloat(aux[6]));
                    players[1].setDirecao_y((int) Float.parseFloat(aux[7]));

                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
        });
        t12.start();

        Thread t10 = new Thread(() -> {
            try {
                String msg = connectionManager.receive("match_ended");
                if (msg.equals("win") | msg.equals("loss")) {
                    System.out.println("ACABOU");
                    t0.interrupt();
                    t1.interrupt();
                    t2.interrupt();
                    t3.interrupt();
                    t4.interrupt();
                    t5.interrupt();
                    t6.interrupt();
                    t7.interrupt();
                    t8.interrupt();
                    t9.interrupt();
                    t11.interrupt();
                    t12.interrupt();
                    matchEnded = true;
                    frame.removeKeyListener(myKeyListener);
                    posMatch.show_PosMatch(msg, playerPoints, enemyPoints);
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        });
        t10.start();
    }

    private class MyKeyListener implements KeyListener {
        @Override
        public void keyTyped(KeyEvent e) {
            // Handle key typed event
        }

        @Override
        public void keyPressed(KeyEvent e) {
            int keyCode = e.getKeyCode();

            if (keyCode == KeyEvent.VK_A) {
                aKeyPressed = true;

                Thread t1 = new Thread(() -> {
                    try {
                        connectionManager.send("input_move", "turn_left");
                    } catch (IOException ex) {
                        throw new RuntimeException(ex);
                    }
                });
                t1.start();

                Thread t3 = new Thread(() -> {
                    if (dKeyPressed) {
                        try {
                            connectionManager.send("input_move", "turn_right");
                        } catch (IOException ex) {
                            throw new RuntimeException(ex);
                        }
                    }
                });
                t3.start();

            } else if (keyCode == KeyEvent.VK_D) {
                dKeyPressed = true;

                Thread t1 = new Thread(() -> {
                    try {
                        connectionManager.send("input_move", "turn_right");
                    } catch (IOException ex) {
                        throw new RuntimeException(ex);
                    }
                });
                t1.start();

                Thread t2 = new Thread(() -> {
                    if (aKeyPressed) {
                        try {
                            connectionManager.send("input_move", "turn_left");
                        } catch (IOException ex) {
                            throw new RuntimeException(ex);
                        }
                    }
                });
                t2.start();

            } else if (keyCode == KeyEvent.VK_SPACE) {

                Thread t1 = new Thread(() -> {
                    if (!spaceKeyPressed) {
                        spaceKeyPressed = true;
                        try {
                            connectionManager.send("input_move", "start_move");
                        } catch (IOException ex) {
                            throw new RuntimeException(ex);
                        }
                    }
                });
                t1.start();

                Thread t2 = new Thread(() -> {
                    if (aKeyPressed) {
                        try {
                            connectionManager.send("input_move", "turn_left");
                        } catch (IOException ex) {
                            throw new RuntimeException(ex);
                        }
                    }
                });
                t2.start();

                Thread t3 = new Thread(() -> {
                    if (dKeyPressed) {
                        try {
                            connectionManager.send("input_move", "turn_right");
                        } catch (IOException ex) {
                            throw new RuntimeException(ex);
                        }
                    }
                });
                t3.start();
            }
        }


        @Override
        public void keyReleased(KeyEvent e) {
            int keyCode = e.getKeyCode();

            if (keyCode == KeyEvent.VK_A) {
                aKeyPressed = false;

            } else if (keyCode == KeyEvent.VK_D) {
                dKeyPressed = false;

            } else if (keyCode == KeyEvent.VK_SPACE) {
                spaceKeyPressed = false;

                try {
                    connectionManager.send("input_move", "stopping_move");
                } catch (IOException ex) {
                    throw new RuntimeException(ex);
                }
            }
        }
    }
}