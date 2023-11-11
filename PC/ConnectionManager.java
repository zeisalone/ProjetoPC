import java.io.*;
import java.net.Socket;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Queue;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

public class ConnectionManager {

    private ReentrantLock l = new ReentrantLock();

    private PrintWriter out;
    private BufferedReader in;
    private Socket socket;

    private ReentrantLock sendLock;
    private ReentrantLock messagesLock;
    private ReentrantLock readLock;
    private HashMap<String, Queue<String>> messagesQueue;
    private HashMap<String, Condition> conditions;

    private Queue<String> getQueue(String type) {
        return messagesQueue.computeIfAbsent(type, k -> new LinkedList<>());
    }

    private ConnectionManager(Socket socket) throws IOException {
        this.socket = socket;
        this.in     = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        this.out    = new PrintWriter(socket.getOutputStream(), true);
        this.sendLock = new ReentrantLock();
        this.readLock = new ReentrantLock();
        this.messagesLock = new ReentrantLock();
        this.messagesQueue = new HashMap<>();
        this.conditions = new HashMap<>();
        defineConditions();
    }

    public static ConnectionManager start(Socket socket) throws IOException {
        return new ConnectionManager(socket);
    };

    private void defineConditions() {
        for (int i = 1; i <= 10; i++) {
            String type = "top" + i;
            conditions.put(type, messagesLock.newCondition());
        }

        conditions.put("initial_setup", messagesLock.newCondition());
        conditions.put("login", messagesLock.newCondition());
        conditions.put("registration", messagesLock.newCondition());
        conditions.put("account_deletion", messagesLock.newCondition());
        conditions.put("logout", messagesLock.newCondition());
        conditions.put("queue", messagesLock.newCondition());
        conditions.put("new_pos_enemy", messagesLock.newCondition());
        conditions.put("new_pos_player", messagesLock.newCondition());
        conditions.put("new_direction_enemy", messagesLock.newCondition());
        conditions.put("new_direction_player", messagesLock.newCondition());
        conditions.put("match_ended", messagesLock.newCondition());
        conditions.put("new_item_red", messagesLock.newCondition());
        conditions.put("new_item_blue", messagesLock.newCondition());
        conditions.put("new_item_green", messagesLock.newCondition());
        conditions.put("delete_item", messagesLock.newCondition());
        conditions.put("point_player", messagesLock.newCondition());
        conditions.put("point_enemy", messagesLock.newCondition());
    }

    public void send(String type, String message) throws IOException {
        sendLock.lock();
        try {
            out.println(type + ":" + message);
        }
        finally {
            sendLock.unlock();
        }
    }

    public String receive(String type) throws InterruptedException {
        String msg;
        messagesLock.lock();
        try {
            Queue<String> queue = getQueue(type);
            msg = queue.poll();
            Condition condition = conditions.get(type);

            while (msg == null) {
                condition.await();
                msg = queue.poll();
            }

            return msg;
        }
        finally {
            messagesLock.unlock();
        }
    }

    public void read() throws IOException {
        String line;
        readLock.lock();
        try {
            line = in.readLine();

            if (line != null) {
                String[] aux = line.split(":");
                String type = aux[0];
                String msg = aux[1];

                messagesLock.lock();
                try {
                    Queue<String> queue = getQueue(type);
                    queue.offer(msg);
                    Condition condition = conditions.get(type);
                    condition.signalAll();
                } finally {
                    messagesLock.unlock();
                }
            }
        } finally {
            readLock.unlock();
        }
    }

    public boolean isOpen() {
        return socket.isConnected();
    }

    public void close() throws IOException {
        this.socket.close();
        this.in.close();
        this.out.close();
    }
}