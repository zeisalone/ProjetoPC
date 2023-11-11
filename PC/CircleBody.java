import javax.swing.*;
import java.awt.*;

public class CircleBody extends JLabel {
    private String type;
    private int radius;
    private int pos_center_x;
    private int pos_center_y;

    public CircleBody(Circle c) {
        this.type = c.getType();
        this.radius = c.getRadius();
        this.pos_center_x = c.getPos_center_x();
        this.pos_center_y = c.getPos_center_y();
        setBounds(this.pos_center_x - this.radius, this.pos_center_y - this.radius, this.radius * 2, this.radius * 2);
    }

    public CircleBody(String type, int pos_center_x, int pos_center_y, int radius) {
        this.type = type;
        this.radius = radius;
        this.pos_center_x = pos_center_x;
        this.pos_center_y = pos_center_y;
        setBounds(this.pos_center_x - this.radius, this.pos_center_y - this.radius, this.radius * 2, this.radius * 2);
    }

    public void paint(Graphics gh) {
        super.paint(gh);

        if (type == "player") {
            gh.setColor(Color.CYAN);
        }

        else if (type == "enemy") {
            gh.setColor(Color.ORANGE);
        }

        else if (type == "itemAzul") {
            gh.setColor(Color.BLUE);
        }

        else if (type == "itemVermelho") {
            gh.setColor(Color.RED);
        }

        else if (type == "itemVerde") {
            gh.setColor(Color.GREEN);
        }

        gh.fillOval(0, 0, getWidth(), getHeight());
    }
}
