import javax.swing.*;
import java.awt.*;

public class CircleEye extends JLabel {
    private String type;
    private int radius;
    private int pos_center_x;
    private int pos_center_y;
    private int direcao_x;
    private int direcao_y;

    public CircleEye(Circle c) {
        this.type = c.getType();
        this.radius = c.getRadius();
        this.pos_center_x = c.getPos_center_x();
        this.pos_center_y = c.getPos_center_y();
        this.direcao_x = c.getDirecao_x();
        this.direcao_y = c.getDirecao_y();

        setBounds(0, 0, 700, 700);
    }

    public void paint(Graphics gh) {
        super.paint(gh);

        if (type == "player") {
            gh.setColor(Color.CYAN);
        }

        else if (type == "enemy") {
            gh.setColor(Color.ORANGE);
        }

        int p_x = this.pos_center_x + this.direcao_x;
        int p_y = this.pos_center_y + this.direcao_y;

        int p1_x = this.pos_center_x - direcao_y;
        int p1_y = this.pos_center_y + direcao_x;

        int p2_x = this.pos_center_x + direcao_y;
        int p2_y = this.pos_center_y - direcao_x;

        if (this.direcao_x > 0) {
            p_x += (radius * 0.32);
        } else {
            p_x -= (radius * 0.32);
        }

        if (this.direcao_y > 0) {
            p_y += (radius * 0.32);
        } else {
            p_y -= (radius * 0.32);
        }

        int[] points_x = {p1_x, p2_x, p_x};
        int[] points_y = {p1_y, p2_y, p_y};

        Polygon triangle = new Polygon(points_x, points_y, 3);
        gh.fillPolygon(triangle);
    }
}
