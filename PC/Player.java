public class Player {
    private int pos_x;
    private int pos_y;
    private int direction_x;
    private int direction_y;

    public Player(int pos_x, int pos_y, int direction_x, int direction_y) {
        this.pos_x = pos_x;
        this.pos_y = pos_y;
        this.direction_x = direction_x;
        this.direction_y = direction_y;
    }

    public int[] getPointsX(int eye_size) {
        int p_x = this.pos_x + this.direction_x;

        int p1_x = this.pos_x - direction_y;

        int p2_x = this.pos_x + direction_y;

        if (this.direction_x > 0) {
            p_x += eye_size;
        } else {
            p_x -= eye_size;
        }

        int[] points_x = {p1_x, p2_x, p_x};
        return points_x;
    }

    public int[] getPointsY(int eye_size) {
        int p_y = this.pos_y + this.direction_y;

        int p1_y = this.pos_y + direction_x;

        int p2_y = this.pos_y - direction_x;

        if (this.direction_y > 0) {
            p_y += eye_size;
        } else {
            p_y -= eye_size;
        }

        int[] points_y = {p1_y, p2_y, p_y};
        return points_y;
    }

    public int getPos_x() {
        return pos_x;
    }

    public void setPos_x(int pos_x) {
        this.pos_x = pos_x;
    }

    public int getPos_y() {
        return pos_y;
    }

    public void setPos_y(int pos_y) {
        this.pos_y = pos_y;
    }

    public int getDirection_x() {
        return direction_x;
    }

    public void setDirection_x(int direction_x) {
        this.direction_x = direction_x;
    }

    public int getDirection_y() {
        return direction_y;
    }

    public void setDirection_y(int direction_y) {
        this.direction_y = direction_y;
    }
}
