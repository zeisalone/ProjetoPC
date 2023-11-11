import javax.swing.*;
import java.awt.*;

public class Circle {
    private String type;
    private int radius;
    private int pos_center_x;
    private int pos_center_y;
    private int direcao_x;
    private int direcao_y;

    public Circle(String type, int radius, int pos_center_x, int pos_center_y, float direcao_x, float direcao_y) {
        this.type = type;
        this.radius = radius;
        this.pos_center_x = pos_center_x;
        this.pos_center_y = pos_center_y;
        this.direcao_x = Math.round(direcao_x);
        this.direcao_y = Math.round(direcao_y);
    }

    public String getType() {
        return type;
    }

    public int getRadius() {
        return radius;
    }

    public int getPos_center_x() {
        return pos_center_x;
    }

    public int getPos_center_y() {
        return pos_center_y;
    }

    public int getDirecao_x() {
        return direcao_x;
    }

    public int getDirecao_y() {
        return direcao_y;
    }

    public void setRadius(int radius) {
        this.radius = radius;
    }

    public void setPos_center_x(int pos_center_x) {
        this.pos_center_x = pos_center_x;
    }

    public void setPos_center_y(int pos_center_y) {
        this.pos_center_y = pos_center_y;
    }

    public void setDirecao_x(int direcao_x) {
        this.direcao_x = direcao_x;
    }

    public void setDirecao_y(int direcao_y) {
        this.direcao_y = direcao_y;
    }
}