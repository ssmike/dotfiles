#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QGLWidget>

class MainWindow : public QGLWidget
{
    Q_OBJECT

public:
    explicit MainWindow(QWidget *parent = 0);
    ~MainWindow();

private:
    void initializeGL();
    void paintGL();
    void resizeGL(int w, int h);
protected slots:
    void timerCallback();
};

#endif // MAINWINDOW_H
