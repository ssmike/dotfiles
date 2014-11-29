#include "mainwindow.h"
#include "ui_mainwindow.h"
#include <QTimer>
#include <QtOpenGL>
#include <cstdio>

//timerF

double angle = 0;
int bluethreshold = 0;

MainWindow::MainWindow(QWidget *parent) :
    QGLWidget(parent)
{
    QTimer * q = new QTimer();
    connect(q, SIGNAL(timeout()), this, SLOT(timerCallback()));
    q->start(40);
}

void MainWindow::initializeGL() {
    glClearColor(0, 0, 0, 1);
    //glEnable(GL_DEPTH_TEST);
}

void MainWindow::paintGL() {
    //fprintf(stderr, "aaa\n");
    glClear(GL_COLOR_BUFFER_BIT);
    //glFlush();
    glPushMatrix();
    glRotatef(1, 0, 0, angle);
    //if (bluethreshold < 40)
    glColor3f(1, 0, 0);
    //else
    //    glColor3f(0, 1, 0);
    glBegin(GL_TRIANGLES);
        glVertex3f(0, 0, 1);
        glVertex3f(0, 1, 0);
        glVertex3f(1, 0, 0);
    glEnd();
    //glColor3f(0, 1, 0);
    //glBegin(GL_TRIANGLES);
    //    glVertex3f(0, 0, -1);
    //    glVertex3f(0, -1, 0);
    //    glVertex3f(-1, 0, 0);
    //glEnd();
    glFlush();
    swapBuffers();
    glPopMatrix();
}

void MainWindow::resizeGL(int w, int h) {
    glViewport(0, 0, w, h);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(-10, 10, -10, 10, -10, 10);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
}

void MainWindow::timerCallback() {
    //fprintf(stderr, "bbb\n");
    angle += 0.1;
    bluethreshold++;
    updateGL();
}

MainWindow::~MainWindow()
{
}
