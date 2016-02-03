#!/usr/bin/env python2
# -*- coding: utf-8 -*-

# From http://mad.unserver.de/2010/xmonad/xmenud/
# xmenud - a small desktop menu
#

# for launching the app
import subprocess

# for drawing the stuff
import gtk

# for catching the error
import glib

# for reading that stuff
import xdg.Menu
import xdg.DesktopEntry

# for finding that stuff to draw
import xdg.IconTheme

# for finding what stuff to do
import getopt

# for not doing anything anymore
import sys

# regular expressions for funny parsing
import re

NAME="xmenud"
VERSION="0.8"
AUTHOR="Matthias Kühlke"
EMAIL="mad@unserver.de"
YEAR="2010"
TAGLINE="A desktop menu, with klickibunti."
LICENSE='''
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
'''

def error(string):
    ''' output errors to stderr '''
    print >>sys.stderr, string

def launcher_execute(string):
    try:
        subprocess.Popen(string, shell=True)
    except:
        # well, the user probably doesn't want anything to happen, so I'll just
        pass

def launcher_print(string):
    print string

def create_menu(menus, use_icons=True, launch=launcher_execute):
    def launch_callback(widget, string):
        launch(string)

    def get_exec(string, terminal=False):
        ''' Parses the string according to the XDG Desktop Entry Specifications. '''
        r1 = re.compile('(?<!%)%[fFuUdDnNickvm]')
        r2 = re.compile('%%')
        result=r2.sub('%', r1.sub('', string))
        if(terminal):
            result = 'urxvt -e "%s"' % result
        return result

    def new_item(label, icon, use_icons):
        def get_icon(iconname):
            if (iconname=="" or iconname.find('.')<>-1):
                try:
                    print (xdg.IconTheme.getIconPath(iconname))
                    pixbuf = gtk.gdk.pixbuf_new_from_file(xdg.IconTheme.getIconPath(iconname))
                    ick = gtk.IconSet(pixbuf)
                    scaled = ick.render_icon(gtk.Style(), gtk.TEXT_DIR_LTR, gtk.STATE_NORMAL, gtk.ICON_SIZE_LARGE_TOOLBAR, None, None)
                    img = gtk.image_new_from_pixbuf(scaled)
                except (TypeError, glib.GError):
                    img = gtk.image_new_from_stock(gtk.STOCK_DIALOG_QUESTION, gtk.ICON_SIZE_LARGE_TOOLBAR)
            else:
                img = gtk.image_new_from_icon_name(iconname, gtk.ICON_SIZE_LARGE_TOOLBAR)
            return img
        if use_icons:
            item = gtk.ImageMenuItem(stock_id=label)
            item.set_image(get_icon(icon))
        else:
            item = gtk.MenuItem(label=label)
        return item


    themenu = gtk.Menu()
    for menu in menus:
      for entry in menu.getEntries():
          if isinstance(entry, xdg.Menu.Menu):
              item = new_item(entry.getName(), entry.getIcon(), use_icons)
              submenu = create_menu([entry], use_icons, launch)
              item.set_submenu(submenu)
              themenu.append(item)
              item.set_tooltip_text(entry.getComment())
              item.show()
          elif isinstance(entry, xdg.Menu.MenuEntry):
              item = new_item(entry.DesktopEntry.getName(), entry.DesktopEntry.getIcon(), use_icons)
              item.connect("activate", launch_callback, get_exec(entry.DesktopEntry.getExec(), entry.DesktopEntry.getTerminal()))
              themenu.append(item)
              item.set_tooltip_text(entry.DesktopEntry.getComment())
              item.show()

    themenu.show()
    return themenu

def create_popup():
    m=gtk.Menu()
    about = gtk.ImageMenuItem(stock_id=gtk.STOCK_ABOUT)
    quit = gtk.ImageMenuItem(stock_id=gtk.STOCK_QUIT)
    about.connect('activate', lambda w: about_dialog())
    quit.connect('activate', lambda w: gtk.main_quit())
    m.append(about)
    m.append(quit)
    about.show()
    quit.show()
    return m

def about_dialog():
    def close(w, r):
        if r == gtk.RESPONSE_CANCEL:
            w.hide()

    d = gtk.AboutDialog()
    d.set_name(NAME)
    d.set_version(VERSION)
    d.set_authors(['%s <%s>' % (AUTHOR,EMAIL)])
    d.set_copyright("(C) %s %s" % (YEAR,AUTHOR))
    d.set_license(LICENSE)
    d.connect('response', close)
    d.show()

def tray():
    i = gtk.StatusIcon()
    i.set_from_stock(gtk.STOCK_EXECUTE)
    i.set_tooltip("xmenud")
    i.set_visible(True)
    return i

def main():
    run_tray = False
    use_icons = True
    launch = launcher_execute
    try:
        opts, args = getopt.getopt(sys.argv[1:],"htvnp",["help", "tray", "version", "no-icons", "pipe-mode"])
    except getopt.GetoptError, err:
        error(str(err))
        usage()
        sys.exit(2)
    for o, a in opts:
        if o in ('-v', '--version'):
            showversion()
            sys.exit()
        elif o in ('-h', '--help'):
            usage(verbose=True)
            sys.exit()
        elif o in ('-t', '--tray'):
            run_tray = True
        elif o in ('-p', '--pipe-mode'):
            launch = launcher_print
        elif o in ('-n', '--no-icons'):
            use_icons = False

    #desktopmenu = xdg.Menu.parse("/etc/xdg/menus/xfce-applications.menu")
    #desktopmenu = xdg.Menu.parse("/etc/xdg/menus/kde4-applications.menu")
    #desktopmenu = xdg.Menu.parse("/etc/xdg/menus/kde-information.menu")
    #desktopmenu = xdg.Menu.parse("/etc/xdg/menus/gnome-applications.menu")
    m1 = xdg.Menu.parse("/etc/xdg/menus/lxqt-applications.menu")
    #m2 = xdg.Menu.parse("/etc/xdg/menus/xfce-applications.menu")
    #m3 = xdg.Menu.parse("/etc/xdg/menus/lxde-applications.menu")

    mainmenu = create_menu([m1], use_icons, launch)
    if run_tray:
        popupmenu=create_popup()
        trayicon=tray()
        trayicon.connect("activate", lambda w: mainmenu.popup(None, None, None, 0, 0))
        trayicon.connect("popup-menu", lambda w,b,t: popupmenu.popup(None, None, None, b, t))
    else:
        mainmenu.connect("hide", lambda w: gtk.main_quit())
        mainmenu.popup(None, None, None, 0, 0)
    try:
        gtk.main()
    except KeyboardInterrupt:
        pass
    return 0

def showversion():
    print '%s %s- %s' % (NAME, VERSION, TAGLINE)
    print ' Copyright (C) %s %s <%s>' % (YEAR, AUTHOR, EMAIL)
    print LICENSE

def usage(verbose=False):
    print 'usage: %s [--tray|--help] [--no-icons] [--pipe-mode] [--version]' % sys.argv[0]
    if verbose:
        print '''Options:
    --help,-h       This help message.
    --tray,-t       Instead of launching a menu right away, put an icon into the systray.
    --no-icons,-n   Don't load or show program icons.
    --pipe-mode,-p  Instead of launching a program, just output its name to stdout.
    --version,-v    Show version information.
'''


if __name__ == "__main__":
    main()

# vim: set et ts=4 sw=4:
