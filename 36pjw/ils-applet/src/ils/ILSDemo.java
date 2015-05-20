package ils;

/* Use like this:

<APPLET
  CODE     = "ils/ILSDemo.class"
  NAME     = "ILSDemo"
  WIDTH    = 532
  HEIGHT   = 556
  HSPACE   = 0
  VSPACE   = 0
  ALIGN    = top
>

*/

import java.net.URL;
import java.awt.*;
import java.awt.event.*;
import java.awt.font.*;
import java.awt.geom.*;
import java.applet.*;
import javax.swing.*;

/**
 * Title:        ILS demonstration
 * Description:
 * Copyright:    Copyright (c) 2001
 * Company:      FEL CVUT
 * @author Petr Kadlec
 * @version 1.01
 */

/**
 * A simple interface to allow a paint call to be distributed to another class.
 */
interface PaintListener {
  /**
   * This method is called when the sender has finished its own paint routine.
   * @param sender The originator of the event
   * @g The Graphics instance that have been passed to the paint method.
   */
  void paintEvent(Object sender, Graphics g);
}

/** The main class of the applet. */
public class ILSDemo extends Applet implements MouseListener,
       MouseMotionListener, PaintListener, KeyListener {

  private MediaTracker tracker;

  private PicturePanel pnlHorizMap, pnlVertMap;
  private Altimeter altimeter;
  private HSI hsi;
  private MarkerPanel markers;

  private Image iconHoriz, iconLeft, iconRight;

  private int hx, hy; // horizontal location [px]
  private int vx, vy; // vertical location [px]
  private float hdg;  // heading [rad mag]
  private float pitch; // pitch [rad]
  private AffineTransform xform_ih, xform_iv;

  private int lasthx, lasthy, lastvx, lastvy; // after the last updateState()

  /**Initialize the applet*/
  public void init() {
    try {
      this.setLayout(new FlowLayout(FlowLayout.LEFT, 0, 0));

      tracker = new MediaTracker(this);
      final URL cb = getCodeBase();

      // map images
      Image horizMap = getImage(cb, "media/horzmap.png");
      tracker.addImage(horizMap, 0);
      Image vertMap = getImage(cb, "media/vertmap.png");
      tracker.addImage(vertMap, 0);

      // airplane icons
      tracker.addImage(iconHoriz = getImage(cb, "media/airplaneh.png"), 0);
      tracker.addImage(iconLeft = getImage(cb, "media/airplanel.png"), 0);
      tracker.addImage(iconRight = getImage(cb, "media/airplaner.png"), 0);

      // instruments
      altimeter = new Altimeter(this, tracker, cb);
      hsi = new HSI(this, tracker, cb);
      markers = new MarkerPanel(this, tracker, cb);

      // listeners
      altimeter.addMouseListener(this);
      altimeter.addMouseMotionListener(this);
      altimeter.addKeyListener(this);
      hsi.addMouseListener(this);
      hsi.addMouseMotionListener(this);
      hsi.addKeyListener(this);
      markers.addMouseListener(this);
      markers.addMouseMotionListener(this);
      markers.addKeyListener(this);
      this.addMouseListener(this);
      this.addMouseMotionListener(this);
      this.addKeyListener(this);

      // now load all images
      tracker.waitForAll();
      if (tracker.isErrorAny())
        throw new Exception("Error loading images");

      // add all components
      this.add(pnlHorizMap = new PicturePanel(this, horizMap));
      this.add(pnlVertMap = new PicturePanel(this, vertMap));
      this.add(hsi);
      this.add(altimeter);
      this.add(markers);

      // the rest of listeners
      pnlHorizMap.addMouseListener(this);
      pnlHorizMap.addMouseMotionListener(this);
      pnlHorizMap.setPaintListener(this);
      pnlHorizMap.addKeyListener(this);
      pnlVertMap.addMouseListener(this);
      pnlVertMap.addMouseMotionListener(this);
      pnlVertMap.setPaintListener(this);
      pnlVertMap.addKeyListener(this);

      // pack the components
      this.validate();
    }
    catch(Exception e) {
      e.printStackTrace();
    }

    // initial settings
    hdg = (float) Math.toRadians(-117.0f); // 243 deg
    pitch = 0.0f;
    hx = 454; hy = 39;
    vy = 45;
    updateState();
  }

  /**Start the applet*/
  public void start() {
    markers.start();  // enable the marker sounds
    repaint();
  }

  /**Stop the applet*/
  public void stop() {
    markers.stop();   // disable the marker sounds
  }

  /**Get Applet information*/
  public String getAppletInfo() {
    return "ILS Demonstration\nVersion 1.01\nCopyright (C) Petr Kadlec, 2001";
  }

  /**
   * Computes distance between two points on horizmap.
   * @returns Distance (nm) between the specified points.
   */
  private double distance(int x1, int y1, int x2, int y2) {
    final int dx = x2 - x1;
    final int dy = y2 - y1;

    return 0.0357432f * Math.sqrt(dx*dx + dy*dy);
  }

  /**
   * Computes distance between two points on horizmap plus an altitude
   * difference.
   * @returns Distance (nm) between the specified points.
   */
  private double distance(int x1, int y1, int x2, int y2, int alty) {
    final float M = 0.036f;
    final int dx = x2 - x1;
    final int dy = y2 - y1;
    final double dz = (163 - alty)*0.004f/M;

    return M * Math.sqrt(dx*dx + dy*dy + dz*dz);
  }

  /** Updates the internal variables when a change has occurred */
  private void updateState() {
    hdg = (float)Math.IEEEremainder(hdg, 2*Math.PI); // hdg => [-pi/2; pi/2)

    int x1, y1, x2, y2;

    // xform for iconHoriz
    xform_ih = AffineTransform.getTranslateInstance(hx, hy);
    xform_ih.rotate(hdg, 8, 8);
    x1 = Math.min(hx, lasthx);
    y1 = Math.min(hy, lasthy);
    x2 = Math.max(hx + 16, lasthx + 16);
    y2 = Math.max(hy + 16, lasthy + 16);
    pnlHorizMap.repaint(x1, y1, x2 - x1 + 1, y2 - y1 + 1); // repaint the icon

    // recompute the horizontal icon position to the vertical icon position
    vx = 95 + (int)((((hx - 132) * 322 - (hy - 183) * 144))/307.0f);
    if (vy > 163) { // ground level
      vy = 163;
      if (pitch < 0.0f) pitch = 0.0f; // "touchdown"
    }

    if (pitch > 1.0f) pitch = 1.0f;
    else if (pitch < -1.0f) pitch = -1.0f; // pitch limits

    // xform for iconLeft/iconRight
    x1 = Math.min(vx, lastvx);
    y1 = Math.min(vy, lastvy);
    x2 = Math.max(vx + 16, lastvx + 16);
    y2 = Math.max(vy + 16, lastvy + 16);
    xform_iv = AffineTransform.getTranslateInstance(vx, vy);
    xform_iv.rotate(pitch, 8, 8);
    pnlVertMap.repaint(x1, y1, x2 - x1 + 1, y2 - y1 + 1); // repaint the icon

    // altitude
    altimeter.setAltitude((int)(1158 + (163 - vy) * 24.67f));

    // HSI
    //  - localizer deviation
    int dX = hx - 82;
    int dY = hy - 209;
    double len = Math.sqrt(dX*dX + dY*dY);
    double dLoc = -Math.toDegrees(Math.asin((dX*124 + dY*271)/(298.02f*len)));
    if (dX < 0) dLoc = -dLoc; // back-course approach
    if (Math.abs(dLoc) > 20.0f) dLoc = hsi.NO_SIGNAL;
    else if (Math.abs(dLoc) > 5.0f) dLoc = 5.0f * dLoc / Math.abs(dLoc);
    else if (dLoc > 5.0f) dLoc = 5.0f;
    else if (dLoc < -5.0f) dLoc = -5.0f;
    //  - glideslope deviation
    dX = vx - 100;
    dY = vy - 163;
    len = Math.sqrt(dX*dX + dY*dY);
    double dGS = Math.toDegrees(Math.asin((dX*118 + dY*270)/(294.66f*len)));
    if (Math.abs(dGS) > 20.0f) dGS = hsi.NO_SIGNAL;
    else if (dGS > 5.0f) dGS = 5.0f;
    else if (dGS < -5.0f) dGS = -5.0f;

    hsi.setValues(hdg, (float)dLoc, (float)dGS);

    // markers
    markers.setValues(false,
                      distance(hx, hy, 152, 176) <= 0.357f,
                      distance(hx, hy, 239, 137) <= 0.5f,
                      (float)distance(hx, hy, 132, 183, vy));

    // store the values for the next frame
    lasthx = hx; lasthy = hy; lastvx = vx; lastvy = vy;
  }

  /**
   * Called when a map has been redrawn and an icon can be drawn upon it.
   * @param sender Object from which did this event come.
   * @param g It is required for this to be in fact a Graphics2D !
   */
  public void paintEvent(Object sender, Graphics g) {
    Graphics2D g2D = (Graphics2D) g;

    if (sender == pnlHorizMap) {
      g2D.drawImage(iconHoriz, xform_ih, null);
    } else if (sender == pnlVertMap) {
      if (hdg > 0) g2D.drawImage(iconRight, xform_iv, null);
      else g2D.drawImage(iconLeft, xform_iv, null);
    }
  }

  /** Called when user has clicked the mouse. */
  public void mouseClicked(MouseEvent e) {
    final Object src = e.getSource();
    final boolean left = (e.getModifiers() & e.BUTTON1_MASK) != 0;
    final boolean right = (e.getModifiers() & e.BUTTON3_MASK) != 0;
    final int mx = e.getX() - 8;
    final int my = e.getY() - 8;

    if (!(left ^ right)) return; // exactly one button have to be pressed

    if (src == pnlHorizMap) {
      if (left) {
        hx = mx; hy = my;
      } else {
        hdg = (float)Math.atan2(mx - hx, hy - my);
      }
      updateState();
    } else if (src == pnlVertMap) {
      if (left) {
        vy = my;
      } else {
        if (mx == vx) return; // unable to turn nose straight down

        if ((hdg > 0.0) ^ (mx > vx)) return; // unable to turn nose backwards

        if (mx > vx) pitch = (float)Math.atan2(my - vy, mx - vx);
        else pitch = (float)Math.atan2(vy - my, vx - mx);
      }
      updateState();
    }
  }

  /** Called when user presses a key */
  public void keyPressed(KeyEvent e) {
    int vk = e.getKeyCode();

    if (vk == e.VK_LEFT)  hdg -= 0.03f;
    else if (vk == e.VK_RIGHT) hdg += 0.03f;

    else if (vk == e.VK_UP)    pitch -= 0.03f;
    else if (vk == e.VK_DOWN)  pitch += 0.03f;

    else if (vk == e.VK_SPACE) {
            hx += (int)(5 * Math.sin(hdg)*Math.cos(pitch));
            hy -= (int)(5 * Math.cos(hdg)*Math.cos(pitch));
            vy -= (int)(5 * Math.sin(pitch));
    } else return;

    updateState();
  }

  // ------ MouseListener, MouseMotionListener, KeyListener empty methods ------
  //** Empty method implementing a listener interface */
  public void mouseMoved(MouseEvent e) {
  }
  public void mouseExited(MouseEvent e) {
  }
  public void mouseEntered(MouseEvent e) {
  }
  public void mousePressed(MouseEvent e) {
  }
  public void mouseReleased(MouseEvent e) {
  }
  public void mouseDragged(MouseEvent e) {
  }
  public void keyReleased(KeyEvent e) {
  }
  public void keyTyped(KeyEvent e) {
  }
}

/** Altimeter instrument component */
class Altimeter extends Panel {
  private Image background;
  private Image needle_100, needle_1000, needle_10000;
  private int altitude;

  private AffineTransform xform_100, xform_1000, xform_10000;

  /**
   * Set altitude to be displayed
   * @param alt New altitude in feet
   */
  public void setAltitude(int alt) {
    if (alt < 0) alt = 0; // do not allow negative altitudes
    if (alt != altitude) {
      altitude = alt;

      double a, r;

      a = altitude / 1000.0;
      r = Math.PI * ((a - Math.floor(a)) * 2 - 0.5);
      xform_100 = AffineTransform.getRotateInstance(r, 50, 55);
      xform_100.translate(50-18, 55-3);

      a /= 10.0;
      r = Math.PI * ((a - Math.floor(a)) * 2 - 0.5);
      xform_1000 = AffineTransform.getRotateInstance(r, 50, 55);
      xform_1000.translate(50-4, 55-5);

      a /= 10.0;
      r = Math.PI * ((a - Math.floor(a)) * 2 - 0.5);
      xform_10000 = AffineTransform.getRotateInstance(r, 50, 55);
      xform_10000.translate(50-3, 55-4);

      repaint();
    }
  }

  /**
   * Create the altimeter component.
   * @param parent Applet into which does the component belong.
   * @param tracker MediaTracker which is used to load images.
   * @param baseurl Base URL, relative to which can the images be retrieved.
   */
  Altimeter(Applet parent, MediaTracker tracker, URL baseurl) {
    super(null); // no layout manager

    setSize(100, 112);
    altitude = -1;
    setAltitude(0);

    tracker.addImage(
         background = parent.getImage(baseurl, "media/altmeter.png"),
         0);
    tracker.addImage(
         needle_100 = parent.getImage(baseurl, "media/alt100.png"),
         0);
    tracker.addImage(
         needle_1000 = parent.getImage(baseurl, "media/alt1000.png"),
         0);
    tracker.addImage(
         needle_10000 = parent.getImage(baseurl, "media/alt10000.png"),
         0);
  }

  /**
   * Repaint the altimeter
   * @param g The altimeter requires this to be in fact a Graphics2D !
   */
  public void paint(Graphics g) {
    Graphics2D g2D = (Graphics2D) g;

    g2D.drawImage(background, 0, 0, null);
    g2D.drawImage(needle_10000, xform_10000, null);
    g2D.drawImage(needle_1000, xform_1000, null);
    g2D.drawImage(needle_100, xform_100, null);
  }
}

/** Horizontal situation indicator instrument component */
class HSI extends Panel {
  private Image background, compass, noNAVflag, leftGS, rightGS, plane, needle;
  private float heading, deltaLoc, deltaGS;

  private AffineTransform xform_compass, xform_needle;
  private int offsetGS;

  private boolean noLSignal, noGSignal;

  /** When receiving no Loc/GS signal, pass NO_SIGNAL as the deviation value */
  public final float NO_SIGNAL = Float.MAX_VALUE;

  /**
   * Set the displayed HSI values.
   * @param hdg Heading (rad mag)
   * @param dLoc Localizer deviation (degrees)
   * @param dGS Glideslope deviation (degrees)
   */
  public void setValues(float hdg, float dLoc, float dGS) {
    heading = hdg;
    deltaLoc = dLoc;
    deltaGS = dGS;

    xform_compass = AffineTransform.getTranslateInstance(16, 15);
    xform_compass.rotate(-hdg, 43, 43);

    if (!(noLSignal = dLoc >= NO_SIGNAL)) {
      xform_needle = AffineTransform.getTranslateInstance(57, 40);
      xform_needle.rotate(-(hdg - Math.toRadians(244)), 2, 19.5);
      xform_needle.translate(-4 * dLoc, 0);
    }

    if (!(noGSignal = dGS >= NO_SIGNAL)) {
      offsetGS = (int)(10.0f*(5.3f - dGS));
      if (offsetGS < 33) offsetGS = 33;
      else if (offsetGS > 75) offsetGS = 75;
    }

    repaint();
  }

  /**
   * Create the HSI component.
   * @param parent Applet into which does the component belong.
   * @param tracker MediaTracker which is used to load images.
   * @param baseurl Base URL, relative to which can the images be retrieved.
   */
  HSI(Applet parent, MediaTracker tracker, URL baseurl) {
    super(null); // no layout manager

    setSize(118, 112);
    setValues(243, NO_SIGNAL, NO_SIGNAL);

    tracker.addImage(
         background = parent.getImage(baseurl, "media/hsi_back.png"),
         0);
    tracker.addImage(
         compass = parent.getImage(baseurl, "media/hsi_compass.png"),
         0);
    tracker.addImage(
         noNAVflag = parent.getImage(baseurl, "media/hsi_flag.png"),
         0);
    tracker.addImage(
         leftGS = parent.getImage(baseurl, "media/hsi_gsleft.png"),
         0);
    tracker.addImage(
         rightGS = parent.getImage(baseurl, "media/hsi_gsright.png"),
         0);
    tracker.addImage(
         plane = parent.getImage(baseurl, "media/hsi_plane.png"),
         0);
    tracker.addImage(
         needle = parent.getImage(baseurl, "media/hsi_needle.png"),
         0);
  }

  /**
   * Repaint the HSI
   * @param g The HSI requires this to be in fact a Graphics2D !
   */
  public void paint(Graphics g) {
    Graphics2D g2D = (Graphics2D) g;

    g2D.drawImage(background, 0, 0, null);
    g2D.drawImage(compass, xform_compass, null);

    if (noLSignal) {
      g2D.drawImage(noNAVflag, 20, 10, null);
    } else {
      g2D.drawImage(needle, xform_needle, null);
      if (!noGSignal) {
        g2D.drawImage(leftGS, 12, offsetGS, null);
        g2D.drawImage(rightGS, 102, offsetGS, null);
      }
    }

    g2D.drawImage(plane, 0, 0, null);
  }
}

/**
 * A component for marker panel with a DME display.
 */
class MarkerPanel extends Panel {
  private Image imgIMon, imgIMoff, imgMMon, imgMMoff, imgOMon, imgOMoff;
  private Image background;
  private AudioClip audIM, audMM, audOM;
  private Font fontDME;
  private FontRenderContext frc;

  private boolean activeIM, activeMM, activeOM;
  private float distDME;

  private boolean stopped;

  /**
   * Set the situation
   * @param im IM signal being received
   * @param mm MM signal being received
   * @param om OM signal being received
   * @param dme Displayed DME distance (nm)
   */
  public void setValues(boolean im, boolean mm, boolean om, float dme) {
    if (dme < 0.0f) dme = 0.0f;
    else if (dme > 99.9f) dme = 99.9f;
    if (im != activeIM || mm != activeMM || om != activeOM || dme != distDME) {
      activeIM = im;
      activeMM = mm;
      activeOM = om;
      distDME = dme;

      if (!stopped) {
        stop();
        start();
      }
      repaint();
    }
  }

  /**
   * Enable marker sounds.
   */
  public void start() {
    if (activeIM) audIM.loop();
    if (activeMM) audMM.loop();
    if (activeOM) audOM.loop();

    stopped = false;
  }

  /**
   * Disable marker sounds.
   */
  public void stop() {
    stopped = true;

    audIM.stop();
    audMM.stop();
    audOM.stop();
  }

  /**
   * Create the MarkerPanel component.
   * @param parent Applet into which does the component belong.
   * @param tracker MediaTracker which is used to load images and sounds.
   * @param baseurl Base URL, relative to which can the images and sounds be
   * retrieved.
   */
  MarkerPanel(Applet parent, MediaTracker tracker, URL baseurl) {
    super(null); // no layout manager

    setSize(110, 41);

    audIM = parent.getAudioClip(baseurl, "media/im.wav");
    audMM = parent.getAudioClip(baseurl, "media/mm.wav");
    audOM = parent.getAudioClip(baseurl, "media/om.wav");

    // because MediaTracker does not support loading audio clips, we use a trick
    // to force their load
    audIM.play(); audIM.stop();
    audMM.play(); audMM.stop();
    audOM.play(); audOM.stop();

    tracker.addImage(
         background = parent.getImage(baseurl, "media/mrkr_back.png"),
         0);
    tracker.addImage(
         imgIMon = parent.getImage(baseurl, "media/im_on.png"),
         0);
    tracker.addImage(
         imgIMoff = parent.getImage(baseurl, "media/im_off.png"),
         0);
    tracker.addImage(
         imgMMon = parent.getImage(baseurl, "media/mm_on.png"),
         0);
    tracker.addImage(
         imgMMoff = parent.getImage(baseurl, "media/mm_off.png"),
         0);
    tracker.addImage(
         imgOMon = parent.getImage(baseurl, "media/om_on.png"),
         0);
    tracker.addImage(
         imgOMoff = parent.getImage(baseurl, "media/om_off.png"),
         0);

    fontDME = new Font("Monospaced", Font.BOLD, 12);
    frc = new FontRenderContext(null, true, true);
  }

  /**
   * Repaint the MarkerPanel
   */
  public void paint(Graphics g) {
    g.drawImage(background, 0, 0, null);

    if (activeIM)
      g.drawImage(imgIMon, 9, 5, null);
    else
      g.drawImage(imgIMoff, 9, 5, null);

    if (activeMM)
      g.drawImage(imgMMon, 9, 16, null);
    else
      g.drawImage(imgMMoff, 9, 16, null);

    if (activeOM)
      g.drawImage(imgOMon, 9, 27, null);
    else
      g.drawImage(imgOMoff, 9, 27, null);

    int d = (int)Math.floor(distDME);
    String s = Integer.toString(d/10 % 10) +
               Integer.toString(d % 10) +
               "." +
               Integer.toString((int)(10*(distDME-d)));

    g.setFont(fontDME);
    g.setColor(Color.red);
    LineMetrics lm = fontDME.getLineMetrics(s, frc);
    g.drawString(s, 45, (int)(7 + lm.getAscent()));
  }
}

/**
 * A component used to display a static image.
 */
class PicturePanel extends Panel {
  private Image picture;

  private PaintListener paintListener;

  /**
   * Create the picture panel component.
   * @param parent Applet into which does the component belong.
   * @param picture Image which is to be displayed by this panel.
   * It must be already loaded (i.e. its size have to be known)
   */
  PicturePanel(Applet parent, Image picture) {
    super(null); // no layout manager

    this.picture = picture;
    picture.getWidth(null);
    this.setSize(picture.getWidth(null), picture.getHeight(null));
  }

  /** Repaint the panel */
  public void paint(Graphics g) {
    g.drawImage(picture, 0, 0, null);

    if (paintListener != null)
      paintListener.paintEvent(this, g);
  }

  /**
   * Set the listener of the paint events of this object. Only one object may
   * listen to the paint event at a time.
   * @param listener The new listener.
   */
  public void setPaintListener(PaintListener listener) {
    paintListener = listener;
  }
}