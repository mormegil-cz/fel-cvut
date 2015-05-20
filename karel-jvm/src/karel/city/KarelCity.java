package karel.city;

import java.io.*;
import java.util.*;
import java.awt.*;
import java.awt.event.*;

/**
 * Title:        Karel for Java VM
 * Description:  Karel compiler & GUI for Java Virtual Machine.
 * Copyright:    Copyright (c) 2001
 * Company:
 * @author Petr Kadlec
 * @version 1.0
 */

/**
 * Universal class for displaying city of Karel the robot.
 */
public class KarelCity {

  /**
   * The city must be repainted within MAX_DISPLAY_LAG milliseconds after
   * a change has occurred. Set to 0 to force immediate repaints.
   */
  public int MAX_DISPLAY_LAG = 100; // [ms]
  /**
   * A delay to be introduced in every updateKarel call. (in milliseconds)
   */
  public int UPDATE_DELAY    = 300; // [ms]

  /**
   * Base URL for images.
   */
  public String imgBaseURL   = "img/";

  /** true = the city field is free (not a wall) */
  boolean[][] cityFields;
  /** number of beepers on a field */
  int[][]     beepers;

  /** Last known Karel state */
  int karelX, karelY, karelDir, karelBag;
  /** Initial Karel state */
  private int initKarelX, initKarelY, initKarelDir, initKarelBag;

  /** The frame in which is the city displayed */
  private MainFrame mainFrame;

  /**
   * Constructor of the city. All city loading/editing, etc. occurs here.
   * @param args The arguments to the city. Normally given from the command
   * line. The command-line syntax is as follows:<BR>
   * <TT><I>program-name</I> [ options ] [ cityfile ]</TT><BR>
   * Where options are:
   * <TABLE>
   * <TR><TD><TT>-maxlag</TT> <I>lag</I></TD><TD>Sets the MAX_DISPLAY_LAG (see above)</TD></TR>
   * <TR><TD><TT>-updatesleep</TT> <I>sleep</I></TD><TD>Sets the UPDATE_DELAY (see above)</TD></TR>
   * <TR><TD><TT>-maxfps</TT> <I>fps</I></TD><TD>Sets the UPDATE_DELAY, so that the FPS does not exceed <I>fps</I>.</TD></TR>
   * <TR><TD><TT>-imgbase</TT> <I>URL</I></TD><TD>Sets imgBaseURL (see above)</TD></TR>
   * </TABLE>
   * @throws java.lang.IllegalArgumentException When the args field contains invalid arguments.
   * @see #MAX_DISPLAY_LAG
   * @see #UPDATE_DELAY
   * @see #imgBaseURL
   */
  public KarelCity ( java.lang.String[] args ) {
    mainFrame = new MainFrame(this, "Karel City");

    String cityFilename = null;

    for (int i = 0; i < args.length; i++) {
      if (args[i].startsWith("-")) {
        // option
        if (args[i].equals("-maxlag")) {
           if ( i == args.length ) throw new IllegalArgumentException("Invalid syntax");
           i++;
           MAX_DISPLAY_LAG = Integer.parseInt(args[i]);
        } else
        if (args[i].equals("-updatesleep")) {
           if ( i == args.length ) throw new IllegalArgumentException("Invalid syntax");
           i++;
           UPDATE_DELAY = Integer.parseInt(args[i]);
        } else
        if (args[i].equals("-maxfps")) {
           if ( i == args.length ) throw new IllegalArgumentException("Invalid syntax");
           i++;
           MAX_DISPLAY_LAG = 1000 / Integer.parseInt(args[i]);
        } else
        if (args[i].equals("-imgbase")) {
           if ( i == args.length ) throw new IllegalArgumentException("Invalid syntax");
           i++;
           imgBaseURL = args[i];
        } else
          throw new IllegalArgumentException("Unknown option");
      } else {
        if (cityFilename != null) throw new IllegalArgumentException("Invalid syntax");
        cityFilename = args[i];
      }
    }

    // init the city (load/edit)
    if (cityFilename == null) {
       // open dialog: Load city (+Edit city?)
       InitDialog dlg = new InitDialog(mainFrame, this);
       dlg.show();
    } else {
       loadCity(args[0]);
    }

    // display the main frame
    mainFrame.show();
  }

  /**
   * When some Karel error has occurred, this method notifies the user.
   * @param e The exception that has been catched
   */
  public void exceptionCatched ( karel.common.KarelException e ) {
    (new MessageBox(mainFrame, "Karel Error", e.getMessage())).show();
    //e.printStackTrace();
  }

  /**
   * Get number of beepers on the specified location.
   * @param x X coordinate of the location.
   * @param y Y coordinate of the location.
   * @return Number of beepers on the location.
   */
  public int getBeepers ( int x, int y ) {
    return beepers[y][x];
  }

  /**
   * Set number of beepers on the specified location.
   * @param x X coordinate of the location.
   * @param y Y coordinate of the location.
   * @param beepers Number of beepers on the location.
   */
  public void setBeepers ( int x, int y, int beepers ) {
    this.beepers[y][x] = beepers;
    repaint(x, y);
  }

  /**
   * Updates the state of Karel.
   * @param x X coordinate of Karel.
   * @param y Y coordinate of Karel
   * @param direction Direction which is Karel facing:
   * <DL>
   * <DT>0</DT><DD>North (towards y=0)</DD>
   * <DT>1</DT><DD>West (towards x=0)</DD>
   * <DT>2</DT><DD>South</DD>
   * <DT>3</DT><DD>East</DD>
   * </DL>
   * @param bagSize Number of beepers inside Karel's bag
   * @see #UPDATE_DELAY
   */
  public void updateKarel ( int x, int y, int direction, int bagSize ) {
    final int oldX = karelX;
    final int oldY = karelY;
    karelX = x;
    karelY = y;
    karelDir = direction;
    karelBag = bagSize;
    repaint(oldX, oldY);
    repaint(karelX, karelY);
    try {
      Thread.sleep(UPDATE_DELAY);
    } catch (InterruptedException e) {
      e.printStackTrace();
      System.exit(-1);
    }
  }

  /**
   * Notify the user that Karel has turned itself off and close the frame.
   */
  public void turnOff () {
    (new MessageBox(mainFrame, "Done", "Karel has turned itself off.")).show();
    mainFrame.dispose();
  }

  /**
   * Is a field free (i.e. without a wall and inside the city) ?
   * @param x X coordinate of the field.
   * @param y Y coordinate of the field.
   * @return True if Karel can move on the field.
   */
  public boolean cityFieldFree ( int x, int y ) {
    return (x >= 0) && (y >= 0) && (y < cityFields.length) && (x < cityFields[y].length) && cityFields[y][x];
  }

  /**
   * Gets initial direction of Karel.
   * @return Initial direction Karel is facing after start.
   */
  public int getInitDirection () {
    return initKarelDir;
  }
  /**
   * Gets initial X coordinate of Karel.
   * @return X coordinate of the field Karel resides on after start.
   */
  public int getInitXPos () {
    return initKarelX;
  }
  /**
   * Gets initial Y coordinate of Karel.
   * @return Y coordinate of the field Karel resides on after start.
   */
  public int getInitYPos () {
    return initKarelY;
  }
  /**
   * Gets initial bag size of Karel.
   * @return Number of beepers Karel has in his bag after start.
   */
  public int getInitBagSize () {
    return initKarelBag;
  }

  //---------------------------------------------------------------------

  /**
   * Checks maximum X coordinate in the city.
   * @return X coordinate of the east-most field in the city.
   * @deprecated You should use {@link #cityFieldFree} for checking if a field
   * is inside the city.
   */
  public int getMaxXSize() {
    int max = 0;
    for (int i = 0; i < cityFields.length; i++) {
      int len = cityFields[i].length;
      if (len > max) max = len;
    }
    return max;
  }

  /**
   * Checks maximum Y coordinate in the city.
   * @return Y coordinate of the south-most field in the city.
   * @deprecated You should use {@link #cityFieldFree} for checking if a field
   * is inside the city.
   */
  public int getMaxYSize() {
    return cityFields.length;
  }

  //---------------------------------------------------------------------

  /** Forces the main frame to repaint the specified fields. */
  private void repaint(int xmin, int ymin, int xmax, int ymax) {
    if (mainFrame.minClipX < xmin) mainFrame.minClipX = xmin;
    if (mainFrame.maxClipX > xmax) mainFrame.maxClipX = xmax;
    if (mainFrame.minClipY < ymin) mainFrame.minClipY = ymin;
    if (mainFrame.maxClipY > ymax) mainFrame.maxClipY = ymax;
    if (MAX_DISPLAY_LAG > 0)
       mainFrame.repaint(MAX_DISPLAY_LAG);
    else
       mainFrame.repaint();
  }

  /** Forces the main frame to repaint the specified field. */
  private void repaint(int x, int y) {
    repaint(x, y, x, y);
  }

  //---------------------------------------------------------------------

  /** Loads a city from a file. */
  void loadCity(String filename) {
    try {
      FileReader f = new FileReader(filename);
      int ic;
      Vector lines = new Vector();
      StringBuffer sb = new StringBuffer();

      initKarelDir = -1;
      while ((ic = f.read()) != -1) {
        char c = (char)ic;

        switch (c) {
          case '\r':
          case '\n': if (sb.length() == 0) break; // ignore empty lines (and CR-LF pairs count as one...)
                     lines.add(sb);
                     sb = new StringBuffer(sb.length());
                     break;
          case 'K':  throw new IllegalArgumentException("The specified file is not a valid city specification");
          case '>':
          case '<':
          case 'A':
          case 'V': if (initKarelDir != -1)
                       throw new IllegalArgumentException("The specified file is not a valid city specification");
                    ic = f.read();
                    if (ic == -1) ic = '0';
                    initKarelBag = ((char)ic) - '0';
                    switch (c) {
                      case '>': initKarelDir++; // -1 -> 3
                      case 'V': initKarelDir++; // -1 -> 2
                      case '<': initKarelDir++; // -1 -> 1
                      case 'A': initKarelDir++; // -1 -> 0
                    }
                    c = 'K';
          default:  if (c != ' ' && c != '#' && c != 'K' && (c <= '0' || c >= '9'))
                       throw new IllegalArgumentException("The specified file is not a valid city specification");
                    sb.append(c);
                    break;
        }
      }
      if (initKarelDir == -1)
         throw new IllegalArgumentException("The specified file is not a valid city specification");
      if (sb.length() != 0) lines.add(sb);

      cityFields = new boolean[lines.size()][];
      beepers = new int[lines.size()][];

      for (int i=0; i<lines.size(); i++) {
        sb = (StringBuffer)lines.elementAt(i);
        cityFields[i] = new boolean[sb.length()];
        beepers[i] = new int[sb.length()];

        for (int j=0; j<sb.length(); j++) {
          char c = sb.charAt(j);
          if (c == 'K') {
            initKarelX = j;
            initKarelY = i;
            c = '0';
          }
          if (c == ' ') c = '0';
          if (c >= '0' && c <= '9') {
            beepers[i][j] = (c - '0');
            cityFields[i][j] = true;
          } else if (c == '#') {
            beepers[i][j] = 0;
            cityFields[i][j] = false;
          } /*WE CANNOT GET HERE!*/;
        }
      }
      karelX = initKarelX;
      karelY = initKarelY;
      karelDir = initKarelDir;
      karelBag = initKarelBag;
      mainFrame.restore();
    } catch(IOException e) {
      throw new IllegalArgumentException("The specified file cannot be read");
    }
  }
}

// **************************************************************************
// **************************************************************************

/**
 * This dialog lets user decide which city he wants to use.
 */
class InitDialog extends Dialog {
  Frame owner;
  KarelCity parentCity;

  CheckboxGroup grpRadios;
  Checkbox radLoad;
  Checkbox radEdit;

  /**
   * Constructs the dialog.
   * @param owner Frame owning this dialog.
   * @param parentCity KarelCity into which should the city be loaded.
   */
  InitDialog(Frame owner, KarelCity parentCity) {
    super(owner, "In which city do you want to run the program?", true);

    this.owner = owner;
    this.parentCity = parentCity;

    this.setLayout(new FlowLayout());
    this.setResizable(false);

    grpRadios = new CheckboxGroup();
    radLoad = new Checkbox("Load city from file", grpRadios, true);
    radEdit = new Checkbox("Edit city in editor", grpRadios, false);
    Button btnOK = new Button("OK");
    Button btnCancel = new Button("Cancel");

    btnOK.setSize(75, 25);
    btnCancel.setSize(75, 25);

    btnOK.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        btnOKClick();
      }
    });

    btnCancel.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        btnCancelClick();
      }
    });

    this.add(radLoad);
    this.add(radEdit);
    this.add(btnOK);
    this.add(btnCancel);

    this.setSize(270, 130);

    Dimension screensize = Toolkit.getDefaultToolkit().getScreenSize();
    this.setLocation(
      (screensize.width-this.getWidth())/2,
      (screensize.height-this.getHeight())/2
    );
  }

  /** The OK button has been clicked. */
  void btnOKClick() {
    if (grpRadios.getSelectedCheckbox() == radLoad) {
      // load city from file
      FileDialog dlgOpen = new FileDialog(owner, "Load city from file", FileDialog.LOAD);
      dlgOpen.show();
      String fileName = dlgOpen.getFile();
      String dirName = dlgOpen.getDirectory();
      if (fileName == null) return;
      if (dirName == null) dirName = "";
      try {
        parentCity.loadCity(dirName.concat(fileName));
      } catch (Exception e) {
        (new MessageBox(this, "Error", e.getMessage())).show();
      }
    } else {
      // edit city in editor
    }
    dispose();
  }

  /** The Cancel button has been clicked. */
  void btnCancelClick() {
    System.exit(10);
  }
}

// **************************************************************************
// **************************************************************************

/** Universal "OK" MessageBox dialog. */
class MessageBox extends Dialog {
  /** Constructs a dialog with a Frame as an owner. */
  MessageBox(Frame owner, String caption, String message) {
    super(owner, caption, true);
    init(message);
  }

  /** Constructs a dialog with a Dialog as an owner. */
  MessageBox(Dialog owner, String caption, String message) {
    super(owner, caption, true);
    init(message);
  }

  /** Initializes the dialog. */
  private void init(String message) {
    this.setLayout(new BorderLayout(5, 5));

    this.add(new Label(message, Label.CENTER), BorderLayout.CENTER);

    Button btnOK = new Button("OK");
    this.add(btnOK, BorderLayout.SOUTH);
    btnOK.setSize(75, 25);
    this.pack();

    Dimension screensize = Toolkit.getDefaultToolkit().getScreenSize();
    this.setLocation(
      (screensize.width-this.getWidth())/2,
      (screensize.height-this.getHeight())/2
    );

    btnOK.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        btnOKClick(e);
      }
    });

    this.addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
        dispose();
      }
    });
  }

  /** The OK button has been clicked */
  void btnOKClick(ActionEvent e) {
    dispose();
  }
}

// **************************************************************************
// **************************************************************************

/** The main frame displaying the Karel City. */
class MainFrame extends Frame {
  private static final int PIC_EMPTY = 4,
                           PIC_WALL  = 5;

  private final int field_x, field_y;

  // Clipping coordinates
  int minClipX, maxClipX, minClipY, maxClipY;

  private KarelCity parentCity;
  int xSize;
  int ySize;
  int top, left;

  private Image[] imgs;

  /** Constructs and initializes the frame. */
  MainFrame(KarelCity parentCity, String caption) {
    this.parentCity = parentCity;
    this.setTitle(caption);
    this.setResizable(false);

    this.addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
        System.exit(0);
      }
    });

    // load images
    Toolkit toolkit = Toolkit.getDefaultToolkit();
    MediaTracker tracker = new MediaTracker(this);

    imgs = new Image[6];
    for (int i = 0; i < 4; i++)
      tracker.addImage(imgs[i] = toolkit.getImage(parentCity.imgBaseURL.concat(Integer.toString(i)).concat(".png")), i);
    tracker.addImage(imgs[PIC_EMPTY] = toolkit.getImage(parentCity.imgBaseURL.concat("empty.png")), PIC_EMPTY);
    tracker.addImage(imgs[PIC_WALL]  = toolkit.getImage(parentCity.imgBaseURL.concat("wall.png")), PIC_WALL);
    try {
      tracker.waitForAll();
    } catch (InterruptedException e) {
      e.printStackTrace();
      System.exit(-1);
    }
    if (tracker.isErrorAny()) {
      (new MessageBox(this, "Error", "Error reading images")).show();
      System.exit(-1);
    }
    field_x = imgs[0].getWidth(null);
    field_y = imgs[0].getHeight(null);
  }

  /** (re)initializes the frame so that it has correct size. */
  public void restore() {
    this.pack();
    Insets border = this.getInsets();

    xSize = parentCity.getMaxXSize();
    ySize = parentCity.getMaxYSize();
    left  = border.left;
    top   = border.top;

    this.setSize(
      left+border.right+field_x*xSize,
      top+border.bottom+field_y*ySize
    );

    Dimension screensize = Toolkit.getDefaultToolkit().getScreenSize();

    this.setLocation(
      (screensize.width-this.getWidth())/2,
      (screensize.height-this.getHeight())/2
    );

    maxClipX = -1;

    repaint();
  }

  public void update(Graphics g) {
    // do NOT erase background!
    paint(g);
  }

  public void paint(Graphics g) {
    if (maxClipX == -1) {
       minClipX = 0;
       minClipY = 0;
       maxClipX = xSize-1;
       maxClipY = ySize-1;
    }

    final int minxc = left + minClipX * field_x,
              maxxc = left + maxClipX * field_x,
              minyc = top + minClipY * field_y,
              maxyc = top + maxClipY * field_y;

    g.setFont(new Font("Dialog", Font.PLAIN, 8));
    for (int y = minClipY, yc = minyc; y < ySize && yc <= maxyc; y++, yc += field_y) {
      final int len = parentCity.beepers[y].length;

      for (int x = minClipX, xc = minxc; x < len && xc <= maxxc; x++, xc += field_x) {
        Image img;
        String str;
        if (parentCity.karelX != x || parentCity.karelY != y) {
          if (parentCity.cityFields[y][x]) {
            int beepers = parentCity.beepers[y][x];
            if (beepers >= 9) str = "9";
            else if (beepers == 0) str = null;
            else str = Integer.toString(beepers);
            img = imgs[PIC_EMPTY];
          } else {
            img = imgs[PIC_WALL];
            str = null;
          }
        } else {
          img = imgs[parentCity.karelDir];
          str = null;
        }
        g.drawImage(img, xc, yc, null);
        if (str != null) g.drawString(str, xc+5, yc+10);
      }
    }

    minClipX = minClipY = Integer.MAX_VALUE;
    maxClipX = -1;
    maxClipY = -1;
  }
}