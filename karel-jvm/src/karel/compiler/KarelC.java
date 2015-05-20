package karel.compiler;

/**
 * Title:        Karel for Java VM
 * Description:  Karel compiler & GUI for Java Virtual Machine.
 * Copyright:    Copyright (c) 2001
 * Company:
 * @author Petr Kadlec
 * @version 1.0
 */

/**
 * A command-line program interfacing KarelCompiler.
 */
class KarelC {

  public static void main(String[] args) {
    String srcFName = "";
    String dstClass = "";
    String dstClassFName;

    boolean haveSrc = false;
    boolean haveDst = false;

    KarelCompiler compiler = null;

    System.out.println("KarelC  v1.0  Copyright (C) DragonSoft, 2001");

    for (int i=0; i<args.length; i++) {
      if (args[i].startsWith("-")) {
        if (args[i].equalsIgnoreCase("-?") ||
            args[i].equalsIgnoreCase("-h") ||
            args[i].equalsIgnoreCase("--help")) {

          System.out.println("Usage: karelc srcfname dstclassname");
          System.exit(0);
        } else {
          System.err.println("Invalid option");
          System.exit(255);
        }
      } else {
        if (!haveSrc) {
          srcFName = args[i];
          haveSrc = true;
        } else if (!haveDst) {
                 dstClass = args[i];
                 haveDst = true;
               } else {
                 System.err.println("Invalid parameter");
                 System.exit(255);
               }
      }
    }

    if (!haveDst) {
      System.err.println("Invalid syntax");
      System.exit(255);
    }
    dstClassFName = dstClass + ".class";

    try {
      compiler = new KarelCompiler(srcFName, dstClass, dstClassFName);
    } catch (java.io.FileNotFoundException e) {
      System.err.println("Cannot open "+srcFName+" ("+e+")");
      System.exit(2);
    }

    if (compiler == null) {
      System.err.println("Internal error");
      System.exit(100);
    }

    try {
      compiler.runCompiler();
    } catch (KarelCompilationException e) {
      System.err.println("[Error] "+e.getMessage());
      System.exit(1);
    }

    System.out.println("Compilation succeeded.");
    System.exit(0);
  }

}