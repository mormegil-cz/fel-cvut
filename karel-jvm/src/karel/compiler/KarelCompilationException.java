package karel.compiler;

/**
 * Title:        Karel for Java VM
 * Description:  Karel compiler & GUI for Java Virtual Machine.
 * Copyright:    Copyright (c) 2001
 * Company:
 * @author Petr Kadlec
 * @version 1.0
 */

import java.text.MessageFormat;

/**
 * Exception during compilation of a Karel program.
 */
public class KarelCompilationException extends Exception {
  private int lineNumber;
  private String fileName;

  /**
   * Returns line number on which has the error occurred.
   * @return Line number on which has the error been detected.
   */
  public int getLineNumber() {
    return lineNumber;
  }

  /**
   * Returns name of the source file in which the error has occurred.
   * @return Name of the source file in which the error has occurred.
   */
  public String getFileName() {
    return fileName;
  }

  /**
   * The message associated with the error.
   * @return The message associated with this exception.
   */
  public String getMessage() {
    Object[] arguments = {
      fileName,
      new Integer(lineNumber),
      super.getMessage()
    };

    if (fileName==null)
       return super.getMessage();
    else
      if (fileName.length() == 0)
         return MessageFormat.format("line {1,number,integer}: {2}", arguments);
      else
         return MessageFormat.format("{0} ({1,number,integer}): {2}", arguments);
  }

  /**
   * Constructs the compilation exception.
   * @param filename Filename of the source file in which the error has occurred.
   * @param msg The compiler error message
   * @param lineNumber Line number on which the error has been detected.
   */
  public KarelCompilationException(String fileName, String msg, int lineNumber) {
    super(msg);
    this.fileName = fileName;
    this.lineNumber = lineNumber;
  }

}