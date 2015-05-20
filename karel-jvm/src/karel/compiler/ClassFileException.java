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
 * ClassFileException occurs when the ClassFile class is unable to handle
 * the requests. In the current code, this exception will not be thrown
 * except on internal errors of the ClassFile code.
 */
class ClassFileException extends Exception {
  ClassFileException(String msg) {
    super(msg);
  }
}