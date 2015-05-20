package karel.compiler;

/**
 * Title:        Karel for Java VM
 * Description:  Karel compiler & GUI for Java Virtual Machine.
 * Copyright:    Copyright (c) 2001
 * Company:
 * @author Petr Kadlec
 * @version 1.0
 */

import java.io.*;
import karel.compiler.*;

/**
 * ClassFileItem class is an abstract superclass of all classes encapsulating
 * the items of the .class file.
 */

abstract class ClassFileItem {
  /** ClassFile into which this item belongs. */
  protected ClassFile owner;

  /**
   * @param owner ClassFile into which this item belongs.
   */
  ClassFileItem(ClassFile owner) {
    this.owner = owner;
  }

  /**
   * Write the item into a stream.
   * @param stream Specifies the output stream into which the item should be
   * written.
   * @throws java.io.IOException
   */
  abstract void write(DataOutputStream stream) throws IOException;

  /**
   * Get the size of this item in the resulting .class file.
   * @result Size (in bytes) of the item in the .class file.
   */
  abstract int getSize();

  /**
   * Check that all required items that this item is referencing to are
   * existing. If not, create them.
   * @return true if some referenced item has been created, false otherwise.
   */
  boolean checkCreated() {
    /* Nothing for this abstract class, will be overriden in most of the
       subclasses. */
    return false;
  }
}