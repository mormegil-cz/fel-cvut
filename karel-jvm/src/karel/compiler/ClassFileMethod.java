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
 * ClassFileMethod encapsulates the "method_info" structure of the .class
 * file format.
 */

class ClassFileMethod extends ClassFileItem {
         /** Declared <CODE>public</CODE>; may be accessed from outside its package. */
  static final short
         ACC_PUBLIC       = 0x0001;
         /** Declared <CODE>private</CODE>; accessible only within the defining class. */
  static final short
         ACC_PRIVATE      = 0x0002;
         /** Declared <CODE>protected</CODE>; may be accessed within subclasses. */
  static final short
         ACC_PROTECTED    = 0x0004;
         /** Declared <CODE>static</CODE>. */
  static final short
         ACC_STATIC       = 0x0008;
         /** Declared <CODE>final</CODE>; may not be overriden. */
  static final short
         ACC_FINAL        = 0x0010;
         /** Declared <CODE>synchronized</CODE>; invocations is wrapped in a monitor lock. */
  static final short
         ACC_SYNCHRONIZED = 0x0020;
         /** Declared <CODE>native</CODE>; implemented in a language other than Java. */
  static final short
         ACC_NATIVE       = 0x0100;
         /** Declared <CODE>abstract</CODE>; no implementation is provided. */
  static final short
         ACC_ABSTRACT     = 0x0400;
         /** Declared <CODE>strictfp</CODE>; floating-point mode is FP-strict. */
  static final short
         ACC_STRICT       = 0x0800;

  private short access_flags;
  private String name;
  private String descriptor;
  private short name_index = -1;
  private short descriptor_index = -1;
  private ClassFileAttribute[] attributes;

  /**
   * @param owner ClassFile into which this method belongs.
   * @param access_flags Access permissions of the method. Combination of the
   * <CODE>ACC_*</CODE> flags.
   * @param name Identifier of the method.
   * @param descriptor Method descriptor of the method.
   * @param attributes Attributes of the method.
   */
  ClassFileMethod(ClassFile owner, short access_flags, String name,
         String descriptor, ClassFileAttribute[] attributes) {
    super(owner);
    this.access_flags = access_flags;
    this.name = name;
    this.descriptor = owner.toInternalForm(descriptor);
    this.attributes = attributes;
  }

  boolean checkCreated() {
    boolean result = false;
    if (name_index == -1) {
      name_index = (short) owner.getUtf8Index(name);
      if (name_index == -1) {
        name_index = (short) owner.addConstantPoolUtf8(name);
        result = true;
      }
    }

    if (descriptor_index == -1) {
      descriptor_index = (short) owner.getUtf8Index(descriptor);
      if (descriptor_index == -1) {
        descriptor_index = (short) owner.addConstantPoolUtf8(descriptor);
        result = true;
      }
    }

    for (int i=0; i<attributes.length; i++)
        if (attributes[i].checkCreated()) result = true;

    return result;
  }

  int getSize() {
    int result = 8;
    for (int i=0; i<attributes.length; i++)
        result += attributes[i].getSize();
    return result;
  }

  void write(DataOutputStream stream) throws IOException {
    stream.writeShort(access_flags);
    stream.writeShort(name_index);
    stream.writeShort(descriptor_index);
    stream.writeShort(attributes.length);
    for (int i=0; i<attributes.length; i++)
        attributes[i].write(stream);
  }
}