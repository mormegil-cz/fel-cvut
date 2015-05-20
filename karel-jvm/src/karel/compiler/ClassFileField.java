package karel.compiler;

/**
 * Title:        Karel for Java VM
 * Description:  Karel compiler & GUI for Java Virtual Machine.
 * Copyright:    Copyright (c) 2001
 * @author Petr Kadlec
 * @version 1.0
 */

import java.io.*;
import karel.compiler.*;

/**
 * ClassFileField encapsulates the "field_info" structure of the .class
 * file format.
 */

class ClassFileField extends ClassFileItem {
         /** Declared <CODE>public</CODE>; may be accessible from outside its package. */
  static final short
         ACC_PUBLIC    = 0x0001;
         /** Declared <CODE>private</CODE>; usable only within the defining class. */
  static final short
         ACC_PRIVATE   = 0x0002;
         /** Declared <CODE>protected</CODE>; may be accessed within subclasses. */
  static final short
         ACC_PROTECTED = 0x0004;
         /** Declared <CODE>static</CODE>. */
  static final short
         ACC_STATIC    = 0x0008;
         /** Declared <CODE>final</CODE>; no further assignment after initialization. */
  static final short
         ACC_FINAL     = 0x0010;
         /** Declared <CODE>volatile</CODE>; cannot be cached. */
  static final short
         ACC_VOLATILE  = 0x0040;
         /** Declared <CODE>transient</CODE>; not written or read by a persistent object manager. */
  static final short
         ACC_TRANSIENT = 0x0080;

  private short access_flags;
  private String name;
  private String descriptor;
  private short name_index = -1;
  private short descriptor_index = -1;
  private ClassFileAttribute[] attributes;

  /**
   * @param owner ClassFile into which this field belongs.
   * @param access_flags Access permissions of the field. Combination of the
   * <CODE>ACC_*</CODE> flags.
   * @param name Identifier of the field.
   * @param descriptor Field descriptor of the field.
   * @param attributes Array of attributes of the field.
   */
  ClassFileField(ClassFile owner, short access_flags, String name,
         String descriptor, ClassFileAttribute[] attributes) {
    super(owner);
    this.access_flags = (short)access_flags;
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
