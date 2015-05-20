package karel.compiler;

/**
 * Title:        Karel for Java VM
 * Description:  Karel compiler & GUI for Java Virtual Machine.
 * Copyright:    Copyright (c) 2001
 * Company:
 * @author Petr Kadlec
 * @version 1.0
 */

/** @todo InnerClasses, Synthetic, LineNumberTable,
 *  LocalVariableTable and Deprecated attributes.
 */

import java.io.*;
import karel.compiler.*;

/** Encapsulation of the ConstantValue_attribute */
class ClassFileAttributeConstantValue extends ClassFileAttribute {
  private short constantvalue_index;

  /**
   * @param owner ClassFile into which this attribute belongs.
   */
  ClassFileAttributeConstantValue(ClassFile owner, short constantvalue_index) {
    super(owner, "ConstantValue");
    this.constantvalue_index = constantvalue_index;
  }

  int getSize() {
    return (super.getSize() + 2);
  }

  void write(DataOutputStream stream) throws IOException {
    super.write(stream);
    stream.writeShort(constantvalue_index);
  }
}

/** Encapsulation of the Code_attribute */
class ClassFileAttributeCode extends ClassFileAttribute {
  private short max_stack;
  private short max_locals;
  private ByteArrayOutputStream code;
  private ClassFileExceptionTableEntry[] exception_table;
  private ClassFileAttribute[] attributes;

  /**
   * @param owner ClassFile into which this attribute belongs.
   * @param max_stack Maximum depth of the operating stack during the method.
   * @param max_locals Number of variables in the local variables array.
   * @param code Actual bytes of the Java VM code.
   * @param exception_table Array of the exception handlers.
   * @param attributes Array of the attributes of the method. (LineNumberTable,
   * LocalVariableTable)
   */
  ClassFileAttributeCode(ClassFile owner, short max_stack,
         short max_locals, ByteArrayOutputStream code,
         ClassFileExceptionTableEntry[] exception_table,
         ClassFileAttribute[] attributes) {

    super(owner, "Code");
    this.max_stack = max_stack;
    this.max_locals = max_locals;
    this.code = code;
    this.exception_table = exception_table;
    this.attributes = attributes;
  }

  boolean checkCreated() {
    boolean result = false;

    for (int i=0; i<exception_table.length; i++)
        if (exception_table[i].checkCreated()) result = true;
    for (int i=0; i<attributes.length; i++)
        if (attributes[i].checkCreated()) result = true;

    return result;
  }

  int getSize() {
    int result = super.getSize() + 2 + 2 + 4 + code.size() + 2 + 2;
    for (int i=0; i<exception_table.length; i++)
        result += exception_table[i].getSize();
    for (int i=0; i<attributes.length; i++)
        result += attributes[i].getSize();
    return result;
  }

  void write(DataOutputStream stream) throws IOException {
    super.write(stream);
    stream.writeShort(max_stack);
    stream.writeShort(max_locals);
    stream.writeInt(code.size());
    code.writeTo(stream);
    stream.writeShort(exception_table.length);
    for (int i=0; i<exception_table.length; i++)
        exception_table[i].write(stream);
    stream.writeShort(attributes.length);
    for (int i=0; i<attributes.length; i++)
        attributes[i].write(stream);
  }
}

/** Encapsulation of the Exceptions_attribute */
class ClassFileAttributeExceptions extends ClassFileAttribute {
  private short[] exception_index_table;

  /**
   * @param owner ClassFile into which this attribute belongs.
   * @param exceptions Array of exception identifiers the method may throw.
   */
  ClassFileAttributeExceptions(ClassFile owner, String[] exceptions) {
    super(owner, "Exceptions");

    exception_index_table = new short[exceptions.length];
    for (int i=0; i<exceptions.length; i++) {
      exception_index_table[i] = (short) owner.getUtf8Index(exceptions[i]);
      if (exception_index_table[i] == -1)
         exception_index_table[i] = (short) owner.addConstantPoolUtf8(exceptions[i]);
    }
  }

  int getSize() {
    return (super.getSize() + 2 + 2*exception_index_table.length);
  }

  void write(DataOutputStream stream) throws IOException {
    super.write(stream);
    stream.writeShort(exception_index_table.length);
    for (int i=0; i<exception_index_table.length; i++)
      stream.writeShort(exception_index_table[i]);
  }
}

/** Encapsulation of the SourceFile_attribute */
class ClassFileAttributeSourceFile extends ClassFileAttribute {
  private short sourcefile_index;

  /**
   * @param owner ClassFile into which this attribute belongs.
   * @param sourcefile Name of the source file the class file was compiled from.
   */
  ClassFileAttributeSourceFile(ClassFile owner, String sourcefile) {
    super(owner, "SourceFile");

    sourcefile_index = (short)owner.getUtf8Index(sourcefile);
    if (sourcefile_index == -1)
      sourcefile_index = (short) owner.addConstantPoolUtf8(sourcefile);
  }

  int getSize() {
    return (super.getSize() + 2);
  }

  void write(DataOutputStream stream) throws IOException {
    super.write(stream);
    stream.writeShort(sourcefile_index);
  }
}

/**
 * Abstract superclass of all classes encapsulating the .class file
 * attributes.
 */
abstract class ClassFileAttribute extends ClassFileItem {
  private short attribute_name_index = -1;

  /**
   * @param owner ClassFile into which this attribute belongs.
   * @param name Name of the attribute.
   */
  ClassFileAttribute(ClassFile owner, String name) {
    super(owner);

    attribute_name_index = (short) owner.getUtf8Index(name);
    if (attribute_name_index == -1)
      attribute_name_index = (short) owner.addConstantPoolUtf8(name);
  }

  /**
   * Create a new ConstantValue attribute.
   * @param constantvalue_index Index of the constant value in the constant pool.
   * @return The created attribute.
   */
  static ClassFileAttribute newConstantValue(ClassFile owner, short constantvalue_index) {
    return new ClassFileAttributeConstantValue(owner, constantvalue_index);
  }

  /**
   * Create a new Code attribute.
   * @param owner ClassFile into which this attribute belongs.
   * @param max_stack Maximum depth of the operating stack during the method.
   * @param max_locals Number of variables in the local variables array.
   * @param code Actual bytes of the Java VM code.
   * @param exception_table Array of the exception handlers.
   * @param attributes Array of the attributes of the method. (LineNumberTable,
   * LocalVariableTable)
   * @return The created attribute.
   */
  static ClassFileAttribute newCode(ClassFile owner, short max_stack,
         short max_locals, ByteArrayOutputStream code,
         ClassFileExceptionTableEntry[] exception_table,
         ClassFileAttribute[] attributes) {

    return new ClassFileAttributeCode(owner, max_stack, max_locals, code,
                                      exception_table, attributes);
  }

  /**
   * Create a new Exceptions attribute.
   * @param owner ClassFile into which this attribute belongs.
   * @param exceptions Array of exception identifiers the method may throw.
   */
  static ClassFileAttribute newExceptions(ClassFile owner,
         String[] exceptions) {

    return new ClassFileAttributeExceptions(owner, exceptions);
  }

  /**
   * Create a new SourceFile attribute.
   * @param owner ClassFile into which this attribute belongs.
   * @param sourcefile Name of the source file the class file was compiled from.
   */
  static ClassFileAttribute newSourceFile(ClassFile owner, String sourcefile) {
    return new ClassFileAttributeSourceFile(owner, sourcefile);
  }

  int getSize() {
    return 6;
  }

  void write(DataOutputStream stream) throws IOException {
    stream.writeShort(attribute_name_index);
    stream.writeInt(getSize() - 6);
  }
} // end of the ClassFileAttribute class
