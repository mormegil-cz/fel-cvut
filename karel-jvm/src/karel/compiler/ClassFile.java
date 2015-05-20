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
import java.util.*;
import karel.compiler.ClassFileException;

/**
 * ClassFile provides all functionality required to create valid .class files.
 */

class ClassFile {
         /**
          * Item has <CODE>public</CODE> modifier. <BR>
          * ClassFile: Declared <CODE>public</CODE>; may be accessed from outside its package.<BR>
          * ClassFileField: Declared <CODE>public</CODE>; may be accessible from outside its package.<BR>
          * ClassFileMethod: Declared <CODE>public</CODE>; may be accessed from outside its package.
          */
   static final short
         ACC_PUBLIC       = 0x0001;
         /**
          * <CODE>private</CODE> modifier. <BR>
          * ClassFileField: Declared <CODE>private</CODE>; usable only within the defining class.<BR>
          * ClassFileMethod: Declared <CODE>private</CODE>; accessible only within the defining class.
          */
   static final short
         ACC_PRIVATE      = 0x0002;
         /**
          * Item has <CODE>protected</CODE> modifier. <BR>
          * ClassFileField: Declared <CODE>protected</CODE>; may be accessed within subclasses.<BR>
          * ClassFileMethod: Declared <CODE>protected</CODE>; may be accessed within subclasses.
          */
   static final short
         ACC_PROTECTED    = 0x0004;
         /**
          * Item has <CODE>static</CODE> modifier. <BR>
          * ClassFileField: Declared <CODE>static</CODE>.<BR>
          * ClassFileMethod: Declared <CODE>static</CODE>.
          */
   static final short
         ACC_STATIC       = 0x0008;
         /**
          * Item has <CODE>final</CODE> modifier. <BR>
          * ClassFile: Declared <CODE>final</CODE>; no subclasses allowed.<BR>
          * ClassFileField: Declared <CODE>final</CODE>; no further assignment after initialization.<BR>
          * ClassFileMethod: Declared <CODE>final</CODE>; may not be overriden.
          */
   static final short
         ACC_FINAL        = 0x0010;
         /** ClassFile: Treat superclass methods specially when invoked by the invokespecial instruction. */
   static final short
         ACC_SUPER        = 0x0020;
         /** ClassFileMethod: Declared <CODE>synchronized</CODE>; invocations is wrapped in a monitor lock. */
   static final short
         ACC_SYNCHRONIZED = 0x0020;
         /** ClassFileField: Declared <CODE>volatile</CODE>; cannot be cached. */
   static final short
         ACC_VOLATILE     = 0x0040;
         /** ClassFileField: Declared <CODE>transient</CODE>; not written or read by a persistent object manager. */
   static final short
         ACC_TRANSIENT    = 0x0080;
         /** ClassFileMethod: Declared <CODE>native</CODE>; implemented in a language other than Java. */
   static final short
         ACC_NATIVE       = 0x0100;
         /** ClassFile: Is an interface, not a class. */
   static final short
         ACC_INTERFACE    = 0x0200;
         /**
          * Item has <CODE>abstract</CODE> modifier. <BR>
          * ClassFile: Declared <CODE>abstract</CODE>; may not be instantiated.<BR>
          * ClassFileMethod: Declared <CODE>abstract</CODE>; no implementation is provided.
          */
   static final short
         ACC_ABSTRACT     = 0x0400;
         /** ClassFileMethod: Declared <CODE>strictfp</CODE>; floating-point mode is FP-strict. */
   static final short
         ACC_STRICT       = 0x0800;

  /** .class file magic number */
  private static final int magic = 0xCAFEBABE;
  /** .class file format version (xx.minor) */
  private static final short minor_version = 3;
  /** .class file format version (major.xx) */
  private static final short major_version = 45;

  /** Access modifiers and properties of the class */
  private short access_flags;
  /** Index into the constant pool for the Class_info of this class type. */
  private short this_class;
  /**
   * Index into the constant pool for the Class_info of the superclass,
   * or 0 if this is java.lang.Object penultimate superclass.
   */
  private short super_class;

  private LinkedList constantPool;
  private short constant_pool_count = 1; // valid indices are 1..constant_pool_count-1
  private ByteArrayOutputStream interfaceList;
  private LinkedList methodList;
  private LinkedList fieldList;
  private LinkedList attributeList;

  /** Identifier of this class. */
  private String classname;

  /**
   * @param access_flags Access permissions and properties of the class.
   * Combination of the <CODE>ACC_*</CODE> flags.
   * @param classname Identifier of the class.
   * @param superclassname Identifier of the superclass or null when denoting
   * the class is java.lang.Object (the penultimate superclass).
   */
   ClassFile(short access_flags, String classname, String superclassname) {
    this.classname = classname;
    this.access_flags = access_flags;

    constantPool = new LinkedList();
    interfaceList = new ByteArrayOutputStream();
    methodList = new LinkedList();
    fieldList = new LinkedList();
    attributeList = new LinkedList();

    this_class = (short) addConstantPoolClass(classname);
    if (superclassname != null)
      super_class = (short) addConstantPoolClass(superclassname);
    else
      super_class = 0;
  }

  /**
   * Utility method: Convert qualifier to internal form.
   * Converts normal syntax qualified identifier of the class within a
   * package (e.g. "java.lang.String") to an internal form (e.g. "java/lang/String")
   * @param normalSyntax The identifier to convert.
   * @return The internal form of the normalSyntax parameter.
   */
   static String toInternalForm(String normalSyntax) {
    return normalSyntax.replace('.', '/');
  }

  /**
   * Get index of this class in the constant pool.
   * @return Index into the constant pool for the Class_info of this class type.
   */
   short getThisClass() {
    return this_class;
  }

  /**
   * Get index of the superclass in the constant pool.
   * @return Index into the constant pool for the Class_info of the superclass,
   * or 0 if this is java.lang.Object penultimate superclass.
   */
   short getSuperClass() {
    return super_class;
  }

  /**
   * After adding all the constants, fields, methods, etc, call this method
   * to create the .class file.
   * @param classdirectory The directory in which should the .class file be created.
   * Should end with a path separator (or a drive separator, if desired).
   * @throws java.io.IOException
   * @throws ClassFileException
   */
   void write(String classdirectory) throws IOException, ClassFileException {
    DataOutputStream stream = new DataOutputStream(
                                  new FileOutputStream(classdirectory + toInternalForm(classname) + ".class")
                              );

    assureConstantPoolFilled();

    stream.writeInt(magic);
    stream.writeShort(minor_version);
    stream.writeShort(major_version);
    writeConstantPool(stream);
    stream.writeShort(access_flags);
    stream.writeShort(this_class);
    stream.writeShort(super_class);
    writeInterfaces(stream);
    writeFields(stream);
    writeMethods(stream);
    writeAttributes(stream);
  }

  /**
   * Add a class info entry into the constant pool.
   * @param name Identifier of the class.
   * @return Index of the new entry in the constant pool.
   */
   int addConstantPoolClass(String name) {
    constantPool.add(new ConstantPoolClass(this, name));
    return constant_pool_count++;
  }

  /**
   * Add a field reference info entry into the constant pool.
   * @param classname Identifier of the class, into which the field belongs.
   * @param name Identifier of the field.
   * @param type Field descriptor of the field.
   * @return Index of the new entry in the constant pool.
   */
   int addConstantPoolFieldRef(String classname, String name, String type) {
    constantPool.add(new ConstantPoolFieldRef(this, classname, name, type));
    return constant_pool_count++;
  }

  /**
   * Add a method reference info entry into the constant pool.
   * @param classname Identifier of the class, into which the method belongs.
   * @param name Identifier of the method.
   * @param type Method descriptor of the method.
   * @return Index of the new entry in the constant pool.
   */
   int addConstantPoolMethodRef(String classname, String name, String type) {
    constantPool.add(new ConstantPoolMethodRef(this, classname, name, type));
    return constant_pool_count++;
  }

  /**
   * Add an interface method reference info entry into the constant pool.
   * @param classname Identifier of the class (interface), into which the method belongs.
   * @param name Identifier of the method.
   * @param type Method descriptor of the method.
   * @return Index of the new entry in the constant pool.
   */
   int addConstantPoolInterfaceMethodRef(String classname, String name, String type) {
    constantPool.add(new ConstantPoolInterfaceMethodRef(this, classname, name, type));
    return constant_pool_count++;
  }

  /**
   * Add a string info entry into the constant pool.
   * @param string The value of the string to be added to the pool.
   * @return Index of the new entry in the constant pool.
   * @see #addConstantPoolUtf8
   */
   int addConstantPoolString(String string) {
    constantPool.add(new ConstantPoolString(this, string));
    return constant_pool_count++;
  }

  /**
   * Add an integer info entry into the constant pool.
   * @param data The value of the integer to be added to the pool.
   * @return Index of the new entry in the constant pool.
   */
   int addConstantPoolInteger(int data) {
    constantPool.add(new ConstantPoolInteger(this, data));
    return constant_pool_count++;
  }

  /**
   * Add a float info entry into the constant pool.
   * @param data The value of the float to be added to the pool.
   * @return Index of the new entry in the constant pool.
   */
   int addConstantPoolFloat(float data) {
    constantPool.add(new ConstantPoolFloat(this, data));
    return constant_pool_count++;
  }

  /**
   * Add a long info entry into the constant pool.
   * @param data The value of the long to be added to the pool.
   * @return Index of the new entry in the constant pool.
   */
   int addConstantPoolLong(long data) {
    int this_index = constant_pool_count;
    constantPool.add(new ConstantPoolLong(this, data));
    constant_pool_count+=2;
    return this_index;
  }

  /**
   * Add a double info entry into the constant pool.
   * @param data The value of the double to be added to the pool.
   * @return Index of the new entry in the constant pool.
   */
   int addConstantPoolDouble(double data) {
    int this_index = constant_pool_count;
    constantPool.add(new ConstantPoolDouble(this, data));
    return this_index;
  }

  /**
   * Add a "name_and_type" info entry into the constant pool.
   * @param name Identifier of the item.
   * @param descriptor Field/Method descriptor of the item.
   * @return Index of the new entry in the constant pool.
   */
   int addConstantPoolNameAndType(String name, String descriptor) {
    constantPool.add(new ConstantPoolNameAndType(this, name, descriptor));
    return constant_pool_count++;
  }

  /**
   * Add an UTF-8 string into the constant pool.
   * @param string The string to be added to the pool.
   * @return Index of the new entry in the constant pool.
   */
   int addConstantPoolUtf8(String string) {
    constantPool.add(new ConstantPoolUtf8(this, string));
    return constant_pool_count++;
  }

  /**
   * Add an interface into the list of direct superinterfaces of this
   * class or interface type. You must call this method in the left-to-right
   * order given in the source for the type.
   * @param name Identifier of the interface
   */
   void addInterface(String name) {
    short constantPoolIndex = (short) addConstantPoolClass(name);
    interfaceList.write((byte)(constantPoolIndex >> 8));
    interfaceList.write((byte)(constantPoolIndex & 0xFF));
  }

  /**
   * Add a field into the list of fields of the class.
   * @param field ClassFileField to be added to the list.
   * @return Index of the added field.
   * @see karel.compiler.ClassFileField
   */
   int addField(ClassFileField field) {
    fieldList.add(field);
    return (fieldList.size() - 1);
  }

  /**
   * Add a method into the list of methods of the class.
   * @param method ClassFileMethod to be added to the list.
   * @return Index of the added method.
   * @see karel.compiler.ClassFileMethod
   */
   int addMethod(ClassFileMethod method) {
    methodList.add(method);
    return (methodList.size() - 1);
  }

  /**
   * Add an attribute into the list of attributes of the class.
   * @param attribute The attribute to be added.
   * @return Index of the added attribute.
   * @see karel.compiler.ClassFileAttribute
   */
   int addAttribute(ClassFileAttribute attribute) {
    attributeList.add(attribute);
    return (attributeList.size() - 1);
  }

  /**
   * Add a field and a field reference constant pool entry simultaneously.
   * @param access_flags Access permissions of the field. Combination of the
   * <CODE>ACC_*</CODE> flags.
   * @param name Identifier of the field.
   * @param descriptor Field descriptor of the field.
   * @param attributes Array of attributes of the field.
   * @return Index of the field reference entry in the constant pool.
   */
   int addFieldWithEntry(short access_flags, String name,
         String descriptor, ClassFileAttribute[] attributes) {
    addField(new ClassFileField(this, access_flags, name, descriptor, attributes));
    return addConstantPoolFieldRef(classname, name, descriptor);
  }

  /**
   * Add a method and a method reference constant pool entry simultaneously.
   * @param access_flags Access permissions of the method. Combination of the
   * <CODE>ACC_*</CODE> flags.
   * @param name Identifier of the method.
   * @param descriptor Method descriptor of the method.
   * @param attributes Attributes of the method.
   * @return Index of the method reference entry in the constant pool.
   */
   int addMethodWithEntry(short access_flags, String name,
         String descriptor, ClassFileAttribute[] attributes) {
    addMethod(new ClassFileMethod(this, access_flags, name, descriptor, attributes));
    return addConstantPoolMethodRef(classname, name, descriptor);
  }

  /**
   * Find a class info in the constant pool.
   * @param name Identifier of the class to be found.
   * @return Index of the class in the pool, or -1 if not found.
   */
  int getClassIndex(String name) {
    ListIterator i=constantPool.listIterator(0);

    while (i.hasNext()) {
      Object item = i.next();

      if (item instanceof ConstantPoolClass) {
        ConstantPoolClass itemClass = (ConstantPoolClass) item;
        if (itemClass.name.equals(name))
           return (i.nextIndex()); // constant pool items are 1-based!
      }
    }
    return -1;
  }

  /**
   * Find a "name_and_type" entry in the constant pool.
   * @param name Identifier of the searched entry
   * @param descriptor Field/Method descriptor of the searched entry.
   * @return Index of the entry in the constant pool, or -1 if not found.
   */
  int getNameAndTypeIndex(String name, String descriptor) {
    ListIterator i=constantPool.listIterator(0);

    while (i.hasNext()) {
      Object item = i.next();

      if (item instanceof ConstantPoolNameAndType) {
        ConstantPoolNameAndType itemNameAndType = (ConstantPoolNameAndType) item;
        if (itemNameAndType.name.equals(name) && itemNameAndType.descriptor.equals(descriptor))
           return (i.nextIndex()); // constant pool items are 1-based!
      }
    }
    return -1;
  }

  /**
   * Find a UTF-8 string in the constant pool.
   * @param s The String to be found.
   * @return Index of the string in the pool, or -1 if not found.
   */
  int getUtf8Index(String s) {
    ListIterator i=constantPool.listIterator(0);

    while (i.hasNext()) {
      Object item = i.next();

      if (item instanceof ConstantPoolUtf8) {
        ConstantPoolUtf8 itemUtf8 = (ConstantPoolUtf8) item;
        if (itemUtf8.string.equals(s))
           return (i.nextIndex()); // constant pool items are 1-based!
      }
    }
    return -1;
  }

  /**
   * Check that all required items that any item in any pool is referencing
   * are properly created.
   * @throws ClassFileException
   */
  private void assureConstantPoolFilled() throws ClassFileException {
    Object[] items;
    boolean changed;

    items = fieldList.toArray();
    for (int i=0; i<items.length; i++) {
      if (!(items[i] instanceof ClassFileField))
         throw new ClassFileException("Internal: fieldList corrupted");

      ((ClassFileField)items[i]).checkCreated();
    }

    items = methodList.toArray();
    for (int i=0; i<items.length; i++) {
      if (!(items[i] instanceof ClassFileMethod))
         throw new ClassFileException("Internal: methodList corrupted");

      ((ClassFileMethod)items[i]).checkCreated();
    }

    items = attributeList.toArray();
    for (int i=0; i<items.length; i++) {
      if (!(items[i] instanceof ClassFileAttribute))
         throw new ClassFileException("Internal: attributeList corrupted");

      ((ClassFileAttribute)items[i]).checkCreated();
    }

    do {
      changed = false;
      items = constantPool.toArray();
      for (int i=0; i<items.length; i++) {
        if (!(items[i] instanceof ConstantPoolEntry))
           throw new ClassFileException("Internal: constantPool corrupted");

        if (((ConstantPoolEntry)items[i]).checkCreated()) changed = true;
      }
    } while (changed);
  }

  /**
   * Write the constant pool into the .class file.
   * @param stream The stream into which the constant pool shall be written.
   * @throws java.io.IOException
   * @throws ClassFileException
   */
  private void writeConstantPool(DataOutputStream stream) throws IOException, ClassFileException {
    Object[] poolItems;

    poolItems = constantPool.toArray();
    stream.writeShort(constant_pool_count);
    for (int i=0; i<poolItems.length; i++) {
      if (!(poolItems[i] instanceof ConstantPoolEntry))
         throw new ClassFileException("Internal: constantPool corrupted");

      ((ConstantPoolEntry)poolItems[i]).write(stream);
    }
  }

  /**
   * Write the interface list into the .class file.
   * @param stream The stream into which the interface list shall be written.
   * @throws java.io.IOException
   */
  private void writeInterfaces(DataOutputStream stream) throws IOException {
    stream.writeShort(interfaceList.size());
    interfaceList.writeTo(stream);
  }

  /**
   * Write the field list into the .class file.
   * @param stream The stream into which the field list shall be written.
   * @throws java.io.IOException
   * @throws ClassFileException
   */
  private void writeFields(DataOutputStream stream) throws IOException, ClassFileException {
    Object[] fieldItems;

    fieldItems = fieldList.toArray();

    stream.writeShort((short) fieldItems.length);
    for (int i=0; i<fieldItems.length; i++) {
      if (!(fieldItems[i] instanceof ClassFileField))
         throw new ClassFileException("Internal: fieldList corrupted");

      ((ClassFileField)fieldItems[i]).write(stream);
    }
  }

  /**
   * Write the method list into the .class file.
   * @param stream The stream into which the method list shall be written.
   * @throws java.io.IOException
   * @throws ClassFileException
   */
  private void writeMethods(DataOutputStream stream) throws IOException, ClassFileException {
    Object[] methodItems;

    methodItems = methodList.toArray();

    stream.writeShort((short) methodItems.length);
    for (int i=0; i<methodItems.length; i++) {
      if (!(methodItems[i] instanceof ClassFileMethod))
         throw new ClassFileException("Internal: methodList corrupted");

      ((ClassFileMethod)methodItems[i]).write(stream);
    }
  }

  /**
   * Write the attribute list into the .class file.
   * @param stream The stream into which the attribute list shall be written.
   * @throws java.io.IOException
   * @throws ClassFileException
   */
  private void writeAttributes(DataOutputStream stream) throws IOException, ClassFileException {
    Object[] attrItems;

    attrItems = attributeList.toArray();

    stream.writeShort((short) attrItems.length);
    for (int i=0; i<attrItems.length; i++) {
      if (!(attrItems[i] instanceof ClassFileAttribute))
         throw new ClassFileException("Internal: attributeList corrupted");

      ((ClassFileAttribute)attrItems[i]).write(stream);
    }
  }

/**
 * ConstantPoolEntry is an abstract superclass of all classes encapsulating
 * the constant pool entries.
 */

abstract class ConstantPoolEntry extends ClassFileItem {
  protected static final byte
          CONSTANT_Class              =  7,
          CONSTANT_Fieldref           =  9,
          CONSTANT_Methodref          = 10,
          CONSTANT_InterfaceMethodref = 11,
          CONSTANT_String             =  8,
          CONSTANT_Integer            =  3,
          CONSTANT_Float              =  4,
          CONSTANT_Long               =  5,
          CONSTANT_Double             =  6,
          CONSTANT_NameAndType        = 12,
          CONSTANT_Utf8               =  1;

  protected byte tag;

  ConstantPoolEntry(ClassFile owner) {
    super(owner);
  }

  int getSize() {
    return 1;
  }

  void write(DataOutputStream stream) throws IOException {
    stream.writeByte(tag);
  }
}

/** Encapsulates the CONSTANT_Class_info structure */
class ConstantPoolClass extends ConstantPoolEntry {
  String name;
  private short name_index = -1;

  ConstantPoolClass(ClassFile owner, String name) {
    super(owner);
    tag = CONSTANT_Class;
    this.name = ClassFile.toInternalForm(name);
  }

  boolean checkCreated() {
    if (name_index == -1) {
      name_index = (short) getUtf8Index(name);
      if (name_index == -1) {
         name_index = (short) owner.addConstantPoolUtf8(name);
         return true;
      }
    }
    return false;
  }

  int getSize() {
    return (super.getSize() + 2);
  }

  void write(DataOutputStream stream) throws IOException {
    super.write(stream);
    stream.writeShort(name_index);
  }
}

/** Common abstract superclass of all classes encapsulating the
 *  CONSTANT_*ref_info structures.
 */
abstract class ConstantPoolXXXRef extends ConstantPoolEntry {
  private String classname, name, type;
  private short class_index = -1;
  private short name_and_type_index = -1;

  ConstantPoolXXXRef(ClassFile owner, String classname, String name, String type) {
    super(owner);
    this.classname = toInternalForm(classname);
    this.name = name;
    this.type = toInternalForm(type);
  }

  boolean checkCreated() {
    boolean result = false;

    if (class_index == -1) {
      class_index = (short) getClassIndex(classname);
      if (class_index == -1) {
         class_index = (short) owner.addConstantPoolClass(classname);
         result = true;
      }
    }

    if (name_and_type_index == -1) {
      name_and_type_index = (short) getNameAndTypeIndex(name, type);
      if (name_and_type_index == -1) {
        name_and_type_index = (short) owner.addConstantPoolNameAndType(name, type);
        result = true;
      }
    }
    return result;
  }

  int getSize() {
    return (super.getSize() + 4);
  }

  void write(DataOutputStream stream) throws IOException {
    super.write(stream);
    stream.writeShort(class_index);
    stream.writeShort(name_and_type_index);
  }

   public boolean equals(Object o) {
    return (
            super.equals(o) ||
            (o instanceof ConstantPoolXXXRef) &&
            (((ConstantPoolXXXRef)o).tag == tag) &&
            ((ConstantPoolXXXRef)o).classname.equals(classname) &&
            ((ConstantPoolXXXRef)o).name.equals(name) &&
            ((ConstantPoolXXXRef)o).type.equals(type)
           );
  }
}

/** Encapsulates the CONSTANT_Fieldref_info structure */
class ConstantPoolFieldRef extends ConstantPoolXXXRef {
  ConstantPoolFieldRef(ClassFile owner, String classname, String name, String type) {
    super(owner, classname, name, type);

    tag = CONSTANT_Fieldref;
  }
}

/** Encapsulates the CONSTANT_Methodref_info structure */
class ConstantPoolMethodRef extends ConstantPoolXXXRef {
  ConstantPoolMethodRef(ClassFile owner, String classname, String name, String type) {
    super(owner, classname, name, type);

    tag = CONSTANT_Methodref;
  }
}

/** Encapsulates the CONSTANT_InterfaceMethodref_info structure */
class ConstantPoolInterfaceMethodRef extends ConstantPoolXXXRef {
  ConstantPoolInterfaceMethodRef(ClassFile owner, String classname, String name, String type) {
    super(owner, classname, name, type);

    tag = CONSTANT_InterfaceMethodref;
  }
}

/** Encapsulates the CONSTANT_String_info structure */
class ConstantPoolString extends ConstantPoolEntry {
  private String string;
  private short string_index = -1;

  ConstantPoolString(ClassFile owner, String string) {
    super(owner);
    tag = CONSTANT_String;
    this.string = string;
  }

  boolean checkCreated() {
    if (string_index == -1) {
      string_index = (short) getUtf8Index(string);
      if (string_index == -1) {
        string_index = (short) owner.addConstantPoolUtf8(string);
        return true;
      }
    }
    return false;
  }

  int getSize() {
    return (super.getSize() + 2);
  }

  void write(DataOutputStream stream) throws IOException {
    super.write(stream);
    stream.writeShort(string_index);
  }

   public boolean equals(Object o) {
    return (
            super.equals(o) ||
            (o instanceof ConstantPoolString) &&
            ((ConstantPoolString)o).string.equals(string)
           );
  }
}

/** Encapsulates the CONSTANT_Integer_info structure */
class ConstantPoolInteger extends ConstantPoolEntry {
  private int integer;

  ConstantPoolInteger(ClassFile owner, int integer) {
    super(owner);
    tag = CONSTANT_Integer;
    this.integer = integer;
  }

  int getSize() {
    return (super.getSize() + 4);
  }

  void write(DataOutputStream stream) throws IOException {
    super.write(stream);
    stream.writeInt(integer);
  }

   public boolean equals(Object o) {
    return (
            super.equals(o) ||
            (o instanceof ConstantPoolInteger) &&
            (((ConstantPoolInteger)o).integer == integer)
           );
  }
}

/** Encapsulates the CONSTANT_Float_info structure */
class ConstantPoolFloat extends ConstantPoolEntry {
  private float data;

  ConstantPoolFloat(ClassFile owner, float data) {
    super(owner);
    tag = CONSTANT_Float;
    this.data = data;
  }

  int getSize() {
    return (super.getSize() + 4);
  }

  void write(DataOutputStream stream) throws IOException {
    super.write(stream);
    stream.writeFloat(data);
  }

   public boolean equals(Object o) {
    return (
            super.equals(o) ||
            (o instanceof ConstantPoolFloat) &&
            (((ConstantPoolFloat)o).data == data)
           );
  }
}

/** Encapsulates the CONSTANT_Long_info structure */
class ConstantPoolLong extends ConstantPoolEntry {
  private long data;

  ConstantPoolLong(ClassFile owner, long data) {
    super(owner);
    tag = CONSTANT_Long;
    this.data = data;
  }

  int getSize() {
    return (super.getSize() + 8);
  }

  void write(DataOutputStream stream) throws IOException {
    super.write(stream);
    stream.writeLong(data);
  }

   public boolean equals(Object o) {
    return (
            super.equals(o) ||
            (o instanceof ConstantPoolLong) &&
            (((ConstantPoolLong)o).data == data)
           );
  }
}

/** Encapsulates the CONSTANT_Double_info structure */
class ConstantPoolDouble extends ConstantPoolEntry {
  private double data;

  ConstantPoolDouble(ClassFile owner, double data) {
    super(owner);
    tag = CONSTANT_Double;
    this.data = data;
  }

  int getSize() {
    return (super.getSize() + 8);
  }

  void write(DataOutputStream stream) throws IOException {
    super.write(stream);
    stream.writeDouble(data);
  }

   public boolean equals(Object o) {
    return (
            super.equals(o) ||
            (o instanceof ConstantPoolDouble) &&
            (((ConstantPoolDouble)o).data == data)
           );
  }
}

/** Encapsulates the CONSTANT_NameAndType_info structure */
class ConstantPoolNameAndType extends ConstantPoolEntry {
  private String name, descriptor;
  private short name_index = -1;
  private short descriptor_index = -1;

  ConstantPoolNameAndType(ClassFile owner, String name, String descriptor) {
    super(owner);
    tag = CONSTANT_NameAndType;
    this.name = name;
    this.descriptor = toInternalForm(descriptor);
  }

  boolean checkCreated() {
    boolean result = false;

    if (name_index == -1) {
      name_index = (short) getUtf8Index(name);
      if (name_index == -1) {
        name_index = (short) owner.addConstantPoolUtf8(name);
        result = true;
      }
    }

    if (descriptor_index == -1) {
      descriptor_index = (short) getUtf8Index(descriptor);
      if (descriptor_index == -1) {
        descriptor_index = (short) addConstantPoolUtf8(descriptor);
        result = true;
      }
    }

    return result;
  }

  int getSize() {
    return (super.getSize() + 4);
  }

  void write(DataOutputStream stream) throws IOException {
    super.write(stream);
    stream.writeShort(name_index);
    stream.writeShort(descriptor_index);
  }

   public boolean equals(Object o) {
    return (
            super.equals(o) ||
            (o instanceof ConstantPoolNameAndType) &&
            ((ConstantPoolNameAndType)o).name.equals(name) &&
            ((ConstantPoolNameAndType)o).descriptor.equals(descriptor)
           );
  }
}

/** Encapsulates the CONSTANT_Utf8_info structure */
class ConstantPoolUtf8 extends ConstantPoolEntry {
  private String string;

  ConstantPoolUtf8(ClassFile owner, String string) {
    super(owner);
    tag = CONSTANT_Utf8;
    this.string = string;
  }

  int getSize() {
    int result = super.getSize();

    for (int i=0; i<string.length(); i++) {
      char c = string.charAt(i);

      if (c >= '\u0001' && c <= '\u007F') result += 1;
      else if (c == '\u0000' || (c >= '\u0080' && c <= '\u07FF')) result += 2;
      else result += 3;
    }

    return result;
  }

  void write(DataOutputStream stream) throws IOException {
    ByteArrayOutputStream bytes;

    super.write(stream);

    bytes = new ByteArrayOutputStream();
    for (int i=0; i<string.length(); i++) {
      char c = string.charAt(i);

      if (c >= '\u0001' && c <= '\u007F') bytes.write((byte) c);
      else if (c == '\u0000' || (c >= '\u0080' && c <= '\u07FF')) {
              bytes.write((byte)((c >> 6) | 0xc0));
              bytes.write((byte)((c & 0x3f) | 0x80));
           }
      else {
        bytes.write((byte)((c >> 12) | 0xe0));
        bytes.write((byte)((c >> 6) | 0x80));
        bytes.write((byte)((c & 0x3f) | 0x80));
      }
    }

    stream.writeShort((short)bytes.size());
    bytes.writeTo(stream);
  }

   public boolean equals(Object o) {
    return (
            super.equals(o) ||
            (o instanceof ConstantPoolUtf8) &&
            ((ConstantPoolUtf8)o).string.equals(string)
           );
  }
}

} // end of ClassFile class
