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
 * ExceptionTableEntry contains one entry in the Code attribute's
 * exception table.
 */
class ClassFileExceptionTableEntry extends ClassFileItem {
    private short start_pc;
    private short end_pc;
    private short handler_pc;
    private short catch_type = -1;
    private String catch_type_name;

    ClassFileExceptionTableEntry(ClassFile owner,
                        short start_pc, short end_pc,
                        short handler_pc, String catch_type) {
      super(owner);
      this.start_pc = start_pc;
      this.end_pc = end_pc;
      this.handler_pc = handler_pc;
      this.catch_type_name = catch_type;
      if (catch_type_name == null)
        this.catch_type = 0;
    }

    boolean checkCreated() {
      if (catch_type == -1) {
        catch_type = (short) owner.getClassIndex(catch_type_name);
        if (catch_type == -1) {
           catch_type = (short) owner.addConstantPoolClass(catch_type_name);
           return true;
        }
      }
      return false;
    }

    int getSize() {
      return 8;
    }

    void write(DataOutputStream stream) throws IOException {
      stream.writeShort(start_pc);
      stream.writeShort(end_pc);
      stream.writeShort(handler_pc);
      stream.writeShort(catch_type);
    }
}
