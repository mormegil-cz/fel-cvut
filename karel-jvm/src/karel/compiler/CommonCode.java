package   karel.compiler;

/**
 * Title:        Karel for Java VM
 * Description:  Karel compiler & GUI for Java Virtual Machine.
 * Copyright:    Copyright (c) 2001
 * Company:
 * @author Petr Kadlec
 * @version 1.0
 */

import java.io.*;

/**
 * The CommonCode class contains the Java VM compiled code to be included
 * in compiled programs. (loader code, built-in instructions, etc.)
 */
class CommonCode {
  // these fields will be filled by the indices into the constant pool
  int        var_dX, var_dY, var_city, var_xPos, // -1, ..., -4
             var_yPos, var_direction, var_hasBeepers, // -5, -6, -7

             string_NoTurnOffOccurred, method_ObjectINIT,   // -8, -9
             class_KarelCity, string_NoBeepersInBag,        // -10, -11

             method_city_getBeepers, method_city_setBeepers, // -12, -13
             method_city_updateKarel, method_city_getInitXPos, // -14, -15
             method_city_getInitYPos, method_city_getInitDirection, // -16, -17
             method_city_exceptionCatched, method_city_turnOff, // -18, -19
             method_city_INIT, method_city_getInitBagSize,      // -20, -21

             method_INIT, method_isInCity, method_moveTo, // -22, -23, -24
             class_KarelException, string_KarelHitTheWall, // -25, -26
             method_KarelExceptionINIT, string_NoBeepersAreNearby, // -27, -28
             method_System_exit, class_this; // -29, -30

  /** A mark for space reserved for the high byte of an opcode */
  final static short SPACE = Short.MAX_VALUE;
  /** A mark for recomputing jump destination */
  final static short JUMP  = Short.MAX_VALUE-1;

  // Java VM opcodes
  final static short
          nop = 0x00,
          aconst_null = 0x01,
          iconst_m1 = 0x02,
          iconst_0 = 0x03,
          iconst_1 = 0x04,
          iconst_2 = 0x05,
          iconst_3 = 0x06,
          iconst_4 = 0x07,
          iconst_5 = 0x08,
          lconst_0 = 0x09,
          lconst_1 = 0x0a,
          fconst_0 = 0x0b,
          fconst_1 = 0x0c,
          fconst_2 = 0x0d,
          dconst_0 = 0x0e,
          dconst_1 = 0x0f,
          bipush = 0x10,
          sipush = 0x11,
          ldc = 0x12,
          ldc_w = 0x13,
          ldc2_w = 0x14,
          iload = 0x15,
          lload = 0x16,
          fload = 0x17,
          dload = 0x18,
          aload = 0x19,
          iload_0 = 0x1a,
          iload_1 = 0x1b,
          iload_2 = 0x1c,
          iload_3 = 0x1d,
          lload_0 = 0x1e,
          lload_1 = 0x1f,
          lload_2 = 0x20,
          lload_3 = 0x21,
          fload_0 = 0x22,
          fload_1 = 0x23,
          fload_2 = 0x24,
          fload_3 = 0x25,
          dload_0 = 0x26,
          dload_1 = 0x27,
          dload_2 = 0x28,
          dload_3 = 0x29,
          aload_0 = 0x2a,
          aload_1 = 0x2b,
          aload_2 = 0x2c,
          aload_3 = 0x2d,
          iaload = 0x2e,
          laload = 0x2f,
          faload = 0x30,
          daload = 0x31,
          dstore_2 = 0x49,
          dstore_3 = 0x4a,
          astore_0 = 0x4b,
          astore_1 = 0x4c,
          astore_2 = 0x4d,
          astore_3 = 0x4e,
          iastore = 0x4f,
          lastore = 0x50,
          fastore = 0x51,
          dastore = 0x52,
          aastore = 0x53,
          bastore = 0x54,
          castore = 0x55,
          sastore = 0x56,
          pop = 0x57,
          pop2 = 0x58,
          dup = 0x59,
          dup_x1 = 0x5a,
          dup_x2 = 0x5b,
          dup2 = 0x5c,
          dup2_x1 = 0x5d,
          dup2_x2 = 0x5e,
          swap = 0x5f,
          iadd = 0x60,
          ladd = 0x61,
          fadd = 0x62,
          dadd = 0x63,
          isub = 0x64,
          lsub = 0x65,
          fsub = 0x66,
          dsub = 0x67,
          imul = 0x68,
          lmul = 0x69,
          fmul = 0x6a,
          dmul = 0x6b,
          idiv = 0x6c,
          ldiv = 0x6d,
          fdiv = 0x6e,
          ddiv = 0x6f,
          irem = 0x70,
          lrem = 0x71,
          frem = 0x72,
          drem = 0x73,
          ineg = 0x74,
          lneg = 0x75,
          fneg = 0x76,
          dneg = 0x77,
          ishl = 0x78,
          lshl = 0x79,
          ishr = 0x7a,
          lshr = 0x7b,
          iushr = 0x7c,
          lushr = 0x7d,
          iand = 0x7e,
          land = 0x7f,
          ior = 0x80,
          lor = 0x81,
          ixor = 0x82,
          lxor = 0x83,
          iinc = 0x84,
          i2l = 0x85,
          i2f = 0x86,
          i2d = 0x87,
          l2i = 0x88,
          l2f = 0x89,
          l2d = 0x8a,
          f2i = 0x8b,
          f2l = 0x8c,
          f2d = 0x8d,
          d2i = 0x8e,
          d2l = 0x8f,
          d2f = 0x90,
          i2b = 0x91,
          i2c = 0x92,
          i2s = 0x93,
          lcmp = 0x94,
          fcmpl = 0x95,
          fcmpg = 0x96,
          dcmpl = 0x97,
          dcmpg = 0x98,
          ifeq = 0x99,
          ifne = 0x9a,
          iflt = 0x9b,
          ifge = 0x9c,
          ifgt = 0x9d,
          ifle = 0x9e,
          if_icmpeq = 0x9f,
          if_icmpne = 0xa0,
          if_icmplt = 0xa1,
          if_icmpge = 0xa2,
          if_icmpgt = 0xa3,
          if_icmple = 0xa4,
          if_acmpeq = 0xa5,
          if_acmpne = 0xa6,
          _goto = 0xa7,
          jsr = 0xa8,
          ret = 0xa9,
          tableswitch = 0xaa,
          lookupswitch = 0xab,
          ireturn = 0xac,
          lreturn = 0xad,
          freturn = 0xae,
          dreturn = 0xaf,
          areturn = 0xb0,
          _return = 0xb1,
          getstatic = 0xb2,
          putstatic = 0xb3,
          getfield = 0xb4,
          putfield = 0xb5,
          invokevirtual = 0xb6,
          invokespecial = 0xb7,
          invokestatic = 0xb8,
          invokeinterface = 0xb9,
          ___unused___ = 0xba,
          _new = 0xbb,
          newarray = 0xbc,
          anewarray = 0xbd,
          arraylength = 0xbe,
          athrow = 0xbf,
          checkcast = 0xc0,
          _instanceof = 0xc1,
          monitorenter = 0xc2,
          monitorexit = 0xc3,
          wide = 0xc4,
          multianewarray = 0xc5,
          ifnull = 0xc6,
          ifnonnull = 0xc7,
          goto_w = 0xc8,
          jsr_w = 0xc9,
          // reserved opcodes:
          breakpoint = 0xca,
          impdep1 = 0xfe,
          impdep2 = 0xff;

  // Java VM special operands
  private final static short
          T_BOOLEAN =  4,
          T_CHAR    =  5,
          T_FLOAT   =  6,
          T_DOUBLE  =  7,
          T_BYTE    =  8,
          T_SHORT   =  9,
          T_INT     = 10,
          T_LONG    = 11;

  // Operands (translated by createCode)
  final static short
             dX = -1,
             dY = -2,
             city = -3,
             xPos = -4,
             yPos = -5,
             direction = -6,
             hasBeepers = -7,

             NoTurnOffOccurred = -8,
             ObjectINIT = -9,
             KarelCity = -10,
             NoBeepersInBag = -11,

             getBeepers = -12,
             setBeepers = -13,
             updateKarel = -14,
             getInitXPos = -15,
             getInitYPos = -16,
             getInitDirection = -17,
             exceptionCatched = -18,
             turnOff = -19,
             KarelCityINIT = -20,
             getInitBagSize = -21,

             INIT = -22,
             isInCity = -23,
             moveTo = -24,
             KarelException = -25,
             KarelHitTheWall = -26,
             KarelExceptionINIT = -27,
             NoBeepersAreNearby = -28,
             exit = -29,
             ThisClass = -30;

  // -------------------- Code -----------------------
  final static short[] code_moveTo = {
         aload_0,
         getfield, SPACE, city,
         iload_1,
         iload_2,
         invokevirtual, SPACE, isInCity,
         ifne, 0, 13,    // goto noException
         _new, SPACE, KarelException,
         dup,
         ldc, KarelHitTheWall,
         invokespecial, SPACE, KarelExceptionINIT,
         athrow,
         // noException:
         aload_0,
         iload_1,
         putfield, SPACE, xPos,
         aload_0,
         iload_2,
         putfield, SPACE, yPos,
         aload_0,
         getfield, SPACE, city,
         iload_1,
         iload_2,
         aload_0,
         getfield, SPACE, direction,
         aload_0,
         getfield, SPACE, hasBeepers,
         invokevirtual, SPACE, updateKarel,
         _return
  };
  final static short[] code_MOVE = {
   aload_0,
   dup,
   getfield, SPACE, xPos,
   getstatic, SPACE, dX,
   aload_0,
   getfield, SPACE, direction,
   iaload,
   iadd,
   aload_0,
   getfield, SPACE, yPos,
   getstatic, SPACE, dY,
   aload_0,
   getfield, SPACE, direction,
   iaload,
   iadd,
   invokespecial, SPACE, moveTo,
   _return
  };
  final static short[] code_TURNLEFT = {
   aload_0,
   getfield, SPACE, city,
   aload_0,
   getfield, SPACE, xPos,
   aload_0,
   getfield, SPACE, yPos,
   aload_0,
   dup,
   getfield, SPACE, direction,
   iconst_1,
   iadd,
   iconst_3,
   iand,
   dup_x1,
   putfield, SPACE, direction,
   aload_0,
   getfield, SPACE, hasBeepers,
   invokevirtual, SPACE, updateKarel,
   _return
  };
  final static short[] code_PICKBEEPER = {
   aload_0,
   getfield, SPACE, city,
   dup,
   aload_0,
   getfield, SPACE, xPos,
   aload_0,
   getfield, SPACE, yPos,
   dup2_x1,
   invokevirtual, SPACE, getBeepers,
   dup,
   ifne, 0, 13, // goto noException
   _new, SPACE, KarelException,
   dup,
   ldc, NoBeepersAreNearby,
   invokespecial, SPACE, KarelExceptionINIT,
   athrow,
   // noException:
   iconst_1,
   isub,
   invokevirtual, SPACE, setBeepers,
   aload_0,
   dup,
   getfield, SPACE, hasBeepers,
   iconst_1,
   iadd,
   putfield, SPACE, hasBeepers,
   aload_0,
   getfield, SPACE, city,
   aload_0,
   getfield, SPACE, xPos,
   aload_0,
   getfield, SPACE, yPos,
   aload_0,
   getfield, SPACE, direction,
   aload_0,
   getfield, SPACE, hasBeepers,
   invokevirtual, SPACE, updateKarel,
   _return
  };
  final static short[] code_PUTBEEPER = {
   aload_0,
   dup,
   getfield, SPACE, hasBeepers,
   dup,
   ifne, 0, 13, // goto noException
   _new, SPACE, KarelException,
   dup,
   ldc, NoBeepersInBag,
   invokespecial, SPACE, KarelExceptionINIT,
   athrow,
   // noException:
   iconst_1,
   isub,
   putfield, SPACE, hasBeepers,
   aload_0,
   getfield, SPACE, city,
   dup,
   aload_0,
   getfield, SPACE, xPos,
   aload_0,
   getfield, SPACE, yPos,
   dup2_x1,
   invokevirtual, SPACE, getBeepers,
   iconst_1,
   iadd,
   invokevirtual, SPACE, setBeepers,
   aload_0,
   getfield, SPACE, city,
   aload_0,
   getfield, SPACE, xPos,
   aload_0,
   getfield, SPACE, yPos,
   aload_0,
   getfield, SPACE, direction,
   aload_0,
   getfield, SPACE, hasBeepers,
   invokevirtual, SPACE, updateKarel,
   _return
  };
  final static short[] code_TURNOFF = {
   aload_0,
   getfield, SPACE, city,
   invokevirtual, SPACE, turnOff,
   iconst_0,
   invokestatic, SPACE, exit,
   _return
  };
  final static short[] code_main = {
   _new, SPACE, ThisClass,
   aload_0,
   invokespecial, SPACE, INIT,
   _return
  };
  final static short[] code_clinit = {
    iconst_4,
    newarray, T_INT,
    dup,
    iconst_0,
    iconst_0,
    iastore,
    dup,
    iconst_1,
    iconst_m1,
    iastore,
    dup,
    iconst_2,
    iconst_0,
    iastore,
    dup,
    iconst_3,
    iconst_1,
    iastore,
    putstatic, SPACE, dX,
    iconst_4,
    newarray, T_INT,
    dup,
    iconst_0,
    iconst_m1,
    iastore,
    dup,
    iconst_1,
    iconst_0,
    iastore,
    dup,
    iconst_2,
    iconst_1,
    iastore,
    dup,
    iconst_3,
    iconst_0,
    iastore,
    putstatic, SPACE, dY,
    _return,
 };
 final static short[] code_init1 = {
    aload_0,
    invokespecial, SPACE, ObjectINIT,
    aload_0,
    _new, SPACE, KarelCity,
    dup,
    aload_1,
    invokespecial, SPACE, KarelCityINIT,
    putfield, SPACE, city,
    aload_0,
    aload_0,
    getfield, SPACE, city,
    invokevirtual, SPACE, getInitXPos,
    putfield, SPACE, xPos,
    aload_0,
    aload_0,
    getfield, SPACE, city,
    invokevirtual, SPACE, getInitYPos,
    putfield, SPACE, yPos,
    aload_0,
    dup,
    getfield, SPACE, city,
    invokevirtual, SPACE, getInitDirection,
    putfield, SPACE, direction,
    aload_0,
    dup,
    getfield, SPACE, city,
    invokevirtual, SPACE, getInitBagSize,
    putfield, SPACE, hasBeepers,
 };
 final static short[] code_init2 = {
    _new, SPACE, KarelException,
    dup,
    ldc, NoTurnOffOccurred,
    invokespecial, SPACE, KarelExceptionINIT,
    athrow
 };
 final static short[] code_init3 = {
    aload_0,
    getfield, SPACE, city,
    dup_x1,
    pop,
    invokevirtual, SPACE, exceptionCatched,
    _return
 };
 // -------- conditions ----------------------------
 final static short[] code_ccFrontClear = {
   aload_0,
   getfield, SPACE, city,
   aload_0,
   getfield, SPACE, xPos,
   getstatic, SPACE, dX,
   aload_0,
   getfield, SPACE, direction,
   iaload,
   iadd,
   aload_0,
   getfield, SPACE, yPos,
   getstatic, SPACE, dY,
   aload_0,
   getfield, SPACE, direction,
   iaload,
   iadd,
   invokevirtual, SPACE, isInCity
  };
 final static short[] code_ccLeftClear = {
   aload_0,
   getfield, SPACE, city,
   aload_0,
   getfield, SPACE, xPos,
   getstatic, SPACE, dX,
   aload_0,
   getfield, SPACE, direction,
   iconst_1,
   iadd,
   iconst_3,
   iand,
   iaload,
   iadd,
   aload_0,
   getfield, SPACE, yPos,
   getstatic, SPACE, dY,
   aload_0,
   getfield, SPACE, direction,
   iconst_1,
   iadd,
   iconst_3,
   iand,
   iaload,
   iadd,
   invokevirtual, SPACE, isInCity
  };
 final static short[] code_ccBackClear = {
   aload_0,
   getfield, SPACE, city,
   aload_0,
   getfield, SPACE, xPos,
   getstatic, SPACE, dX,
   aload_0,
   getfield, SPACE, direction,
   iconst_2,
   iadd,
   iconst_3,
   iand,
   iaload,
   iadd,
   aload_0,
   getfield, SPACE, yPos,
   getstatic, SPACE, dY,
   aload_0,
   getfield, SPACE, direction,
   iconst_2,
   iadd,
   iconst_3,
   iand,
   iaload,
   iadd,
   invokevirtual, SPACE, isInCity
  };
 final static short[] code_ccRightClear = {
   aload_0,
   getfield, SPACE, city,
   aload_0,
   getfield, SPACE, xPos,
   getstatic, SPACE, dX,
   aload_0,
   getfield, SPACE, direction,
   iconst_3,
   iadd,
   iconst_3,
   iand,
   iaload,
   iadd,
   aload_0,
   getfield, SPACE, yPos,
   getstatic, SPACE, dY,
   aload_0,
   getfield, SPACE, direction,
   iconst_3,
   iadd,
   iconst_3,
   iand,
   iaload,
   iadd,
   invokevirtual, SPACE, isInCity
  };
 final static short[] code_ccNextToABeeper = {
   aload_0,
   getfield, SPACE, city,
   aload_0,
   getfield, SPACE, xPos,
   aload_0,
   getfield, SPACE, yPos,
   invokevirtual, SPACE, getBeepers
  };
 final static short[] code_ccAnyBeepersInBag = {
   aload_0,
   getfield, SPACE, hasBeepers
  };
 final static short[] code_ccFacingNorth = {
   aload_0,
   getfield, SPACE, direction,
   iconst_0,
   isub
  };
 final static short[] code_ccFacingWest = {
   aload_0,
   getfield, SPACE, direction,
   iconst_1,
   isub
  };
 final static short[] code_ccFacingSouth = {
   aload_0,
   getfield, SPACE, direction,
   iconst_2,
   isub
  };
 final static short[] code_ccFacingEast = {
   aload_0,
   getfield, SPACE, direction,
   iconst_3,
   isub
  };
 // ------------------------------------------------

  /**
   * Creates a copy of a common code routine with properly filled operands.
   * @param code One of the <CODE>code_*</CODE> fields.
   * @return Processed code with operands filled in from the fields.
   */
  short[] createCode(short[] code) {
    int[] table = {var_dX, var_dY, var_city, var_xPos,
                   var_yPos, var_direction, var_hasBeepers,
                   string_NoTurnOffOccurred, method_ObjectINIT,
                   class_KarelCity, string_NoBeepersInBag,
                   method_city_getBeepers, method_city_setBeepers,
                   method_city_updateKarel, method_city_getInitXPos,
                   method_city_getInitYPos, method_city_getInitDirection,
                   method_city_exceptionCatched, method_city_turnOff,
                   method_city_INIT, method_city_getInitBagSize,
                   method_INIT, method_isInCity, method_moveTo,
                   class_KarelException, string_KarelHitTheWall,
                   method_KarelExceptionINIT, string_NoBeepersAreNearby,
                   method_System_exit, class_this
                   };
    short[] result = new short[code.length];

    for (int i=0; i<code.length; i++)
        if (code[i] < 0)
           switch (result[i-1]) {
            case SPACE: result[i-1] = (short)((table[-1-code[i]] >> 8) & 0xFF);
                        result[i] = (short)(table[-1-code[i]] & 0xFF);
                        break;
            case JUMP:  {
              short offset = (short)(code[i]-(i-2));

              result[i-1] = (short)((offset >> 8) & 0xFF);
              result[i] = (short)(offset & 0xFF);
              break;
            }
            default:    if (((short)((table[-1-code[i]] >> 8) & 0xFF)) != 0)
                          throw new ArrayStoreException("Cannot encode a short operand into a byte");
                        else
                          result[i] = (short)(table[-1-code[i]] & 0xFF);
           }
        else
          if (i>0)
             switch (result[i-1]) {
               case JUMP: {
                 short offset = (short)(code[i]-(i-2));

                 result[i-1] = (short)((offset >> 8) & 0xFF);
                 result[i] = (short)(offset & 0xFF);
                 break;
               }
               default: result[i] = code[i];
             }
          else
            result[i] = code[i];

    return result;
  }

  /**
   * Returns the maximum stack size of a common code procedure.
   * @param code The common code procedure to retrieve the stack size.
   * The procedure MUST be one of the <CODE>code_*</CODE> constants.
   * @return The stack depth needed by the common code.
   */
  short getCodeMaxStack(short[] code) {
    if (code == code_moveTo) return 5;
    if (code == code_MOVE) return 5;
    if (code == code_TURNLEFT) return 6;
    if (code == code_PICKBEEPER) return 7;
    if (code == code_PUTBEEPER) return 6;
    if (code == code_TURNOFF) return 1;
    if (code == code_main) return 2;
    if (code == code_clinit) return 4;
    if (code == code_init1) return 4;
    if (code == code_init2) return 3;
    if (code == code_init3) return 3;
    if (code == code_ccFrontClear) return 5;
    if (code == code_ccRightClear) return 6;
    if (code == code_ccLeftClear) return 6;
    if (code == code_ccBackClear) return 6;
    if (code == code_ccNextToABeeper) return 3;
    if (code == code_ccNextToABeeper) return 3;
    if (code == code_ccAnyBeepersInBag) return 1;
    if (code == code_ccFacingNorth) return 2;
    if (code == code_ccFacingEast) return 2;
    if (code == code_ccFacingWest) return 2;
    if (code == code_ccFacingSouth) return 2;

    /* Should not get here! */
    return -1;
  }
}