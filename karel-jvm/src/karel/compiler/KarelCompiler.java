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
import karel.compiler.*;

/**
 * This class contains all information required when using the compiled
 * Karel program instruction defined by "DEFINE-NEW-INSTRUCTION" word.
 */
class DefinedInstruction {
  private String ident;
  private int index;
  private final static ClassFileAttribute[] noAttributes = {};

  /**
   * Here is a space to note whether the instruction's code has been already
   * defined.
   */
  boolean hasCode;

  /**
   * @param owner ClassFile in which is this instruction being compiled to.
   * @param ident Identification of the instruction.
   * @param index Index of the instruction's compiled method's reference entry
   * in the constant pool.
   */
  DefinedInstruction(ClassFile owner, String ident, int index) {
    this.ident = ident;
    this.index = index;
  }

  /**
   * @return Identifier of this instruction.
   */
  String getIdent() {
    return ident;
  }

  /**
   * @return Index of the instruction's compiled method's reference entry
   * in the constant pool.
   */
  int getIndex() {
    return index;
  }
}

/**
 * This class contains the main executive part of the Karel compiler.
 * After constructing an instance, call runCompiler() to compile.
 */
public final class KarelCompiler {
  private LineNumberReader fIn;
  private FileOutputStream fOut;
  private String className;
  private String srcFileName;
  private String lineBuffer;
  private ClassFile classFile;
  private CommonCode commonCode;

  // Current method code is built up here
  private ByteArrayOutputStream currMethod;
  // Current stack size of the currently built method
  private int currStack;
  // Maximum stack size of the currently built method
  private int currMaxStack;

  // All defined instructions (list of DefinedInstruction instances)
  private LinkedList instList;

  // utility constants
  private final static ClassFileAttribute[] noAttributes = {};
  private final static ClassFileExceptionTableEntry[] noExceptions = {};

  // token classes
  private final static int
          tkEOF = 0,
          tkReserved = 1,
          tkCondition = 2,
          tkIdent = 3,
          tkNumber = 4,
          tkInvalid = 5;

  private final static String[] reservedWords = {
    "BEGINNING-OF-PROGRAM",
    "END-OF-PROGRAM",
    "BEGINNING-OF-EXECUTION",
    "END-OF-EXECUTION",         //!
    "DEFINE-NEW-INSTRUCTION",
    "AS",
    "BEGIN",                    //!
    "END",                      //!
    "IF",                       //!
    "THEN",
    "ELSE",                     //!
    "ITERATE",                  //!
    "TIMES",
    "WHILE",                    //!
    "DO"
  };
  private final static String[] conditionWords = {
    "FRONT-IS-CLEAR",
    "FRONT-IS-BLOCKED",
    "LEFT-IS-CLEAR",
    "LEFT-IS-BLOCKED",
    "RIGHT-IS-CLEAR",
    "RIGHT-IS-BLOCKED",
    "BACK-IS-CLEAR",
    "BACK-IS-BLOCKED",
    "NEXT-TO-A-BEEPER",
    "NOT-NEXT-TO-A-BEEPER",
    "ANY-BEEPERS-IN-BEEPER-BAG",
    "NO-BEEPERS-IN-BEEPER-BAG",
    "FACING-NORTH",
    "NOT-FACING-NORTH",
    "FACING-SOUTH",
    "NOT-FACING-SOUTH",
    "FACING-EAST",
    "NOT-FACING-EAST",
    "FACING-WEST",
    "NOT-FACING-WEST"
  };

  /*
  private final static String[] reservedWords = {
    "KARLUV-PROGRAM",
    "KONEC-PROGRAMU",
    "ZACATEK-PROVADENI",
    "KONEC-PROVADENI",          //!
    "INSTRUKCE",
    "ZNAMENA",
    "ZACATEK",                  //!
    "KONEC",                    //!
    "JESTLI",                   //!
    "PAK",
    "JINAK",                    //!
    "OPAKUJ",                   //!
    "KRAT",
    "DOKUD",                    //!
    "DELEJ"
  };
  private final static String[] conditionWords = {
    "VPREDU-JE-VOLNO",
    "VPREDU-JE-ZED",
    "VLEVO-JE-VOLNO",
    "VLEVO-JE-ZED",
    "VPRAVO-JE-VOLNO",
    "VPRAVO-JE-ZED",
    "VZADU-JE-VOLNO",
    "VZADU-JE-ZED",
    "JE-PIPATKO",
    "NENI-PIPATKO",
    "MAM-PIPATKO-V-BATOHU",
    "NEMAM-PIPATKO-V-BATOHU",
    "DIVAM-SE-NA-SEVER",
    "NEDIVAM-SE-NA-SEVER",
    "DIVAM-SE-NA-JIH",
    "NEDIVAM-SE-NA-JIH",
    "DIVAM-SE-NA-VYCHOD",
    "NEDIVAM-SE-NA-VYCHOD",
    "DIVAM-SE-NA-ZAPAD",
    "NEDIVAM-SE-NA-ZAPAD"
  };*/

  private final static int
    rwBEGINNING_OF_PROGRAM   =  0,
    rwEND_OF_PROGRAM         =  1,
    rwBEGINNING_OF_EXECUTION =  2,
    rwEND_OF_EXECUTION       =  3,
    rwDEFINE_NEW_INSTRUCTION =  4,
    rwAS                     =  5,
    rwBEGIN                  =  6,
    rwEND                    =  7,
    rwIF                     =  8,
    rwTHEN                   =  9,
    rwELSE                   = 10,
    rwITERATE                = 11,
    rwTIMES                  = 12,
    rwWHILE                  = 13,
    rwDO                     = 14,

    cwFRONT_IS_CLEAR            =  0,
    cwFRONT_IS_BLOCKED          =  1,
    cwLEFT_IS_CLEAR             =  2,
    cwLEFT_IS_BLOCKED           =  3,
    cwRIGHT_IS_CLEAR            =  4,
    cwRIGHT_IS_BLOCKED          =  5,
    cwBACK_IS_CLEAR             =  6,
    cwBACK_IS_BLOCKED           =  7,
    cwNEXT_TO_A_BEEPER          =  8,
    cwNOT_NEXT_TO_A_BEEPER      =  9,
    cwANY_BEEPERS_IN_BEEPER_BAG = 10,
    cwNO_BEEPERS_IN_BEEPER_BAG  = 11,
    cwFACING_NORTH              = 12,
    cwNOT_FACING_NORTH          = 13,
    cwFACING_SOUTH              = 14,
    cwNOT_FACING_SOUTH          = 15,
    cwFACING_EAST               = 16,
    cwNOT_FACING_EAST           = 17,
    cwFACING_WEST               = 18,
    cwNOT_FACING_WEST           = 19;

  /** Is the character a "letter" in Karel programming language? */
  private boolean isLetter(char c) {
    return (((c>='A') && (c<='Z')) || ((c>='a') && (c<='z')) || (c=='-'));
    //return Character.isUnicodeIdentifierStart(c);
  }

  /** Is the character a "digit" in Karel programming language? */
  private boolean isDigit(char c) {
    return ((c>='0') && (c<='9'));
    //return Character.isDigit(c);
  }

  /** Is the character a "letter" or a "digit" in Karel programming language? */
  private boolean isAlphaNumeric(char c) {
    return (isLetter(c) || isDigit(c));
    //return Character.isUnicodeIdentifierPart(c);
  }

  /** Is the character a whitespace? */
  private boolean isWhitespace(char c) {
    //return (c <= ' ');
    return Character.isWhitespace(c);
  }

  /** Utility function for token classification. */
  private int classifyToken(String token) {
      char firstCharacter;

      if (token == null) return tkEOF;

      for (int i=0; i<reservedWords.length; i++)
          if (token.equals(reservedWords[i])) return tkReserved;

      for (int i=0; i<conditionWords.length; i++)
          if (token.equals(conditionWords[i])) return tkCondition;

      firstCharacter = token.charAt(0);
      if (isLetter(firstCharacter)) {
        boolean OK = true;

        for (int i=1; i<token.length(); i++)
            if (!isAlphaNumeric(token.charAt(i))) {
              OK = false;
              break;
            }

        if (OK) return tkIdent;
      }

      for (int i=0; i<token.length(); i++)
          if (!isDigit(token.charAt(i))) return tkInvalid;

      return tkNumber;
  }

  /** Utility function to classify a token (for an error message) */
  private String nameClassification(String token) throws KarelCompilationException {
    switch(classifyToken(token)) {
      case tkEOF:      return "end of file";
      case tkCondition:
      case tkReserved: return '"' + token + '"';
      case tkIdent:    return "identifier";
      case tkNumber:   return "number";
      case tkInvalid:  return "invalid character";
      default:         throw new KarelCompilationException(srcFileName,
                                      "INTERNAL#1", fIn.getLineNumber());
    }
  }

  /** Utility function to throw a compilation error exception. */
  private void errIllegalNumber() throws KarelCompilationException {
    throw new KarelCompilationException(srcFileName,
                                        "Illegal number format",
                                        fIn.getLineNumber()
                                       );
  }

  /** Utility function to throw a compilation error exception. */
  private void errInvalidCharacter(char c) throws KarelCompilationException {
    String msg = "Invalid character in source: ";

    if (c>=' ') msg = msg + '\'' + c + "' (";
    msg = msg + "0x" + Integer.toHexString(c);
    if (c>=' ') msg = msg + ')';

    throw new KarelCompilationException(srcFileName,
                                        msg,
                                        fIn.getLineNumber()
                                       );
  }

  /** Utility function to throw a compilation error exception. */
  private void errXExpectedYFound(String X, String Y) throws KarelCompilationException {
    throw new KarelCompilationException(srcFileName,
                                        X + " expected but " + Y + " found",
                                        fIn.getLineNumber()
                                       );
  }

  /** Utility function to throw a compilation error exception. */
  private void errXClassExpectedYFound(String X, String Y) throws KarelCompilationException {
    errXExpectedYFound(nameClassification(X), nameClassification(Y));
  }

  /** Utility function to throw a compilation error exception. */
  private void errXClassesExpectedYFound(String X1, String X2, String Y) throws KarelCompilationException {
    errXExpectedYFound(nameClassification(X1) +
                       " or " +
                       nameClassification(X2), nameClassification(Y));
  }

  /** Utility function to throw a compilation error exception. */
  private void errAlreadyDefined(String name) throws KarelCompilationException {
    throw new KarelCompilationException(srcFileName,
                                        name + " instruction already defined",
                                        fIn.getLineNumber()
                                       );
  }

  /** Utility function to throw a compilation error exception. */
  private void errInstructionsNotDefined(LinkedList undefineds) throws KarelCompilationException {
    String msg = "";
    ListIterator i = undefineds.listIterator();
    while (i.hasNext()) {
      String o = (String)i.next();
      msg = msg + o;
      if (i.hasNext()) msg = msg + ", ";
    }
    throw new KarelCompilationException(srcFileName,
                                        "Instruction(s) used without definition: "+msg,
                                        fIn.getLineNumber()
                                       );
  }

  /**
   * LEXICAL ANALYZER.
   * Reads the next token from the input stream.
   * @return The next token.
   */
  private String getToken() throws KarelCompilationException {
    lineBuffer = lineBuffer.trim();

    while (lineBuffer != null) {
      char firstChar;

      while (lineBuffer.length() == 0) {
        try {
          lineBuffer = fIn.readLine();
        } catch (IOException e) {
            throw new KarelCompilationException(srcFileName,
                                                "I/O Error: "+e.getMessage(),
                                                fIn.getLineNumber());
          }
        if (lineBuffer != null) lineBuffer = lineBuffer.trim();
        else return null;
      }

      firstChar = lineBuffer.charAt(0);
      if (firstChar == '/') {
        if ( (lineBuffer.length() > 1) &&
             (lineBuffer.charAt(1) == '/') ) lineBuffer = "";
        else errInvalidCharacter(firstChar);
      } else
        if (isLetter(firstChar)) {
          int i;
          String result;

          for (i=0; i<lineBuffer.length() &&
                    isAlphaNumeric(lineBuffer.charAt(i)); i++);

          result = lineBuffer.substring(0, i).toUpperCase();
          lineBuffer = lineBuffer.substring(i);

          return result;
        } else
          if (isDigit(firstChar)) {
            int i;
            String result;

            for (i=0; i<lineBuffer.length() &&
                      isDigit(lineBuffer.charAt(i)); i++);

            if (i<lineBuffer.length() && !isWhitespace(lineBuffer.charAt(i)))
               errIllegalNumber();

            result = lineBuffer.substring(0, i);
            lineBuffer = lineBuffer.substring(i);

            return result;
          } else errInvalidCharacter(firstChar);
    }

    throw new KarelCompilationException(srcFileName, "INTERNAL#2", fIn.getLineNumber());
  }

  /** Unget token back into the input stream */
  private void ungetToken(String token) {
    if (token != null)
       lineBuffer = token + " " + lineBuffer;
  }

  /** Method for "Block" non-terminal, after the "BEGIN" token has been processed. */
  private void processBeganBlock() throws IOException, KarelCompilationException {
    String token;

    do {
      token = getToken();
      if (token == null) errXClassExpectedYFound(reservedWords[rwEND], token);

      if (!token.equals(reservedWords[rwEND])) processBeganStatement(token);
    } while (!token.equals(reservedWords[rwEND]));
  }

  /**
   * Method for "Condition" non-terminal.
   * @return true if the condition is positive (i.e. the following code
   * should be processed on TRUE (i.e. it should be followed by ifeq)),
   * false in the opposite case.
   */
  private boolean processCondition() throws IOException, KarelCompilationException {
    String token = getToken();
    short[] code = null;
    boolean positive = false;
    short[][] condCode = {
      CommonCode.code_ccFrontClear, CommonCode.code_ccLeftClear, CommonCode.code_ccRightClear,
      CommonCode.code_ccBackClear, CommonCode.code_ccNextToABeeper,
      CommonCode.code_ccAnyBeepersInBag, CommonCode.code_ccFacingNorth,
      CommonCode.code_ccFacingSouth, CommonCode.code_ccFacingEast, CommonCode.code_ccFacingWest
    };

    if (token == null) errXExpectedYFound("condition", token);

    for (int i=0; i<conditionWords.length; i++) {
      if (conditionWords[i].equals(token)) {
        code = condCode[i/2];
        positive = (i & 1) == 0;
      }
    }

    if (code == null) errXExpectedYFound("condition", token);

    int stacksize = commonCode.getCodeMaxStack(code);
    if (currStack + stacksize > currMaxStack)
       currMaxStack = currStack + stacksize;

    // the condition result left on the stack
    currStack++;

    code = commonCode.createCode(code);

    for (int i=0; i<code.length; i++)
         currMethod.write(code[i]);

    return positive;
  }

  /** Method for "Conditional" non-terminal, after the "IF" token has been processed. */
  private void processBeganConditional() throws IOException, KarelCompilationException {
    String token;
    boolean ccPositive;
    ByteArrayOutputStream saveCode;

    ccPositive = processCondition();
    // the condition result left on the stack is popped by the conditional jump instruction
    currStack--;

    token = getToken();
    if (token == null || !token.equals(reservedWords[rwTHEN]))
       errXClassExpectedYFound(reservedWords[rwTHEN], token);

    saveCode = currMethod;
    currMethod = new ByteArrayOutputStream();

    token = getToken();
    if (token != null && !token.equals(reservedWords[rwELSE])) {
      int statementLen;
      processBeganStatement(token);
      token = getToken();
    }

    if (token != null && token.equals(reservedWords[rwELSE])) {
      ByteArrayOutputStream thenPart = currMethod;
      int statementLen;
      currMethod = new ByteArrayOutputStream();

      // the conditional contains an ELSE part
      processStatement();

      // the "skip elsepart" jump
      statementLen = currMethod.size() + 3; // 3 for the jump instruction itself
      if (statementLen > Short.MAX_VALUE) {
        statementLen += 2; // longer goto instruction
        thenPart.write(CommonCode.goto_w);
        thenPart.write((statementLen >> 24) & 0xFF);
        thenPart.write((statementLen >> 16) & 0xFF);
        thenPart.write((statementLen >> 8) & 0xFF);
        thenPart.write(statementLen & 0xFF);
      } else {
        thenPart.write(CommonCode._goto);
        thenPart.write((statementLen >> 8) & 0xFF);
        thenPart.write(statementLen & 0xFF);
      }

      // the "goto elsepart" jump
      statementLen = thenPart.size() + 3; // 3 for the jump instruction itself
      if (statementLen > Short.MAX_VALUE) {
        statementLen += 5; // longer goto instruction
        if (ccPositive) saveCode.write(CommonCode.ifne);
        else saveCode.write(CommonCode.ifeq);
        // size of the jump to skip:
        saveCode.write(0);
        saveCode.write(8);
        // now the jump
        saveCode.write(CommonCode.goto_w);
        saveCode.write((statementLen >> 24) & 0xFF);
        saveCode.write((statementLen >> 16) & 0xFF);
        saveCode.write((statementLen >> 8) & 0xFF);
        saveCode.write(statementLen & 0xFF);
      } else {
        if (ccPositive) saveCode.write(CommonCode.ifeq);
        else saveCode.write(CommonCode.ifne);
        saveCode.write((statementLen >> 8) & 0xFF);
        saveCode.write(statementLen & 0xFF);
      }

      thenPart.writeTo(saveCode);
      currMethod.writeTo(saveCode);
      currMethod = saveCode;
    } else {
      int statementLen;

      // the "goto elsepart" jump
      statementLen = currMethod.size() + 3; // 3 for the jump instruction itself
      if (statementLen > Short.MAX_VALUE) {
        statementLen += 5; // longer goto instruction
        if (ccPositive) saveCode.write(CommonCode.ifne);
        else saveCode.write(CommonCode.ifeq);
        // size of the jump to skip:
        saveCode.write(0);
        saveCode.write(8);
        // now the jump
        saveCode.write(CommonCode.goto_w);
        saveCode.write((statementLen >> 24) & 0xFF);
        saveCode.write((statementLen >> 16) & 0xFF);
        saveCode.write((statementLen >> 8) & 0xFF);
        saveCode.write(statementLen & 0xFF);
      } else {
        if (ccPositive) saveCode.write(CommonCode.ifeq);
        else saveCode.write(CommonCode.ifne);
        saveCode.write((statementLen >> 8) & 0xFF);
        saveCode.write(statementLen & 0xFF);
      }

      currMethod.writeTo(saveCode);
      currMethod = saveCode;

      ungetToken(token);
    }
  }

  /** Output instruction and prefix it with "wide" if required */
  private void outputVarWideInstruction(byte opcode, int operand) throws KarelCompilationException {
    if (operand > Short.MAX_VALUE)
       throw new KarelCompilationException(srcFileName,
                                      "Too many nested levels", fIn.getLineNumber());

    if (operand <= Byte.MAX_VALUE) {
       currMethod.write(opcode);
       currMethod.write(operand & 0xFF);
    } else {
       currMethod.write(commonCode.wide);
       currMethod.write(opcode);
       currMethod.write((operand >> 8) & 0xFF);
       currMethod.write(operand & 0xFF);
    }
  }

  /** Method for "IterateLoop" non-terminal, after the "ITERATE" token has been processed. */
  private void processBeganIteration() throws IOException, KarelCompilationException {
    String token = getToken();
    if (classifyToken(token) != tkNumber)
       errXClassExpectedYFound("1", token); // "1" is just any valid number

    int highBound = Integer.parseInt(token);

    token = getToken();
    if (token == null || !token.equals(reservedWords[rwTIMES]))
       errXClassExpectedYFound(reservedWords[rwTIMES], token);

    if (highBound<=5) currMethod.write(commonCode.iconst_0 + highBound);
    else if (highBound<=Byte.MAX_VALUE) {
                                          currMethod.write(commonCode.bipush);
                                          currMethod.write(highBound);
    } else {
      int boundIndex = classFile.addConstantPoolInteger(highBound);
      if (boundIndex<=Byte.MAX_VALUE) {
        currMethod.write(commonCode.ldc);
        currMethod.write(boundIndex);
      } else {
        currMethod.write(commonCode.ldc_w);
        currMethod.write((boundIndex >> 8) & 0xFF);
        currMethod.write(boundIndex & 0xFF);
      }
    }
    currStack++;
    if (currStack > currMaxStack) currMaxStack = currStack;

    ByteArrayOutputStream storeCode = currMethod;
    currMethod = new ByteArrayOutputStream();
    if (currStack+1 > currMaxStack) currMaxStack = currStack+1;
    currMethod.write(commonCode.dup);

    ByteArrayOutputStream storeLoopCode = currMethod;
    currMethod = new ByteArrayOutputStream();

    processStatement();

    // loop variable decrement
    if (currStack+1 > currMaxStack) currMaxStack = currStack+1;
    currMethod.write(commonCode.iconst_1);
    currMethod.write(commonCode.isub);

    int breakDelta = 8 + currMethod.size() + 5; // 8 for the long break jump, 5 for the long continue jump
    int continueDelta = - currMethod.size() - storeLoopCode.size() - 8; // 8 for the long break jump
    if (breakDelta <= Short.MAX_VALUE) {
      // shorter break jump
      continueDelta += 5;
      breakDelta -= 5;
    }
    if (continueDelta <= Short.MAX_VALUE) {
      // short continue jump
      breakDelta -= 2;
    }

    // continue jump
    if (continueDelta>Short.MAX_VALUE) {
      currMethod.write(commonCode.goto_w);
      currMethod.write((continueDelta >> 24) & 0xFF);
      currMethod.write((continueDelta >> 16) & 0xFF);
      currMethod.write((continueDelta >> 8) & 0xFF);
      currMethod.write(continueDelta & 0xFF);
    } else {
      currMethod.write(commonCode._goto);
      currMethod.write((continueDelta >> 8) & 0xFF);
      currMethod.write(continueDelta & 0xFF);
    }

    if (breakDelta>Short.MAX_VALUE) {
      // the "skip jump"
      storeLoopCode.write(commonCode.ifne);
      storeLoopCode.write(0); // size of the unconditional jump to skip
      storeLoopCode.write(8);
      storeLoopCode.write(commonCode.goto_w);
      storeLoopCode.write((breakDelta >> 24) & 0xFF);
      storeLoopCode.write((breakDelta >> 16) & 0xFF);
      storeLoopCode.write((breakDelta >> 8) & 0xFF);
      storeLoopCode.write(breakDelta & 0xFF);
    } else {
      storeLoopCode.write(commonCode.ifeq);
      storeLoopCode.write((breakDelta >> 8) & 0xFF);
      storeLoopCode.write(breakDelta & 0xFF);
    }

    storeLoopCode.writeTo(storeCode);
    currMethod.writeTo(storeCode);
    currMethod = storeCode;
    currStack--;
    currMethod.write(commonCode.pop); // pop the iteration counter (now zero) off the stack
  }

  /** Method for "WhileLoop" non-terminal, after the "WHILE" token has been processed */
  private void processBeganWhileLoop() throws IOException, KarelCompilationException {
    String token;
    ByteArrayOutputStream saveCode = currMethod;
    boolean ccPositive;

    currMethod = new ByteArrayOutputStream();
    ccPositive = processCondition();
    int condCodeLen = currMethod.size();
    currMethod.writeTo(saveCode);
    currMethod = new ByteArrayOutputStream();
    currStack--; // the condition result is popped by the conditional jump instruction

    token = getToken();
    if (token == null || !token.equals(reservedWords[rwDO]))
       errXClassExpectedYFound(reservedWords[rwDO], token);

    processStatement();

    int stmtSize = currMethod.size();
    int loopDelta = - (condCodeLen + 8 + stmtSize);
    int breakDelta = 8 + stmtSize + 5;

    if (breakDelta <= Short.MAX_VALUE) {
      // shorter break-jump
      loopDelta += 5;
      breakDelta -= 5;
    }
    if (loopDelta >= Short.MIN_VALUE) {
      // short loop-jump
      breakDelta -= 2;
    }

    // the break jump
    if (breakDelta > Short.MAX_VALUE) {
        if (ccPositive) saveCode.write(CommonCode.ifne);
        else saveCode.write(CommonCode.ifeq);
        // size of the jump to skip:
        saveCode.write(0);
        saveCode.write(8);
        // now the jump
        saveCode.write(CommonCode.goto_w);
        saveCode.write((breakDelta >> 24) & 0xFF);
        saveCode.write((breakDelta >> 16) & 0xFF);
        saveCode.write((breakDelta >> 8) & 0xFF);
        saveCode.write(breakDelta & 0xFF);
    } else {
        if (ccPositive) saveCode.write(CommonCode.ifeq);
        else saveCode.write(CommonCode.ifne);
        saveCode.write((breakDelta >> 8) & 0xFF);
        saveCode.write(breakDelta & 0xFF);
    }

    currMethod.writeTo(saveCode);

    // the loop jump
    if (loopDelta < Short.MIN_VALUE) {
        saveCode.write(CommonCode.goto_w);
        saveCode.write((loopDelta >> 24) & 0xFF);
        saveCode.write((loopDelta >> 16) & 0xFF);
        saveCode.write((loopDelta >> 8) & 0xFF);
        saveCode.write(loopDelta & 0xFF);
    } else {
        saveCode.write(CommonCode._goto);
        saveCode.write((loopDelta >> 8) & 0xFF);
        saveCode.write(loopDelta & 0xFF);
    }

    currMethod = saveCode;
  }

  /**
   * Method for "Statement" non-terminal, after its first token has been read.
   * @param firstToken The first token of the statement.
   */
  private void processBeganStatement(String firstToken) throws IOException, KarelCompilationException {
    if (firstToken == null) {}
    else if (firstToken.equals(reservedWords[rwBEGIN])) processBeganBlock();
    else if (firstToken.equals(reservedWords[rwIF])) processBeganConditional();
    else if (firstToken.equals(reservedWords[rwITERATE])) processBeganIteration();
    else if (firstToken.equals(reservedWords[rwWHILE])) processBeganWhileLoop();
    else {
      // call to an instruction
      int instIndex = findInstruction(firstToken).getIndex();
      if (currStack+1 > currMaxStack) currMaxStack = currStack+1;
      currMethod.write(CommonCode.aload_0);
      currMethod.write(CommonCode.invokespecial);
      currMethod.write((instIndex >> 8) & 0xFF);
      currMethod.write(instIndex & 0xFF);
    };
  }

  /** Method for "Statement" non-terminal */
  private void processStatement() throws IOException, KarelCompilationException {
    processBeganStatement(getToken());
  }

  /** Method for "MainDeclaration" non-terminal, after BEGINNING-OF-EXECUTION token has been processed. */
  private void processBeganMainDeclaration() throws IOException, KarelCompilationException {
    String token;
    short[] processedCode;
    ClassFileExceptionTableEntry[] exceptionTable;
    ClassFileAttribute[] attributes;
    short exceptionStartPC;
    int stackSize;

    currMethod = new ByteArrayOutputStream();
    currStack = 0;
    currMaxStack = 0;

    // the common code for constructor
    processedCode = commonCode.createCode(commonCode.code_init1);
    stackSize = commonCode.getCodeMaxStack(commonCode.code_init1);
    if (currStack + stackSize > currMaxStack)
       currMaxStack = currStack + stackSize;
    for (int i=0; i<processedCode.length; i++)
        currMethod.write(processedCode[i]);

    exceptionStartPC = (short)currMethod.size();

    do {
      token = getToken();
      if (token == null)
         errXExpectedYFound(reservedWords[rwEND_OF_EXECUTION], token);

      if (!token.equals(reservedWords[rwEND_OF_EXECUTION]))
         processBeganStatement(token);
    } while (!token.equals(reservedWords[rwEND_OF_EXECUTION]));

    // the common code for the end of the constructor
    stackSize = commonCode.getCodeMaxStack(commonCode.code_init2);
    if (currStack + stackSize > currMaxStack)
       currMaxStack = currStack + stackSize;
    processedCode = commonCode.createCode(commonCode.code_init2);
    for (int i=0; i<processedCode.length; i++)
        currMethod.write(processedCode[i]);

    exceptionTable = new ClassFileExceptionTableEntry[1];
    exceptionTable[0] = new ClassFileExceptionTableEntry(classFile,
                            (short)exceptionStartPC, (short)currMethod.size(), (short)currMethod.size(),
                            "karel.common.KarelException");

    // the common code for the constructor's exception handler
    stackSize = commonCode.getCodeMaxStack(commonCode.code_init3);
    if (currStack + stackSize > currMaxStack)
       currMaxStack = currStack + stackSize;
    processedCode = commonCode.createCode(commonCode.code_init3);
    for (int i=0; i<processedCode.length; i++)
        currMethod.write(processedCode[i]);

    attributes = new ClassFileAttribute[1];
    attributes[0] = ClassFileAttribute.newCode(classFile,
                  (short) currMaxStack, (short) 2, currMethod,
                  exceptionTable, noAttributes);
    classFile.addMethodWithEntry((short)ClassFile.ACC_PRIVATE, "<init>", "([Ljava.lang.String;)V", attributes);
  }

  /** Constructs a "common code" method */
  private int constructMethod(String name, String descriptor,
                               short[] code, int max_stack, int max_locals,
                               ClassFileExceptionTableEntry[] exception_table) {
    return constructMethod(name, descriptor, code, max_stack, max_locals,
                                 exception_table, ClassFile.ACC_PRIVATE);
  }

  /** Constructs a "common code" method, with special access_flags. */
  private int constructMethod(String name, String descriptor,
                               short[] code, int max_stack, int max_locals,
                               ClassFileExceptionTableEntry[] exception_table,
                               short access_flags) {
    ClassFileAttribute[] attributes;
    short[] processedCode = commonCode.createCode(code);

    currMethod = new ByteArrayOutputStream(processedCode.length);
    for (int i=0; i<processedCode.length; i++)
        currMethod.write(processedCode[i]);

    attributes = new ClassFileAttribute[1];
    attributes[0] = ClassFileAttribute.newCode(classFile,
                  (short) max_stack, (short) max_locals, currMethod,
                  exception_table, noAttributes);
    return classFile.addMethodWithEntry(access_flags, name, descriptor, attributes);
  }

  /**
   * Defines a code to (possibly already used) instruction.
   * @throws KarelCompilationException when the instruction has been
   * already defined.
   */
  private int defineInstructionCode(String name, int max_stack, int max_locals,
                        ClassFileExceptionTableEntry[] exception_table) throws KarelCompilationException {
    ClassFileAttribute[] attributes = new ClassFileAttribute[1];
    attributes[0] = ClassFileAttribute.newCode(classFile,
                  (short) max_stack, (short) max_locals, currMethod,
                  exception_table, noAttributes);

    DefinedInstruction inst = findInstruction(name);
    if (inst.hasCode)
       errAlreadyDefined(name);
    inst.hasCode = true;

    return classFile.addMethod(new ClassFileMethod(
                     classFile, ClassFile.ACC_PRIVATE, name, "()V", attributes)
                     );
  }

  /** Defines a built in instruction (one of MOVE, TURNLEFT, ...) */
  private int defineBuiltInInstruction(String name, short[] code,
                                       int max_stack, int max_locals) throws KarelCompilationException {
    short[] processedCode = commonCode.createCode(code);

    currMethod = new ByteArrayOutputStream(processedCode.length);
    for (int i=0; i<processedCode.length; i++)
        currMethod.write(processedCode[i]);

    ClassFileAttribute[] attributes = new ClassFileAttribute[1];
    attributes[0] = ClassFileAttribute.newCode(classFile,
                  (short) max_stack, (short) max_locals, currMethod,
                  noExceptions, noAttributes);

    DefinedInstruction inst = findInstruction(name);
    if (inst.hasCode)
       throw new KarelCompilationException(srcFileName,
                                      "INTERNAL#3", fIn.getLineNumber());
    inst.hasCode = true;

    return classFile.addMethod(new ClassFileMethod(
                     classFile, ClassFile.ACC_PRIVATE, name, "()V", attributes)
                     );
  }

  /**
   * Finds a defined instruction. If such instruction has been found,
   * returns it. If not, create a new instruction, which has no code
   * assigned to it. (The code has to be defined later.)
   * @param name Name of the instruction.
   * @return The DefinedInstruction
   */
  private DefinedInstruction findInstruction(String name) {
    ListIterator i = instList.listIterator();

    while (i.hasNext()) {
      DefinedInstruction inst = (DefinedInstruction) i.next();

      if (name.equals(inst.getIdent())) {
        return inst;
      }
    }

    // not found => create new
    DefinedInstruction newinst = new DefinedInstruction(classFile, name,
                                       classFile.addConstantPoolMethodRef(className, name, "()V")
                                     );
    instList.add(newinst);

    return newinst;
  }

  /**
   * Compiles the program.
   * @throws KarelCompilationException if a compile-time error occurs.
   */
  public void runCompiler() throws KarelCompilationException {
    ClassFileExceptionTableEntry[] exceptionTblKarelException = new ClassFileExceptionTableEntry[1];
    String token;

    try {
    commonCode = new CommonCode();
    classFile = new ClassFile((short)(ClassFile.ACC_PUBLIC | ClassFile.ACC_SUPER), className, "java.lang.Object");
    commonCode.class_this = classFile.getThisClass();

    if (srcFileName.length() > 0)
      classFile.addAttribute(ClassFileAttribute.newSourceFile(classFile, srcFileName));

    token = getToken();
    if (token == null || !token.equals(reservedWords[rwBEGINNING_OF_PROGRAM]))
       errXClassExpectedYFound(reservedWords[rwBEGINNING_OF_PROGRAM], token);

    // Create the fields of the class
    commonCode.var_dX = classFile.addFieldWithEntry(
                   (short)(ClassFile.ACC_PRIVATE | ClassFile.ACC_FINAL | ClassFile.ACC_STATIC),
                   "dX",
                   "[I", noAttributes
              );
    commonCode.var_dY = classFile.addFieldWithEntry(
                   (short)(ClassFile.ACC_PRIVATE | ClassFile.ACC_FINAL | ClassFile.ACC_STATIC),
                   "dY",
                   "[I", noAttributes
              );
    commonCode.var_city = classFile.addFieldWithEntry(
                   ClassFile.ACC_PRIVATE,
                   "city",
                   "Lkarel.city.KarelCity;", noAttributes
              );
    commonCode.var_xPos = classFile.addFieldWithEntry(
                   ClassFile.ACC_PRIVATE,
                   "xPos",
                   "I", noAttributes
              );
    commonCode.var_yPos = classFile.addFieldWithEntry(
                   ClassFile.ACC_PRIVATE,
                   "yPos",
                   "I", noAttributes
              );
    commonCode.var_direction = classFile.addFieldWithEntry(
                   ClassFile.ACC_PRIVATE,
                   "direction",
                   "I", noAttributes
              );
    commonCode.var_hasBeepers = classFile.addFieldWithEntry(
                   ClassFile.ACC_PRIVATE,
                   "hasBeepers",
                   "I", noAttributes
              );

    commonCode.class_KarelException = classFile.addConstantPoolClass("karel.common.KarelException");
    commonCode.class_KarelCity = classFile.addConstantPoolClass("karel.city.KarelCity");

    commonCode.string_KarelHitTheWall = classFile.addConstantPoolString("Karel hit the wall!");
    commonCode.string_NoBeepersAreNearby = classFile.addConstantPoolString("No beepers are nearby!");
    commonCode.string_NoBeepersInBag = classFile.addConstantPoolString("Karel has no beepers in his bag!");
    commonCode.string_NoTurnOffOccurred = classFile.addConstantPoolString("No turnoff has occurred!");

    commonCode.method_city_exceptionCatched = classFile.addConstantPoolMethodRef("karel.city.KarelCity","exceptionCatched","(Lkarel.common.KarelException;)V");
    commonCode.method_city_getBeepers = classFile.addConstantPoolMethodRef("karel.city.KarelCity","getBeepers","(II)I");
    commonCode.method_city_setBeepers = classFile.addConstantPoolMethodRef("karel.city.KarelCity","setBeepers","(III)V");
    commonCode.method_city_updateKarel = classFile.addConstantPoolMethodRef("karel.city.KarelCity","updateKarel","(IIII)V");
    commonCode.method_city_getInitDirection = classFile.addConstantPoolMethodRef("karel.city.KarelCity","getInitDirection","()I");
    commonCode.method_city_getInitXPos = classFile.addConstantPoolMethodRef("karel.city.KarelCity","getInitXPos","()I");
    commonCode.method_city_getInitYPos = classFile.addConstantPoolMethodRef("karel.city.KarelCity","getInitYPos","()I");
    commonCode.method_city_getInitBagSize = classFile.addConstantPoolMethodRef("karel.city.KarelCity","getInitBagSize","()I");
    commonCode.method_city_turnOff = classFile.addConstantPoolMethodRef("karel.city.KarelCity","turnOff","()V");
    commonCode.method_city_INIT = classFile.addConstantPoolMethodRef("karel.city.KarelCity","<init>","([Ljava.lang.String;)V");
    commonCode.method_KarelExceptionINIT = classFile.addConstantPoolMethodRef("karel.common.KarelException","<init>","(Ljava.lang.String;)V");
    commonCode.method_System_exit = classFile.addConstantPoolMethodRef("java.lang.System", "exit", "(I)V");
    commonCode.method_INIT = classFile.addConstantPoolMethodRef(className, "<init>", "([Ljava.lang.String;)V");
    commonCode.method_isInCity = classFile.addConstantPoolMethodRef("karel.city.KarelCity", "cityFieldFree", "(II)Z");
    commonCode.method_ObjectINIT = classFile.addConstantPoolMethodRef("java.lang.Object", "<init>", "()V");

    commonCode.method_moveTo = constructMethod("moveTo", "(II)V", commonCode.code_moveTo,
                             commonCode.getCodeMaxStack(commonCode.code_moveTo), 3, noExceptions);
    constructMethod("main", "([Ljava.lang.String;)V", commonCode.code_main,
                            commonCode.getCodeMaxStack(commonCode.code_main), 1, noExceptions,
                            (short)(ClassFile.ACC_PUBLIC | ClassFile.ACC_STATIC));
    constructMethod("<clinit>", "()V", commonCode.code_clinit,
                            commonCode.getCodeMaxStack(commonCode.code_clinit),
                            0, noExceptions, ClassFile.ACC_STATIC);

    defineBuiltInInstruction("MOVE", commonCode.code_MOVE,
                                     commonCode.getCodeMaxStack(commonCode.code_MOVE), 1);
    defineBuiltInInstruction("TURNLEFT", commonCode.code_TURNLEFT,
                                         commonCode.getCodeMaxStack(commonCode.code_TURNLEFT), 1);
    defineBuiltInInstruction("PICKBEEPER", commonCode.code_PICKBEEPER,
                                           commonCode.getCodeMaxStack(commonCode.code_PICKBEEPER), 1);
    defineBuiltInInstruction("PUTBEEPER", commonCode.code_PUTBEEPER,
                                          commonCode.getCodeMaxStack(commonCode.code_PUTBEEPER), 1);
    defineBuiltInInstruction("TURNOFF", commonCode.code_TURNOFF,
                                        commonCode.getCodeMaxStack(commonCode.code_TURNOFF), 1);

    token = getToken();
    while (token != null && token.equals(reservedWords[rwDEFINE_NEW_INSTRUCTION])) {
      String ident = getToken();

      if (classifyToken(ident) != tkIdent)
        errXClassExpectedYFound("A", ident); // "A" is just any valid identifier

      token = getToken();
      if (token == null || !token.equals(reservedWords[rwAS]))
        errXClassExpectedYFound(reservedWords[rwAS], token);

      currMethod = new ByteArrayOutputStream();
      currStack = 0;
      currMaxStack = 0;
      processStatement();
      currMethod.write(CommonCode._return);
      defineInstructionCode(ident, currMaxStack, 1, noExceptions);

      token = getToken();
    }

    if (token == null || !token.equals(reservedWords[rwBEGINNING_OF_EXECUTION]))
       errXClassesExpectedYFound(reservedWords[rwDEFINE_NEW_INSTRUCTION],
                                 reservedWords[rwBEGINNING_OF_EXECUTION], token);

    // process the main executable code (converted into constructor)
    processBeganMainDeclaration();

    // check if all used instructions were defined
    ListIterator i = instList.listIterator();
    LinkedList undefineds = new LinkedList();
    while (i.hasNext()) {
      DefinedInstruction inst = (DefinedInstruction) i.next();
      if (!inst.hasCode)
         undefineds.add(inst.getIdent());
    }
    if (undefineds.size() != 0)
      errInstructionsNotDefined(undefineds);
    else
      undefineds = null; // dispose
    } catch(Exception e) {
      throw new KarelCompilationException(srcFileName,
                                      e.getMessage(), fIn.getLineNumber());
    }

    try {
      classFile.write("");
    } catch (Exception e) {
      throw new KarelCompilationException(srcFileName,
                                      e.getMessage(), fIn.getLineNumber());
    }
  }

  /** Compiler initialization */
  private void initCompiler() {
    instList = new LinkedList();
    lineBuffer = "";
  }

  /**
   * Creates a compiler which reads the source from any LineNumberReader
   * stream, writing the resulting code to any FileOutputStream.
   * @param karelSource The source stream containing the source text in the
   * Karel programming language.
   * @param resultClassName Identifier of the resulting class.
   * @param classFile The destination stream which will get the resulting
   * code in the .class format.
   */
  public KarelCompiler(LineNumberReader karelSource, String resultClassName,
                       FileOutputStream classFile) {
    fIn = karelSource;
    fOut = classFile;
    className = resultClassName;
    srcFileName = "";
    initCompiler();
  }

  /**
   * Creates a compiler which reads the source from a file and writes
   * the resulting code to a file.
   * @param karelSourceFName Filename of the source file, containing the source
   * text of the program in the Karel programming language. May contain path.
   * @param resultClassName Identifier of the resulting class.
   * @param classFileName Filename of the destination file, which will be
   * overwritten by the .class format program.
   * @throws FileNotFoundException if any filename specified cannot be opened.
   */
  public KarelCompiler(String karelSourceFName, String resultClassName,
                       String classFileName) throws FileNotFoundException {
    srcFileName = karelSourceFName;
    className = resultClassName;

    fIn = new LineNumberReader(new FileReader(karelSourceFName));
    fOut = new FileOutputStream(classFileName);

    initCompiler();
  }
}