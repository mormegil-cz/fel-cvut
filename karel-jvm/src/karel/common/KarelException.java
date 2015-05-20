package karel.common;

/**
 * Exception class for run-time errors in "Karel VM".
 */
public class KarelException extends Exception {
    /** General constructor without a specific error message. */
    public KarelException() {
        super();
    }
    /**
     * Constructor with a specific error message.
     * @param msg Error message.
     */
    public KarelException(String msg) {
        super(msg);
    }
}
