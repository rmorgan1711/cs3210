/**
 * Enumeration for parse errors that will be summarized. Each
 * error type has a corresponding error description printed
 * in the summary.
 */

public enum ParseError{
    StartError,
    EndError,
    LabelError,
    BadOpCode,
    BadOperand,
    WhiteSpaceError,
    DefineError,
    NoCodeFound,
}

