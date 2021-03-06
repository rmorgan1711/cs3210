import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Translation program
 */
public class Main {

    public static String EXT = ".pal";
    public static String EXT_ALT = ".txt";
    public static Set<String> Registers = MakeRegisterSet();
    public static Set<String> RegOperands = Registers.stream().map(r -> r + ",").collect(Collectors.toSet());
    public static Map<String, OpGroup> OpTypeMap = MakeOpTypeMap();
    public static Map<ParseError, Integer> ErrNumMap = MakeErrorCountMap();
    public static Map<ParseError, String> ErrDesc = MakeErrorDescriptionMap();
    public static Map<String, String> MemLabels = new HashMap<>();
    public static Map<String, String> MemLabOperands = new HashMap<>();
    public static Map<String, Integer> LabelsDefined = new HashMap<>();
    public static Set<String> LabelsBranchedTo = new HashSet<>();
    public static int CodeLine = 1;

    public static void main(String[] args){

        System.out.println("The searching for " + args[0]);
        System.out.println("at location");
        System.out.println("\t" + Paths.get("").toAbsolutePath().toString());

        String fileName = args[0];
        List<String> lines = FileOps.ReadFile(fileName, EXT, EXT_ALT);
        if (lines.size() < 1)
            return;

        lines = ConsumeToCode(lines);

        List<String> outLines = FileOps.MakeHeaderLines(fileName);
        if (lines.size() < 1){
            outLines.add("** Error: File contains no code");
            ErrNumMap.put(ParseError.NoCodeFound, ErrNumMap.get(ParseError.NoCodeFound) + 1);
            outLines.addAll(FileOps.MakeFooterLines(ErrNumMap, ErrDesc));
            FileOps.WriteLogFile(fileName + ".log", outLines);
            return;
        }

        lines.set(0, RemoveComment(lines.get(0)));
        String[] startLine = lines.get(0).replaceAll("\\s+", " ").split(" ");
        if (!startLine[0].equals("SRT")){
            outLines.add("** error: Program must begin with SRT. Labels and tabs are not allowed.");
            ErrNumMap.put(ParseError.StartError, ErrNumMap.get(ParseError.StartError) + 1);
        }else {
            outLines.add(CodeLine + ".\t" + lines.get(0));
            CodeLine++;
            lines.remove(0);
        }

        lines = ConsumeToCode(lines);
        List<String> defLines = ConsumeDefinitionLines(lines, CodeLine);
        outLines.addAll(defLines);
        CodeLine = CodeLine + defLines.size();

        String line;
        boolean endFound = false;
        for (int i = 0; i < lines.size(); i++){
            line = lines.get(i);

            if (line.contains("\t")){
                outLines.add("** Error: Tab is used instead of space");
                ErrNumMap.put(ParseError.WhiteSpaceError, ErrNumMap.get(ParseError.WhiteSpaceError) + 1);
                continue;
            }

            line = RemoveComment(line);
            if (line.isEmpty())
                continue;

            outLines.add((CodeLine++) + ".\t" + line);

            if (line.equals("END")) {
                endFound = true;
                break;
            }

            String errorStr = CheckLabel(line);
            if (!errorStr.isEmpty()){
                outLines.add(errorStr);
                ErrNumMap.put(ParseError.LabelError, ErrNumMap.get(ParseError.LabelError) + 1);
                continue;
            }

            line = line.substring(line.lastIndexOf(':') + 1).trim(); // strip any label
            if (line.isEmpty())
                continue;

            line = line.replaceAll("\\s+", " ");
            String[] tokens = line.split(" ");

            if (!OpTypeMap.containsKey(tokens[0])){
                outLines.add("** Error: The opcode is not recognized");
                ErrNumMap.put(ParseError.BadOpCode, ErrNumMap.get(ParseError.BadOpCode) + 1);
                continue;
            }

            OpGroup op = OpTypeMap.get(tokens[0]);
            errorStr = ProcessOpCode(op, line, tokens);
            if (!errorStr.isEmpty()){
                outLines.add(errorStr);
                continue;
            }
        }

        if (!endFound){
            outLines.add("** Error: END opcode not found alone on a line");
            ErrNumMap.put(ParseError.EndError, ErrNumMap.get(ParseError.EndError) + 1);
        }

        outLines.addAll(CheckLabelBranchAndDef());
        outLines.addAll(FileOps.MakeFooterLines(ErrNumMap, ErrDesc));
        FileOps.WriteLogFile(fileName + ".log", outLines);
    }

    //***************************************
    // Translation operations
    //***************************************
    /**
     * Consume file lines until a code line is reached
     * @param lines All lines of the file to process
     * @return Lines of the file to process with beginning blank
     *          and comment lines removed
     */
    public static List<String> ConsumeToCode(List<String> lines){

        for (int i = 0; i < lines.size(); ){
            String line = lines.get(i);

            if (line.contains(";"))
                line = line.substring(0, line.indexOf(';'));

            line = line.trim();
            if (line.length() == 0)
                lines.remove(0);
            else
                break;
        }
        return lines;
    }

    /**
     * Remove any comment from a line
     * @param line The line to remove comments from
     * @return The line stripped of any comments
     */
    public static String RemoveComment(String line){
        if (line.contains(";"))
            line = line.substring(0, line.indexOf(';'));

        line = line.trim();
        return line;
    }

    /**
     * Process and remove definition (DEF) lines and record any
     * errors found
     * @param lines Lines to process
     * @param lineNum The line num for the code in the log file
     * @return All lines following the last valid definition line
     */
    public static List<String> ConsumeDefinitionLines(List<String> lines, int lineNum){
        List<String> outLines = new ArrayList<>();
        String line;
        lines.set(0, RemoveComment(lines.get(0)).replaceAll("\\s+", " "));
        while (lines.size() > 0 && lines.get(0).split(" ")[0].equals("DEF")){
            line = lines.get(0);
            outLines.add(lineNum + ".\t" + line);
            lineNum++;
            lines.remove(0);

            String[] tokens = line.split(" ");
            String errorMsg = CheckOperandNum(line, 2);
            if (!errorMsg.isEmpty()){
                outLines.add(errorMsg);
                ErrNumMap.put(ParseError.BadOperand, ErrNumMap.get(ParseError.BadOperand) + 1);
                continue;
            }

            tokens[1] = tokens[1].substring(0, tokens[1].indexOf(","));
            errorMsg = CheckLabelFormat(tokens[1]);
            if (!errorMsg.isEmpty()){
                outLines.add(errorMsg);
                ErrNumMap.put(ParseError.LabelError, ErrNumMap.get(ParseError.LabelError) + 1);
                continue;
            }

            if (!tokens[2].matches("[0-7]+")){
                outLines.add("** Error: 2nd operand must be base 8 immediate value");
                ErrNumMap.put(ParseError.BadOperand, ErrNumMap.get(ParseError.BadOperand) + 1);
                continue;
            }

            MemLabels.put(tokens[1], tokens[2]);
            MemLabOperands.put(tokens[1] + ",", tokens[2]);
        }
        return outLines;
    }

    /**
     * Check a line for a label error
     * @param line The line to check
     * @return The error message or "" if no error
     */
    public static String CheckLabel(String line){
        if (line.contains(":")){
            if (line.chars().filter(ch -> ch == ':').count() > 1){
                return "** error: Only one label per line";
            }

            String label = line.substring(0, line.indexOf(':')).trim();
            String errorMsg = CheckLabelFormat(label);
            if (!errorMsg.isEmpty()){
                return errorMsg;
            }

            if (LabelsDefined.containsKey(label))
                LabelsDefined.put(label, LabelsDefined.get(label) + 1);
            else
                LabelsDefined.put(label, 1);
        }
        return "";
    }

    /**
     * Check the format of the label excluding the ":" character
     * @param label The string to check
     * @return The error message or "" if no error
     */
    public static String CheckLabelFormat(String label){
        String errorMsg = "";
        if (label.length() > 5){
            errorMsg = "** error: Labels must be 5 letters or less";
        }
        else if (!label.matches("[a-zA-Z]+")){
            errorMsg = "** error: Labels must use only a-z and A-Z";
        }
        else if (Registers.contains(label)){
            errorMsg = "** error: A register is not a valid label";
        }
        return errorMsg;
    }

    /**
     * Process opcode and return error message if any
     * @param op The op code type to check
     * @param line The line containing the op code
     * @param tokens Array of white separated strings in the line
     * @return The error message or "" if no error
     */
    public static String ProcessOpCode(OpGroup op, String line, String[] tokens){
        String errorMsg = "";

        if (op == OpGroup.Start) {
            errorMsg = "** Error: Code appears before SRT opcode";
            ErrNumMap.put(ParseError.StartError, ErrNumMap.get(ParseError.StartError) + 1);
        }

        if (op == OpGroup.End && tokens.length > 1){
            errorMsg = "** Error: END must be alone on a line";
            ErrNumMap.put(ParseError.EndError, ErrNumMap.get(ParseError.EndError) + 1);
        }

        if (op == OpGroup.OneOp){
            errorMsg = CheckOperandNum(line, 1);
            if (!errorMsg.isEmpty()){
                ErrNumMap.put(ParseError.BadOperand, ErrNumMap.get(ParseError.BadOperand) + 1);
            }
            else if (!Registers.contains(tokens[1]) && !MemLabels.containsKey(tokens[1])){
                errorMsg = "** Error: The operand is not a register or defined mem label";
                ErrNumMap.put(ParseError.BadOperand, ErrNumMap.get(ParseError.BadOperand) + 1);
            }
        }

        if (op == OpGroup.ThreeOp){
            errorMsg = CheckOperandNum(line, 3);
            boolean opError = false;
            if (!errorMsg.isEmpty()){
                ErrNumMap.put(ParseError.BadOperand, ErrNumMap.get(ParseError.BadOperand) + 1);
            }
            else if (!RegOperands.contains(tokens[1]) && !MemLabOperands.containsKey(tokens[1])){
                opError = true;
            }
            else if (!RegOperands.contains(tokens[2]) && !MemLabOperands.containsKey(tokens[2])){
                opError = true;
            }
            else if (!Registers.contains(tokens[3]) && !MemLabOperands.containsKey(tokens[3])){
                opError = true;
            }

            if (opError){
                errorMsg = "** Error: An operand is not a register or defined mem label";
                ErrNumMap.put(ParseError.BadOperand, ErrNumMap.get(ParseError.BadOperand) + 1);
            }
        }

        if (op == OpGroup.TestAndBranch){
            errorMsg = CheckOperandNum(line, 3);
            if (!errorMsg.isEmpty()){
                ErrNumMap.put(ParseError.BadOperand, ErrNumMap.get(ParseError.BadOperand) + 1);
            }
            else if (!RegOperands.contains(tokens[1]) && !MemLabOperands.containsKey(tokens[1])){
                errorMsg = "** Error: Operands 1 and 2 must be registers or defined mem labels";
                ErrNumMap.put(ParseError.BadOperand, ErrNumMap.get(ParseError.BadOperand) + 1);
            }
            else if (!RegOperands.contains(tokens[2]) && !MemLabOperands.containsKey(tokens[2])){
                errorMsg = "** Error: Operands 1 and 2 must be registers or defined mem labels";
                ErrNumMap.put(ParseError.BadOperand, ErrNumMap.get(ParseError.BadOperand) + 1);
            }
            else{
                errorMsg = CheckLabelFormat(tokens[3]);
                if (!errorMsg.isEmpty()){
                    ErrNumMap.put(ParseError.BadOperand, ErrNumMap.get(ParseError.BadOperand) + 1);
                }
            }

            if (errorMsg.isEmpty())
                LabelsBranchedTo.add(tokens[3]);
        }

        if (op == OpGroup.Branch){
            errorMsg = CheckOperandNum(line, 1);
            if (!errorMsg.isEmpty()){
                ErrNumMap.put(ParseError.BadOperand, ErrNumMap.get(ParseError.BadOperand) + 1);
            }
            else{
                errorMsg = CheckLabelFormat(tokens[1]);
                if (!errorMsg.isEmpty()){
                    ErrNumMap.put(ParseError.BadOperand, ErrNumMap.get(ParseError.BadOperand) + 1);
                }
            }

            if (errorMsg.isEmpty())
                LabelsBranchedTo.add(tokens[1]);
        }

        if (op == OpGroup.Copy){
            errorMsg = CheckOperandNum(line, 2);
            if (!errorMsg.isEmpty()){
                ErrNumMap.put(ParseError.BadOperand, ErrNumMap.get(ParseError.BadOperand) + 1);
            }
            else if (!RegOperands.contains(tokens[1]) && !MemLabOperands.containsKey(tokens[1])){
                errorMsg = "** Error: Operands must be registers or defined memory labels";
                ErrNumMap.put(ParseError.BadOperand, ErrNumMap.get(ParseError.BadOperand) + 1);
            }
            else if (!Registers.contains(tokens[2]) && !MemLabels.containsKey(tokens[2])){
                errorMsg = "** Error: Operands must be registers or defined memory labels";
                ErrNumMap.put(ParseError.BadOperand, ErrNumMap.get(ParseError.BadOperand) + 1);
            }
        }

        if (op == OpGroup.Move){
            errorMsg = CheckOperandNum(line, 2);
            if (!errorMsg.isEmpty()){
                ErrNumMap.put(ParseError.BadOperand, ErrNumMap.get(ParseError.BadOperand) + 1);
            }else if (!tokens[1].matches("[0-7]+,")){
                errorMsg = "** Error: 1st operand must be base 8 immediate value";
                ErrNumMap.put(ParseError.BadOperand, ErrNumMap.get(ParseError.BadOperand) + 1);
            }
            else if (!Registers.contains(tokens[2]) && !MemLabels.containsKey(tokens[2])){
                errorMsg = "** Error: 2nd operand must be a register or defined memory label";
                ErrNumMap.put(ParseError.BadOperand, ErrNumMap.get(ParseError.BadOperand) + 1);
            }
        }

        if (op == OpGroup.Define){
            errorMsg = "** Error: Definitions must immediately follow SRT";
            ErrNumMap.put(ParseError.DefineError, ErrNumMap.get(ParseError.DefineError) + 1);
        }

        return errorMsg;
    }

    /**
     * Check that a line contains the expected number of operands
     * @param line The line to check
     * @param num The number of operands expected
     * @return The error message or "" if no error
     */
    public static String CheckOperandNum(String line, int num){
        String errorMsg = "";

        String[] wsTokens = line.split(" ");
        String[] commaTokens = line.split(",");
        if (wsTokens.length > commaTokens.length + 1){
            return "** Error: Operands must be comma separated";
        }

        if (commaTokens.length != num){
            if (commaTokens.length < num){
                return "** Error: Too few operands";
            }
            else{
                return "** Error: Too many operands";
            }
        }
        return "";
    }

    /**
     * Check tracking tables and report memory labels defined more then
     * once, labels that are defined but never branched to, and labels
     * that are branched to but never defined
     * @return Error report lines or an empty list if no errors
     */
    public static List<String> CheckLabelBranchAndDef(){
        List<String> outLines = new ArrayList<>();

        Set<String> labelsDefinedButNotBranchedTo = new HashSet<>(LabelsDefined.keySet());
        labelsDefinedButNotBranchedTo.removeAll(LabelsBranchedTo);
        Set<String> labelsBranchedToButNotDefined = new HashSet<>(LabelsBranchedTo);
        labelsBranchedToButNotDefined.removeAll(LabelsDefined.keySet());

        for (String label : LabelsDefined.keySet()){
            if (LabelsDefined.get(label) > 1){
                outLines.add("** Error: Label [" + label + "] is defined more than once");
                ErrNumMap.put(ParseError.LabelError, ErrNumMap.get(ParseError.LabelError) + 1);
            }
        }

        if (labelsDefinedButNotBranchedTo.size() > 0){
            outLines.add("** Error: Labels defined but not branched to:");
            outLines.add("\t" + labelsDefinedButNotBranchedTo.toString());
            ErrNumMap.put(ParseError.LabelError, ErrNumMap.get(ParseError.LabelError) + 1);
        }

        if (labelsBranchedToButNotDefined.size() > 0){
            outLines.add("** Error: Labels branched to but not defined:");
            outLines.add("\t" + labelsBranchedToButNotDefined.toString());
            ErrNumMap.put(ParseError.LabelError, ErrNumMap.get(ParseError.LabelError) + 1);
        }

        if (outLines.size() > 0){
            outLines.add(0, "\r\nThe following errors may be due to invalid lines");
            outLines.add(1, "above not being fully processed");
        }

        return outLines;
    }


    //***************************************
    // Global data creators
    //***************************************
    /**
     * Produce the set of allowed register terminal symbols
     * @return The set of allowed register terminal symbols
     */
    public static Set<String> MakeRegisterSet(){
        Set<String> regSet = new HashSet<>();
        regSet.add("R0");
        regSet.add("R1");
        regSet.add("R2");
        regSet.add("R3");
        regSet.add("R4");
        regSet.add("R5");
        regSet.add("R6");
        regSet.add("R7");
        return  regSet;
    }

    /**
     * Produce a map from operator terminals to the op code type
     * @return Map from operator terminals to enum type
     */
    public static Map<String, OpGroup> MakeOpTypeMap(){
        Map<String, OpGroup> opMap = new HashMap<>();
        opMap.put("SRT", OpGroup.Start);
        opMap.put("END", OpGroup.End);
        opMap.put("INC", OpGroup.OneOp);
        opMap.put("DEC", OpGroup.OneOp);
        opMap.put("ADD", OpGroup.ThreeOp);
        opMap.put("SUB", OpGroup.ThreeOp);
        opMap.put("MUL", OpGroup.ThreeOp);
        opMap.put("DIV", OpGroup.ThreeOp);
        opMap.put("BEQ", OpGroup.TestAndBranch);
        opMap.put("BGT", OpGroup.TestAndBranch);
        opMap.put("BR", OpGroup.Branch);
        opMap.put("COPY", OpGroup.Copy);
        opMap.put("MOVE", OpGroup.Move);
        opMap.put("DEF", OpGroup.Define);

        return  opMap;
    }

    /**
     * Produce a map from error type to error count with count
     * initialized to 0
     * @return Map from error type to error count
     */
    public static Map<ParseError, Integer> MakeErrorCountMap(){
        Map<ParseError, Integer> errorCountMap = new HashMap<>();
        for (ParseError err : ParseError.values())
            errorCountMap.put(err, 0);
        return  errorCountMap;
    }

    /**
     * Produce a map from error type to its description
     * @return Map from error type to error description
     */
    public static Map<ParseError, String> MakeErrorDescriptionMap(){
        Map<ParseError, String> eMap = new HashMap<>();
        eMap.put(ParseError.StartError, "Program start invalid");
        eMap.put(ParseError.EndError, "Program end invalid");
        eMap.put(ParseError.LabelError, "Label error(s)");
        eMap.put(ParseError.BadOpCode, "Opcode invalid error(s)");
        eMap.put(ParseError.BadOperand, "Operand error(s)");
        eMap.put(ParseError.WhiteSpaceError, "Illegal white space error(s)");
        eMap.put(ParseError.DefineError, "Mem address alias undefined error(s)");
        eMap.put(ParseError.NoCodeFound, "File contains no code error(s)");
        return eMap;
    }
}
