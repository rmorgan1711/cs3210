import java.io.*;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Translation program
 */
public class Main {

    //
    public static Set<String> Registers = MakeRegisterSet();
    public static Set<String> RegOperands = Registers.stream().map(r -> r + ",").collect(Collectors.toSet());
    public static Map<String, OpGroup> OpTypeMap = MakeOpTypeMap();
    public static Map<ParseError, Integer> ErrNumMap = MakeErrorCountMap();
    public static Map<ParseError, String> ErrDesc = MakeErrorDescriptionMap();
    public static Map<String, String> MemLabels = new HashMap<>();
    public static Map<String, String> MemLabOperands = new HashMap<>();
    public static Map<String, Integer> LabelsDefined = new HashMap<>();
    public static Set<String> LabelsBranchedTo = new HashSet<>();

    public static void main(String[] args){

        System.out.println("The arg is " + args[0]);

        String fileName = args[0];
        List<String> lines = FileOps.ReadFile(fileName + ".txt");
        lines = RemoveCommentsAndBlanks(lines);

        List<String> outLines = FileOps.MakeHeaderLines(fileName);
        if (lines.size() < 1){
            outLines.add("** Error: File contains no code");
            ErrNumMap.put(ParseError.NoCodeFound, ErrNumMap.get(ParseError.NoCodeFound) + 1);
            outLines.addAll(FileOps.MakeFooterLines(ErrNumMap, ErrDesc));
            FileOps.WriteLogFile(fileName + ".log", outLines);
            return;
        }

        int lineNum = 1;
        String[] startLine = lines.get(0).split(" ");
        if (!startLine[0].equals("SRT")){
            outLines.add("** error: Program must begin with SRT");
            ErrNumMap.put(ParseError.WhiteSpaceError, ErrNumMap.get(ParseError.WhiteSpaceError) + 1);
        }else {
            outLines.add(lineNum + ".\t" + lines.get(0));
            lineNum++;
            lines.remove(0);
        }

        int linesBefore = lines.size();
        outLines.addAll(ConsumeDefinitionLines(lines, lineNum));
        lineNum = lineNum + (linesBefore - lines.size());

        String line;

        boolean endFound = false;
        for (int i = 0; i < lines.size(); i++){
            line = lines.get(i);
            outLines.add((lineNum++) + ".\t" + line);

            if (line.contains("\t")){
                outLines.add("** Error: Tab is used instead of space");
                ErrNumMap.put(ParseError.LabelError, ErrNumMap.get(ParseError.LabelError) + 1);
                continue;
            }

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

            line = line.substring(line.lastIndexOf(':') + 1); // strip any label
            if (line.isEmpty())
                continue;

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
            outLines.add("** Error: Legal END opcode not found");
            ErrNumMap.put(ParseError.EndError, ErrNumMap.get(ParseError.EndError) + 1);
        }

        outLines.addAll(CheckLabelBranchAndDef());
        outLines.addAll(FileOps.MakeFooterLines(ErrNumMap, ErrDesc));
        FileOps.WriteLogFile(fileName + ".log", outLines);
    }

    //***************************************
    // Translation operations
    //***************************************
    public static List<String> RemoveCommentsAndBlanks(List<String> lines){
        List<String> contentLines = new ArrayList<>();

        for (int i = 0; i < lines.size(); i++){

            String line = lines.get(i);

            if (line.contains(";"))
                line = line.substring(0, line.indexOf(';'));

            line = line.trim();
            if (line.length() == 0)
                continue;

            contentLines.add(line);
        }
        return contentLines;
    }

    public static List<String> ConsumeDefinitionLines(List<String> lines, int lineNum){
        List<String> outLines = new ArrayList<>();

        String line = lines.size() > 0 ? lines.get(0) : null;
        while (lines.size() > 0 && lines.get(0).split(" ")[0].equals("DEF")){
            outLines.add(lineNum + ".\t" + lines.get(0));
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
                errorMsg = "** Error: The operand is not a register or mem label";
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
                errorMsg = "** Error: An operand is not a register or mem label";
                ErrNumMap.put(ParseError.BadOperand, ErrNumMap.get(ParseError.BadOperand) + 1);
            }
        }

        if (op == OpGroup.TestAndBranch){
            errorMsg = CheckOperandNum(line, 3);
            if (!errorMsg.isEmpty()){
                ErrNumMap.put(ParseError.BadOperand, ErrNumMap.get(ParseError.BadOperand) + 1);
            }
            else if (!RegOperands.contains(tokens[1]) && !MemLabOperands.containsKey(tokens[1])){
                errorMsg = "** Error: Operands 1 and 2 must be registers or mem labels";
                ErrNumMap.put(ParseError.BadOperand, ErrNumMap.get(ParseError.BadOperand) + 1);
            }
            else if (!RegOperands.contains(tokens[2]) && !MemLabOperands.containsKey(tokens[2])){
                errorMsg = "** Error: Operands 1 and 2 must be registers or mem labels";
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
                errorMsg = "** Error: Operands must be registers";
                ErrNumMap.put(ParseError.BadOperand, ErrNumMap.get(ParseError.BadOperand) + 1);
            }
            else if (!Registers.contains(tokens[2]) && !MemLabels.containsKey(tokens[2])){
                errorMsg = "** Error: Operands must be registers";
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
                errorMsg = "** Error: 2nd operand must be a register";
                ErrNumMap.put(ParseError.BadOperand, ErrNumMap.get(ParseError.BadOperand) + 1);
            }
        }

        if (op == OpGroup.Define){
            errorMsg = "** Error: Definitions must immediately follow SRT";
            ErrNumMap.put(ParseError.DefineError, ErrNumMap.get(ParseError.DefineError) + 1);
        }

        return errorMsg;
    }

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
        return outLines;
    }


    //***************************************
    // Global data creators
    //***************************************
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

    public static Map<ParseError, Integer> MakeErrorCountMap(){
        Map<ParseError, Integer> errorCountMap = new HashMap<>();
        for (ParseError err : ParseError.values())
            errorCountMap.put(err, 0);
        return  errorCountMap;
    }

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
