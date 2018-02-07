import java.io.*;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;

/**
 * Translation program
 */
public class Main {

    public static String[] REGS = new String[] { "R0", "R1", "R2", "R3", "R4", "R5", "R6", "R7"};



    public static void main(String[] args){
        System.out.println("Hello world");
        System.out.println("The arg is " + args[0]);

        String fileName = args[0];
        List<String> lines = ReadFile(fileName + ".txt");
        lines = RemoveCommentsAndBlanks(lines);

        Map<ParseError, Integer> errorCount = new HashMap<>();
        List<String> outLines = MakeHeaderLines(fileName);

        String[] startLine = lines.get(0).split(" ");
        if (!startLine[0].equals("SRT")){
            outLines.add("** error: Program must begin with \"SRT\"");
            errorCount.put(ParseError.StartError, 1);
        }else {
            outLines.add("1.\t" + lines.get(0));
        }

        int progLine = 2;
        for (int i = 1; i < lines.size(); i++){
            String line = lines.get(i);

            outLines.add(progLine + ".\t" + line);
            progLine++;

            String labelError = CheckLabel(line);
            if (!labelError.isEmpty()){
                outLines.add(labelError);
                errorCount.put(ParseError.LabelError, 1);
            }

            String[] words = line.split(" ");

        }

        outLines.addAll(MakeFooterLines(errorCount));

        WriteLogFile(fileName + ".log", outLines);


    }

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

    public static String CheckLabel(String line){
        String errorMsg = "";
        if (line.contains(":")){
            String label = line.substring(0, line.indexOf(':')).trim();

            if (line.chars().filter(ch -> ch == ':').count() > 1){
                errorMsg = "** error: Only one label per line";
            }
            else if (label.length() > 5){
                errorMsg = "** error: Lables must be 5 letters or less";
            }
            else if (!label.matches("[a-zA-Z]+")){
                errorMsg = "** error: Lables must use only a-z and A-Z";
            }
        }
        return errorMsg;
    }

    public static List<String> ReadFile(String fileName){
        List<String> lines = new ArrayList<>();
        try{

            FileReader fileReader = new FileReader(fileName);
            BufferedReader br = new BufferedReader(fileReader);
            String line;
            while ((line = br.readLine()) != null)
                lines.add(line);
        }catch (IOException e){
            System.out.println(e.getMessage());
        }

        return lines;
    }

    public static void WriteLogFile(String fileName, List<String> lines){

        try{
            FileWriter fw = new FileWriter(fileName);
            BufferedWriter bw = new BufferedWriter(fw);
            for (int i = 0; i < lines.size(); i++)
                bw.write(lines.get(i) + "\r\n");
            bw.close();
        }catch(IOException e){
            System.out.println(e.getMessage());
        }

    }

    public static List<String> MakeHeaderLines(String fileName){
        List<String> header = new ArrayList<>();
        header.add("Compilation Log File");
        header.add("Command argument: " + fileName);
        header.add("Log file name: " + fileName + ".log");
        DateFormat df = new SimpleDateFormat("dd/MM/yy HH:mm:ss");
        header.add("Begin time: " + df.format(new Date()));
        header.add("\r\n");

        return header;
    }

    public static List<String> MakeFooterLines(Map<ParseError, Integer> errorCount){
        List<String> footer = new ArrayList<>();
        Map<ParseError, String> errorDescr = MakeErrorDescriptionMap();

        footer.add("\r\nSummary --------------\r\n");
        int totErrors = errorCount.values().stream().mapToInt(Number::intValue).sum();
        footer.add("Total errors = " + totErrors + "\r\n");

        for (ParseError key : errorCount.keySet()){
            String msg = "\t" + errorCount.get(key) + " " + errorDescr.get(key);
            footer.add(msg);
        }

        String last = "Processing complete - ";
        if (totErrors > 0)
            last += "PAL program is not valid";
        else
            last += "PAL program is valid";
        footer.add(last);

        return footer;
    }

    public enum ParseError{
        StartError,
        LabelError
    }

    public enum Op {
        Start,
        End,
        OneOp,
        ThreeOp,
        Copy,
        Move,
        TestAndBranch,
        Branch
    }

    public static Map<String, Op> MakeOpTypeMap(){
        Map<String, Op> opMap = new HashMap<>();
        opMap.put("SRT", Op.Start);
        opMap.put("END", Op.End);
        opMap.put("INC", Op.OneOp);
        opMap.put("DEC", Op.OneOp);
        opMap.put("ADD", Op.ThreeOp);
        opMap.put("SUB", Op.ThreeOp);
        opMap.put("MUL", Op.ThreeOp);
        opMap.put("DIV", Op.ThreeOp);
        opMap.put("BEQ", Op.TestAndBranch);
        opMap.put("BGT", Op.TestAndBranch);
        opMap.put("BR", Op.Branch);
        opMap.put("COPY", Op.Copy);
        opMap.put("MOVE", Op.Move);

        return  opMap;
    }

    public static Map<ParseError, String> MakeErrorDescriptionMap(){
        Map<ParseError, String> eMap = new HashMap<>();
        eMap.put(ParseError.StartError, "Invalid program start");
        eMap.put(ParseError.LabelError, "Invalid label");

        return eMap;
    }
}
