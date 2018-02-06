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

        List<String> outLines = MakeHeaderLines(fileName);

        int progLine = 1;
        for (int i = 0; i < lines.size(); i++){
            String line = lines.get(i);

            if (line.contains(";"))
                line = line.substring(0, line.indexOf(';'));

            line = line.trim();

            if (line.length() == 0)
                continue;

            String[] words = line.split(" ");

            outLines.add(progLine + ".\t" + line);
            progLine++;

            if (line.contains(":")){
                if (!words[0].contains(":")){
                    System.out.println("labels must leftmost on the line they appear in");
                    continue;
                }

                if (words[0].length() - 1 > 5){
                    System.out.println("labels must be 5 or fewer letters");
                    continue;
                }

                if (line.chars().filter(c -> c == ':').count() > 1){
                    System.out.println("only one label is allowed per line");
                    continue;
                }

                if (!words[0].substring(0, line.indexOf(':')).matches("[a-zA-z]+")) {
                    System.out.println("labels use only a-z and A-Z");
                    continue;
                }
            }

            //System.out.println("bottom of loop");

        }


        WriteLogFile(fileName + ".log", outLines);


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

    public enum OpType{
        Start,
        End,
        OneOp,
        ThreeOp,
        Copy,
        Move,
        TestAndBranch,
        Branch
    }

    public static Map<String, OpType> OpTypeMap(){
        Map<String,OpType> opMap = new HashMap<>();
        opMap.put("SRT", OpType.Start);
        opMap.put("END", OpType.End);
        opMap.put("INC", OpType.OneOp);
        opMap.put("DEC", OpType.OneOp);
        opMap.put("ADD", OpType.ThreeOp);
        opMap.put("SUB", OpType.ThreeOp);
        opMap.put("MUL", OpType.ThreeOp);
        opMap.put("DIV", OpType.ThreeOp);
        opMap.put("BEQ", OpType.TestAndBranch);
        opMap.put("BGT", OpType.TestAndBranch);
        opMap.put("BR", OpType.Branch);
        opMap.put("COPY", OpType.Copy);
        opMap.put("MOVE", OpType.Move);

        return  opMap;
    }
}
