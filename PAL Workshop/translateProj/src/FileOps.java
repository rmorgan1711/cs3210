import java.io.*;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * Manages Reading in a file, returning formatted blocks of
 * output text, and writing to file.
 */
public class FileOps {

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

    public static List<String> MakeFooterLines(Map<ParseError, Integer> errorCount,
                                               Map<ParseError, String> errorDescription){
        List<String> footer = new ArrayList<>();

        footer.add("\r\nSummary --------------\r\n");
        int totErrors = errorCount.values().stream().mapToInt(Number::intValue).sum();
        footer.add("Total errors = " + totErrors + "\r\n");

        for (ParseError key : errorCount.keySet()){
            if (errorCount.get(key) > 0){
                footer.add("\t" + errorCount.get(key) + " " + errorDescription.get(key));
            }
        }

        String last = "Processing complete - ";
        if (totErrors > 0)
            last += "PAL program is NOT valid";
        else
            last += "PAL program is valid";
        footer.add(last);

        return footer;
    }
}
