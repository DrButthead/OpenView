package b.ov;

import java.util.ArrayList;

/**
 * Main.java
 *
 * Parse the command line arguments are start execution as required.
 **/
public class Main{
  private String format;
  private ArrayList<String> input;
  private ArrayList<String> output;
  private boolean recover;
  private String type;

  /**
   * main()
   *
   * Entry point into the program that starts an instance of Main.
   *
   * @param args The command line arguments.
   **/
  public static void main(String[] args){
    new Main(args);
  }

  /**
   * Main()
   *
   * Parse the command line arguments and perform the conversion as required.
   *
   * @param args The command line arguments.
   **/
  public Main(String[] args){
    /* Check if any arguments given */
    if(args.length <= 0){
      System.out.println("See '--help' for usage");
      System.exit(0);
    }
    /* Setup default global variables */
    format = "json";
    input = new ArrayList<String>();
    output = new ArrayList<String>();
    recover = false;
    type = "png";
    /* Parse command line parameters */
    for(int x = 0; x < args.length; x++){
      switch(args[x]){
        case "-f" :
        case "--format" :
          x = format(args, x);
          break;
        case "-h" :
        case "--help" :
          x = help(args, x);
          System.exit(0);
          break;
        case "-i" :
        case "--input" :
          x = input(args, x);
          break;
        case "-o" :
        case "--output" :
          x = output(args, x);
          break;
        case "-r" :
        case "--recover" :
          x = recover(args, x);
          break;
        case "-t" :
        case "--type" :
          x = type(args, x);
          break;
        case "-z" :
        case "--test" :
          x = test(args, x);
          System.exit(0);
          break;
        default :
          System.err.println("(error) Unknown argument '" + args[x] + "'");
          System.exit(0);
          break;
      }
    }
    /* Check if we want to perform a conversion */
    if(input.size() > 0){
      /* Loop over the inputs */
      for(int x = 0; x < input.size(); x++){
        double p = ((double)x / input.size()) * 100.0;
        String in = input.get(x);
        String out = null;
        /* Set an output if one not defined */
        if(output.size() > x){
          out = output.get(x);
        }else{
          out = in + "." + type;
        }
        System.out.println("Converting '" + in + "'->'" + out + "' " + p + "%");
        IMQ imq = new IMQ(in);
        imq.decompress(recover);
        imq.save(out, type);
        imq.saveTable(out + ".txt", format);
      }
    }else{
      System.err.println("(error) Need an input to continue execution");
    }
  }

  /**
   * format()
   *
   * Get the engineering data format.
   *
   * @param args The command line arguments.
   * @param x The current offset into the parameters.
   * @return New offset into parameters.
   **/
  private int format(String[] args, int x){
    if(x + 1 >= args.length){
      System.err.println("(error) No input file given");
      System.exit(0);
    }
    ++x;
    format = args[x];
    /* Sanity check it */
    switch(format){
      case "none" :
      case "prop" :
      case "json" :
        /* Do nothing */
        break;
      default :
        System.err.println("(error) Unsupported format '" + format + "'");
        break;
    }
    return x;
  }

  /**
   * help()
   *
   * Display program help.
   *
   * @param args The command line arguments.
   * @param x The current offset into the parameters.
   * @return New offset into parameters.
   **/
  private int help(String[] args, int x){
    System.out.println("openview [OPT]");
    System.out.println("");
    System.out.println("  OPTions");
    System.out.println("");
    System.out.println("    -h  --help     Display this help");
    System.out.println("    -f  --format   Format of engineering table");
    System.out.println("                     <FORMAT> 'none' No file");
    System.out.println("                              'prop' Properties");
    System.out.println("                              'json' (default)");
    System.out.println("    -i  --input    Input a file(s) to be converted");
    System.out.println("                     <FILE(s)> Compressed IMQ file");
    System.out.println("    -o  --output   Output filename for image(s)");
    System.out.println("                     <FILE(s)> Output image name");
    System.out.println("    -r  --recover  Attempt to recover image");
    System.out.println("    -t  --type     Output image type");
    System.out.println("                     <TYPE> 'jpg'");
    System.out.println("                            'png' (default)");
    System.out.println("    -z  --test     Perform internal checks");
    System.out.println("");
    System.out.println("  Usage");
    System.out.println("");
    System.out.println("    openview -i in.imq");
    System.out.println("");
    System.out.println("    Output: 'in.imq.png' and 'in.imq.png.txt'");
    return x;
  }

  /**
   * input()
   *
   * Get an input file.
   *
   * @param args The command line arguments.
   * @param x The current offset into the parameters.
   * @return New offset into parameters.
   **/
  private int input(String[] args, int x){
    while(x + 1 < args.length && args[x + 1].charAt(0) != '-'){
      input.add(args[++x]);
    }
    return x;
  }

  /**
   * output()
   *
   * Get an output file.
   *
   * @param args The command line arguments.
   * @param x The current offset into the parameters.
   * @return New offset into parameters.
   **/
  private int output(String[] args, int x){
    while(x + 1 < args.length && args[x + 1].charAt(0) != '-'){
      output.add(args[++x]);
    }
    return x;
  }

  /**
   * recover()
   *
   * An option to attempt image recovery.
   *
   * @param args The command line arguments.
   * @param x The current offset into the parameters.
   * @return New offset into parameters.
   **/
  private int recover(String[] args, int x){
    recover = true;
    return x;
  }

  /**
   * type()
   *
   * Get an output type.
   *
   * @param args The command line arguments.
   * @param x The current offset into the parameters.
   * @return New offset into parameters.
   **/
  private int type(String[] args, int x){
    if(x + 1 >= args.length){
      System.err.println("(error) No type given");
      System.exit(0);
    }
    ++x;
    type = args[x];
    /* Sanity check it */
    switch(type){
      case "jpg" :
      case "png" :
        /* Do nothing */
        break;
      default :
        System.err.println("(error) Unsupported type '" + type + "'");
        break;
    }
    return x;
  }

  /**
   * test()
   *
   * Perform internal integrity checks.
   *
   * @param args The command line arguments.
   * @param x The current offset into the parameters.
   * @return New offset into parameters.
   **/
  private int test(String[] args, int x){
    if(Decomp.test()){
      System.out.println("      [ OK ] Decomp");
    }else{
      System.out.println("[FAIL]       Decomp");
    }
    if(Test.test()){
      System.out.println("      [ OK ] Test");
    }else{
      System.out.println("[FAIL]       Test");
    }
    if(Util.test()){
      System.out.println("      [ OK ] Util");
    }else{
      System.out.println("[FAIL]       Util");
    }
    return x;
  }
}
