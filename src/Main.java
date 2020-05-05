package b.ov;

/**
 * Main.java
 *
 * Parse the command line arguments are start execution as required.
 **/
public class Main{
  private String input;
  private String output;
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
    input = null;
    output = null;
    type = "png";
    /* Parse command line parameters */
    for(int x = 0; x < args.length; x++){
      switch(args[x]){
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
    if(input != null){
      /* Set an output if one not defined */
      if(output == null){
        output = input + "." + type;
      }
      IMQ imq = new IMQ(input);
      imq.decompress();
      imq.save(output, type);
      imq.saveTable(output + ".txt");
    }else{
      System.err.println("(error) Need an input to continue execution");
    }
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
    System.out.println("    -h  --help    Display this help");
    System.out.println("    -i  --input   Input a file to be converted");
    System.out.println("                    <FILE> Compressed IMQ file");
    System.out.println("    -o  --output  Output filename for image");
    System.out.println("                    <FILE> The desired image name");
    System.out.println("    -t  --type    Output image type");
    System.out.println("                    <TYPE> 'jpg' or 'png'");
    System.out.println("    -z  --test    Perform internal checks");
    System.out.println("");
    System.out.println("  Usage");
    System.out.println("");
    System.out.println("    openview -t png -i in.imq");
    System.out.println("");
    System.out.println("    Output: in.imq.png");
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
    if(x + 1 >= args.length){
      System.err.println("(error) No input file given");
      System.exit(0);
    }
    ++x;
    input = args[x];
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
    if(x + 1 >= args.length){
      System.err.println("(error) No output file given");
      System.exit(0);
    }
    ++x;
    output = args[x];
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
    switch(type){
      case "jpg" :
      case "png" :
        break;
      default :
        System.err.println("(error) Image format unsupported '" + type + "'");
        System.exit(0);
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
