package b.ov;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;

/**
 * IMQ.java
 *
 * Load the IMQ file and abstract any information of interest from it. Perform
 * a conversion to an image and save to disk in a given format.
 **/
public class IMQ{
  private HashMap<String, String> config;
  private long[] hist;
  private Decomp decomp;
  private byte[] file;
  private int ptr;
  private byte[] img;
  private int width;
  private int height;
  private int recordBytes;

  /**
   * IMQ()
   *
   * Load the IMQ file into RAM and extract useful information from the final.
   *
   * @param filename The filename of the IMQ file.
   **/
  public IMQ(String filename){
    /* Initialize internal values */
    config = new HashMap<String, String>();
    hist = null;
    decomp = null;
    img = null;
    /* Read the file into byte array */
    Path path = Paths.get(filename);
    file = null;
    try{
      file = Files.readAllBytes(path);
    }catch(IOException e){
      System.err.println("(internal) Unable to read file '" + filename + "'");
    }
    /* Read variables from header */
    boolean header = true;
    String obj = "";
    for(ptr = 0; ptr < file.length && header;){
      String str = readVar();
      str.trim();
      str = str.replace("\0", "");
      /* Check for special cases */
      switch(str){
        case "END" :
          /* Indicate that we're coming out of the header! */
          header = false;
          break;
        case "END_OBJECT" :
          /* No longer in an object */
          obj = "";
          break;
        default :
          /* Remove comments */
          int cmnt = str.indexOf("/*");
          if(cmnt >= 0){
            /* We only want the substring */
            str = str.substring(0, cmnt);
          }
          /* We still have something? */
          if(str.length() <= 0){
            break;
          }
          /* If we are here, we have a key value pair yaay! */
          String[] kvp = str.split("=");
          String k = kvp[0].trim();
          String v = kvp[1].trim();
          /* We have a new object? */
          if(k.equals("OBJECT")){
            obj = v + ".";
            break;
          }
          /* If we're here, it's interesting */
          config.put(obj + k, v);
          break;
      }
    }
    /* Assert we understand the type */
    if(!config.get("IMAGE.SAMPLE_TYPE").equals("UNSIGNED_INTEGER")){
      System.err.println("(error) Unknown image sample type");
    }
    if(!config.get("ENCODING_HISTOGRAM.ITEM_TYPE").equals("VAX_INTEGER")){
      System.err.println("(error) Unknown encoding histogram item type");
    }
    if(!config.get("IMAGE_HISTOGRAM.ITEM_TYPE").equals("VAX_INTEGER")){
      System.err.println("(error) Unknown image histogram image type");
    }
    if(!config.get("IMAGE.ENCODING_TYPE").equals("HUFFMAN_FIRST_DIFFERENCE")){
      System.err.println("(error) Unknown image encoding type");
    }
    if(!config.get("IMAGE_HISTOGRAM.ITEMS").equals("256")){
      System.err.println("(error) Unknown image histogram items");
    }
    if(!config.get("IMAGE.SAMPLE_BITS").equals("8")){
      System.err.println("(error) Unknown image sample bits");
    }
    int histBits = Integer.parseInt(config.get("IMAGE_HISTOGRAM.ITEM_BITS"));
    if(histBits <= 0 || histBits % 8 != 0){
      System.err.println("(error) Unknown histogram data type");
    }
    int histBytes = histBits / 8;
    /* Pull out required variables from engineering tables */
    width = Integer.parseInt(config.get("IMAGE.LINE_SAMPLES"));
    height = Integer.parseInt(config.get("IMAGE.LINES"));
    recordBytes = Integer.parseInt(config.get("RECORD_BYTES"));
    /* Initialize variables to be used */
    hist = new long[Integer.parseInt(config.get("ENCODING_HISTOGRAM.ITEMS"))];
    img = new byte[width * height];
    /* TODO: Figure out what to do with the image histogram. */
    System.out.println("(internal) Skipping " + readVar().length() + " bytes");
    System.out.println("(internal) Skipping " + readVar().length() + " bytes");
    /* Pull out histogram */
    String histTemp = "";
    while(ptr < file.length && histTemp.length() < hist.length * histBytes){
      String temp = readVar();
      histTemp += temp;
    }
    /* Fill Histogram array */
    byte[] histRaw = histTemp.getBytes();
    int x = 0;
    for(int i = 0; i < hist.length; i++){
      hist[i] = ((long)histRaw[x++] & 0xFF)
              | ((long)histRaw[x++] & 0xFF) <<  8
              | ((long)histRaw[x++] & 0xFF) << 16
              | ((long)histRaw[x++] & 0xFF) << 24;
    }
    /* Generate histogram */
    decomp = new Decomp(hist);
  }

  /**
   * readVar()
   *
   * Read the next variable from the input file.
   *
   * @return The variable read from the array.
   **/
  private String readVar(){
    int len = ((int)file[ptr++] & 0xFF) | ((int)(file[ptr++] & 0xFF) << 8);
    len = len + (1 * len % 2);
    String str = new String(file, ptr, len);
    ptr += len;
    return str;
  }

  /**
   * decompress()
   *
   * Perform the decompression on the image.
   **/
  public void decompress(){
    /* Don't double decompress */
    if(decomp != null){
      /* TODO: Figure out what to do with the engineering summary. */
      System.out.println("(internal) Skipping " + readVar().length() + " bytes");
      /* TODO: Check we have the requirements. */
      /* Pull out lines from file */
      for(int line = 0; ptr < file.length && line < height; line++){
        /* Read next line */
        byte[] lin = readVar().getBytes();
        /*  Decompress the line */
        byte[] lout = decomp.decompress(lin, lin.length, recordBytes);
        /* Copy into image buffer */
        System.arraycopy(lout, 0, img, line * width, width);
      }
    }
  }

  /**
   * save()
   *
   * Save the decompressed image to disk.
   *
   * @param filename The name of the image file (including the expected
   * extension for the image type).
   * @param type The type of the image to be output.
   **/
  public void save(String filename, String type){
    /* Ensure decompression has been done */
    if(img != null){
      /* TODO: Save the image to disk. */
      Util.PGM pgmOut = new Util.PGM("test-lin-out.pgm", 800, 800, 256); // TODO
      pgmOut.write(img); // TODO
      pgmOut.close(); // TODO
    }else{
      System.err.println("(error) Image must be decompressed first");
    }
  }

  /**
   * saveTable()
   *
   * Save the image table to disk.
   *
   * @param filename The name of the image file (including the expected
   * extension for the image type).
   **/
  public void saveTable(String filename){
    /* Ensure decompression has been done */
    if(img != null){
      /* TODO: Save the table to disk. */
      for(String k : config.keySet()){
        System.out.println(k + " -> " + config.get(k));
      }
    }else{
      System.err.println("(error) Image must be decompressed first");
    }
  }
}
