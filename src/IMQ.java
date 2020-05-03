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
  private int[] img;
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
    for(ptr = 0; ptr < file.length && header; ptr++){
      /* TODO: Remove hack array skipping. */
      if(file[ptr] == 0){
        ++ptr;
      }
      /* Read next variable */
      int len = file[ptr] | (file[ptr + 1] << 8);
      String str = new String(file, ptr + 2, len);
      ptr += len + 1;
      str.trim();
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
    img = new int[width * height];
    /* Pull out histogram */
    for(int x = 0; ptr < file.length && x < hist.length; x++){
      hist[x] = ((((int)file[ptr    ]) & 0xFF) << 24)
              | ((((int)file[ptr + 1]) & 0xFF) << 16)
              | ((((int)file[ptr + 2]) & 0xFF) <<  8)
              | ((((int)file[ptr + 3]) & 0xFF)      );
      System.out.println(hist[x]); // TODO
      ptr += histBytes;
    }
    /* Generate histogram */
    decomp = new Decomp(hist);
    /* TODO: Read raw data from image. */
    /* TODO */
    for(String k : config.keySet()){
      System.out.println(k + " -> " + config.get(k));
    }
  }

  /**
   * decompress()
   *
   * Perform the decompression on the image.
   **/
  public void decompress(){
    /* Don't double decompress */
    if(decomp == null){
      /* TODO: Check we have the requirements. */
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
    }else{
      System.err.println("(error) Image must be decompressed first");
    }
  }
}
