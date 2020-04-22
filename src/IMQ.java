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
  private byte[] raw;
  private byte[] img;
  private int imgSize;

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
    raw = null;
    img = null;
    imgSize = -1;
    /* Read the file into byte array */
    Path path = Paths.get(filename);
    byte[] file = null;
    try{
      file = Files.readAllBytes(path);
    }catch(IOException e){
      System.err.println("(internal) Unable to read file '" + filename + "'");
    }
    /* Read variables from header */
    boolean header = true;
    String obj = "";
    for(int x = 0; x < file.length && header; x++){
      /* TODO: Remove hack array skipping. */
      if(file[x] == 0){
        ++x;
      }
      /* Read next variable */
      int len = file[x] | (file[x + 1] << 8);
      String str = new String(file, x + 2, len);
      x += len + 1;
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
      /* Check we have the requirements */
      if(hist != null && raw != null && imgSize >= 0){
        /* Initialise the decompression */
        decomp = new Decomp(hist);
        /* Decompress the image */
        img = decomp.decompress(raw, raw.length, imgSize);
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
    }else{
      System.err.println("(error) Image must be decompressed first");
    }
  }
}
