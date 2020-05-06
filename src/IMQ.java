package b.ov;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import javax.imageio.ImageIO;

/**
 * IMQ.java
 *
 * Load the IMQ file and abstract any information of interest from it. Perform
 * a conversion to an image and save to disk in a given format.
 **/
public class IMQ{
  private HashMap<String, String> config;
  private long[] imgHist;
  private long[] encHist;
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
    imgHist = null;
    encHist = null;
    decomp = null;
    file = null;
    img = null;
    /* Initialize configuration */
    initConfig(filename);
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
    /* Pull out required variables from engineering tables */
    width = Integer.parseInt(config.get("IMAGE.LINE_SAMPLES"));
    height = Integer.parseInt(config.get("IMAGE.LINES"));
    img = new byte[width * height];
    recordBytes = Integer.parseInt(config.get("RECORD_BYTES"));
    /* Generate from image histogram */
    initImgHist();
    /* Generate from encoding histogram */
    initEncHist();
    decomp = new Decomp(encHist);
  }

  /**
   * initConfig()
   *
   * Load the image file and parse the initial table.
   *
   * @param filename The filename to load from.
   **/
  private void initConfig(String filename){
    /* Read the file into byte array */
    Path path = Paths.get(filename);
    try{
      file = Files.readAllBytes(path);
    }catch(IOException e){
      System.err.println("(internal) Unable to read file '" + filename + "'");
    }
    /* Read variables from header */
    boolean header = true;
    String obj = "";
    for(ptr = 0; ptr < file.length && header;){
      String str = new String(readVar());
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
          String v = "";
          /* NOTE: Not supposed to happen, but can. */
          if(kvp.length > 1){
            v = kvp[1].trim();
          }
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
  }

  /**
   * initImgHist()
   *
   * Initialize the image histogram from the file.
   **/
  private void initImgHist(){
    /* Image histogram data structure */
    int imgHistBits = Integer.parseInt(config.get("IMAGE_HISTOGRAM.ITEM_BITS"));
    if(imgHistBits <= 0 || imgHistBits % 8 != 0){
      System.err.println("(error) Unknown image histogram data type");
    }
    int imgHistBytes = imgHistBits / 8;
    /* Pull out image histogram */
    imgHist = new long[Integer.parseInt(config.get("IMAGE_HISTOGRAM.ITEMS"))];
    byte[] imgHistRaw = new byte[imgHist.length * imgHistBytes];
    byte[] t = new byte[0];
    for(int x = 0; ptr < file.length && x < imgHistRaw.length - imgHistBytes; x += t.length){
      t = readVar();
      System.arraycopy(t, 0, imgHistRaw, x, t.length);
    }
    /* Fill image histogram array */
    int x = 0;
    for(int i = 0; i < imgHist.length; i++){
      imgHist[i] = (((long)imgHistRaw[x++] & 0xFF)      )
                 | (((long)imgHistRaw[x++] & 0xFF) <<  8)
                 | (((long)imgHistRaw[x++] & 0xFF) << 16)
                 | (((long)imgHistRaw[x++] & 0xFF) << 24);
    }
  }

  /**
   * initEncHist()
   *
   * Initialize the encoding histogram from the file.
   **/
  private void initEncHist(){
    /* Encoding histogram data structure */
    int encHistBits = Integer.parseInt(config.get("ENCODING_HISTOGRAM.ITEM_BITS"));
    if(encHistBits <= 0 || encHistBits % 8 != 0){
      System.err.println("(error) Unknown encoding histogram data type");
    }
    int encHistBytes = encHistBits / 8;
    /* Pull out encoding histogram */
    encHist = new long[Integer.parseInt(config.get("ENCODING_HISTOGRAM.ITEMS")) + 1];
    byte[] encHistRaw = new byte[encHist.length * encHistBytes];
    byte[] t = new byte[0];
    for(int x = 0; ptr < file.length && x < encHistRaw.length - encHistBytes; x += t.length){
      t = readVar();
      System.arraycopy(t, 0, encHistRaw, x, t.length);
    }
    /* Fill encoding histogram array */
    int x = 0;
    for(int i = 0; i < encHist.length; i++){
      encHist[i] = (((long)encHistRaw[x++] & 0xFF)      )
                 | (((long)encHistRaw[x++] & 0xFF) <<  8)
                 | (((long)encHistRaw[x++] & 0xFF) << 16)
                 | (((long)encHistRaw[x++] & 0xFF) << 24);
    }
  }

  /**
   * readVar()
   *
   * Read the next variable from the input file.
   *
   * @return The variable read from the array.
   **/
  private byte[] readVar(){
    int len = (((int)file[ptr++]) & 0xFF) | (((int)(file[ptr++]) & 0xFF) << 8);
    len = len + (1 * len % 2);
    byte[] b = new byte[len];
    System.arraycopy(file, ptr, b, 0, len);
    ptr += len;
    return b;
  }

  /**
   * decompress()
   *
   * Perform the decompression on the image.
   *
   * @param recover True if image recovery should be attempted if corruption is
   * detected, false to ignore corruption.
   **/
  public void decompress(boolean recover){
    /* Don't double decompress */
    if(decomp != null){
      /* TODO: Figure out what to do with the engineering summary. */
      System.out.println("(internal) Skipping " + readVar().length + " bytes");
      /* Pull out lines from file */
      long[] realHist = new long[imgHist.length];
      for(int line = height - 1; ptr < file.length && line >= 0; line--){
        /* Read next line */
        byte[] lin = readVar();
        /*  Decompress the line */
        byte[] lout = decomp.decompress(lin, lin.length, recordBytes);
        /* Copy into image buffer */
        System.arraycopy(lout, 0, img, line * width, width);
        /* Update histogram */
        for(int x = 0; x < width; x++){
          ++realHist[(int)lout[x] & 0xFF];
        }
      }
      /* Check that output histograms match */
      boolean match = true;
      for(int x = 0; x < realHist.length && match; x++){
        match &= realHist[x] == imgHist[x];
      }
      if(!match){
        System.err.println("(error) Issues when decoding image");
        /* If set, attempt recovery */
        if(recover){
          System.out.println("Attempting image recovery...");
          /* TODO: Write the recovery. */
        }
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
      /* Create a buffered image containing the data */
      BufferedImage bi = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
      for(int y = 0; y < height; y++){
        for(int x = 0; x < width; x++){
          bi.setRGB(
            x,
            y,
            0xFF                               << 24 |
            ((int)img[x + (y * width)] & 0xFF) << 16 |
            ((int)img[x + (y * width)] & 0xFF) << 8  |
            ((int)img[x + (y * width)] & 0xFF)
          );
        }
      }
      /* Save the image to disk */
      File f = new File(filename);
      try{
        ImageIO.write(bi, type, f);
      }catch(IOException e){
        System.err.println("(error) Unable to save image");
      }
      /* Adjust the file properties */
      Util.setFileProperties(
        filename,
        Util.parseFileTime(config.get("IMAGE_TIME")),
        Util.parseFileTime(config.get("EARTH_RECEIVED_TIME")),
        Util.parseFileTime(config.get("IMAGE_TIME"))
      );
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
   * @param format The desired output format.
   **/
  public void saveTable(String filename, String format){
    /* Ensure decompression has been done */
    if(img != null && format != null && format != "none"){
      /* Generate an output file */
      String temp = "";
      switch(format){
        case "prop" :
          for(String k : config.keySet()){
            temp += k + "=" + config.get(k) + "\n";
          }
          break;
        case "json" :
          /* Find nodes */
          HashMap<String, ArrayList<String>> data = new HashMap<String, ArrayList<String>>();
          for(String k : config.keySet()){
            String[] parts = k.split("\\.");
            /* Sanity check our one parent max assumption */
            if(parts.length > 2){
              System.err.println("(internal) Too many parents");
            }
            ArrayList<String> childs = data.get(parts[0]);
            /* Generate a new entry if needed */
            if(childs == null){
              childs = new ArrayList<String>();
              data.put(parts[0], childs);
            }
            /* Throw the element in there if needed */
            if(parts.length >= 2){
              childs.add(parts[1]);
            }
          }
          /* Generate JSON */
          temp += "{\n";
          String[] parents = data.keySet().toArray(new String[data.size()]);
          for(int x = 0; x < parents.length; x++){
            ArrayList<String> childs = data.get(parents[x]);
            /* Case where parent has childs */
            if(childs.size() > 0){
              /* Build the header */
              temp += "  \"" + parents[x] + "\":{\n";
              /* Fill with childs */
              for(int y = 0; y < childs.size(); y++){
                String k = Util.escape(childs.get(y));
                String v = Util.escape(config.get(parents[x] + "." + childs.get(y)));
                temp += "    \"" + k + "\":\"" + v + "\"";
                temp += y < childs.size() - 1 ? ",\n" : "\n";
              }
              /* Build the footer */
              temp += x < parents.length - 1 ? "  },\n" : "  }\n";
            /* Case where parent is all alone :( */
            }else{
              String k = Util.escape(parents[x]);
              String v = Util.escape(config.get(parents[x]));
              temp += "  \"" + k + "\":\"" + v + "\"";
              temp += x < parents.length - 1 ? ",\n" : "\n";
            }
          }
          temp += "}";
          break;
        default :
          System.err.println("(internal) Bad format slipped through");
          break;
      }
      /* Save the table to disk */
      try{
        Files.write(Paths.get(filename), temp.getBytes());
      }catch(IOException e){
        System.err.println("(error) Unable to save engineering table to disk");
      }
      /* Adjust the file properties */
      Util.setFileProperties(
        filename,
        Util.parseFileTime(config.get("IMAGE_TIME")),
        Util.parseFileTime(config.get("EARTH_RECEIVED_TIME")),
        Util.parseFileTime(config.get("IMAGE_TIME"))
      );
    }else{
      System.err.println("(error) Image must be decompressed first");
    }
  }
}
