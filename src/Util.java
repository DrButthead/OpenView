package b.ov;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.NumberFormatException;
import java.nio.file.attribute.FileTime;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.format.DateTimeParseException;
import java.time.Instant;

/**
 * Util.java
 *
 * Provide basic utilities for performing various conversion compensation for
 * older machine architecture.
 **/
public class Util{
  /**
   * PGM.Util.java
   *
   * A simple method for writing foolproof grey scale debug images.
   **/
  public static class PGM{
    private FileWriter fw;
    private int width;
    private int height;
    private int colMax;
    private long count;

    /**
     * PGM()
     *
     * Open a PGM file for writing and pre-write the header information.
     *
     * @param filename The filename to be written to.
     * @param width The width of the image of at least one pixel.
     * @param height The height of the image of at least one pixel.
     * @param colMax The maximum colour that should be considered white. Note
     * that black is always zero and this number should be positive.
     **/
    public PGM(String filename, int width, int height, int colMax){
      fw = null;
      if(filename == null){
        System.err.println("(internal) filename cannot be NULL");
      }
      if(width <= 0){
        System.err.println("(internal) width is supposed to be > 0");
      }
      if(height <= 0){
        System.err.println("(internal) height is supposed to be > 0");
      }
      if(colMax <= 0){
        System.err.println("(internal) colMax is supposed to be > 0");
      }
      try{
        fw = new FileWriter(new File(filename));
        fw.write("P2\n" + width + " " + height + "\n" + colMax + "\n");
      }catch(IOException e){
        fw = null;
        System.err.println("(internal) Failed to create PGM file");
      }
      this.width = width;
      this.height = height;
      this.colMax = colMax;
      count = 0;
    }

    /**
     * write()
     *
     * Write a single pixel to the file. This method will track the number of
     * pixels entered and auto-line break.
     *
     * @param pixel The pixel to be written. It must be positive and less than
     * or equal to the maximum colour.
     **/
    public void write(int pixel){
      if(fw != null){
        /* Check that pixel is valid */
        if(pixel >= 0 && pixel <= colMax){
          try{
            /* Print String version of number */
            fw.write(Integer.toString(pixel));
            ++count;
            /* Check if space or newline needed */
            if(count % width != 0){
              fw.write(' ');
            }else{
              fw.write('\n');
            }
          }catch(IOException e){
            System.err.println("(internal) Failed to write to PGM file");
          }
        }else{
          System.err.println("(internal) Invalid pixel '" + pixel + "'");
        }
      }else{
        System.err.println("(internal) Unable to write pixel as no PGM file");
      }
    }

    /**
     * write()
     *
     * Write several pixels to the file. This method will track the number of
     * pixels entered and auto-line break.
     *
     * @param pixels The pixels to be written. Each pixel must be positive and
     * less than or equal to the maximum colour.
     **/
    public void write(int[] pixels){
      if(pixels != null && pixels.length > 0){
        for(int x = 0; x < pixels.length; x++){
          write(pixels[x]);
        }
      }else{
        System.err.println("(internal) No pixels to write");
      }
    }

    /**
     * write()
     *
     * Write several pixels to the file. This method will track the number of
     * pixels entered and auto-line break.
     *
     * @param pixels The pixels to be written. Each pixel must be positive and
     * less than or equal to the maximum colour.
     **/
    public void write(byte[] pixels){
      if(pixels != null && pixels.length > 0){
        for(int x = 0; x < pixels.length; x++){
          write((int)(pixels[x] & 0xFF));
        }
      }else{
        System.err.println("(internal) No pixels to write");
      }
    }

    /**
     * write()
     *
     * Write several pixels to the file. This method will track the number of
     * pixels entered and auto-line break.
     *
     * @param pixels The pixels to be written. Each pixel must be positive and
     * less than or equal to the maximum colour.
     **/
    public void write(char[] pixels){
      if(pixels != null && pixels.length > 0){
        for(int x = 0; x < pixels.length; x++){
          write((int)pixels[x]);
        }
      }else{
        System.err.println("(internal) No pixels to write");
      }
    }

    /**
     * close()
     *
     * Finish writing and close safely.
     **/
    public void close(){
      if(fw != null){
        try{
          fw.flush();
          fw.close();
          fw = null;
        }catch(IOException e){
          System.err.println("(internal) Failed to close PGM file");
        }
      }else{
        System.err.println("(internal) File not open, so cannot close");
      }
    }
  }

  /**
   * reverseEndian()
   *
   * Reverse the endian structure of an unsigned integer. Older machines had
   * different byte ordering. Currently only 4 byte reverse is supported.
   *
   * @param v The unsigned value to be converted.
   * @param len The number of bytes to be reversed.
   * @return The reverse integer.
   **/
  public static long reverseEndian(long v, int len) throws NumberFormatException{
    /* Check preconditions */
    if(len > 0 && len <= 4){
      /* Perform reversal for half the length only */
      for(int x = 0; x < len / 2; x++){
        /* Calculate byte offset for swaps */
        int loff = x;
        int hoff = len - x - 1;
        /* Calculate bitwise offsets */
        int lbo = 8 * loff;
        int hbo = 8 * hoff;
        /* Calculate masks for swaps */
        int lmask = 0xFF << lbo;
        int hmask = 0xFF << hbo;
        /* Pull out bytes */
        byte lb = (byte)((v & lmask) >> lbo);
        byte hb = (byte)((v & hmask) >> hbo);
        /* Remove bytes from value */
        v &= ~lmask;
        v &= ~hmask;
        /* Place bytes back swapped */
        v |= lb << hbo;
        v |= hb << lbo;
      }
    }else{
      throw new NumberFormatException("Failed length check of integer.");
    }
    return v;
  }

  /**
   * charArrByte()
   *
   * Convert a char array to a byte array.
   *
   * @param a The input char array.
   * @return The output byte array.
   **/
  public static byte[] charArrByte(char[] a){
    byte[] b = new byte[a.length];
    for(int x = 0; x < a.length; x++){
      b[x] = (byte)a[x];
    }
    return b;
  }

  /**
   * escape()
   *
   * Escape a give String to make it safe to be printed or stored.
   *
   * @param s The input String.
   * @return The output String.
   **/
  public static String escape(String s){
    return s.replace("\\", "\\\\")
            .replace("\t", "\\t")
            .replace("\b", "\\b")
            .replace("\n", "\\n")
            .replace("\r", "\\r")
            .replace("\f", "\\f")
            .replace("\'", "\\'")
            .replace("\"", "\\\"");
  }

  /**
   * parseFileTime()
   *
   * Convert a given date String into a FileTime object.
   *
   * @param date The date string.
   * @return The converted FileTime, otherwise the current time.
   **/
  public static FileTime parseFileTime(String date){
    try{
      return FileTime.from(Instant.parse(date));
    }catch(DateTimeParseException e){
      /* Do nothing */
    }
    /* Failed, so just return the current time */
    return FileTime.from(Instant.now());
  }

  /**
   * setFileProperties()
   *
   * Set various file properties for a given file.
   *
   * @param file The file to be adjusted.
   * @param lastMod The instant the file was last modified.
   * @param lastAcc The instant the file was last accessed.
   * @param create The instant the file was created.
   **/
  public static void setFileProperties(
    String file,
    FileTime lastMod,
    FileTime lastAcc,
    FileTime create
  ){
    Path path = Paths.get(file);
    try{
      Files.setAttribute(path, "lastModifiedTime", lastMod);
      Files.setAttribute(path, "lastAccessTime", lastAcc);
      Files.setAttribute(path, "creationTime", create);
    }catch(IOException e){
      System.err.println("(error) Date properties could not be set");
    }
  }

  /**
   * test()
   *
   * Test that the functions documented here work correctly.
   *
   * @return True if tests pass, otherwise false.
   **/
  public static boolean test(){
    boolean pass = true;
    pass |= reverseEndian(0xAA, 1) == 0xAA;
    pass |= reverseEndian(0xAABB, 2) == 0xBBAA;
    pass |= reverseEndian(0xAABBCC, 3) == 0xCCBBAA;
    pass |= reverseEndian(0xAABBCCDD, 4) == 0xDDCCBBAA;
    pass |= escape("Hello\r\n\tW\"o\"rld\n").equals("Hello\\r\\n\\tW\\\"o\\\"rld\\n");
    return pass;
  }
}
