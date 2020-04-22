package b.ov;

import java.lang.NumberFormatException;

/**
 * Util.java
 *
 * Provide basic utilities for performing various conversion compensation for
 * older machine architecture.
 **/
public class Util{
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
  public static int reverseEndian(int v, int len) throws NumberFormatException{
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
    return pass;
  }
}
