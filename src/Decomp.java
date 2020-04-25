package b.ov;

/**
 * Decomp.java
 *
 * An implementation of the decompression for IMQ NASA images. Reads a variable
 * length compressed VOYAGER image and outputs a fixed length uncompressed
 * image file in PDS format with labels, image histogram, engineering table and
 * 800 lines of 836 bytes (800 samples, 36 engineering bytes); or an 800 by 800
 * array with FITS, VICAR or no labels. If used on a non-byte-swapped machine
 * the image histogram is un-swapped.
 **/
public class Decomp{
  /**
   * Node.Decomp.java
   *
   * This defines the basic element of a node used to build the Huffman tree
   * for data decompression. The code declares a user defined type that
   * consists of an integer field and a left and right pointer field. The right
   * and left pointers point to another Node structure, a recursive reference
   * of itself. The dn field will contain a -1 if the node is not a leaf (or
   * end node) otherwise it will contain the pixel difference value. This value
   * is then subtracted from the preceding pixel and 256 is added to normalize
   * the actual first difference value. The left and right pointers are
   * undefined (NULL) if the node is a leaf, otherwise they will point to the
   * next adjacent node in the tree.
   **/
  private class Node{
    public Node right;
    public int dn;
    public Node left;

    /**
     * Node()
     *
     * Create a new Node with a given difference value.
     *
     * @param dn The difference value.
     **/
    public Node(int dn){
      this.right = null;
      this.dn = dn;
      this.left = null;
    }
  }

  private Node tree;

  /**
   * Decomp()
   *
   * This routine is relatively simple; it is responsible for creating the
   * Huffman tree from the first difference histogram of the image. In a first
   * difference histogram, the first byte in each image line is the actual
   * pixel value. All other pixels are obtained by subtracting the first
   * difference value at the current pixel from the actual value of the
   * preceding pixel and adding 256 to provide a positive number. The I-th
   * element of the array "hist" should be the frequency of occurrences for the
   * value I. Note the declaration of the pointer tree. This external variable
   * is defined by this routine. It returns the pointer to the root of the
   * Huffman tree created by "huffTree()". The huffTree() routine will
   * automatically swap the low and high order bytes of the 32-bit elements in
   * the of the first difference histogram for the computer systems which store
   * integers in "most significant byte first" order. For computer systems
   * which store 32-bit words in "least significant byte first order", no
   * swapping of the bytes occurs.
   *
   * @param hist First-difference histogram. This array MUST be dimensioned to
   * at least 511 elements. There are a total of 511 first-difference values.
   * The least first-difference value is 0-255 or -255, while the largest
   * first-difference is 255-0 or 255. The first-difference values are
   * normalized for table use to the range 1 to 511 by adding 256 to each
   * difference.
   **/
  public Decomp(long[] hist){
    /* Simply call the huff_tree routine and return */
    tree = huffTree(hist);
  }

  /**
   * huffTree()
   *
   * This function constructs a binary Huffman tree and returns the pointer to
   * the root of the tree. The implementation here may not be the most
   * efficient, but conditions of the algorithm used to compress the data
   * governed the design of this algorithm. Other implementations are available
   * in FORTRAN and VAX MACRO Assembler. This routine allocates memory as
   * needed to construct the tree. The tree is implemented as a user defined
   * structure described above. The algorithm uses an array of node pointers
   * allocated for all possible values. This array is then initialized by
   * assigning all leafs to the array. Each leaf has a corresponding frequency
   * assigned to it and the frequencies are sorted in ascending order. All zero
   * frequencies are ignored and tree construction begins. The tree is built by
   * combining the two least occurring frequencies into one node. This new node
   * is treated as one by adding together the two frequencies forming a
   * cumulative frequency of the combining nodes. The second smallest node now
   * contains the newly combined node and the smallest node is deleted from the
   * list. The frequency list is then resorted to determine the next two node
   * combinations until one node is left. This node will be the root of the
   * tree. This pointer is then returned to the calling routine.
   *
   * @param hist First difference histogram.
   * @return The constructed tree.
   **/
  private Node huffTree(long[] hist){
    /* Histogram frequency list */
    long[] freqList = new long[512];
    /* Node array list */
    Node[] nodeList = new Node[freqList.length];
    /* Initialize array with numbers corresponding with the frequency list */
    /* NOTE: There are only 511 possible permutations of first difference
             histograms. There are 512 allocated here to adhere to the FORTRAN
             version. */
    /* Track histogram pointer */
    int h = 0;
    /* Frequency list pointer */
    int fp = 0;
    /* Node list pointer */
    int np = 0;
    /* Miscellaneous counter */
    int cnt = 512;
    for(int numNodes = 1; cnt-- > 0; numNodes++){
      long l = hist[h++];
      int[] cp = new int[]{
        (int)((l >> 24) & 0xFF),
        (int)((l >> 16) & 0xFF),
        (int)((l >>  8) & 0xFF),
        (int)((l      ) & 0xFF)
      };
      long j = 0;
      for(int i = 4; --i >= 0; j = (j << 8) | cp[i]);
      /* Now make the assignment */
      freqList[fp++] = j;
      nodeList[np++] = new Node(numNodes);
    }
    freqList[--fp] = 0;         /* Ensure the last element is zeroed out.  */
    /* Now, sort the frequency list and eliminate all frequencies of zero */
    Object[] res = sortFreq(freqList, nodeList, 0, freqList.length);
    freqList = (long[])(res[0]);
    nodeList = (Node[])(res[1]);
    fp = 0;
    np = 0;
    int numFreq;
    for(numFreq = freqList.length; freqList[fp] == 0 && numFreq > 0; numFreq--){
      fp++;
      np++;
    }
    /* Now create the tree */
    /* NOTE: If there is only one difference value, it is returned as the root.
             On each iteration, a new node is created and the least frequently
             occurring difference is assigned to the right pointer and the next
             least frequency to the left pointer. The node assigned to the left
             pointer now becomes the combination of the two nodes and it's
             frequency is the sum of the two combining nodes. */
    int t;
    for(t = np; (numFreq--) > 1;){
      nodeList[t] = new Node(-1);
      nodeList[t].right = nodeList[np++];
      nodeList[t].left = nodeList[np];
      np = t;
      freqList[fp + 1] = freqList[fp + 1] + freqList[fp];
      freqList[fp++] = 0;
      res = sortFreq(freqList, nodeList, fp, numFreq);
      freqList = (long[])(res[0]);
      nodeList = (Node[])(res[1]);
    }
    return nodeList[t];
  }

  /**
   * sortFreq()
   *
   * This routine uses an insertion sort to reorder a frequency list in order
   * of increasing frequency. The corresponding elements of the node list are
   * reordered to maintain correspondence. The node list is actually a pointer
   * to an array of pointers to tree nodes.
   *
   * @param freqList Pointer to frequency list.
   * @param nodeList Pointer to array of node pointers.
   * @param offset An offset into the array to begin sorting from.
   * @param numFreq Number of values in freq list.
   * @return The sorted lists in an object array
   **/
  private static Object[] sortFreq(long[] freqList, Node[] nodeList, int offset, long numFreq){
    /* Primary pointer into freqList */
    int i = offset;
    /* Secondary pointer into freqList */
    int j = offset;
    /* Primary pointer to nodeList */
    int k = offset;
    /* Secondary pointer into nodeList */
    int l = offset;
    /* Temporary storage for freqList */
    long temp1 = 0;
    /* Temporary storage for nodeList */
    Node temp2 = null;
    /* Count of list elements */
    long cnt = numFreq;
    /* If no elements or invalid, return */
    if(numFreq > 0){
      while(--cnt > 0){
        temp1 = freqList[++i];
        temp2 = nodeList[++k];
        for(j = i, l = k; freqList[j - 1] > temp1;){
          freqList[j] = freqList[j - 1];
          nodeList[l] = nodeList[l - 1];
          j--;
          l--;
          /* Are we back at the beginning? */
          if(j <= 0){
            break;
          }
        }
        freqList[j] = temp1;
        nodeList[l] = temp2;
      }
    }
    return new Object[]{ freqList, nodeList };
  }

  /**
   * decompress()
   *
   * This routine follows a path from the root of the Huffman tree to one of
   * it's leaves. The choice at each branch is decided by the successive bits
   * of the compressed input stream.  Left for 1, right for 0. Only leaf nodes
   * have a value other than -1. The routine traces a path through the tree
   * until it finds a node with a value not equal to -1 (a leaf node). The
   * value at the leaf node is subtracted from the preceding pixel value plus
   * 256 to restore the uncompressed pixel. This algorithm is then repeated
   * until the entire line has been processed.
   *
   * @param ibuf Compressed data buffer.
   * @param nin Number of bytes on input buffer.
   * @param nout Number of bytes in output buffer.
   * @return Decompressed image line.
   **/
  public byte[] decompress(byte[] ibuf, int nin, int nout){
    /* Create output buffer */
    byte[] obuf = new byte[nout];
    /* Track buffer pointers */
    int ip = 0;
    int op = 0;
    /* Make initial assignments */
    byte odn = ibuf[ip++];
    obuf[op++] = odn;
    /* Decompress the input buffer */
    Node ptr = tree;
    /* Assign the first byte to the working variable, idn */
    for(; ip < nin; ++ip){
      /* An arithmetic AND is performed using 'test' that is bit shifted to the right */
      for(int test = 0x80; test != 0; test >>= 1){
        /* If the result is 0, then go to right else go to left */
        ptr = (test & ibuf[ip]) != 0 ? ptr.left : ptr.right;
        if(ptr.dn != -1){
          /* Have we run out of output? */
          if(op >= nout){
            return obuf;
          }
          odn -= ptr.dn + 256;
          obuf[op] = odn;
          op++;
          ptr = tree;
        }
      }
    }
    return obuf;
  }
}
