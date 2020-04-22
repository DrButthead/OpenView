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
   * Decomp.Node.java
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
    public Node right = null;
    public int dn = -1;
    public Node left = null;
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
  public Decomp(long hist){
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
  private Node huffTree(long hist){
    /* Histogram frequency list */
    long int freq_list[512];
    /* DN pointer array list */
    Node** node_list;
    /* Frequency list pointer */
    long int* fp;
    /* Node list pointer */
    Node** np;
    /* Number non-zero frequencies in histogram */
    long int numFreq;
    /* Sum of all frequencies */
    long int sum;
    /* Counter for DN initialization */
    short int numNodes;
    /* Miscellaneous counter */
    short int cnt;
    /* Null node value */
    short int znull = -1;
    /* Temporary node pointer */
    Node* temp;
    /* TODO: Functions called. */
    void sort_freq();
    NODE *new_node();
    char *malloc();
/***************************************************************************
  Allocate the array of nodes from memory and initialize these with numbers
  corresponding with the frequency list.  There are only 511 possible
  permutations of first difference histograms.  There are 512 allocated
  here to adhere to the FORTRAN version.
****************************************************************************/
    fp = freq_list;
    node_list = (NODE**)malloc(sizeof(temp) * 512);
    if(node_list == NULL){
      printf("\nOut of memory in huff_tree!\n");
      exit(1);
    }
    np = node_list;
    for(num_nodes = 1, cnt = 512; cnt--; num_nodes++){
/**************************************************************************
    The following code has been added to standardize the VAX byte order
    for the "long int" type.  This code is intended to make the routine
    as machine independant as possible.
***************************************************************************/
      unsigned char *cp = (unsigned char *)hist++;
      unsigned long int j;
      short int i;
      for(i = 4; --i >= 0; j = (j << 8) | *(cp + i));
      /* Now make the assignment */
      *fp++ = j;
      temp = new_node(num_nodes);
      *np++ = temp;
    }
    /* Ensure the last element is zeroed out */
    (*--fp) = 0;
    /* Now, sort the frequency list and eliminate all frequencies of zero */
    num_freq = 512;
    sort_freq(freq_list, node_list, num_freq);
    fp = freq_list;
    np = node_list;
    for(num_freq = 512; (*fp) == 0 && (num_freq); fp++, np++, num_freq--);
/***************************************************************************
  Now create the tree.  Note that if there is only one difference value,
  it is returned as the root.  On each interation, a new node is created
  and the least frequently occurring difference is assigned to the right
  pointer and the next least frequency to the left pointer.  The node
  assigned to the left pointer now becomes the combination of the two
  nodes and it's frequency is the sum of the two combining nodes.
****************************************************************************/
    for(temp = (*np); (num_freq--) > 1;){
      temp = new_node(znull);
      temp->right = (*np++);
      temp->left = (*np);
      *np = temp;
      *(fp+1) = *(fp+1) + *fp;
      *fp++ = 0;
      sort_freq(fp, np, num_freq);
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
  private Object[] sortFreq(long[] freqList, Node[] nodeList, int offset, long numFreq){
    /* If no elements or invalid continue */
    if(numFreq > 0){
      for(int i = offset + 1; i < numFreq; i++){
        for(int j = i; j > offset ; j--){
          if(freqList[j] < freqList[j - 1]){
            /* Swap frequencies */
            long t1 = freqList[j];
            freqList[j] = freqList[j - 1];
            freqList[j - 1] = t1;
            /* Swap nodes */
            Node t2 = nodeList[j];
            nodeList[j] = nodeList[j - 1];
            nodeList[j - 1] = t2;
          }
        }
      }
    }
    return new Object[]{ freqList, nodeList };
  }
}
