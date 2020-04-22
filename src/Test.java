package b.ov;

/**
 * Test.java
 *
 * Run tests on the source to ensure it works correctly.
 **/
public class Test{
  private static long[] hist = new long[]{
    12,     6,     13,      3,     13,     15,     13,     49,      8,     7,
    13,     8,     16,      4,     17,     33,     13,      8,     11,     3,
    26,     4,     15,     42,     19,      6,     19,     12,      3,     4,
    19,    10,      7,     11,     13,      3,     21,     11,      3,    45,
    14,     6,     14,      8,     15,      8,     13,     43,      2,     7,
    15,    18,     11,      3,     13,     14,     14,     11,     10,     5,
    14,     6,     12,     50,     15,      5,     15,      4,      7,     5,
    16,    33,      9,      5,     15,      4,    436,      7,      4,    22,
    20,     8,      4,      7,     15,      4,     14,     23,      5,     3,
    15,     9,      4,      6,     13,     29,     17,      5,      4,     3,
    16,     6,      5,     36,     32,      4,     43,      3,     39,     3,
    53,    21,     32,      3,    106,      4,     78,      4,     14,    32,
    18,     6,     10,      4,     24,      3,     20,     41,     14,     3,
    29,     4,     17,      5,     15,     16,     17,      5,      3,     4,
    17,     5,      8,     28,     13,      3,     14,      6,      7,     7,
    13,    31,      6,      5,     13,      4,     19,      5,      4,    41,
    14,    10,      9,      3,     14,      5,     21,     44,      8,     4,
    14,    11,      7,      6,     15,      9,     20,      3,      6,     5,
    22,     8,      9,     30,     22,      3,     20,     10,      3,    11,
    19,    39,      2,     13,     13,      3,     27,      8,      9,    22,
    16,    10,      4,     11,     14,     13,     21,     59,      3,    25,
    13,    25,      8,     31,     15,     72,     26,     50,      6,    47,
    14,    71,      5,    486,     17,     65,     21,     59,      8,    48,
    20,    49,     17,     66,     14,     97,     20,    103,      6,   155,
    35,   157,     15,    152,     17,    247,     15,    367,      8,  1147,
    14,  9225,     24, 113488,     23, 405608,   1073, 112576,    126,  9256,
    86,  1179,     94,    441,    109,    347,     95,    187,     10,   183,
     4,   148,     31,    192,     92,    139,     71,    298,    274,   137,
    14,    43,     21,     40,    406,     40,     20,     49,     13,    67,
     5,    59,     17,     79,      6,     72,     30,     54,      6,    23,
    11,    19,     13,     21,     23,     14,     22,     19,      5,    24,
     4,    11,     17,      6,      9,      9,     28,      4,      3,    24,
    23,    34,     19,     12,      6,     10,     20,     11,     12,    31,
   813,     8,     25,      5,      6,      5,     38,      9,      6,    18,
     6,    13,     14,      4,     23,      9,     24,      3,     85,    94,
    87,    94,     99,     90,     83,     85,    100,     85,      3,    26,
     8,    22,     13,      5,     13,      7,     19,      7,     17,    14,
     3,     7,     27,      6,      3,      5,     21,     12,     13,    14,
     5,     7,     14,      3,     26,      7,     16,      8,      8,    18,
     4,     3,     23,     16,      6,      4,     16,      5,      3,    14,
     4,    19,     16,      4,      3,      3,     15,      6,     10,    21,
     3,     6,     15,      4,      3,      8,     28,      6,      8,    16,
     8,    10,     15,      3,     26,     24,     14,      3,      8,    17,
     5,    13,     19,      3,     11,      4,     17,      5,      6,    15,
    24,    20,     15,      4,      9,      4,     16,      8,      3,    28,
    21,    13,     25,     11,      3,      3,     21,     17,     10,    17,
     4,    11,     13,      9,     22,     17,     25,      5,     10,    24,
     4,    12,     25,     18,      3,     13,     19,     11,      9,    16,
     6,    34,     26,      6,      8,      6,     27,      3,     16,    13,
    11,     9,     14,      5,      7,      4,     26,      8,      3,    17,
     8,    12,     13,     14,      3,      3,     23,      6,      4,    12,
     1,     0 };
  private static int len_in = 227;
  private static char[] lin_in = new char[]{
'\034','\352','\215','\066','\026',
'\145','\107','\030','\306','\233',
'\346','\313','\161','\070','\240',
'\037','\036','\062','\301','\216',
'\227','\055','\247','\036','\322',
'\332','\134','\122','\124','\226',
'\063','\143','\313','\372','\031',
'\154','\041','\227','\225','\370',
'\111','\151','\162','\306','\316',
'\227','\161','\264','\217','\330',
'\317','\216','\347','\067','\377',
'\151','\154','\307','\313','\217',
'\342','\242','\226','\137','\227',
'\270','\317','\161','\315','\225',
'\046','\315','\226','\131','\122',
'\130','\240','\330','\347','\270',
'\322','\367','\377','\140','\243',
'\057','\122','\154','\146','\301',
'\103','\202','\213','\217','\057',
'\317','\335','\245','\307','\036',
'\136','\311','\161','\264','\356',
'\052','\107','\145','\221','\332',
'\137','\274','\334','\037','\124',
'\056','\155','\057','\352','\245',
'\212','\115','\366','\224','\320',
'\313','\052','\052','\116','\342',
'\201','\332','\135','\245','\307',
'\151','\250','\145','\111','\343',
'\370','\241','\246','\322','\241',
'\103','\216','\225','\166','\106',
'\227','\024','\013','\111','\143',
'\037','\032','\133','\140','\306',
'\132','\134','\123','\113','\366',
'\130','\066','\026','\322','\307',
'\330','\306','\166','\166','\161',
'\214','\154','\330','\077','\145',
'\242','\315','\014','\156','\270',
'\177','\377','\377','\377','\377',
'\377','\377','\377','\377','\377',
'\377','\377','\377','\377','\377',
'\377','\377','\377','\377','\376',
'\205','\100','\135','\026','\274',
'\122','\072','\361','\110','\372',
'\374','\224','\260','\377','\377',
'\361','\035','\035','\174','\144',
'\033','\200' };
  private static int len_out = 836;
  private static char[] lin_tst = new char[]{
'\034','\034','\034','\034','\030',
'\030','\032','\030','\034','\034',
'\032','\034','\036','\036','\034',
'\036','\036','\036','\040','\040',
'\034','\036','\036','\036','\036',
'\040','\036','\040','\036','\040',
'\036','\042','\042','\040','\040',
'\040','\040','\042','\042','\042',
'\040','\042','\042','\040','\036',
'\036','\040','\044','\044','\044',
'\046','\042','\044','\046','\050',
'\046','\046','\046','\046','\050',
'\046','\046','\046','\050','\046',
'\050','\050','\046','\050','\052',
'\050','\052','\050','\050','\054',
'\052','\052','\054','\054','\052',
'\050','\054','\054','\054','\056',
'\054','\054','\054','\052','\056',
'\054','\052','\056','\054','\054',
'\056','\052','\056','\052','\056',
'\054','\056','\054','\056','\056',
'\056','\054','\056','\054','\054',
'\054','\056','\056','\054','\054',
'\054','\054','\054','\054','\044',
'\024','\014','\024','\050','\054',
'\056','\056','\054','\060','\056',
'\056','\060','\060','\056','\060',
'\056','\054','\056','\056','\056',
'\056','\062','\060','\060','\056',
'\056','\060','\056','\054','\060',
'\062','\060','\060','\060','\060',
'\060','\056','\060','\056','\060',
'\060','\060','\060','\060','\060',
'\062','\060','\060','\056','\056',
'\060','\060','\060','\060','\062',
'\062','\062','\060','\060','\060',
'\060','\060','\060','\060','\060',
'\060','\060','\056','\062','\060',
'\056','\060','\060','\060','\062',
'\060','\060','\060','\060','\062',
'\062','\060','\060','\062','\060',
'\060','\060','\060','\060','\060',
'\062','\056','\056','\060','\054',
'\062','\060','\060','\060','\060',
'\060','\062','\062','\060','\060',
'\060','\056','\056','\060','\056',
'\060','\060','\060','\060','\060',
'\056','\056','\060','\056','\056',
'\060','\060','\060','\056','\060',
'\060','\054','\060','\060','\056',
'\060','\060','\060','\056','\060',
'\060','\056','\060','\060','\056',
'\060','\060','\054','\060','\056',
'\060','\054','\056','\060','\060',
'\060','\056','\060','\056','\056',
'\060','\060','\060','\060','\060',
'\056','\056','\060','\056','\062',
'\060','\060','\060','\056','\056',
'\056','\056','\056','\056','\056',
'\056','\056','\056','\054','\056',
'\060','\054','\056','\056','\056',
'\060','\060','\056','\056','\056',
'\052','\056','\056','\054','\056',
'\054','\056','\056','\056','\054',
'\056','\060','\054','\056','\054',
'\054','\056','\060','\054','\056',
'\056','\054','\054','\056','\054',
'\054','\054','\056','\056','\054',
'\054','\054','\054','\054','\056',
'\056','\056','\056','\056','\056',
'\056','\054','\054','\052','\056',
'\054','\054','\056','\054','\054',
'\056','\054','\054','\054','\056',
'\056','\054','\054','\054','\052',
'\054','\054','\056','\056','\054',
'\054','\056','\054','\052','\056',
'\056','\056','\054','\054','\056',
'\052','\052','\054','\054','\056',
'\054','\054','\052','\054','\054',
'\052','\054','\054','\056','\054',
'\054','\052','\056','\054','\054',
'\054','\054','\054','\052','\052',
'\052','\054','\054','\054','\052',
'\052','\054','\056','\054','\054',
'\054','\054','\050','\050','\052',
'\054','\054','\052','\052','\054',
'\054','\054','\052','\056','\054',
'\054','\054','\054','\054','\054',
'\050','\050','\054','\052','\054',
'\050','\054','\054','\052','\052',
'\052','\052','\050','\054','\060',
'\060','\050','\056','\052','\054',
'\054','\050','\054','\054','\054',
'\052','\052','\054','\050','\052',
'\054','\052','\052','\050','\054',
'\052','\052','\050','\054','\052',
'\052','\054','\052','\052','\050',
'\054','\054','\050','\052','\050',
'\052','\052','\046','\052','\052',
'\052','\052','\054','\052','\052',
'\052','\052','\052','\052','\054',
'\050','\052','\050','\054','\054',
'\052','\056','\052','\054','\050',
'\052','\050','\050','\052','\050',
'\050','\054','\050','\050','\050',
'\050','\046','\050','\050','\052',
'\050','\054','\052','\052','\054',
'\050','\052','\054','\046','\052',
'\050','\052','\050','\052','\050',
'\050','\050','\050','\052','\050',
'\054','\052','\050','\046','\050',
'\052','\050','\052','\050','\052',
'\052','\050','\054','\052','\052',
'\054','\050','\046','\052','\050',
'\050','\050','\050','\050','\046',
'\050','\050','\046','\050','\052',
'\050','\046','\050','\052','\052',
'\050','\046','\052','\050','\052',
'\050','\050','\050','\050','\046',
'\050','\046','\050','\046','\050',
'\050','\050','\050','\046','\050',
'\050','\050','\050','\046','\050',
'\050','\050','\050','\052','\050',
'\052','\050','\052','\050','\046',
'\050','\050','\050','\046','\050',
'\052','\050','\050','\050','\050',
'\050','\046','\050','\050','\046',
'\060','\056','\046','\050','\050',
'\050','\046','\046','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\263','\150','\002','\000','\001',
'\000','\001','\000','\000','\000',
'\030','\024','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\000','\000','\000','\000',
'\000','\002','\001','\000','\203',
'\002' };

  /**
   * test()
   *
   * Test that the functions documented here work correctly.
   *
   * @return True if tests pass, otherwise false.
   **/
  public static boolean test(){
    byte[] lin_out;
    /*---- Edited original source (below) ----*/
//      extern void decmpinit();
//      extern void decompress();
//      extern void swapit();
      int  i,k,ifail;
      swapit(hist);
//      decmpinit(hist);
      Decomp d = new Decomp(hist); // ADDITION
      ifail = 0;
      for (k=0; k<2; ++k)
      {
//         decompress(lin_in,lin_out,&len_in,&len_out);
         lin_out = d.decompress(Util.charArrByte(lin_in), len_in, len_out); // ADDITION
         for (i=0; i<836; ++i)
         {
         if (lin_out[i] != lin_tst[i])
            { ifail = 1;
//              printf("Error, decompression routines failed\n");
//              printf(" sample number:            %d\n",i);
//              printf(" sample value should be:   %d\n",lin_tst[i]);
//              printf(" output of decompressinon: %d\n",lin_out[i]);
             }
         }
      }
//      if (ifail == 0) printf("Decompression routines succeeded\n");
//      if (ifail != 0) printf("Decompression routines failed\n");
    /*---- Edited original source (above) ----*/
    return ifail == 0;
  }

  /**
   * swapit()
   *
   * This routine will swap bytes of the histogram integer long words for
   * computer hardware which stores long words in "most significant byte order
   * first" order. This is necessary because the DECOMPRESSION routines will
   * automatically swap the byte order for this computer hardware.
   **/
  private static long[] swapit(long[] longwords){
    for(int x = 0; x < longwords.length; x++){
      longwords[x] = Util.reverseEndian(longwords[x], 4);
    }
    return longwords;
  }
}