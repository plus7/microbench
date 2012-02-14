C From: mcmahon@ocfmail.ocf.llnl.gov (Francis H Mcmahon)
C   echo     '********** LFK tests script for  CRAY / UNICOS'
C   rm $1.o $1.l  $1.m $1.x a.out output  bino
C   cft77    -o aggress,bl  -dp -e sxz $1.f
C   echo     '********** ASSEMBLED.'
C   bld      qz bino  $1.o
C   bld      dz bino     :SECOND
C   segldr   -M $1.m  -o $1.x  bino
C   echo     '********** LINKED.'
C   $1.x
c
c
c     PROGRAM DPMFLOPS(TAPE6=OUTPUT)              Double Precision Test
c                    LATEST KERNEL MODIFICATION DATE: 22/DEC/86
c                    LATEST FILE   MODIFICATION DATE: 12/OCT/93 version mf528
c****************************************************************************
c MEASURES CPU PERFORMANCE RANGE OF THE COMPUTATION/COMPILER/COMPUTER COMPLEX
c****************************************************************************
c                                                                           *
c     L. L. N. L.   F O R T R A N   K E R N E L S  T E S T:   M F L O P S   *
c                                                                           *
c                                  Our little systems have their day;       *
c                                  They have their day and cease to be:     *
c                                  They are but broken parts of Thee,       *
c                                  And Thou, O Lord, are more than they.    *
c                                           Alfred, Lord Tennyson (1850)    *
c                                                                           *
c                                                                           *
c     These kernels measure  Fortran  numerical  computation rates for a    *
c     spectrum of  CPU-limited  computational  structures.  Mathematical    *
c     through-put is measured  in  units  of  millions of floating-point    *
c     operations executed per Second, called Mega-Flops/Sec.                *
c                                                                           *
c     The experimental  design  of some traditional  benchmark tests  is    *
c     defective when  applied  to computers employing vector or parallel    *
c     processing because the range of cpu performance is 10 to 100 times    *
c     the range  of conventional, serial processors.  In particular, the    *
c     effective Cpu performance  of supercomputers now ranges from a few    *
c     megaflops to a few thousand megaflops. Attempts by some marketeers    *
c     and decision makers to reduce this three orders of magnitude range    *
c     of cpu  performance  to  a  single  number is unscientific and has    *
c     produced much confusion.   The  LFK  test  also has been abused by    *
c     some analysts who quote only a single, average performance number.    *
c                                                                           *
c     The Livermore  Fortran  Kernels (LFK) test contains a broad sample    *
c     of generic Fortran computations which have been used to measure an    *
c     effective numerical  performance range, thus avoiding the peril of    *
c     a single performance  "rating".   A complete report of 72 LFK test    *
c     results must  quote  six performance range statistics(rates):  the    *
c     minimum, the  harmonic,   geometric,  and  arithmetic  means,  the    *
c     maximum and  the  standard deviation.  No single rate quotation is    *
c     sufficient or  honest.    These   measurements  show  a  realistic    *
c     variance in  Fortran  cpu  performance  that has stood the test of    *
c     time and that is vital data for circumspect computer evaluations.     *
c     Quote statistics from the SUMMARY table of 72 timings (DO Span= 167). *
c                                                                           *
c     This LFK test may be used as a standard performance test, as a test   *
c     of compiler accuracy (checksums), or as a hardware endurance test.    *
c     The LFK methodology is discussed in subroutine REPORT with references.*
c     The glossary and module hierarchy are documented in subroutine INDEX. *
c                                                                           *
c     Use of this program is granted with the request that a copy of the    *
c     results be sent to  the  author  at the address shown below, to be    *
c     added to  our studies of  computer performance.   Please send your    *
c     complete LFK test output file on 5" DOS floppy-disk, or by E-mail.    *
c     Your timing results  may be held as proprietary data, if so marked.   *
c     Otherwise your results will be quoted in published reports and will   *
c     be disseminated through a publicly accessable computer network.       *
c     Most computer vendors have run the LFK test(akas Livermore Loops test)*
c     and can provide LFK test results to prospective customers on request. *
c     Enhanced versions of this LFK test may be obtained from the author:   *
c                                                                           *
c                                                                           *
c          F.H. McMahon     L-35                                            *
c          Lawrence Livermore National Laboratory                           *
c          P.0. Box 808                                                     *
c          Livermore, CA.   94550                                           *
c                                                                           *
c          (510) 422-1647                                                   *
c          mcmahon@ocfmail.ocf.llnl.gov                                     *
c          MCMAHON3@LLNL.GOV                                                *
c                                                                           *
c                                                                           *
c                    (C) Copyright 1983 the Regents of the                  *
c                University of California. All Rights Reserved.             *
c                                                                           *
c               This work was produced under the sponsorship of             *
c                the U.S. Department of Energy. The Government              *
c                       retains certain rights therein.                     *
c****************************************************************************
c
c
c                             DIRECTIONS
c
c  1. We REQUIRE one test-run of the Fortran kernels as is, that is, with
c     no reprogramming.  Standard product compiler directives may be used
c     for optimization as these do not constitute reprogramming. Use of
c     special compiler coding used only for specific LFK kernels is PROHIBITED.
c     We REQUIRE one mono-processed run (1 cpu) of this unaltered test.
c
c     The performance of the standard, "as is" LFK test (no modifications)
c     correlates well with the performance of the majority of cpu-bound,
c     Fortran applications and hence of diverse workloads.  These measured
c     correlations show the LFK to be a good sampling of the existing
c     inventory of Fortran coding practice in general.  The extrema in
c     the Fortran inventory are represented from serial recurrences on
c     small arrays to global-parallel computation on large arrays.
c
c  2. In addition, the vendor may, if so desired, reprogram the kernels to
c     demonstrate high performance hardware features.  Kernels 13,14,23
c     are partially vectorisable and kernels 15,16,24 are vectorisable if
c     re-written. Kernels 5,6,11,17,19,20,23 are implicit computations that
c     must NOT be explicitly vectorised using compiler directives to
c     ignore dependencies.  In any case, compiler listings of the codes
c     actually used should be returned along with the timing results.
c
c     We permit the LFK kernels to be reprogrammed ONLY as a partial
c     demonstration of the performance of innovative, high performance
c     architectures.  We may then infer from the reprogramming work
c     the kind and degree of optimisations which are necessary to achive
c     high performance as well as the cost in time and effort.
c     Only if it can be shown that this reprogramming can be automated
c     could we establish a correlation with the existing Fortran inventory.
c     These non-standard tests using the LFK samples are intended to explore
c     programming requirements and should not be correlated with standard
c     LFK test results (as in 1 above).
c
c  3. For vector processors, we REQUIRE an ALL-scalar compilation test-run
c     to measure the basic scalar performance range of the processor.
c
c  4. On computers where default single precision is REAL*4 we REQUIRE an
c     additional test-run with all mantissas.ge.47 .  Declare all REAL*8 using:
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
c
c     To change REAL*4 (MFLOPS) to REAL*8 Double Precision:
c
c      vi... :1,$s/cANSI/     /g
c      vi... :1,$s/      DOUBLE  PRE/Cout  DOUBLE  PRE/g
c     ( some redundance in IQRANF,REPORT,RESULT,SEQDIG,TALLY,TRIAL,VALUES)
c
c     To reverse REAL*8 (DPMFLOPS) to REAL*4 Single Precision:
c
c      vi... :1,$s/      IMPLICIT  DOUBLE PRE/      IMPLICIT  DOUBLE PRE/g
c      vi... :1,$s/Cout  DOUBLE  PRE/      DOUBLE  PRE/g
c
c  5. Installation includes verifying or changing the following:
c
c      First :  the definition of function SECOND for CPU time only, and
c      Second:  the definition of function MOD2N in KERNEL
c      Third :  the system names Komput, Kontrl, and Kompil in MAIN.
c     During check-out run-time can be reduced by setting:    Nruns= 1 in SIZES.
c     For Standard LFK Benchmark Test verify:                 Nruns= 7 in SIZES.
c
c  6. Each kernel's computation is check-summed for easy validation.
c     Your checksums should compare to the precision used, within round-off.
c     The number of correct, significant digits in your check-sums is printed
c     in the OK column next to each check-sum.  Single precision should produce
c     6 to 8 OK digits and double precision should produce 11 to 16 OK digits.
c     Try REAL*16 in subr SIGNEL and SUMO to improve accuracy of DP checksums.
c
c  7. Verify CPU Time measurements from function SECOND by comparing the clock
c     calibration printout of total CPU time with system or real-time measures.
c     The accuracy of SECOND is also tested using subr VERIFY and CALIBR.
c     Each kernel's execution may be repeated arbitrarily many times
c     (MULTI >> 100) without overflow and produce verifiable checksums.
c
c     Default, uni-processor tests measure job  Cpu-time in SECOND (TSS mode).
c     Parallel processing tests should measure Real-time in stand-alone mode.
c
c  8. On computers with Virtual Storage Systems assure a working-set space
c     larger than the entire program so that page faults are negligible,
c     because we must measure the CPU-limited computation rates.
c     IT IS ALSO NECESSARY to run this test stand-alone, i.e. NO timesharing.
c     In VS Systems a series of runs are needed to show stable CPU timings.
c
c  9. On computers with Cache memories and high resolution CPU clocks we
c     need, if feasible, another ALL-scalar test-run setting Loop= 1
c     in SIZES to test un-primed cache (as well as encached) cpu rates.
c     Increase the size of array CACHE(in subr. VALUES) from 8192 to cache size.
c
c 10. On parallel computer systems which compile parallel Multi-tasking
c     at the Do-loop level (Micro-tasking) parallelisation of each
c     kernel is encouraged, but the number of processors used must be
c     reported.  Parallelisation of, or invarient code hoisting outside of
c     the outermost, repetition loop around each kernel (including TEST)
c     is PROHIBITED.  You may NOT declare NO-SIDE-EFFECTS function TEST.
c
c 11. A long endurance test can be set-up by redefining "laps" in SIZES.
c
c
c
c 12. Interpretation of LFK performance rates is discussed in Subr REPORT and:
c
c              F.H. McMahon,   The Livermore Fortran Kernels:
c              A Computer Test Of The Numerical Performance Range,
c              Lawrence Livermore National Laboratory,
c              Livermore, California, UCRL-53745, December 1986.
c
c
c     Quote statistics from the SUMMARY table of 72 timings (DO Span= 167)
c     located near line 700+ in the output file and terminated with a banner>>>
c
c     ********************************************
c     THE LIVERMORE  FORTRAN KERNELS:  * SUMMARY *
c     ********************************************
c
c                  Computer : CRAY Y-MP1
c                  System   : UNICOS 5.1
c                  Compiler : CF77 4.0
c                  Date     : 06/03/90
c           .
c           .
c           .
c             MFLOPS    RANGE:             REPORT ALL RANGE STATISTICS:
c             Mean  DO Span  =   167
c             Code Samples   =    72
c
c             Maximum   Rate =    294.34   Mega-Flops/Sec.
c             Quartile  Q3   =    123.27   Mega-Flops/Sec.
c             Average   Rate =     82.71   Mega-Flops/Sec.
c             Geometric Mean =     43.42   Mega-Flops/Sec.
c             Median    Q2   =     31.14   Mega-Flops/Sec.
c             Harmonic  Mean =     23.20   Mega-Flops/Sec.
c             Quartile  Q1   =     17.16   Mega-Flops/Sec.
c             Minimum   Rate =      2.74   Mega-Flops/Sec.
c             <<<<<<<<<<<<<<<<<<<<<<<<<<<*>>>>>>>>>>>>>>>>>>>>>>>>>>>
c             < BOTTOM-LINE:   72 SAMPLES LFK TEST RESULTS SUMMARY. >
c             < USE RANGE STATISTICS ABOVE FOR OFFICIAL QUOTATIONS. >
c             <<<<<<<<<<<<<<<<<<<<<<<<<<<*>>>>>>>>>>>>>>>>>>>>>>>>>>>
c
c     Sadly some analysts quote only the long vector(DO span=471) LFK statistics
c     because they are the most impressive but they are not the best guide to
c     the performance of a large, diverse workload; the SUMMARY statistics are.
c
c     A complete LFK perform-range report must include the minimum, the Harmonic
c     Geometric, and Arithmetic means, the maximum and the standard deviation.
c     The best central measure is the Geometric Mean(GM) of 72 rates because the
c     GM is less biased by outliers than the Harmonic(HM) or Arithemetic(AM).
c     CRAY hardware monitors have demonstrated that net Mflop rates for the
c     LLNL and UCSD tuned workloads are closest to the 72 LFK test GM rate.
c   [ However, CRAY memories are "all cache". LLNL codes ported to smaller cache
c     microprocessors typically perform at only LFK Harmonic mean MFlop rates.]
c
c
c     CORRESPONDENCE OF LFK TEST PERFORMANCE MEANS WITH LARGE WORKLOAD TUNING
c
c       -------      --------      ----------     -----------------------
c       Type of      CRAY-C90/1    Fraction       Tuning of Workload
c       Mean         (VL=167)      Flops in       Correlated with
c                    (MFlops)      Vector Ops     LFK Mean Performance
c       -------      --------      ----------     -----------------------
c
c        2*AM          382.            .97        Best applications
c
c          AM          191.            .89        Optimized applications
c
c          GM           86.            .74        Tuned workload
c
c          HM           41.            .45        Untuned workload
c
c          HM(scalar)   18.            .0         All-scalar applications
c       -------      --------      ----------     -----------------------
c       (AM,GM,HM  stand for Arithmetic, Geometric, Harmonic Mean Rates)
c
c
c
c
c         The Livermore Loops test reports 8 standard statistics 
c 	  (min to max) in order to represent the entire MFlops performance
c         distribution measured by the LFK samples. These statistics
c 	  provide a few well known points in the performance range
c 	  which analysts may use to establish a correspondence point
c 	  in the LFK performance range with the MFlops performance of 
c 	  their application or a workload of applications.
c 
c 	  For example,  an application named SNAIL has an
c 	  MFlops performance a little below Q1, the lowest quartile
c 	  defined by the LFK test on a half dozen different workstations.
c 	  The reason for this poor performance is that SNAIL was
c 	  formulated for CRAY using vector gather/scatter heavily which
c 	  causes poor cache performance on workstations.
c 	  Using this correspondence,  we can now predict that SNAIL will 
c 	  run near Q1 on the IBM-590, i.e. about  17 MFlops,
c 	  BEFORE it is timed on the 590.  In general, SNAIL's speedup
c         can be predicted to be the ratio of LFK Q1(new)/Q1(old)
c 	  for any future new cache based workstation.
c 	  [ On CRAY-YMP which is NOT cache based and has vector gather/
c 	  scatter,  SNAIL runs about 150 MFlops.  On C90 about 300 MFlops.]
c 
c
c
c
c
c     Some of the following super-scalar workstations are over 200 times faster
c     than the VAX-780 workhorse of the 1980's (GM= 0.17 MFlops):
c
c                                                                               
c  D.335 LFK Test#--335.1  ----335.3  ----335.4  ----335.5  ----335.6  ----335.7
c 
c    Vendor      CRAY RI    DEC /AXP   HP         IBM        CRAY RI    CRAY    
c    Model       YMP1 6.0   10000610   PA-755     6000/590   YMP1 6.0   Y16/1C90
c    OSystem     UNICOS 5   OpenVMS    HPUX 9.0   AIX 3.2    UNICOS 5   UNICOS 7
c    Compiler    CF77 4.0   GEM X3.2   f77 9.0b   XLF 3.1    CF77 4.0   CF77 5.0
c    OptLevel    Scalar     200MHz     -O+OS+OP   VAST2 4.   VAST       VAST 
c    Nr.Procs           1          1          1          1          1          1
c    Samples           72         72         72         72         72         72
c    WordSize          64         64         64         64         64         64
c    DO Span          167        167        167        167        167        167
c    Year            1990       1992       1992       1993       1990       1992
c    Kernel/MFlops-------- ---------- ---------- ---------- ---------- ---------
c           1       23.39      49.71      72.73     110.31     254.88     708.38
c           2       14.35      24.47      29.54      92.49      68.06     105.56
c           3       25.12      36.60      61.10     206.83     237.95     495.60
c           4       22.70      34.30      57.52     123.85      85.67     172.02
c           5       19.22      20.95      37.23      22.17      19.65      33.80
c           6        9.26      20.90      44.14      31.41      20.97      31.94
c           7       29.95     106.53      68.75     204.03     294.34     826.09
c           8       29.74      74.22      65.14     178.42     214.43     596.88
c           9       29.29      49.01      64.99     151.26     235.05     604.54
c          10       18.63      13.63      23.60      46.99     113.05     291.82
c          11       19.67      22.65      26.05      25.34      19.59      33.64
c          12       16.82      51.36      19.75      81.03     125.57     309.15
c          13        6.68      11.30       9.94       7.22      20.81      62.69
c          14        9.86      10.39      19.05      17.25      29.37      99.98
c          15        7.39      15.88      24.60      15.49      32.00     130.25
c          16        8.31      24.22      15.46      15.16       8.32      10.76
c          17       15.92      28.14      24.31      27.32      15.91      21.93
c          18       24.93      30.04      47.70      76.57     197.67     553.79
c          19       20.02      27.13      37.21      33.27      19.89      35.81
c          20       17.89      16.26      28.91      18.34      17.68      31.97
c          21       20.50      96.96      48.30     223.97     281.72     798.72
c          22        8.82      16.20      17.27      15.14      97.06     187.66
c          23       20.01      37.51      51.09      63.98      35.71      73.65
c          24        3.94      12.96      16.83      13.07      38.60      83.51
c  -------------     ....       ....       ....       ....       ....       ....
c 
c Maximum   Rate =  29.95     131.60      72.82     223.98     294.34     826.09
c Quartile  Q3   =  20.50      47.69      47.70     106.50     123.27     261.57
c Average   Rate =  16.55      35.52      35.53      68.98      82.72     190.56
c Geometric Mean =  14.59      28.06      30.63      43.21      43.43      86.26
c Median    Q2   =  16.97      24.47      28.91      33.27      31.15      83.51
c Harmonic  Mean =  12.45      23.13      26.26      27.84      23.21      40.73
c Quartile  Q1   =   8.96      16.32      18.81      17.25      17.16      31.15
c Minimum   Rate =   3.87       8.08       9.70       7.21       2.75       6.79
c 
c Average   Ratio=   1.00       2.15       2.15       4.17       5.00      11.52
c Geometric Ratio=   1.00       1.92       2.10       2.96       2.98       5.91
c Harmonic  Ratio=   1.00       1.86       2.11       2.24       1.86       3.27
c 
c Standard  Dev. =   7.52      27.41      18.88      65.13      88.20     227.25
c Avg Efficiency =  48.71%     21.32%     42.06%     19.29%     14.75%     10.4%
c           
c
c
c  
c   REALISTIC CPU PERFORMANCE COMPARISONS USING LIVERMORE LOOPS TEST MEAN RATES
c  
c         The range of speed-ups shown below as ratios of three performance mean
c         statistics has a very small variance compared to the enormous
c         performance ranges; these ratios  are convergent speed-up estimates
c         of the relative performance of diverse workloads.
c  
c  
c  
c     TABLE OF SPEED-UP RATIOS OF LIVERMORE LOOPS MEAN RATES (72 Samples
c  
c     The Geometric Mean is the statistic least biased by outliers.
c     (AM,GM,HM  stand for Arithmetic, Geometric, Harmonic Mean Rates)
c     But HM is the best MFlops estimate for cache based workstation workloads.
c  
c 
c  --------  ----  ------  -------- -------- -------- -------- -------- --------
c  SYSTEM    MEAN  MFLOPS  SX-3/14  VP2600   Y16/1C90 6000/590 9000/755 200MH610
c  --------  ----  ------  -------- -------- -------- -------- -------- --------
c 
c 
c  NEC       AM=  311.820 :   1.000    1.054    1.636    4.520    8.776    8.779
c  SX-3/14   GM=   95.590 :   1.000    1.028    1.108    2.212    3.121    3.407
c  F77v.012  HM=   38.730 :   1.000    0.916    0.951    1.391    1.475    1.674
c            SD=  499.780
c 
c 
c  FUJITSU   AM=  295.790 :   0.949    1.000    1.552    4.288    8.325    8.327
c  VP2600    GM=   93.030 :   0.973    1.000    1.078    2.153    3.037    3.315
c  F77  V12  HM=   42.260 :   1.091    1.000    1.038    1.518    1.609    1.827
c            SD=  514.490
c 
c 
c  CRAY      AM=  190.560 :   0.611    0.644    1.000    2.763    5.363    5.365
c  Y16/1C90  GM=   86.270 :   0.903    0.927    1.000    1.997    2.817    3.074
c  CF77 5.0  HM=   40.730 :   1.052    0.964    1.000    1.463    1.551    1.761
c            SD=  227.250
c 
c 
c  IBM       AM=   68.980 :   0.221    0.233    0.362    1.000    1.941    1.942
c  6000/590  GM=   43.210 :   0.452    0.464    0.501    1.000    1.411    1.540
c  XLF 3.1.  HM=   27.840 :   0.719    0.659    0.684    1.000    1.060    1.204
c            SD=   65.130
c 
c 
c  HP        AM=   35.530 :   0.114    0.120    0.186    0.515    1.000    1.000
c  9000/755  GM=   30.630 :   0.320    0.329    0.355    0.709    1.000    1.092
c  f77 9.0b  HM=   26.260 :   0.678    0.621    0.645    0.943    1.000    1.135
c            SD=   18.880
c 
c 
c  DEC       AM=   35.520 :   0.114    0.120    0.186    0.515    1.000    1.000
c  200MH610  GM=   28.060 :   0.294    0.302    0.325    0.649    0.916    1.000
c  GEM X3.2  HM=   23.130 :   0.597    0.547    0.568    0.831    0.881    1.000
c            SD=   27.410
c  --------  ----  ------  -------- -------- -------- -------- -------- --------
c
c
c
c
c  EVOLUTION OF AVERAGE COMPUTING RATES
c
c  ----------  --  ----  ----    --------------    ------------    -------------
c                                                  Uniprocessor
c                                Primary           Average         1.5e+10 Flops
c  Computer        Nr.   Oper    Memory(K=1024)    Computing       Problem
c  Vendor      YR  Proc  Regs    ( *dec digits)    Rate(MFlops)    Time (Hours)
c  ----------  --  ----  ----    --------------    ------------    -------------
c
c  UNIVAC      52     1     1          .1K *12            .001         4000.
c
c  IBM-650     53     1     4           2K *10            .0002       20000.
c
c  IBM-704     54     1     6           8K * 7            .008          500.
c
c  IBM-7090    59     1    11          32K * 7            .05            83.
c
c  IBM-7030    61     1                90K *16            .2             20.
c
c  CDC-6600    64     1    24         128K *14            .5              8.
c
c  CDC-7600    69     1    24         576K *14           3.               1.3
c
c  CRAY-1      76     1   656        1024K *14      5 - 50.         .80  - .08
c
c  CRAY-YMP    89     8   656      131072K *14     15 -150.         .26  - .026
c
c  CRAY-C90    91    16   656     1048576K *14     30 -600.         .13  - .006
c  ----------  --  ----  ----    --------------    ------------    -------------
c
c
c  ----------  --  ----  ----    --------------    ------------    -------------
c                                                  Microprocessor
c                                Primary           Average         1.5e+10 Flops
c  Computer        Nr.   Oper    Memory(K=1024)    Computing       Problem
c  Vendor      YR  Proc  Regs    ( *dec digits)    Rate(MFlops)    Time (Hours)
c  ----------  --  ----  ----    --------------    ------------    -------------
c
c  IBM-8086    81     1                64K *16            .002         2000.
c
c  IBM-8087    81     1                64K *16            .009          440.
c
c  IBM-80286   84     1               512K *16            .07            57.
c
c  IBM-80386   87     1              2048K *16            .3             13.
c
c  IBM-80486   89     1              4096K *16            .9              4.
c
c  IBM-6M560   92     1             16384K *16     14 - 45.                .2
c  ----------  --  ----  ----    --------------    ------------    -------------
c
c  1.  IBM-650 Magnetic Drum Data Processing Machine, Manual of Operation,
c      Form 22-6060-1, pp79-83, (1953).
c  2.  IBM-650 MDDPM Additional Features, Form 22-6258-0, p11, (1955).
c  3.  IBM-704 Electronic Data Processing Machine, p6, p91, (1954).
c  4.  F.H. McMahon, The Livermore Fortran Kernels: A Computer Test Of The
c      Numerical Performance Range, LLNL, Livermore, CA, UCRL-53745, (1986).
c
c****************************************************************************
c
c
c
c     DEVELOPMENT HISTORY OF THE LIVERMORE LOOPS TEST PROGRAM
c
c     The first version of the LFK Test (a.k.a. the Livermore Loops, circa
c     1970) consisting of 12 numerical Fortran kernels was developed
c     and enhanced by F.H. McMahon unless noted otherwise below.
c     The author is grateful for the constructive criticism of colleagues:
c     J.Owens, H.Nelson, L.Berdahl, D.Fuss, L.Sloan, T.Rudy, M.Seager.
c     Since mainframe computers in that era all provided cpu-timers
c     with micro-second time resolution, each kernal was executed just
c     once and timed with negligible experimental timing errors.
c
c     In 1980 the number of Fortran samples was doubled to 24 kernels
c     to represent a broad range of computational structures that would
c     challenge a comiler's capability to generate optimal machine code.
c
c     In 1983 the LFK test driver was extended to execute all 24 kernels
c     three times using three sets of DO loop limits (Avg: 18, 89, 468)
c     since parallel computer performace depends on scale or granularity.
c     These 72 sample statistics are more robust and definitive.
c
c     In 1985 a repetition loop was placed around each kernel to execute
c     them long enough for accurate timing using the standard UNIX
c     timer ETIME which has a crude time resolution of 0.01 seconds.
c
c     In 1986 the LFK test driver was extended to run the entire test
c     seven times so that experimental timing errors for each of the
c     72 samples could be measured.  Reports of these timing errors
c     are necessary for honest scientific experiments. See App. B, C:
c
c           F.H.McMahon,   The Livermore Fortran Kernels:
c           A Computer Test Of The Numerical Performance Range,
c           Lawrence Livermore National Laboratory,
c           Livermore, California, UCRL-53745, December 1986.
c
c     In 1986 Greg Astfalk (AT&T) reprogrammed subroutine KERNEL containing
c     the 24 samples in the C language. This C module can then be linked
c     with the standard Fortran LFK Test-driver program for testing under
c     identical benchmark conditions as the Fortran samples benchmark.
c     This C module was refined at LLNL by K.O'Hair, C.Rasbold, and M.Seager.
c
c     In 1990 the repetition loops around each kernel were modified
c     following reports of some code-hoisting by global optimization.
c     These repetition loops were submerged into function TEST beyond
c     the scope of optimizers so the 72 samples are now bullet-proof.
c     New, highly accurate, convergent methods to measure overhead time
c     were implemented ( in VERIFY, SECOVT, TICK ).
c
c     In 1991 the LFK test runtime control MULTI was increased twenty fold
c     for accurate timing when crude UNIX timers having poor time resolution
c     (Tmin= 0.01 sec) were used on very fast computers.  This was only a
c     temporary fix since under UNIX each kernel must always be run
c     at least 1 sec for 1% accuracy despite ever increasing cpu speeds.
c     Thus new algorithms were implemented that automatically determine
c     appropriate values for MULTI which are sufficiently large for
c     accurate timing of the kernels in any system.  A new method
c     of repetition is used that allows MULTI to be increased indefinately
c     (MULTI >> 100) in future without causing overflow and still compute
c     verifiable checksums.  New checksums were generated using IEEE 754
c     standard floating-point hardware on SUN, SGI, and HP workstations.
c     Operational accuracy of the test program is assured in future.
c
c****************************************************************************
c
c
c
c
c/      PARAMETER( kn= 47, kn2= 95, np= 3, ls= 3*47, krs= 24)
c/      PARAMETER( nk= 47, nl= 3, nr= 8 )
      parameter( ntimes= 18 )
c
      CHARACTER  Komput*24, Kontrl*24, Kompil*24, Kalend*24, Identy*24
      COMMON /SYSID/ Komput, Kontrl, Kompil, Kalend, Identy
c
      COMMON /ALPHA/ mk,ik,im,ml,il,Mruns,Nruns,jr,iovec,NPFS(8,3,47)
      COMMON /ORDER/ inseq, match, NSTACK(20), isave, iret
      COMMON /TAU/   tclock, tsecov, testov, cumtim(4)
      DIMENSION  FLOPS(141), TR(141), RATES(141), ID(141)
      DIMENSION  LSPAN(141), WG(141), OSUM (141), TERR(141), TK(6)
cLOX  REAL*8 SECOND
cLLNL      CALL  DROPFILE (   '+MFLOPS' )
c                        Job start Cpu time
      cumtim(1)= 0.0d0
             ti= SECND2( cumtim(1))
c
c                                            DEFINE YOUR COMPUTER SYSTEM:
       Komput  =  'CRAY-YMP (6.0ns)        '
       Kontrl  =  'UNICOS  fully loaded    '
       Kompil  =  'CFT77 4.0.3.4           '
       Kalend  =  '91.07.14                '
       Identy  =  'Frank McMahon, LLNL     '
c
c                        Initialize variables and Open Files
           CALL  INDATA( TK, iou)
c                        Record name in active linkage chain in COMMON /DEBUG/
           CALL  TRACE (' MAIN.  ')
c
c                        Verify Sufficient Loop Size Versus Cpu Clock Accuracy
           CALL  VERIFY( iou )
             tj= SECND2( cumtim(1))
             nt= ntimes
c                        Define control limits:  Nruns(runs), Loop(time)
           CALL  SIZES(-1)
c
c                        Run test Mruns times Cpu-limited; I/O is deferred:
      DO 2    k= 1,Mruns
              i= k
             jr= MOD( i-1,7) + 1
           CALL  IQRAN0( 256)
c                        Run test using one of 3 sets of DO-Loop spans:
c                        Set iou Negative to supress all I/O during Cpu timing.
      DO 1    j= im,ml
             il= j
           tock= TICK( -iou, nt)
c
           CALL  KERNEL( TK)
    1 continue
           CALL  TRIAL( iou, i, ti, tj)
    2 continue
c
c                        Report timing errors, Mflops statistics:
      DO 3    j= im,ml
             il= j
           CALL  RESULT( iou,FLOPS,TR,RATES,LSPAN,WG,OSUM,TERR,ID)
c
c                Report  Mflops for Vector Cpus( short, medium, long vectors):
c
                 iovec= 0
        IF(      iovec.EQ.1 )  THEN
           CALL  REPORT( iou,   mk,mk,FLOPS,TR,RATES,LSPAN,WG,OSUM,ID)
        ENDIF
    3 continue
c                Report  Mflops SUMMARY Statistics: for Official Quotations
c
           CALL  REPORT( iou,3*mk,mk,FLOPS,TR,RATES,LSPAN,WG,OSUM,ID)
c
      cumtim(1)= 0.0d0
         totjob= SECND2( cumtim(1)) - ti - tsecov
          WRITE( iou,9)  inseq, totjob, TK(1), TK(2)
          WRITE(   *,9)  inseq, totjob, TK(1), TK(2)
    9    FORMAT( '1',//,' Version: 22/DEC/86  mf528 ',2X,I12,/,1P,
     1                  ' CHECK FOR CLOCK CALIBRATION ONLY: ',/,
     2                  ' Total Job    Cpu Time =  ',e14.5, ' Sec.',/,
     3                  ' Total 24 Kernels Time =  ',e14.5, ' Sec.',/,
     4                  ' Total 24 Kernels Flops=  ',e14.5, ' Flops')
c
c                        Optional Cpu Clock Calibration Test of SECOND:
c          CALL  CALIBR
      STOP
      END
c***********************************************
      BLOCK DATA
c***********************************************
c
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
cout  DOUBLE  PRECISION  SUMS                                           REDUNDNT
c
c     l1 :=  param-dimension governs the size of most 1-d arrays
c     l2 :=  param-dimension governs the size of most 2-d arrays
c
c  ISPAN :=  Array of limits for DO loop control in the kernels
c  IPASS :=  Array of limits for multiple pass execution of each kernel
c  FLOPN :=  Array of floating-point operation counts for one pass thru kernel
c     WT :=  Array of weights to average kernel execution rates.
c  SKALE :=  Array of scale factors for SIGNEL data generator.
c   BIAS :=  Array of scale factors for SIGNEL data generator.
c
c    MUL :=  Array of multipliers * FLOPN  for each pass
c    WTP :=  Array of multipliers *    WT  for each pass
c     FR :=  Array of vectorisation fractions in REPORT
c   SUMW :=  Array of quartile weights in REPORT
c     IQ :=  Array of workload weights in REPORT
c   SUMS :=  Array of Verified Checksums of Kernels results: Nruns= 1 and 7.
c
c/      PARAMETER( l1= 1001, l2=  101, l1d= 2*1001 )
c/      PARAMETER( l13=  64, l13h= l13/2, l213= l13+l13h, l813= 8*l13 )
c/      PARAMETER( l14=2048, l16=  75, l416= 4*l16 , l21= 25 )
c
c/      PARAMETER( l1=   27, l2=   15, l1d= 2*1001 )
c/      PARAMETER( l13=   8, l13h= 8/2, l213= 8+4, l813= 8*8 )
c/      PARAMETER( l14=  16, l16= 15, l416= 4*15 , l21= 15)
c
c
c/      PARAMETER( l1=   1001, l2=   101, l1d= 2*1001 )
c/      PARAMETER( l13= 64, l13h= 64/2, l213= 64+32, l813= 8*64 )
c/      PARAMETER( l14= 2048, l16= 75, l416= 4*75 , l21= 25)
c
c/      PARAMETER( kn= 47, kn2= 95, np= 3, ls= 3*47, krs= 24)
c/      PARAMETER( m1= 1001-1, m2= 101-1, m7= 1001-6 )
      parameter( nsys= 5, ns= nsys+1, nd= 11, nt= 4 )
c
      COMMON /SPACES/ ion,j5,k2,k3,MULTI,laps,Loop,m,kr,LP,n13h,ibuf,nx,
     1 L,npass,nfail,n,n1,n2,n13,n213,n813,n14,n16,n416,n21,nt1,nt2,
     2 last,idebug,mpy,Loops2,mucho,mpylim, intbuf(16)
c
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), ticks,
     1                FR(9), TERR1(47), SUMW(7), START,
     2              SKALE(47), BIAS(47), WS(95), TOTAL(47), FLOPN(47),
     3                IQ(7), NPF, NPFS1(47)
c
      CHARACTER  NAMES*8
      COMMON /TAGS/  NAMES(nd,nt)
      COMMON /RATS/  RATED(nd,nt)
      COMMON /SPACEI/ WTP(3), MUL(3), ISPAN(47,3), IPASS(47,3)
c
      COMMON /ORDER/ inseq, match, NSTACK(20), isave, iret
c
      COMMON /PROOF/  SUMS(24,3,8)
c     ****************************************************************
c
      DATA  ( ISPAN(i,1), i= 1,47) /
     1 1001, 101, 1001, 1001, 1001, 64, 995, 100,
     2 101, 101, 1001, 1000, 64, 1001, 101, 75,
     3 101, 100, 101, 1000, 101, 101, 100, 1001, 23*0/
c
c*   : l1, l2, l1, l1, l1, l13, m7, m2,
c*   : l2, l2, l1, m1, l13, l1, l2, l16,
c*   : l2, m2, l2, m1, l21, l2, m2, l1, 23*0/
c
      DATA  ( ISPAN(i,2), i= 1,47) /
     1 101, 101, 101, 101, 101,  32, 101, 100,
     2 101, 101, 101, 100,  32, 101, 101,  40,
     3 101, 100, 101, 100,  50, 101, 100, 101,  23*0/
c
      DATA  ( ISPAN(i,3), i= 1,47) /
     1 27, 15, 27, 27, 27,  8, 21, 14,
     2 15, 15, 27, 26,  8, 27, 15, 15,
     3 15, 14, 15, 26, 20, 15, 14, 27,  23*0/
c
      DATA  ( IPASS(i,1), i= 1,47) /
     1   7, 67,  9, 14, 10,  3,  4, 10, 36, 34, 11, 12,
     2  36,  2,  1, 25, 35,  2, 39,  1,  1, 11,  8,  5,  23*0/
c
      DATA  ( IPASS(i,2), i= 1,47) /
     1   40, 40, 53, 70, 55,  7, 22,  6, 21, 19, 64, 68,
     2   41, 10,  1, 27, 20,  1, 23,  8,  1,  7,  5, 31,  23*0/
c
      DATA  ( IPASS(i,3), i= 1,47) /
     1   28, 46, 37, 38, 40, 21, 20,  9, 26, 25, 46, 48,
     2   31,  8,  1, 14, 26,  2, 28,  7,  1,  8,  7, 23,  23*0/
c
      DATA  (  MUL(i), i= 1,3) / 1, 2, 8 /
      DATA  (  WTP(i), i= 1,3) / 1.0, 2.0, 1.0 /
c
c     The following flop-counts (FLOPN) are required for scalar or serial
c     execution.  The scalar version defines the NECESSARY computation
c     generally, in the absence of proof to the contrary.  The vector
c     or parallel executions are only credited with executing the same
c     necessary computation.  If the parallel methods do more computation
c     than is necessary then the extra flops are not counted as through-put.
c
      DATA  ( FLOPN(i), i= 1,47)
     1     /5., 4., 2., 2., 2., 2., 16., 36., 17., 9., 1., 1.,
     2     7., 11., 33.,10., 9., 44., 6., 26., 2., 17., 11., 1., 23*0.0/
c
      DATA  ( WT(i), i= 1,47) /
     1 1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,
     2 1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,
     3 1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0, 23*0.0/
c
c
      DATA  ( SKALE(i), i= 1,47) /
     1 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0,
     2 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0,
     3 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0,
     4 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0,
     5       23*0.000D+0 /
c
c    : 0.1,  0.1,  0.1,  0.1,  0.1,  0.1,  0.1,  0.1,
c    : 0.1,  0.1,  0.1,  0.1,  0.1,  0.1,  0.1,  0.1,
c    : 0.1,  0.1,  0.1,  0.1,  0.1,  0.1,  0.1,  0.1,  23*0.0/
c
      DATA  ( BIAS(i), i= 1,47) /
     1 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,
     2 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,
     3 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  23*0.0/
c
      DATA  ( FR(i), i= 1,9) /
     1  0.0, 0.2, 0.4, 0.6, 0.7, 0.8, 0.9, 0.95, 1.0/
c
      DATA  ( SUMW(i), i= 1,7) /
     1 1.0, 0.95, 0.9, 0.8, 0.7, 0.6, 0.5/
c
      DATA  ( IQ(i), i= 1,7) /
     1 1, 2, 1, 2, 1, 2, 1/
c
c
c
c                                                 NEC SX-3/14
c
      DATA  ( NAMES(1,i), i= 1,3) /
     1        'NEC     ', 'SX-3/14 ', 'F77v.012' /
c
      DATA  ( RATED(1,i), i= 1,4) /
     1        311.82,  95.59,  38.73, 499.78 /
c
c                                                 FUJITSU VP2600
c
      DATA  ( NAMES(2,i), i= 1,3) /
     1        'FUJITSU ','VP2600  ','F77  V12' /
c
      DATA  ( RATED(2,i), i= 1,4) /
     1         295.79,  93.03,  42.26, 514.49 /
c
c                                                 CRAY-Y16/1 C90
c
      DATA  ( NAMES(3,i), i= 1,3) /
     1        'CRAY    ', 'Y16/1C90', 'CF77 5.0' /
c
      DATA  ( RATED(3,i), i= 1,4) /
     1         190.56,  86.27, 40.73,  227.25 /
c
c                                                 IBM 6000/560
c
      DATA  ( NAMES(4,i), i= 1,3) /
     1        'IBM     ', '6000/560', 'XLF 2.2.' /
c
      DATA  ( RATED(4,i), i= 1,4) /
     1         27.15,  20.21,  14.52,  20.15 /
c
c                                                 HP  9000/730
c
      DATA  ( NAMES(5,i), i= 1,3) /
     1        'HP      ', '9000/730', 'f77 8.05' /
c
      DATA  ( RATED(5,i), i= 1,4) /
     1         18.31,  15.72,  13.28,   9.68 /
c
c
c
c
c                                                 CRAY-YMP/1
c
c     DATA  ( NAMES(2,i), i= 1,3) /
c    :        'CRAY    ', 'YMP/1   ', 'CFT771.2' /
c
c     DATA  ( RATED(2,i), i= 1,4) /
c    :         78.23,  36.63,  17.66,  86.75 /
c
c                                                 IBM 3090S180
c
c     DATA  ( NAMES(2,i), i= 1,3) /
c    :        'IBM     ', '3090s180', 'VSF2.2.0' /
c
c     DATA  ( RATED(2,i), i= 1,4) /
c    :         17.56,  12.23,   9.02,  16.32 /
c
c                                                 IBM 6000/540
c
c     DATA  ( NAMES(4,i), i= 1,3) /
c    :        'IBM     ', '6000/540', 'XL v0.90' /
c
c     DATA  ( RATED(4,i), i= 1,4) /
c    :         14.17,  10.73,   7.45,   9.59 /
c
c                                                 COMPAQ i486/25
c
c     DATA  ( NAMES(5,i), i= 1,3) /
c    :        'COMPAQ  ', 'i486/25 ', '        ' /
c
c     DATA  ( RATED(5,i), i= 1,4) /
c    :          1.15,   1.05,   0.92,   0.48 /
c
c
c
c
      DATA  START /0.0/, NPF/0/, ibuf/0/, match/0/, MULTI/200/, laps/1/
      DATA  npass/0/, nfail/0/, last/-1/
c
c MULTI= 200
c
      DATA  ( SUMS(i,1,5), i= 1,24 ) /
     15.114652693224671D+04,1.539721811668385D+03,1.000742883066363D+01,
     25.999250595473891D-01,4.548871642387267D+03,4.375116344729986D+03,
     36.104251075174761D+04,1.501268005625798D+05,1.189443609974981D+05,
     47.310369784325296D+04,3.342910972650109D+07,2.907141294167248D-05,
     51.202533961842803D+11,3.165553044000334D+09,3.943816690352042D+04,
     65.650760000000000D+05,1.114641772902486D+03,1.015727037502300D+05,
     75.421816960147207D+02,3.040644339351239D+07,1.597308280710199D+08,
     82.938604376566697D+02,3.549900501563623D+04,5.000000000000000D+02/
c
      DATA  ( SUMS(i,2,5), i= 1,24 ) /
     15.253344778937972D+02,1.539721811668385D+03,1.009741436578952D+00,
     25.999250595473891D-01,4.589031939600982D+01,8.631675645333210D+01,
     36.345586315784055D+02,1.501268005625798D+05,1.189443609974981D+05,
     47.310369784325296D+04,3.433560407475758D+04,7.127569130821465D-06,
     59.816387810944345D+10,3.039983465145393D+07,3.943816690352042D+04,
     66.480410000000000D+05,1.114641772902486D+03,1.015727037502300D+05,
     75.421816960147207D+02,3.126205178815431D+04,7.824524877232093D+07,
     82.938604376566697D+02,3.549900501563623D+04,5.000000000000000D+01/
c
      DATA  ( SUMS(i,3,5), i= 1,24 ) /
     13.855104502494961D+01,3.953296986903059D+01,2.699309089320672D-01,
     25.999250595473891D-01,3.182615248447483D+00,1.120309393467088D+00,
     32.845720217644024D+01,2.960543667875003D+03,2.623968460874250D+03,
     41.651291227698265D+03,6.551161335845770D+02,1.943435981130448D-06,
     53.847124199949426D+10,2.923540598672011D+06,1.108997288134785D+03,
     65.152160000000000D+05,2.947368618589360D+01,9.700646212337040D+02,
     71.268230698051003D+01,5.987713249475302D+02,5.009945671204667D+07,
     86.109968728263972D+00,4.850340602749970D+02,1.300000000000000D+01/
c
c MULTI= 100
c
      DATA  ( SUMS(i,1,4), i= 1,24 ) /
     15.114652693224671D+04,1.539721811668385D+03,1.000742883066363D+01,
     25.999250595473891D-01,4.548871642387267D+03,4.375116344729986D+03,
     36.104251075174761D+04,1.501268005625798D+05,1.189443609974981D+05,
     47.310369784325296D+04,3.342910972650109D+07,2.907141294167248D-05,
     54.958101723583047D+10,3.165278275112100D+09,3.943816690352042D+04,
     62.825760000000000D+05,1.114641772902486D+03,7.507386432940455D+04,
     75.421816960147207D+02,3.040644339351239D+07,8.002484742089500D+07,
     82.938604376566697D+02,3.549900501563623D+04,5.000000000000000D+02/
c
      DATA  ( SUMS(i,2,4), i= 1,24 ) /
     15.253344778937972D+02,1.539721811668385D+03,1.009741436578952D+00,
     25.999250595473891D-01,4.589031939600982D+01,8.631675645333210D+01,
     36.345586315784055D+02,1.501268005625798D+05,1.189443609974981D+05,
     47.310369784325296D+04,3.433560407475758D+04,7.127569130821465D-06,
     53.542728632259964D+10,3.015943681556781D+07,3.943816690352042D+04,
     63.240410000000000D+05,1.114641772902486D+03,7.507386432940455D+04,
     75.421816960147207D+02,3.126205178815431D+04,3.916171317449981D+07,
     82.938604376566697D+02,3.549900501563623D+04,5.000000000000000D+01/
c
      DATA  ( SUMS(i,3,4), i= 1,24 ) /
     13.855104502494961D+01,3.953296986903059D+01,2.699309089320672D-01,
     25.999250595473891D-01,3.182615248447483D+00,1.120309393467088D+00,
     32.845720217644024D+01,2.960543667875003D+03,2.623968460874250D+03,
     41.651291227698265D+03,6.551161335845770D+02,1.943435981130448D-06,
     51.161063924078402D+10,2.609194549277411D+06,1.108997288134785D+03,
     62.576160000000000D+05,2.947368618589360D+01,9.700646212337040D+02,
     71.268230698051003D+01,5.987713249475302D+02,2.505599006414913D+07,
     86.109968728263972D+00,4.850340602749970D+02,1.300000000000000D+01/
c
c MULTI= 50
c
      DATA  ( SUMS(i,1,3), i= 1,24 ) /
     15.114652693224671D+04,1.539721811668385D+03,1.000742883066363D+01,
     25.999250595473891D-01,4.548871642387267D+03,4.375116344729986D+03,
     36.104251075174761D+04,1.501268005625798D+05,1.189443609974981D+05,
     47.310369784325296D+04,3.342910972650109D+07,2.907141294167248D-05,
     52.217514090251080D+10,3.165140890667983D+09,3.943816690352042D+04,
     61.413260000000000D+05,1.114641772902486D+03,6.203834985242972D+04,
     75.421816960147207D+02,3.040644339351239D+07,4.017185709583275D+07,
     82.938604376566697D+02,3.549900501563623D+04,5.000000000000000D+02/
c
      DATA  ( SUMS(i,2,3), i= 1,24 ) /
     15.253344778937972D+02,1.539721811668385D+03,1.009741436578952D+00,
     25.999250595473891D-01,4.589031939600982D+01,8.631675645333210D+01,
     36.345586315784055D+02,1.501268005625798D+05,1.189443609974981D+05,
     47.310369784325296D+04,3.433560407475758D+04,7.127569130821465D-06,
     51.430504282675192D+10,3.003923789762475D+07,3.943816690352042D+04,
     61.620410000000000D+05,1.114641772902486D+03,6.203834985242972D+04,
     75.421816960147207D+02,3.126205178815431D+04,1.961994537558922D+07,
     82.938604376566697D+02,3.549900501563623D+04,5.000000000000000D+01/
c
      DATA  ( SUMS(i,3,3), i= 1,24 ) /
     13.855104502494961D+01,3.953296986903059D+01,2.699309089320672D-01,
     25.999250595473891D-01,3.182615248447483D+00,1.120309393467088D+00,
     32.845720217644024D+01,2.960543667875003D+03,2.623968460874250D+03,
     41.651291227698265D+03,6.551161335845770D+02,1.943435981130448D-06,
     53.899370197966012D+09,2.452021524580127D+06,1.108997288134785D+03,
     61.288160000000000D+05,2.947368618589360D+01,9.700646212337040D+02,
     71.268230698051003D+01,5.987713249475302D+02,1.253425674020030D+07,
     86.109968728263972D+00,4.850340602749970D+02,1.300000000000000D+01/
c
c
c MULTI= 10     Old Checksums used before 1991 (longer run-times were needed)
c
      DATA  ( SUMS(i,1,2), i= 1,24 ) /
     15.114652693224671D+04,1.539721811668385D+03,1.000742883066363D+01,
     25.999250595473891D-01,4.548871642387267D+03,4.375116344729986D+03,
     36.104251075174761D+04,1.501268005625798D+05,1.189443609974981D+05,
     47.310369784325296D+04,3.342910972650109D+07,2.907141294167248D-05,
     54.057110454105199D+09,3.165030983112689D+09,3.943816690352042D+04,
     62.832600000000000D+04,1.114641772902486D+03,5.165625410754861D+04,
     75.421816960147207D+02,3.040644339351239D+07,8.289464835782872D+06,
     82.938604376566697D+02,3.549834542443621D+04,5.000000000000000D+02/
c
      DATA  ( SUMS(i,2,2), i= 1,24 ) /
     15.253344778937972D+02,1.539721811668385D+03,1.009741436578952D+00,
     25.999250595473891D-01,4.589031939600982D+01,8.631675645333210D+01,
     36.345586315784055D+02,1.501268005625798D+05,1.189443609974981D+05,
     47.310369784325296D+04,3.433560407475758D+04,7.127569130821465D-06,
     52.325318944820753D+09,2.994307876327030D+07,3.943816690352042D+04,
     63.244100000000000D+04,1.114641772902486D+03,5.165625410754861D+04,
     75.421816960147207D+02,3.126205178815431D+04,3.986531136460764D+06,
     82.938604376566697D+02,3.549894609774404D+04,5.000000000000000D+01/
c
      DATA  ( SUMS(i,3,2), i= 1,24 ) /
     13.855104502494961D+01,3.953296986903059D+01,2.699309089320672D-01,
     25.999250595473891D-01,3.182615248447483D+00,1.120309393467088D+00,
     32.845720217644024D+01,2.960543667875003D+03,2.623968460874250D+03,
     41.651291227698265D+03,6.551161335845770D+02,1.943435981130448D-06,
     54.755211251524082D+08,2.326283104822299D+06,1.108997288134785D+03,
     62.577600000000000D+04,2.947368618589360D+01,9.700646212337040D+02,
     71.268230698051003D+01,5.987713249475302D+02,2.516870081041265D+06,
     86.109968728263972D+00,4.850340602749970D+02,1.300000000000000D+01/
c
c MULTI=  1     Old Checksums used before 1986 (longer run-times were needed)
c
      DATA  ( SUMS(i,1,1), i= 1,24 ) /
     15.114652693224671D+04,1.539721811668385D+03,1.000742883066363D+01,
     25.999250595473891D-01,4.548871642387267D+03,4.375116344729986D+03,
     36.104251075174761D+04,1.501268005625798D+05,1.189443609974981D+05,
     47.310369784325296D+04,3.342910972650109D+07,2.907141294167248D-05,
     54.468741170140841D+08,3.165006253912748D+09,3.943816690352042D+04,
     62.901000000000000D+03,1.227055736845479D+03,4.932243865816480D+04,
     75.421816960147207D+02,3.040644339351239D+07,1.115926577271652D+06,
     82.938604376566697D+02,3.138872788135057D+04,5.000000000000000D+02/
c
      DATA  ( SUMS(i,2,1), i= 1,24 ) /
     15.253344778937972D+02,1.539721811668385D+03,1.009741436578952D+00,
     25.999250595473891D-01,4.589031939600982D+01,8.631675645333210D+01,
     36.345586315784055D+02,1.501268005625798D+05,1.189443609974981D+05,
     47.310369784325296D+04,3.433560407475758D+04,7.127569130821465D-06,
     52.323352389500009D+08,2.992144295804055D+07,3.943816690352042D+04,
     63.281000000000000D+03,1.114641772902486D+03,4.932243865816480D+04,
     75.421816960147207D+02,3.126205178815431D+04,4.690129326568575D+05,
     82.938604376566697D+02,3.228104575530876D+04,5.000000000000000D+01/
c
      DATA  ( SUMS(i,3,1), i= 1,24 ) /
     13.855104502494961D+01,3.953296986903059D+01,2.699309089320672D-01,
     25.999250595473891D-01,3.182615248447483D+00,1.120309393467088D+00,
     32.845720217644024D+01,2.960543667875003D+03,2.623968460874250D+03,
     41.651291227698265D+03,6.551161335845770D+02,1.943435981130448D-06,
     54.143805389489125D+07,2.297991960376787D+06,1.108997288134785D+03,
     62.592000000000000D+03,2.947368618589360D+01,9.700646212337040D+02,
     71.268230698051003D+01,5.987713249475302D+02,2.629580827304779D+05,
     86.109968728263972D+00,4.850340602749970D+02,1.300000000000000D+01/
c
c****************************************************************************
c
c     The following DP checksums are NOT used for the standard LFK
c     performance test but may be used to test Fortran compiler precision.
c
c     Checksums for Quadruple-Precision (IBM,DEC); or CRAY Double-Precision.
c     Quadruple precision checksums computed by Dr. D.S. Lindsay, HITACHI.
c     These Checksums were obtained with   MULTI= 10. (BLOCKDATA)
c     Change the numerical edit descriptor Q to D on CRAY systems.
cQc
cQ      DATA  ( SUMS(i,1,1), i= 1,24 )  /
cQ     a 0.5114652693224705102247326Q+05, 0.5150345372943066022569677Q+03,
cQ     b 0.1000742883066623145122027Q+02, 0.5999250595474070357564935Q+00,
cQ     c 0.4548871642388544199267412Q+04, 0.5229095383954675635496207Q+13,
cQ     d 0.6104251075163778121943921Q+05, 0.1501268005627157186827043Q+06,
cQ     e 0.1189443609975085966254160Q+06, 0.7310369784325972183233686Q+05,
cQ     f 0.3342910972650530676553892Q+08, 0.2907141428639174056565229Q-04,
cQ     g 0.4057110454105263471505061Q+10, 0.2982036205992255154832180Q+10,
cQ     h 0.3943816690352311804312052Q+05, 0.2832600000000000000000000Q+05,
cQ     i 0.1114641772903091760464680Q+04, 0.5165625410757306606559174Q+05,
cQ     j 0.5421816960150398899460410Q+03, 0.3040644339317275409518862Q+08,
cQ     k 0.8289464835786202431495974Q+07, 0.2938604376567099667790619Q+03,
cQ     l 0.3549834542446150511553453Q+05, 0.5000000000000000000000000Q+03/
cQc
cQ      DATA  ( SUMS(i,2,1), i= 1,24 )  /
cQ     a 0.5253344778938000681994399Q+03, 0.5150345372943066022569677Q+03,
cQ     b 0.1009741436579188086885138Q+01, 0.5999250595474070357564935Q+00,
cQ     c 0.4589031939602131581035992Q+02, 0.2693280957416549457193910Q+16,
cQ     d 0.6345586315772524401198340Q+03, 0.1501268005627157186827043Q+06,
cQ     e 0.1189443609975085966254160Q+06, 0.7310369784325972183233686Q+05,
cQ     f 0.3433560407476162346605343Q+05, 0.7127569144561925151361427Q-05,
cQ     g 0.2325318944820836005421577Q+10, 0.3045676741897511424188763Q+08,
cQ     h 0.3943816690352311804312052Q+05, 0.3244100000000000000000000Q+05,
cQ     i 0.1114641772903091760464680Q+04, 0.5165625410757306606559174Q+05,
cQ     j 0.5421816960150398899460410Q+03, 0.3126205178811007613028089Q+05,
cQ     k 0.3986531136462291709063170Q+07, 0.2938604376567099667790619Q+03,
cQ     l 0.3549894609776936556634240Q+05, 0.5000000000000000000000000Q+02/
cQc
cQ      DATA  ( SUMS(i,3,1), i= 1,24 )  /
cQ     a 0.3855104502494983491740258Q+02, 0.1199847611437483513040755Q+02,
cQ     b 0.2699309089321296439173090Q+00, 0.5999250595474070357564935Q+00,
cQ     c 0.3182615248448271678796560Q+01, 0.8303480073326955433087865Q+12,
cQ     d 0.2845720217638848365786224Q+02, 0.2960543667877649943946702Q+04,
cQ     e 0.2623968460874419268457298Q+04, 0.1651291227698377392796690Q+04,
cQ     f 0.6551161335846537217862474Q+03, 0.1943435981776804808483341Q-05,
cQ     g 0.4755211251524563699634913Q+09, 0.2547733008933910800455698Q+07,
cQ     h 0.1108997288135066584075059Q+04, 0.2577600000000000000000000Q+05,
cQ     i 0.2947368618590713935189324Q+02, 0.9700646212341513210532085Q+03,
cQ     j 0.1268230698051747067958265Q+02, 0.5987713249471801461035250Q+03,
cQ     k 0.2516870081042209239664473Q+07, 0.6109968728264795136407718Q+01,
cQ     l 0.4850340602751675804605762Q+03, 0.1300000000000000000000000Q+02/
cQc
      END
c
c
c***************************************
      SUBROUTINE CALIBR
c***********************************************************************
c                                                                      *
c     CALIBR - Cpu clock calibration tests accuracy of SECOND function.*
c                                                                      *
c     CALIBR tests function SECOND by using it to time a computation   *
c     repeatedly.  These SECOND timings are written to stdout(terminal)*
c     one at a time as the cpu-clock is read, so we can observe a real *
c     external clock time and thus check the accuracy of SECOND code.  *
c     Comparisons with an external clock require a stand-alone run.    *
c     Otherwise compare with system charge for total job cpu time.     *
c                                                                      *
c     Sample Output from CRAY-YMP1:                                    *
c                                                                      *
c                                                                      *
c CPU CLOCK CALIBRATION:  START STOPWATCH NOW !                        *
c           TESTS ACCURACY OF FUNCTION SECOND()                        *
c           Monoprocess this test, stand-alone, no TSS                 *
c           Verify  T or DT  observe external clock:                   *
c                                                                      *
c        -------     -------      ------      -----                    *
c        Total T ?   Delta T ?    Mflops ?    Flops                    *
c        -------     -------      ------      -----                    *
c  1        0.00        0.00        9.15    4.00000e+04    4.98000e-02 *
c  2        0.01        0.01       11.67    1.20000e+05    8.98000e-02 *
c  3        0.02        0.01       12.84    2.80000e+05    1.69800e-01 *
c  4        0.04        0.02       13.47    6.00000e+05    3.29800e-01 *
c  5        0.09        0.05       13.81    1.24000e+06    6.49800e-01 *
c  6        0.18        0.09       14.00    2.52000e+06    1.28980e+00 *
c  7        0.36        0.18       14.12    5.08000e+06    2.56980e+00 *
c  8        0.72        0.36       14.19    1.02000e+07    5.12980e+00 *
c  9        1.44        0.72       14.20    2.04400e+07    1.02498e+01 *
c 10        2.88        1.44       14.23    4.09200e+07    2.04898e+01 *
c 11        5.74        2.87       14.27    8.18800e+07    4.09698e+01 *
c 12       11.48        5.74       14.27    1.63800e+08    8.19298e+01 *
c 13       22.98       11.50       14.26    3.27640e+08    1.63850e+02 *
c 14       45.92       22.94       14.27    6.55320e+08    3.27690e+02 *
c 15       91.88       45.96       14.26    1.31068e+09    6.55369e+02 *
c***********************************************************************
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
c
      parameter( limitn= 101, ndim= limitn+10 )
      DIMENSION  X(ndim), Y(ndim), cumtim(10)
c
c     CALL TRACE ('CALIBR  ')
      cumtim(1)= 0.0d0
             t0= SECND2( cumtim(1))
c
          WRITE( *,111)
          WRITE( *,110)
          WRITE( *,112)
          WRITE( *,113)
          WRITE( *,114)
          WRITE( *,115)
          WRITE( *,114)
  111    FORMAT(//,' CPU CLOCK CALIBRATION:  START STOPWATCH NOW !')
  110    FORMAT('           TESTS ACCURACY OF FUNCTION SECOND()')
  112    FORMAT('           Monoprocess this test, stand-alone, no TSS')
  113    FORMAT('           Verify  T or DT  observe external clock:',/)
  114    FORMAT('           -------     -------      ------      -----')
  115    FORMAT('           Total T ?   Delta T ?    Mflops ?    Flops')
  119    FORMAT(4X,I2,3F12.2,2E15.5)
c
              l= 0
              n= 0
              m= 200
          nflop= 0
         totalt= 0.00d0
         deltat= 0.00d0
          flops= 0.00d0
             rn= 0.00d0
             t1= 0.00d0
             t2= 0.00d0
      cumtim(1)= 0.0d0
             t2= SECND2( cumtim(1))
             IF( t2.GT. 1.00d04 ) GO TO 911
             IF( t2.LT. 1.00d-8 ) GO TO 911
c
   10         l= l + 1
              m= m + m
c
           X(1)= 0.0098000d0
           Y(1)= 0.0000010d0
         DO 2 i= 2,limitn
           Y(i)= Y(1)
    2 continue
c                                  Compute LFK Kernel 11  m times
         DO 5 j= 1,m
         DO 4 k= 2,limitn
           X(k)= X(k-1) + Y(k)
    4 continue
           X(1)= X(limitn)
    5 continue
c
             t1= t2
      cumtim(1)= 0.0d0
             t2= SECND2( cumtim(1))
c                                  IF elapsed time can be observed, Print Mark.
         totalt= t2 - t0
         deltat= t2 - t1
          nflop= nflop + (limitn - 1) * m
             IF( deltat .GT. 2.00d0  .OR.  l.GT.12 )  THEN
                     n= n + 1
                    rn= REAL( nflop)
                 flops= 1.00d-6 *( REAL( nflop)/( totalt +1.00d-9))
                 WRITE( *,119)  l, totalt, deltat, flops, rn, X(limitn)
             ENDIF
             IF( deltat .LT. 200.0d0  .OR.  n.LT.3 )  GO TO 10
c
             IF( n.LE.0 )  THEN
                 WRITE( *,119)  l, totalt, deltat, flops, rn, X(limitn)
             ENDIF
      STOP
c
  911     WRITE( *,61)
          WRITE( *,62) totalt
      STOP
c
   61 FORMAT(1X,'FATAL(CALIBR): cant measure time using func SECOND()')
   62 FORMAT(/,13X,'using SECOND():  totalt=',1E20.8,' ?')
c
      END
c
c***********************************************
      SUBROUTINE INDEX
c***********************************************
c       MODULE     PURPOSE
c       ------     -----------------------------------------------
c
c       CALIBR     cpu clock calibration tests accuracy of SECOND function
c
c       INDATA     initialize variables
c
c       IQRANF     computes a vector of pseudo-random indices
c       IQRAN0     define seed for new IQRANF sequence
c
c       KERNEL     executes 24 samples of Fortran computation
c
c       PFM        optional call to system hardware performance monitor
c
c       RELERR     relative error between  u,v  (0.,1.)
c
c       REPORT     prints timing results
c
c       RESULT     computes execution rates  into pushdown store
c
c       SECOND     cumulative CPU time for task in seconds (M.K.S. units)
c
c       SECOVT     measures the Overhead time for calling   SECOND
c
c       SENSIT     sensitivity analysis of harmonic mean to 49 workloads
c
c       SEQDIG     computes nr significant, equal digits in pairs of numbers
c
c       SIGNEL     generates a set of floating-point numbers near 1.0
c
c       SIMD       sensitivity analysis of harmonic mean to SISD/SIMD model
c
c       SIZES      test and set the loop controls before each kernel test
c
c       SORDID     simple sort
c
c       SPACE      sets memory pointers for array variables.  optional.
c
c       SPEDUP     computes Speed-ups: A circumspect method of comparison.
c
c       STATS      calculates unweighted statistics
c
c       STATW      calculates   weighted statistics
c
c       SUMO       check-sum with ordinal dependency
c
c       SUPPLY     initializes common blocks containing type real arrays.
c
c       TALLY      computes average and minimum Cpu timings and variances.
c
c       TDIGIT     counts lead digits followed by trailing zeroes
c
c       TEST       Repeats and times the execution of each kernel
c
c       TESTS      Checksums and initializes the data for each kernel test
c
c       TICK       measures timing overhead of subroutine test
c
c       TILE       computes  m-tile value and corresponding index
c
c       TRACE ,TRACK    push/pop caller's name and serial nr. in /DEBUG/
c
c       TRAP       checks that index-list values are in valid domain
c
c       TRIAL      validates checksums of current run for endurance trial
c
c       VALID      compresses valid timing results
c
c       VALUES     initializes special values
c
c       VERIFY     verifies sufficient Loop size versus cpu clock accuracy
c
c       WATCH      can continually test COMMON variables and localize bugs
c
c  ------------ -------- -------- -------- -------- -------- --------
c  ENTRY LEVELS:   1        2        3        4        5        6
c  ------------ -------- -------- -------- -------- -------- --------
c               MAIN.    SECOND
c                        INDATA
c                        VERIFY   SECOND
c                                 SIZES    IQRAN0
c                                 STATS    SQRT
c                                 TDIGIT   LOG10
c                        SIZES    IQRAN0
c
c                        TICK     TEST     TESTS    SECOND
c                                                   SIZES
c                                                   SUMO
c                                                   VALUES   SUPPLY   SIGNEL
c                                                            IQRANF   MOD
c                                          SECOND
c                                 VALID    TRAP              TRAP
c                                 STATS    SQRT
c                                 IQRANF   MOD
c                                          TRAP
c                        KERNEL   SPACE
c                                 SQRT
c                                 EXP
c                                 TEST     TESTS    SECOND
c                                                   SIZES
c                                                   SUMO
c                                                   VALUES   SUPPLY   SIGNEL
c                                                            IQRANF   MOD
c                                          SECOND
c                        TRIAL    SEQDIG   LOG10    TDIGIT
c                                 IQRAN0
c
c                        RESULT   TALLY    SIZES    IQRAN0   TRAP
c                                          PAGE
c                                          STATS    SQRT
c
c                                 SEQDIG   LOG10    TDIGIT
c
c                        REPORT   VALID    TRAP
c                                 MOD
c                                 STATW    SORDID   TRAP
c                                          TILE
c                                          SQRT
c                                          LOG10
c                                 PAGE
c                                 TRAP
c                                 SENSIT   VALID    TRAP
c                                          SORDID   TRAP
c                                          PAGE
c                                          STATW    SORDID   TRAP
c                                                   TILE
c                                 SIMD     VALID    TRAP
c                                          STATW    SORDID   TRAP
c                                                   TILE
c                                 SPEDUP
c                        STOP
c
c
c
c
c    All subroutines also call TRACE , TRACK , and WATCH to assist debugging.
c
c
c
c
c
c
c
c    ------ ---- ------     -----   ------------------------------------
c    BASE   TYPE CLASS      NAME    GLOSSARY
c    ------ ---- ------     -----   ------------------------------------
c    SPACE0    R Array      BIAS  - scale factors for SIGNEL data generator
c    SPACE0    R Array      CSUM  - checksums of KERNEL result arrays
c    BETA      R Array      CSUMS - sets of CSUM for all test runs
c    BETA      R Array      DOS   - sets of TOTAL flops for all test runs
c    SPACE0    R Array      FLOPN - flop counts for one execution pass
c    BETA      R Array      FOPN  - sets of FLOPN for all test runs
c    SPACE0    R Array      FR    - vectorisation fractions; abscissa for REPORT
c    SPACES    I scalar     ibuf  - flag enables one call to SIGNEL
c    ALPHA     I scalar     ik    - current number of executing kernel
c    ALPHA     I scalar     il    - selects one of three sets of loop spans
c    SPACES    I scalar     ion   - logical I/O unit number for output
c    SPACEI    I Array      IPASS - Loop control limits for multiple-pass loops
c    SPACE0    I Array      IQ    - set of workload weights for REPORT
c    SPACEI    I Array      ISPAN - loop control limits for each kernel
c    SPACES    I scalar     j5    - datum in kernel 16
c    ALPHA     I scalar     jr    - current test run number (1 thru 7)
c    SPACES    I scalar     k2    - counter in kernel 16
c    SPACES    I scalar     k3    - counter in kernel 16
c    SPACES    I scalar     kr    - a copy of mk
c    SPACES    I scalar     laps  - multiplies Nruns for long Endurance test
c    SPACES    I scalar     Loop  - current multiple-pass loop limit in KERNEL
c    SPACES    I scalar     m     - temp integer datum
c    ALPHA     I scalar     mk    - number of kernels to evaluate .LE.24
c    ALPHA     I scalar     ml    - maximum value of il=  3
c    SPACES    I scalar     mpy   - repetiton counter of MULTI pass loop
c    SPACES    I scalar     Loops2- repetiton loop limit
c    ALPHA     I scalar     Mruns - number of complete test runs .GE.Nruns
c    SPACEI    I Array      MUL   - multipliers * IPASS defines Loop
c    SPACES    I scalar     MULTI - Multiplier used to compute Loop in SIZES
c    SPACES    I scalar     n     - current DO loop limit in KERNEL
c    SPACES    I scalar     n1    - dimension of most 1-D arrays
c    SPACES    I scalar     n13   - dimension used in kernel 13
c    SPACES    I scalar     n13h  - dimension used in kernel 13
c    SPACES    I scalar     n14   - dimension used in kernel 14
c    SPACES    I scalar     n16   - dimension used in kernel 16
c    SPACES    I scalar     n2    - dimension of most 2-D arrays
c    SPACES    I scalar     n21   - dimension used in kernel 21
c    SPACES    I scalar     n213  - dimension used in kernel 21
c    SPACES    I scalar     n416  - dimension used in kernel 16
c    SPACES    I scalar     n813  - dimension used in kernel 13
c    SPACE0    I scalar     npf   - temp integer datum
c    ALPHA     I Array      NPFS  - sets of NPFS1 for all test runs
c    SPACE0    I Array      NPFS1 - number of page-faults for each kernel
c    ALPHA     I scalar     Nruns - number of complete test runs .LE.7
c    SPACES    I scalar     nt1   - total size of common -SPACE1- words
c    SPACES    I scalar     nt2   - total size of common -SPACE2- words
c    BETA      R Array      SEE   - (i,1,jr,il) sets of TEST overhead times
c    BETA      R Array      SEE   - (i,2,jr,il) sets of csums of SPACE1
c    BETA      R Array      SEE   - (i,3,jr,il) sets of csums of SPACE2
c    SPACE0    R Array      SKALE - scale factors for SIGNEL data generator
c    SPACE0    R scalar     start - temp start time of each kernel
c    PROOF     R Array      SUMS  - sets of verified checksums for all test runs
c    SPACE0    R Array      SUMW  - set of quartile weights for REPORT
c    TAU       R scalar     tclock- minimum cpu clock time= resolution
c    SPACE0    R Array      TERR1 - overhead-time errors for each kernel
c    BETA      R Array      TERRS - sets of TERR1 for all runs
c    TAU       R scalar     testov- average overhead time in TEST linkage
c    BETA      R scalar     tic   - average overhead time in SECOND (copy)
c    SPACE0    R scalar     ticks - average overhead time in TEST linkage(copy)
c    SPACE0    R Array      TIME  - net execution times for all kernels
c    BETA      R Array      TIMES - sets of TIME for all test runs
c    SPACE0    R Array      TOTAL - total flops computed by each kernel
c    TAU       R scalar     tsecov- average overhead time in SECOND
c    SPACE0    R Array      WS    - unused
c    SPACE0    R Array      WT    - weights for each kernel sample
c    SPACEI    R Array      WTP   - weights for the 3 span-varying passes
c    SPACE0    R Array      WW    - unused
c
c
c  --------- -----------------------------------------------------------------
c   COMMON   Usage
c  --------- -----------------------------------------------------------------
c
c   /ALPHA /
c            VERIFY    TICK      TALLY     SIZES     RESULT    REPORT    KERNEL
c            MAIN.
c   /BASE1 /
c            SUPPLY
c   /BASE2 /
c            SUPPLY
c   /BASER /
c            SUPPLY
c   /BETA  /
c            TICK      TALLY     SIZES     RESULT    REPORT    KERNEL
c   /DEBUG /
c            TRACE     TRACK     TRAP
c   /ORDER /
c            TRACE     TRACK     TRAP
c   /PROOF /
c            RESULT    BLOCKDATA
c   /SPACE0/
c            VALUES    TICK      TEST      TALLY     SUPPLY    SIZES     RESULT
c            REPORT    KERNEL    BLOCKDATA
c   /SPACE1/
c            VERIFY    VALUES    TICK      TEST      SUPPLY    SPACE     KERNEL
c   /SPACE2/
c            VERIFY    VALUES    TICK      TEST      SUPPLY    SPACE     KERNEL
c   /SPACE3/
c            VALUES
c   /SPACEI/
c            VERIFY    VALUES    TICK      TEST      SIZES     RESULT    REPORT
c            KERNEL    BLOCKDATA
c   /SPACER/
c            VALUES    TICK      TEST      SUPPLY    SIZES     KERNEL
c   /SPACES/
c            VERIFY    VALUES    TICK      TEST      SUPPLY    SIZES     KERNEL
c            BLOCKDATA
c  --------- -----------------------------------------------------------------
c
c
c           SubrouTine Timing on CRAY-XMP1:
c
c           Subroutine   Time(%) All Scalar
c
c           KERNEL       52.24%
c           SUPPLY       17.85%
c           VERIFY        8.76%
c           VALUES        6.15%
c           STATS         5.44%
c           DMPY          1.97%
c           DADD          1.53%
c           EXP           1.02%
c           SQRT           .99%
c           SORDID         .81%
c           DDIV           .38%
c           IQRANF         .25%
c           SUMO           .22%
c           TRACE          .19%
c           SIGNEL         .16%
c           TRAP           .10%
c           TRACK          .10%
c           STATW          .08%
c           TILE           .04%
c           SIZES          .03%
c           ALOG10         .03%
c
c           Subroutine   Time(%)  Auto Vector
c
c           KERNEL       56.28%
c           VALUES       10.33%
c           STATS         8.57%
c           DADD          4.34%
c           DMPY          3.86%
c           VERIFY        2.61%
c           SUPPLY        2.28%
c           SQRT          2.10%
c           SORDID        1.84%
c           SUMO           .80%
c           DDIV           .78%
c           SDOT           .67%
c           TRACE          .53%
c           IQRANF         .50%
c           SIGNEL         .36%
c           EXP            .32%
c           TRACK          .23%
c           TRAP           .20%
c           ALOG10         .18%
c           STATW          .16%
c
c
      RETURN
      END
c
c***************************************
      SUBROUTINE INDATA( TK, iou)
c***************************************
c       INDATA     initialize variables
c
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
c
c/      PARAMETER( kn= 47, kn2= 95, np= 3, ls= 3*47, krs= 24)
c/      PARAMETER( nk= 47, nl= 3, nr= 8 )
      DIMENSION  TK(6)
      COMMON /ALPHA/ mk,ik,im,ml,il,Mruns,Nruns,jr,iovec,NPFS(8,3,47)
      COMMON /TAU/   tclock, tsecov, testov, cumtim(4)
      COMMON /BETA / tic, TIMES(8,3,47), SEE(5,3,8,3),
     1              TERRS(8,3,47), CSUMS(8,3,47),
     2              FOPN(8,3,47), DOS(8,3,47)
c
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), ticks,
     1                FR(9), TERR1(47), SUMW(7), START,
     2              SKALE(47), BIAS(47), WS(95), TOTAL(47), FLOPN(47),
     3                IQ(7), NPF, NPFS1(47)
c
      COMMON /ORDER/ inseq, match, NSTACK(20), isave, iret
      COMMON /SPACES/ ion,j5,k2,k3,MULTI,laps,Loop,m,kr,LP,n13h,ibuf,nx,
     1 L,npass,nfail,n,n1,n2,n13,n213,n813,n14,n16,n416,n21,nt1,nt2,
     2 last,idebug,mpy,Loops2,mucho,mpylim, intbuf(16)
c
       TK(1)= 0.00d0
       TK(2)= 0.00d0
      testov= 0.00d0
      ticks = 0.00d0
      tclock= 0.00d0
      tsecov= 0.00d0
      tic   = 0.00d0
c
      jr    = 1
      Nruns = 1
      il    = 1
      mk    = 1
      ik    = 1
c
      inseq = 0
      isave = 0
      iret  = 0
c
      Loops2= 1
      mpylim= Loops2
      mpy   = 1
      MULTI = 1
      mucho = 1
      L     = 1
      Loop  = 1
      LP    = Loop
      n     = 0
c
      iou   = 8
      ion   = iou
        CALL  INITIO( 8, 'output')
c       CALL  INITIO( 7, 'chksum')
c
      CALL TRACE ('INDATA  ')
cPFM       IF( INIPFM( ion, 0) .NE. 0 )  THEN
cPFM           CALL WHERE(20)
cPFM       ENDIF
c
cLLL.      call  Q8EBM
c
          WRITE (   *,7002)
          WRITE (   *,7003)
          WRITE (   *,7002)
          WRITE ( iou,7002)
          WRITE ( iou,7003)
          WRITE ( iou,7002)
 7002 FORMAT( ' *********************************************')
 7003 FORMAT( ' THE LIVERMORE  FORTRAN KERNELS "MFLOPS" TEST:')
      WRITE( iou, 797)
      WRITE( iou, 798)
  797 FORMAT(' >>> USE 72 SAMPLES LFK TEST RESULTS SUMMARY (line 330+)')
  798 FORMAT(' >>> USE ALL RANGE STATISTICS FOR OFFICIAL QUOTATIONS.  ')
      CALL TRACK ('INDATA  ')
      RETURN
      END
c
c*************************************************
      SUBROUTINE INITIO( iou, name )
c***********************************************************************
c                                                                      *
c       INITIO - Assign logdevice nr "iou" to disk file "name"         *
c                                                                      *
c          iou - logical i/o device number                             *
c         name - name to assign to disk file                           *
c                                                                      *
c***********************************************************************
      LOGICAL LIVING
      CHARACTER *(*) name
c     CALL TRACE ('INITIO  ')
c
           INQUIRE( FILE=name, EXIST= LIVING )
                IF( LIVING ) THEN
                    OPEN ( UNIT=iou, FILE=name, STATUS='OLD')
                    CLOSE( UNIT=iou, STATUS='DELETE')
               ENDIF
             OPEN  (UNIT=iou, FILE=name, STATUS='NEW')
c
c     CALL TRACK ('INITIO  ')
      RETURN
      END
c
c***************************************
      SUBROUTINE IQRAN0( newk)
c***************************************
c
c     IQRAN0  - define seed for new IQRANF sequence
c
      IMPLICIT  DOUBLE PRECISION (A-H,K,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,K,O-Z)
c
      COMMON /IQRAND/ k0, k, k9
      CALL TRACE ('IQRAN0  ')
c
      IF( newk.LE.0 ) THEN
          CALL WHERE(1)
      ENDIF
      k = newk
c
      CALL TRACK ('IQRAN0  ')
      RETURN
      END
c
c***************************************
      SUBROUTINE IQRANF( M, Mmin,Mmax, n)
c***********************************************************************
c                                                                      *
c     IQRANF  - computes a vector of psuedo-random indices             *
c               in the domain (Mmin,Mmax)                              *
c                                                                      *
c     M     - result array ,  psuedo-random positive integers          *
c     Mmin  - input integer,  lower bound for random integers          *
c     Mmax  - input integer,  upper bound for random integers          *
c     n     - input integer,  number of results in M.                  *
c                                                                      *
c       M(i)= Mmin + INT( (Mmax-Mmin) * RANF(0))                       *
c                                                                      *
c        CALL IQRAN0( 256 )                                            *
c        CALL IQRANF( IX, 1,1001, 30)      should produce in IX:       *
c           3  674  435  415  389   54   44  790  900  282             *
c         177  971  728  851  687  604  815  971  155  112             *
c         877  814  779  192  619  894  544  404  496  505  ...        *
c                                                                      *
c     S.K.Park, K.W.Miller, Random Number Generators: Good Ones        *
c     Are Hard To Find, Commun ACM, 31(10), 1192-1201 (1988).          *
c***********************************************************************
c
      IMPLICIT  DOUBLE PRECISION (A-H,K,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,K,O-Z)
cout  DOUBLE  PRECISION  dq, dp, per, dk, spin, span                    REDUNDNT
c
      dimension  M(n)
      COMMON /IQRAND/ k0, k, k9
c     save k
      CALL TRACE ('IQRANF  ')
            IF( n.LE.0 )  GO TO 73
         inset= Mmin
          span= Mmax - Mmin
c         spin= 16807.00d0
c          per= 2147483647.00d0
          spin= 16807
           per= 2147483647
         realn= n
         scale= 1.0000100d0
             q= scale*(span/realn)
c
            dk= k
      DO  1  i= 1,n
            dp= dk*spin
c           dk=    DMOD( dp, per)
            dk= dp -INT( dp/per)*per
            dq= dk*span
          M(i)= inset + ( dq/ per)
            IF( M(i).LT.Mmin .OR. M(i).GT.Mmax )  M(i)= inset + i*q
    1 continue
             k= dk
c
c
ciC     double precision  k, ip, iq, id
ci         inset= Mmin
ci         ispan= Mmax - Mmin
ci         ispin= 16807
ci            id= 2147483647
ci             q= (REAL(ispan)/REAL(n))*1.00001
ciC
ci      DO  2  i= 1,n
ci            ip= k*ispin
ci             k=      MOD( ip, id)
ci            iq= k*ispan
ci          M(i)= inset + ( iq/ id)
ci            IF( M(i).LT.Mmin .OR. M(i).GT.Mmax )  M(i)= inset + i*q
ci    2 continue
c
            CALL TRAP( M, ' IQRANF ' , 1, Mmax, n)
c
   73 CONTINUE
      CALL TRACK ('IQRANF  ')
      RETURN
c     DATA  k /256/
c                        IQRANF TEST PROGRAM:
c      parameter( nrange= 10000, nmaps= 1001 )
c      DIMENSION  IX(nrange), IY(nmaps), IZ(nmaps), IR(nmaps)
c      COMMON /IQRAND/ k0, k, k9
cc
c        CALL  LINK( 'UNIT6=( output,create,text)//')
c         iou= 8
c      DO 7 j= 1,256,255
c      CALL IQRAN0( j )
c      CALL IQRANF( IX, 1, nmaps, nrange)
c      DO 1 i= 1,nmaps
c       IY(i)= 0
c    1  IZ(i)= 0
cc                     census for each index generated in (1:nmaps)
c      DO 2 i= 1,nrange
c    2 IY( IX(i))= IY( IX(i)) + 1
cc                     distribution of census tallies about nrange/nmaps
c      DO 3 i= 1,nmaps
c    3 IZ( IY(i))= IZ( IY(i)) + 1
c       IR(1)= IZ(1)
cc                     integral of distribution
c      DO 4 i= 1,nmaps
c    4  IR(i)= IR(i-1) + IZ(i)
c      WRITE( iou,112)   j, IR(nmaps), k
c      WRITE( iou,113) ( IX(i), i= 1,20 )
c      WRITE( iou,113) ( IY(i), i= 1,20 )
c      WRITE( iou,113) ( IZ(i), i= 1,20 )
c      WRITE( iou,113) ( IR(i), i= 1,20 )
c  112 FORMAT(/,1X,4I20)
c  113 FORMAT(20I4)
c    7 continue
c      STOP
c
c                   1                1000          1043618065
c  1 132 756 459 533 219  48 679 680 935 384 520 831  35  54 530 672   8 384  67
c 17  12   7  10  10  10  10  12   9   9   4  15  10   7   7   9   9   9  10  11
c  0   1   8  19  40  60  86 109 133 128 107 104  70  52  39  26   7   7   2   2
c  0   1   9  28  68 128 214 323 456 584 691 795 865 917 956 982 989 996 9981000
c
c                 256                1000           878252412
c  3 674 435 415 389  54  44 790 900 282 177 971 728 851 687 604 815 971 155 112
c 11  17  19   6  11  11   7   9  12   7  13   7   9  11  14   9   9  12   9   9
c  1   2  10  16  30  71  93 109 131 119 118 105  69  47  28  15  15   9   5   3
c  1   3  13  29  59 130 223 332 463 582 700 805 874 921 949 964 979 988 993 996
      END
c
c***********************************************
      SUBROUTINE KERNEL( TK)
c***********************************************************************
c                                                                      *
c            KERNEL     executes 24 samples of Fortran computation     *
c               TK(1) - total cpu time to execute only the 24 kernels. *
c               TK(2) - total Flops executed by the 24 Kernels         *
c***********************************************************************
c                                                                      *
c     L. L. N. L.   F O R T R A N   K E R N E L S:   M F L O P S       *
c                                                                      *
c   These kernels measure  Fortran  numerical  computation rates for a *
c   spectrum of  CPU-limited  computational  structures.  Mathematical *
c   through-put is measured  in  units  of  millions of floating-point *
c   operations executed per Second, called Mega-Flops/Sec.             *
c                                                                      *
c   This program  measures  a realistic  CPU performance range for the *
c   Fortran programming system  on  a  given day.  The CPU performance *
c   rates depend  strongly  on  the maturity of the Fortran compiler's *
c   ability to translate Fortran code into efficient machine code.     *
c   [ The CPU hardware  capability  apart  from  compiler maturity (or *
c   availability), could be measured (or simulated) by programming the *
c   kernels in assembly  or machine code directly.  These measurements *
c   can also  serve  as a framework for tracking the maturation of the *
c   Fortran compiler during system development.]                       *
c                                                                      *
c     Fonzi's Law: There is not now and there never will be a language *
c                  in which it is the least bit difficult to write     *
c                  bad programs.                                       *
c                                                    F.H.MCMAHON  1972 *
c***********************************************************************
c
c     l1 :=  param-dimension governs the size of most 1-d arrays
c     l2 :=  param-dimension governs the size of most 2-d arrays
c
c     Loop :=  multiple pass control to execute kernel long enough to time.
c     n  :=  DO loop control for each kernel.  Controls are set in subr. SIZES
c
c     ******************************************************************
c
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
c
c/      PARAMETER( l1= 1001, l2=  101, l1d= 2*1001 )
c/      PARAMETER( l13=  64, l13h= l13/2, l213= l13+l13h, l813= 8*l13 )
c/      PARAMETER( l14=2048, l16=  75, l416= 4*l16 , l21= 25 )
c/      PARAMETER( kn= 47, kn2= 95, np= 3, ls= 3*47, krs= 24)
c
c
c/      PARAMETER( nk= 47, nl= 3, nr= 8 )
      INTEGER TEST, AND
c
      COMMON /ALPHA/ mk,ik,im,ml,il,Mruns,Nruns,jr,iovec,NPFS(8,3,47)
      COMMON /BETA / tic, TIMES(8,3,47), SEE(5,3,8,3),
     1              TERRS(8,3,47), CSUMS(8,3,47),
     2              FOPN(8,3,47), DOS(8,3,47)
c
      COMMON /SPACES/ ion,j5,k2,k3,MULTI,laps,Loop,m,kr,LP,n13h,ibuf,nx,
     1 L,npass,nfail,n,n1,n2,n13,n213,n813,n14,n16,n416,n21,nt1,nt2,
     2 last,idebug,mpy,Loops2,mucho,mpylim, intbuf(16)
c
      COMMON /SPACER/ A11,A12,A13,A21,A22,A23,A31,A32,A33,
     1                AR,BR,C0,CR,DI,DK,
     2  DM22,DM23,DM24,DM25,DM26,DM27,DM28,DN,E3,E6,EXPMAX,FLX,
     3  Q,QA,R,RI,S,SCALE,SIG,STB5,T,XNC,XNEI,XNM
c
cPFM  COMMON /KAPPA/ iflag1, ikern, statis(100,20), istats(100,20)
c
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), ticks,
     1                FR(9), TERR1(47), SUMW(7), START,
     2              SKALE(47), BIAS(47), WS(95), TOTAL(47), FLOPN(47),
     3                IQ(7), NPF, NPFS1(47)
c
      COMMON /SPACEI/ WTP(3), MUL(3), ISPAN(47,3), IPASS(47,3)
c
c/      INTEGER    E,F,ZONE
c/      COMMON /ISPACE/ E(l213), F(l213),
c/     1  IX(l1), IR(l1), ZONE(l416)
c/C
c/      COMMON /SPACE1/ U(l1), V(l1), W(l1),
c/     1  X(l1), Y(l1), Z(l1), G(l1),
c/     2  DU1(l2), DU2(l2), DU3(l2), GRD(l1), DEX(l1),
c/     3  XI(l1), EX(l1), EX1(l1), DEX1(l1),
c/     4  VX(l14), XX(l14), RX(l14), RH(l14),
c/     5  VSP(l2), VSTP(l2), VXNE(l2), VXND(l2),
c/     6  VE3(l2), VLR(l2), VLIN(l2), B5(l2),
c/     7  PLAN(l416), D(l416), SA(l2), SB(l2)
c/C
c/      COMMON /SPACE2/ P(4,l813), PX(l21,l2), CX(l21,l2),
c/     1  VY(l2,l21), VH(l2,7), VF(l2,7), VG(l2,7), VS(l2,7),
c/     2  ZA(l2,7)  , ZP(l2,7), ZQ(l2,7), ZR(l2,7), ZM(l2,7),
c/     3  ZB(l2,7)  , ZU(l2,7), ZV(l2,7), ZZ(l2,7),
c/     4  B(l13,l13), C(l13,l13), H(l13,l13),
c/     5  U1(5,l2,2),  U2(5,l2,2),  U3(5,l2,2)
c
c     ******************************************************************
c
c
c/      PARAMETER( l1=   1001, l2=   101, l1d= 2*1001 )
c/      PARAMETER( l13= 64, l13h= 64/2, l213= 64+32, l813= 8*64 )
c/      PARAMETER( l14= 2048, l16= 75, l416= 4*75 , l21= 25)
c
c
care
c
      INTEGER    E,F,ZONE
      COMMON /ISPACE/ E(96), F(96),
     1  IX(1001), IR(1001), ZONE(300)
c
      COMMON /SPACE1/ U(1001), V(1001), W(1001),
     1  X(1001), Y(1001), Z(1001), G(1001),
     2  DU1(101), DU2(101), DU3(101), GRD(1001), DEX(1001),
     3  XI(1001), EX(1001), EX1(1001), DEX1(1001),
     4  VX(1001), XX(1001), RX(1001), RH(2048),
     5  VSP(101), VSTP(101), VXNE(101), VXND(101),
     6  VE3(101), VLR(101), VLIN(101), B5(101),
     7  PLAN(300), D(300), SA(101), SB(101)
c
      COMMON /SPACE2/ P(4,512), PX(25,101), CX(25,101),
     1  VY(101,25), VH(101,7), VF(101,7), VG(101,7), VS(101,7),
     2  ZA(101,7)  , ZP(101,7), ZQ(101,7), ZR(101,7), ZM(101,7),
     3  ZB(101,7)  , ZU(101,7), ZV(101,7), ZZ(101,7),
     4  B(64,64), C(64,64), H(64,64),
     5  U1(5,101,2),  U2(5,101,2),  U3(5,101,2)
c
c     ******************************************************************
c
      DIMENSION     ZX(1023), XZ(1500), TK(6)
      EQUIVALENCE ( ZX(1), Z(1)), ( XZ(1), X(1))
c
c
c//      DIMENSION       E(96), F(96), U(1001), V(1001), W(1001),
c//     1  X(1001), Y(1001), Z(1001), G(1001),
c//     2  DU1(101), DU2(101), DU3(101), GRD(1001), DEX(1001),
c//     3  IX(1001), XI(1001), EX(1001), EX1(1001), DEX1(1001),
c//     4  VX(1001), XX(1001), IR(1001), RX(1001), RH(2048),
c//     5  VSP(101), VSTP(101), VXNE(101), VXND(101),
c//     6  VE3(101), VLR(101), VLIN(101), B5(101),
c//     7  PLAN(300), ZONE(300), D(300), SA(101), SB(101)
c//C
c//      DIMENSION       P(4,512), PX(25,101), CX(25,101),
c//     1  VY(101,25), VH(101,7), VF(101,7), VG(101,7), VS(101,7),
c//     2  ZA(101,7)  , ZP(101,7), ZQ(101,7), ZR(101,7), ZM(101,7),
c//     3  ZB(101,7)  , ZU(101,7), ZV(101,7), ZZ(101,7),
c//     4  B(64,64), C(64,64), H(64,64),
c//     5  U1(5,101,2),  U2(5,101,2),  U3(5,101,2)
c//C
c//C     ******************************************************************
c//C
c//      COMMON /POINT/ ME,MF,MU,MV,MW,MX,MY,MZ,MG,MDU1,MDU2,MDU3,MGRD,
c//     1  MDEX,MIX,MXI,MEX,MEX1,MDEX1,MVX,MXX,MIR,MRX,MRH,MVSP,MVSTP,
c//     2  MVXNE,MVXND,MVE3,MVLR,MVLIN,MB5,MPLAN,MZONE,MD,MSA,MSB,
c//     3  MP,MPX,MCX,MVY,MVH,MVF,MVG,MVS,MZA,MZP,MZQ,MZR,MZM,MZB,MZU,
c//     4  MZV,MZZ,MB,MC,MH,MU1,MU2,MU3
c//C
c//      POINTER  (ME,E), (MF,F), (MU,U), (MV,V), (MW,W),
c//     1         (MX,X), (MY,Y), (MZ,Z), (MG,G),
c//     2         (MDU1,DU1),(MDU2,DU2),(MDU3,DU3),(MGRD,GRD),(MDEX,DEX),
c//     3         (MIX,IX), (MXI,XI), (MEX,EX), (MEX1,EX1), (MDEX1,DEX1),
c//     4         (MVX,VX), (MXX,XX), (MIR,IR), (MRX,RX), (MRH,RH),
c//     5         (MVSP,VSP), (MVSTP,VSTP), (MVXNE,VXNE), (MVXND,VXND),
c//     6         (MVE3,VE3), (MVLR,VLR), (MVLIN,VLIN), (MB5,B5),
c//     7         (MPLAN,PLAN), (MZONE,ZONE), (MD,D), (MSA,SA), (MSB,SB)
c//C
c//      POINTER  (MP,P), (MPX,PX), (MCX,CX),
c//     1         (MVY,VY), (MVH,VH), (MVF,VF), (MVG,VG), (MVS,VS),
c//     2         (MZA,ZA), (MZP,ZP), (MZQ,ZQ), (MZR,ZR), (MZM,ZM),
c//     3         (MZB,ZB), (MZU,ZU), (MZV,ZV), (MZZ,ZZ),
c//     4         (MB,B), (MC,C), (MH,H),
c//     5         (MU1,U1), (MU2,U2), (MU3,U3)
c..      COMMON DUMMY(2000)
c..      LOC(X)  =.LOC.X
c..      IQ8QDSP = 64*LOC(DUMMY)
c
c     ******************************************************************
c
c     STANDARD PRODUCT COMPILER DIRECTIVES MAY BE USED FOR OPTIMIZATION
c
cDIR$ VECTOR
cLLL. OPTIMIZE LEVEL i
cLLL. OPTION INTEGER (7)
cLLL. OPTION ASSERT (NO HAZARD)
cLLL. OPTION NODYNEQV
c
c     ******************************************************************
c       BINARY MACHINES MAY USE THE  AND(P,Q)  FUNCTION IF AVAILABLE
c       IN PLACE OF THE FOLLOWING CONGRUENCE FUNCTION (SEE KERNEL 13, 14)
c                                 IFF:   j= 2**N
c     IAND(j,k) = AND(j,k)
cLLL. IAND(j,k) = j.INT.k
c     MOD2N(i,j)= MOD(i,j)
      MOD2N(i,j)= IAND(i,j-1)
c                             i  is Congruent to  MOD2N(i,j)   mod(j)
c     ******************************************************************
c
c
c
c
c
      CALL TRACE ('KERNEL  ')
c
      CALL SPACE
c
cPFM       call  OUTPFM( 0, ion)
      mpy   = 1
      Loops2= 1
      mpylim= Loops2
      L     = 1
      Loop  = 1
      LP    = Loop
      it0   = TEST(0)
cPFM  iflag1= 13579
c
c*******************************************************************************
c***  KERNEL 1      HYDRO FRAGMENT
c*******************************************************************************
c
cdir$ ivdep
 1001    DO 1 k = 1,n
    1       X(k)= Q + Y(k) * (R * ZX(k+10) + T * ZX(k+11))
c
c...................
      IF( TEST(1) .GT. 0) GO TO 1001
c                   we must execute    DO k= 1,n  repeatedly for accurate timing
c
c*******************************************************************************
c***  KERNEL 2      ICCG EXCERPT (INCOMPLETE CHOLESKY - CONJUGATE GRADIENT)
c*******************************************************************************
c
c
 1002     II= n
       IPNTP= 0
  222   IPNT= IPNTP
       IPNTP= IPNTP+II
          II= II/2
           i= IPNTP+1
cdir$ ivdep
c:ibm_dir:ignore recrdeps (x)
c
      DO 2 k= IPNT+2,IPNTP,2
           i= i+1
    2   X(i)= X(k) - V(k) * X(k-1) - V(k+1) * X(k+1)
          IF( II.GT.1) GO TO 222
c
c...................
      IF( TEST(2) .GT. 0) GO TO 1002
c
c*******************************************************************************
c***  KERNEL 3      INNER PRODUCT
c*******************************************************************************
c
c
 1003      Q= 0.000d0
      DO 3 k= 1,n
    3      Q= Q + Z(k) * X(k)
c
c...................
      IF( TEST(3) .GT. 0) GO TO 1003
c
c*******************************************************************************
c***  KERNEL 4      BANDED LINEAR EQUATIONS
c*******************************************************************************
c
              m= (1001-7)/2
             fw= 1.000d-25
c
 1004 DO 404  k= 7,1001,m
             lw= k-6
           temp= XZ(k-1)
cdir$ ivdep
      DO   4  j= 5,n,5
         temp  = temp   - XZ(lw) * Y(j)
    4        lw= lw+1
        XZ(k-1)= Y(5) * temp
 404  CONTINUE
c
c...................
      IF( TEST(4) .GT. 0) GO TO 1004
c
c*******************************************************************************
c***  KERNEL 5      TRI-DIAGONAL ELIMINATION, BELOW DIAGONAL (NO VECTORS)
c*******************************************************************************
c
c
cdir$ novector
 1005 DO 5 i = 2,n
    5    X(i)= Z(i) * (Y(i) - X(i-1))
cdir$ vector
c
c...................
      IF( TEST(5) .GT. 0) GO TO 1005
c
c*******************************************************************************
c***  KERNEL 6      GENERAL LINEAR RECURRENCE EQUATIONS
c*******************************************************************************
c
c
 1006 DO  6  i= 2,n
          W(i)= 0.0100d0
cdir$ novector
      DO  6  k= 1,i-1
          W(i)= W(i)  + B(i,k) * W(i-k)
    6 CONTINUE
cdir$ vector
c
c...................
      IF( TEST(6) .GT. 0) GO TO 1006
c
c*******************************************************************************
c***  KERNEL 7      EQUATION OF STATE FRAGMENT
c*******************************************************************************
c
c
cdir$ ivdep
 1007 DO 7 k= 1,n
        X(k)=     U(k  ) + R*( Z(k  ) + R*Y(k  )) +
     1        T*( U(k+3) + R*( U(k+2) + R*U(k+1)) +
     2        T*( U(k+6) + Q*( U(k+5) + Q*U(k+4))))
    7 CONTINUE
c
c...................
      IF( TEST(7) .GT. 0) GO TO 1007
c
c
c*******************************************************************************
c***  KERNEL 8      A.D.I. INTEGRATION
c*******************************************************************************
c
c
 1008          nl1 = 1
               nl2 = 2
                fw= 2.000d0
      DO  8     kx = 2,3
cdir$ ivdep
      DO  8     ky = 2,n
            DU1(ky)=U1(kx,ky+1,nl1)  -  U1(kx,ky-1,nl1)
            DU2(ky)=U2(kx,ky+1,nl1)  -  U2(kx,ky-1,nl1)
            DU3(ky)=U3(kx,ky+1,nl1)  -  U3(kx,ky-1,nl1)
      U1(kx,ky,nl2)=U1(kx,ky,nl1) +A11*DU1(ky) +A12*DU2(ky) +A13*DU3(ky)
     1       + SIG*(U1(kx+1,ky,nl1) -fw*U1(kx,ky,nl1) +U1(kx-1,ky,nl1))
      U2(kx,ky,nl2)=U2(kx,ky,nl1) +A21*DU1(ky) +A22*DU2(ky) +A23*DU3(ky)
     1       + SIG*(U2(kx+1,ky,nl1) -fw*U2(kx,ky,nl1) +U2(kx-1,ky,nl1))
      U3(kx,ky,nl2)=U3(kx,ky,nl1) +A31*DU1(ky) +A32*DU2(ky) +A33*DU3(ky)
     1       + SIG*(U3(kx+1,ky,nl1) -fw*U3(kx,ky,nl1) +U3(kx-1,ky,nl1))
    8 CONTINUE
c
c...................
      IF( TEST(8) .GT. 0) GO TO 1008
c
c*******************************************************************************
c***  KERNEL 9      INTEGRATE PREDICTORS
c*******************************************************************************
c
c
 1009 DO 9  k = 1,n
      PX( 1,k)= DM28*PX(13,k) + DM27*PX(12,k) + DM26*PX(11,k) +
     1          DM25*PX(10,k) + DM24*PX( 9,k) + DM23*PX( 8,k) +
     2          DM22*PX( 7,k) +  C0*(PX( 5,k) +      PX( 6,k))+ PX( 3,k)
    9 CONTINUE
c
c...................
      IF( TEST(9) .GT. 0) GO TO 1009
c
c*******************************************************************************
c***  KERNEL 10     DIFFERENCE PREDICTORS
c*******************************************************************************
c
c
 1010 DO 10  k= 1,n
      AR      =      CX(5,k)
      BR      = AR - PX(5,k)
      PX(5,k) = AR
      CR      = BR - PX(6,k)
      PX(6,k) = BR
      AR      = CR - PX(7,k)
      PX(7,k) = CR
      BR      = AR - PX(8,k)
      PX(8,k) = AR
      CR      = BR - PX(9,k)
      PX(9,k) = BR
      AR      = CR - PX(10,k)
      PX(10,k)= CR
      BR      = AR - PX(11,k)
      PX(11,k)= AR
      CR      = BR - PX(12,k)
      PX(12,k)= BR
      PX(14,k)= CR - PX(13,k)
      PX(13,k)= CR
   10 CONTINUE
c
c...................
      IF( TEST(10) .GT. 0) GO TO 1010
c
c*******************************************************************************
c***  KERNEL 11     FIRST SUM.   PARTIAL SUMS.              (NO VECTORS)
c*******************************************************************************
c
c
 1011     X(1)= Y(1)
cdir$ novector
      DO 11 k = 2,n
   11     X(k)= X(k-1) + Y(k)
cdir$ vector
c
c...................
      IF( TEST(11) .GT. 0) GO TO 1011
c
c*******************************************************************************
c***  KERNEL 12     FIRST DIFF.
c*******************************************************************************
c
c
cdir$ ivdep
 1012 DO 12 k = 1,n
   12     X(k)= Y(k+1) - Y(k)
c
c...................
      IF( TEST(12) .GT. 0) GO TO 1012
c
c*******************************************************************************
c***  KERNEL 13      2-D PIC   Particle In Cell
c*******************************************************************************
c
                fw= 1.000d0
c
 1013 DO  13     k= 1,n
                i1= P(1,k)
                j1= P(2,k)
                i1=       1 + MOD2N(i1,64)
                j1=       1 + MOD2N(j1,64)
            P(3,k)= P(3,k)  + B(i1,j1)
            P(4,k)= P(4,k)  + C(i1,j1)
            P(1,k)= P(1,k)  + P(3,k)
            P(2,k)= P(2,k)  + P(4,k)
                i2= P(1,k)
                j2= P(2,k)
                i2=            MOD2N(i2,64)
                j2=            MOD2N(j2,64)
            P(1,k)= P(1,k)  + Y(i2+32)
            P(2,k)= P(2,k)  + Z(j2+32)
                i2= i2      + E(i2+32)
                j2= j2      + F(j2+32)
          H(i2,j2)= H(i2,j2) + fw
   13 CONTINUE
c
c...................
      IF( TEST(13) .GT. 0) GO TO 1013
c
c*******************************************************************************
c***  KERNEL 14      1-D PIC   Particle In Cell
c*******************************************************************************
c
c
               fw= 1.000d0
c
 1014 DO   141  k= 1,n
            VX(k)= 0.0d0
            XX(k)= 0.0d0
            IX(k)= INT(  GRD(k))
            XI(k)= REAL( IX(k))
           EX1(k)= EX   ( IX(k))
          DEX1(k)= DEX  ( IX(k))
 141  CONTINUE
c
      DO   142  k= 1,n
            VX(k)= VX(k) + EX1(k) + (XX(k) - XI(k))*DEX1(k)
            XX(k)= XX(k) + VX(k)  + FLX
            IR(k)= XX(k)
            RX(k)= XX(k) - IR(k)
            IR(k)= MOD2N(  IR(k),2048) + 1
            XX(k)= RX(k) + IR(k)
 142  CONTINUE
c
      DO  14    k= 1,n
      RH(IR(k)  )= RH(IR(k)  ) + fw - RX(k)
      RH(IR(k)+1)= RH(IR(k)+1) + RX(k)
  14  CONTINUE
c
c...................
      IF( TEST(14) .GT. 0) GO TO 1014
c
c
c
c
c
c
c
c
c
c
c
c
c
c
c
c
c
c
c
c*******************************************************************************
c***  KERNEL 15     CASUAL FORTRAN.  DEVELOPMENT VERSION.
c*******************************************************************************
c
c
c       CASUAL ORDERING OF SCALAR OPERATIONS IS TYPICAL PRACTICE.
c       THIS EXAMPLE DEMONSTRATES THE NON-TRIVIAL TRANSFORMATION
c       REQUIRED TO MAP INTO AN EFFICIENT MACHINE IMPLEMENTATION.
c
c
 1015          NG= 7
               NZ= n
               AR= 0.05300d0
               BR= 0.07300d0
        DO 45  j = 2,NG
        DO 45  k = 2,NZ
               IF( j-NG) 31,30,30
   30     VY(k,j)= 0.0d0
                   GO TO 45
   31          IF( VH(k,j+1) -VH(k,j)) 33,33,32
   32           T= AR
                   GO TO 34
   33           T= BR
   34          IF( VF(k,j) -VF(k-1,j)) 35,36,36
   35           R= MAX( VH(k-1,j), VH(k-1,j+1))
                S= VF(k-1,j)
                   GO TO 37
   36           R= MAX( VH(k,j),   VH(k,j+1))
                S= VF(k,j)
   37     VY(k,j)= SQRT( VG(k,j)**2 +R*R)*T/S
               IF( k-NZ) 40,39,39
   39     VS(k,j)= 0.0d0
                   GO TO 45
   40          IF( VF(k,j) -VF(k,j-1)) 41,42,42
   41           R= MAX( VG(k,j-1), VG(k+1,j-1))
                S= VF(k,j-1)
                T= BR
                   GO TO 43
   42           R= MAX( VG(k,j),   VG(k+1,j))
                S= VF(k,j)
                T= AR
   43     VS(k,j)= SQRT( VH(k,j)**2 +R*R)*T/S
   45    CONTINUE
c
c...................
      IF( TEST(15) .GT. 0) GO TO 1015
c
c
c
c
c
c
c
c
c
c
c
c
c
c
c*******************************************************************************
c***  KERNEL 16     MONTE CARLO SEARCH LOOP
c*******************************************************************************
c
            II= n/3
            LB= II+II
            k2= 0
            k3= 0
c
c
 1016        m= 1
            i1= m
  410       j2= (n+n)*(m-1)+1
      DO 470 k= 1,n
            k2= k2+1
            j4= j2+k+k
            j5= ZONE(j4)
            IF( j5-n      ) 420,475,450
  415       IF( j5-n+II   ) 430,425,425
  420       IF( j5-n+LB   ) 435,415,415
  425       IF( PLAN(j5)-R) 445,480,440
  430       IF( PLAN(j5)-S) 445,480,440
  435       IF( PLAN(j5)-T) 445,480,440
  440       IF( ZONE(j4-1)) 455,485,470
  445       IF( ZONE(j4-1)) 470,485,455
  450       k3= k3+1
            IF( D(j5)-(D(j5-1)*(T-D(j5-2))**2+(S-D(j5-3))**2
     1                        +(R-D(j5-4))**2)) 445,480,440
  455        m= m+1
            IF( m-ZONE(1) ) 465,465,460
  460        m= 1
  465       IF( i1-m) 410,480,410
  470 CONTINUE
  475 CONTINUE
  480 CONTINUE
  485 CONTINUE
c
c...................
      IF( TEST(16) .GT. 0) GO TO 1016
c
c*******************************************************************************
c***  KERNEL 17     IMPLICIT, CONDITIONAL COMPUTATION       (NO VECTORS)
c*******************************************************************************
c
c         RECURSIVE-DOUBLING VECTOR TECHNIQUES CAN NOT BE USED
c         BECAUSE CONDITIONAL OPERATIONS APPLY TO EACH ELEMENT.
c
                 dw= 5.0000d0/3.0000d0
                 fw= 1.0000d0/3.0000d0
                 tw= 1.0300d0/3.0700d0
cdir$ novector
c
 1017             k= n
                  j= 1
                ink= -1
              SCALE= dw
                XNM= fw
                 E6= tw
                     GO TO 61
c                                            STEP MODEL
  60             E6= XNM*VSP(k)+VSTP(k)
            VXNE(k)= E6
                XNM= E6
             VE3(k)= E6
                  k= k+ink
                 IF( k.EQ.j) GO TO  62
  61             E3= XNM*VLR(k) +VLIN(k)
               XNEI= VXNE(k)
            VXND(k)= E6
                XNC= SCALE*E3
c                                            SELECT MODEL
                 IF( XNM .GT.XNC) GO TO  60
                 IF( XNEI.GT.XNC) GO TO  60
c                                            LINEAR MODEL
             VE3(k)= E3
                 E6= E3+E3-XNM
            VXNE(k)= E3+E3-XNEI
                XNM= E6
                  k= k+ink
                 IF( k.NE.j) GO TO 61
   62 CONTINUE
cdir$ vector
c
c...................
      IF( TEST(17) .GT. 0) GO TO 1017
c
c*******************************************************************************
c***  KERNEL 18     2-D EXPLICIT HYDRODYNAMICS FRAGMENT
c*******************************************************************************
c
c
 1018           T= 0.003700d0
                S= 0.004100d0
               KN= 6
               JN= n
         DO 70  k= 2,KN
         DO 70  j= 2,JN
          ZA(j,k)= (ZP(j-1,k+1)+ZQ(j-1,k+1)-ZP(j-1,k)-ZQ(j-1,k))
     1            *(ZR(j,k)+ZR(j-1,k))/(ZM(j-1,k)+ZM(j-1,k+1))
          ZB(j,k)= (ZP(j-1,k)+ZQ(j-1,k)-ZP(j,k)-ZQ(j,k))
     1            *(ZR(j,k)+ZR(j,k-1))/(ZM(j,k)+ZM(j-1,k))
   70    CONTINUE
c
         DO 72  k= 2,KN
         DO 72  j= 2,JN
          ZU(j,k)= ZU(j,k)+S*(ZA(j,k)*(ZZ(j,k)-ZZ(j+1,k))
     1                    -ZA(j-1,k) *(ZZ(j,k)-ZZ(j-1,k))
     2                    -ZB(j,k)   *(ZZ(j,k)-ZZ(j,k-1))
     3                    +ZB(j,k+1) *(ZZ(j,k)-ZZ(j,k+1)))
          ZV(j,k)= ZV(j,k)+S*(ZA(j,k)*(ZR(j,k)-ZR(j+1,k))
     1                    -ZA(j-1,k) *(ZR(j,k)-ZR(j-1,k))
     2                    -ZB(j,k)   *(ZR(j,k)-ZR(j,k-1))
     3                    +ZB(j,k+1) *(ZR(j,k)-ZR(j,k+1)))
   72    CONTINUE
c
         DO 75  k= 2,KN
         DO 75  j= 2,JN
          ZR(j,k)= ZR(j,k)+T*ZU(j,k)
          ZZ(j,k)= ZZ(j,k)+T*ZV(j,k)
   75    CONTINUE
c
c...................
      IF( TEST(18) .GT. 0) GO TO 1018
c
c*******************************************************************************
c***  KERNEL 19      GENERAL LINEAR RECURRENCE EQUATIONS    (NO VECTORS)
c*******************************************************************************
c
 1019            KB5I= 0
c
c     IF( JR.LE.1 )  THEN
cdir$ novector
             DO 191 k= 1,n
           B5(k+KB5I)= SA(k) +STB5*SB(k)
                 STB5= B5(k+KB5I) -STB5
  191        CONTINUE
c     ELSE
c
             DO 193 i= 1,n
                    k= n-i+1
           B5(k+KB5I)= SA(k) +STB5*SB(k)
                 STB5= B5(k+KB5I) -STB5
  193        CONTINUE
c     ENDIF
cdir$ vector
c
c...................
      IF( TEST(19) .GT. 0) GO TO 1019
c
c*******************************************************************************
c***  KERNEL 20     DISCRETE ORDINATES TRANSPORT: RECURRENCE (NO VECTORS)
c*******************************************************************************
c
           dw= 0.200d0
cdir$ novector
c
 1020 DO 20 k= 1,n
           DI= Y(k)-G(k)/( XX(k)+DK)
           DN= dw
           IF( DI.NE.0.0) DN= MAX( S,MIN( Z(k)/DI, T))
         X(k)= ((W(k)+V(k)*DN)* XX(k)+U(k))/(VX(k)+V(k)*DN)
      XX(k+1)= (X(k)- XX(k))*DN+ XX(k)
   20 CONTINUE
cdir$ vector
c
c...................
      IF( TEST(20) .GT. 0) GO TO 1020
c
c*******************************************************************************
c***  KERNEL 21     MATRIX*MATRIX PRODUCT
c*******************************************************************************
c
c
 1021 DO 21 k= 1,25
      DO 21 i= 1,25
      DO 21 j= 1,n
      PX(i,j)= PX(i,j) +VY(i,k) * CX(k,j)
   21 CONTINUE
c
c...................
      IF( TEST(21) .GT. 0) GO TO 1021
c
c
c
c
c
c
c
c*******************************************************************************
c***  KERNEL 22     PLANCKIAN DISTRIBUTION
c*******************************************************************************
c
c
c      EXPMAX= 234.500d0
       EXPMAX= 20.0000d0
           fw= 1.00000d0
         U(n)= 0.99000d0*EXPMAX*V(n)
c
 1022 DO 22 k= 1,n
care       IF( U(k) .LT. EXPMAX*V(k))  THEN
                                            Y(k)= U(k)/V(k)
care                                   ELSE
care                                        Y(k)= EXPMAX
care    ENDIF
         W(k)= X(k)/( EXP( Y(k)) -fw)
   22 CONTINUE
c...................
      IF( TEST(22) .GT. 0) GO TO 1022
c
c*******************************************************************************
c***  KERNEL 23     2-D IMPLICIT HYDRODYNAMICS FRAGMENT
c*******************************************************************************
c
            fw= 0.17500d0
c
 1023 DO 23  j= 2,6
      DO 23  k= 2,n
            QA= ZA(k,j+1)*ZR(k,j) +ZA(k,j-1)*ZB(k,j) +
     1          ZA(k+1,j)*ZU(k,j) +ZA(k-1,j)*ZV(k,j) +ZZ(k,j)
   23  ZA(k,j)= ZA(k,j) +fw*(QA -ZA(k,j))
c
c...................
      IF( TEST(23) .GT. 0) GO TO 1023
c
c*******************************************************************************
c***  KERNEL 24     FIND LOCATION OF FIRST MINIMUM IN ARRAY
c*******************************************************************************
c
c      X( n/2)= -1.000d+50
       X( n/2)= -1.000d+10
c
 1024        m= 1
      DO 24  k= 2,n
            IF( X(k).LT.X(m))  m= k
   24 CONTINUE
c
c            m= imin1( n,x,1)        35 nanosec./element STACKLIBE/CRAY
c...................
      IF( TEST(24) .NE. 0) GO TO 1024
c
c*******************************************************************************
c
cPFM  iflag1= 0
           sum= 0.00d0
           som= 0.00d0
      DO 999 k= 1,mk
           sum= sum + TIME (k)
      TIMES(jr,il,k)= TIME (k)
      TERRS(jr,il,k)= TERR1(k)
      NPFS (jr,il,k)= NPFS1(k)
      CSUMS(jr,il,k)= CSUM (k)
      DOS  (jr,il,k)= TOTAL(k)
      FOPN (jr,il,k)= FLOPN(k)
           som= som + FLOPN(k) * TOTAL(k)
  999 continue
c
         TK(1)= TK(1) + sum
         TK(2)= TK(2) + som
c                                  Dumpout Checksums
c     WRITE ( 7,706) jr, il
c 706 FORMAT(1X,2I3)
c     WRITE ( 7,707) ( CSUM(k), k= 1,mk)
c 707 FORMAT(5X,'&',1PE21.15,',',1PE21.15,',',1PE21.15,',')
c
      CALL TRACK ('KERNEL  ')
      RETURN
      END
c***********************************************
      SUBROUTINE  PAGE( iou)
c***********************************************
      CALL TRACE ('PAGE    ')
      WRITE(iou,1)
    1 FORMAT('1')
c   1 FORMAT(1H)
      CALL TRACK ('PAGE    ')
      RETURN
      END
c
c********************************************
      FUNCTION  RELERR( U,V)
c********************************************
c
c       RELERR - RELATIVE ERROR BETWEEN  U,V  (0.,1.)
c            U - INPUT
c            V - INPUT
c********************************************
c
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
cout  DOUBLE  PRECISION  x, y                                           REDUNDNT
c
      CALL TRACE ('RELERR  ')
                 w= 0.00d0
      IF( u .NE. v ) THEN
                 w= 1.00d0
                 o= 1.00d0
          IF( SIGN( o, u) .EQ. SIGN( o, v)) THEN
              a= ABS( u)
              b= ABS( v)
              x= MAX( a, b)
              y= MIN( a, b)
             IF( x .NE.  0.00d0) THEN
                 w= 1.00d0 - y/x
             ENDIF
          ENDIF
      ENDIF
c
      RELERR= w
      CALL TRACK ('RELERR  ')
      RETURN
      END
c
c***********************************************************************
      SUBROUTINE REPORT( iou, ntk,nek,FLOPS,TR,RATES,LSPAN,WG,OSUM,ID)
c***********************************************************************
c                                                                      *
c     REPORT -  Prints Statistical Evaluation Of Fortran Kernel Timings*
c                                                                      *
c     iou    -  Logical Output Device Number                           *
c     ntk    -  Total number of Kernels to Edit in Report              *
c     nek    -  Number of Effective Kernels in each set to Edit        *
c     FLOPS  -  Array:  Number of Flops executed by each kernel        *
c     TR     -  Array:  Time of execution of each kernel(microsecs)    *
c     RATES  -  Array:  Rate of execution of each kernel(megaflops/sec)*
c     LSPAN  -  Array:  Span of inner DO loop in each kernel           *
c     WG     -  Array:  Weight assigned to each kernel for statistics  *
c     OSUM   -  Array:  Checksums of the results of each kernel        *
c***********************************************************************
c
c                                REFERENCES
c
c               F.H.McMahon,   The Livermore Fortran Kernels:
c               A Computer Test Of The Numerical Performance Range,
c               Lawrence Livermore National Laboratory,
c               Livermore, California, UCRL-53745, December 1986.
c
c        from:  National Technical Information Service
c               U.S. Department of Commerce
c               5285 Port Royal Road
c               Springfield, VA.  22161
c
c               J.T. Feo,  An Analysis Of The Computational And Parallel
c               Complexity Of The Livermore Loops, PARALLEL COMPUTING
c               (North Holland), Vol 7(2), 163-185, (1988).
c
c                                NOTICE
c
c               "This report was  prepared  as an account
c               of work  sponsored  by  the United States
c               Government.  Neither  the  United  States
c               nor the   United   States  Department  of
c               Energy, nor any  of  their employees, nor
c               any of their contractors, subcontractors,
c               or their employees,  makes  any warranty,
c               express or  implied, or assumes any legal
c               liability or   responsibility   for   the
c               accuracy, completeness  or  usefulness of
c               any information,  apparatus,  product  or
c               process disclosed, or represents that its
c               use would  not  infringe  privateiy-owned
c               rights."
c
c               Reference to  a  company  or product name
c               does not impiy approval or recommendation
c               of the   product  by  the  University  of
c               California or  the   U.S. Department   of
c               Energy to  the  exclusion  of others that
c               may be suitable.
c
c
c               Work performed under  the auspices of the
c               U.S. Department of Energy by the Lawrence
c               Livermore Laboratory    under    contract
c               number W-7405-ENG-48.
c
c***********************************************************************
c
c  Abstract
c
c  A computer performance  test that measures a realistic floating-point
c  performance range  for  Fortran applications is described.  A variety
c  of computer performance analyses may be easily carried out using this
c  small central  processing unit (cpu) test that would be infeasible or
c  too costly using complete applications as benchmarks, particularly in
c  the developmental  phase of an immature computer system.  The problem
c  of benchmarking numerical  applications  sufficiently,  especially on
c  new supercomputers,  is analyzed to identify several useful roles for
c  the Livermore Fortran  Kernal  (LFK) test.  The 24 LFK contain enough
c  samples of Fortran practice to expose many specific inefficiencies in
c  the formulation of the Fortran source, in the quality of compiled cpu
c  code, and   in   the  capability  of  the  instruction  architecture.
c  Examples show how the  LFK may be used to study compiled Fortran code
c  efficiency, to test the ability of compilers to vectorize Fortran, to
c  simulate mature coding  of  Fortran on new computers, and to estimate
c  the effective  subrange  of  supercomputer  performance  for  Fortran
c  applications.
c
c  Cpu performance  measurements   of  several  Fortran  benchmarks  and
c  numerical applications  that  correlate well with the cpu performance
c  range measured  by  the   LFK  test  are  presented.   The  numerical
c  performance metric  Mflops, first introduced in 1970 in this cpu test
c  to quantify the cpu  performance  range of numerical applications, is
c  discussed.  Analyses  of  the  LFK  performance results argue against
c  reducing the cpu performance  range  of  supercomputers  to  a single
c  number.  The  24  LFK  measured  rates  show  a realistic variance in
c  Fortran cpu  performance  that  is  essential  data  for  circumspect
c  computer evaluations.   Cpu performance data measured by the LFK test
c  on a number of recent computer systems are tabulated for reference.
c
c
c
c  I: FORTRAN CPU PERFORMANCE ANALYSIS
c
c
c     These kernels measure  Fortran numerical computation rates for a
c     spectrum of  CPU-limited computational structures or benchmarks.
c     The kernels benchmark  contains  extracts  or  kernels from more
c     than a score CPU-limited scientific application programs.  These
c     kernels are The most  important  CPU  time  components  from The
c     application programs.   This  benchmark  may  be easily extended
c     with important new kernels leaving performance statistics intact.
c
c     The time required  to  convert,  debug,  execute  and time many,
c     entire, large  programs  on  new  machines  each  having  a  new
c     implementation of  Fortran,   or   several   implementations  or
c     dialects rapidly  becomes  excessive.  Almost all The conversion
c     costs are in segments  of  The programs which are irrelevant for
c     evaluation of  The  CPU,  e.g.,  I/O, Fortran variations, memory
c     allocation, overlays,   job  control,   etc.    all   of   these
c     complexities are reduced to a single, small benchmark which uses
c     a minimum of I/O  and  a  single level of storage.  further, the
c     computation in  the  kernels  is  the  most  stable  part of the
c     Fortran language.
c
c     The kernels benchmark  is sufficient to determine a range of CPU
c     performance for  many  different  computational  structures in a
c     single computer run.   Since The range in performance is usually
c     large the  mean  has  a secondary significance.  To estimate the
c     performance of  a particular,  CPU-limited  application  program
c     select the  case(s) which are most similar to the application as
c     most relevent to the  estimate.   The  performance  ratio  of  a
c     kernel on  two  different  machines or compiled by two different
c     compilers on the same  machine  will  approximate  the  ratio of
c     through-puts for   an  application  which  is  very  similar  in
c     structure.
c
c     This set of kernels was chosen to measure lower and upper bounds
c     for scalar Fortran computation rates.  The upper bound on scalar
c     rates serves as a base  to  evaluate the effectiveness of vector
c     computation.  The  kind  of  Fortran  which  has the highest MIP
c     rates is pure arithmetic  in  DO-loops where complete local code
c     optimization by a Fortran compiler is possible.  All other kinds
c     of Fortran  operations  execute  at  much  lower  MIP  rates  on
c     multiple register machines (these ops may not be necessary).
c
c     Through-put is  measured  in  units of floating-point operations
c     executed per micro-second;  called  results  per micro-second or
c     mega-flops.  The  Mflop is a measure of the NECESSARY results in
c     a scientific application  program  regardless  of  the number or
c     kind of  operations  or processing.  The ratio of Mflops for two
c     different machines will approximate  the  ratio  of through-puts
c     for the  majority  of compute-limited scientific applications on
c     the two  machines.    The   kernels  measure  performance  scale
c     factors.
c
c
c II: FORTRAN PROGRAMMING SYSTEM MATURITY
c
c     Hardware performance   gains   depend   criticaly   on  compiler
c     maturity.  These  kernels   measure  the  joint  performance  of
c     hardware and  Fortran  compiler  software and may easily be used
c     for a comparative  analysis  of  all  the available compilers or
c     options on a given machine.  For a new or proposed machine where
c     no compiler is available  the  performance  may  be estimated by
c     simulating a  reasonable  compilation.  An example of simulation
c     rationale is given below.
c
c     Fortran compilers for new  types  of  machines require a lengthy
c     development cycle  to  achive  an  effective  level  of  machine
c     utilization.  A fully  mature  compiler  may not be completed in
c     the first  years  of  a  new machine.  Indeed, maturity is not a
c     stationary state   but  evolves   with   advances   in   program
c     optimization techniques.   Some  of  these  techniques depend on
c     special facilities in the  new  machines and serious development
c     and implementation cannot start much earlier than development of
c     the new machine.   Assumptions  on  the  maturity  of  available
c     Fortran compilers  are  crucial  to  the  evaluation  of Fortran
c     performance and  thus,   compiler   characteristics   should  be
c     explicit parameters of the performance analysis.
c
c
c -----------------------------------------------------------------------------
c III: A CPU Performance Metric For Computational Physics:    Mega-Flops/sec.
c -----------------------------------------------------------------------------
c
c
c A:  Floating-Point Instructions:  The Necessary Mathematics
c
c Computational physics applies  systems  of  PDEs from Mathematical physics to
c simulate the  evolution of physical systems.  The mathematical methods depend
c on real  valued  functions   and   the   algorithms  are  programmed,  almost
c exclusively, in  Fortran  Floating-point  computer operations (Flops).  These
c floating-point operations  are,  unquestionably,   the   NECESSARY   computer
c operations on  ANY  computer  and  the  total  number  is  INVARIANT.  Thus a
c meaningful computation rate can  always  be  measured  by  counting the total
c number of Flops and dividing by the total execution time of a program.
c
c B:   Procedural Machine  Instructions:   Artifices Of An Archetecture
c
c All of  the non-arithmetic instructions in a machine program are artifices of
c a particular hardware  architecture,  i.e.  machine dependant, as well as the
c result of  a  particular compiler's imperfect coding techniques.  How many of
c these procedural machine  instructions  are  strictly  necessary  can only be
c determined by further, tedious analysis which is ALWAYS machine dependant.  A
c famous example  of software  masking  hardware  capabilities  is  the  PASCAL
c compiler written  by  n.Wirth  which  used  only  50%  of  the command set to
c generate machine programs for the CDC-7600.
c
c Unless the next generation computer design is constrained for some reason, to
c closely resemble  its  obsolete  predecessor,  the  instruction  mix  used in
c current machines is not necessarily  relevent.   Furthermore, the instruction
c mix is  not  a  definitive  characterization  of the intrinsic physics or the
c mathematical algorithms.
c
c  1.  Primary Memory Access Instructions
c
c The number of memory  instructions  that  are necessary for a given algorithm
c depends strongly  on  the  number  and  kind of CPU registers and is a highly
c machine dependent number.   Operating registers, scratch-pad memories, vector
c buffers, short-stop  and  feed-back paths in the cpu are examples of hardware
c artifices which reduce the  number  of  primary memory operations.  Compilers
c and other  coders must make intelligent use of these particular cpu resources
c to minimize memory operations  and this is generally not the case, as is well
c known.
c
c  2.  Branching Instructions
c
c Branching instructions   are   the  slowest  and  most  expensive  procedural
c instructions and are very  often  unecessary.  Here the source programmer has
c primary responsibility  to  minimize  branching in the program by avoiding IF
c statements whenever possible  by  using MAX, MIN, or merge functions like
c CSMG.  Careful  logical  reduction  and  placement of IF tests is required to
c minimize the execution of branching operations.  Compilers can do very little
c to change or optimize the branch graph specified in the source program.
c
c On vector  computers ALL IF tests over mesh or array (state) variables can be
c eliminated.  Conditional computation can be vectorized by direct construction
c using explicit  sub-set mappings.  Vector relationals replace the IF clauses.
c Then sparse,  one-to-one   mappings  called  vector  Compress/Decompress  and
c one-to-many mappings   called   vector   Gather/Scatter   are  necessary  and
c sufficient to compose sub-vector operands for simple vector operations.
c
c
c
c
c
c IV: PERFORMANCE MEASUREMENTS
c
c
c     Through-put is measured in units of millions of floating-point
c     operations executed per second, called mflops.
c
c
c     Artificially long computer  runs do not have to be contrived for
c     timing on  machines  where  a cpu clock may be read in job mode.
c     Statistics on  the accuracy  of  the  timing  method  should  be
c     measured.
c
c     Net mflops is meaningful only if real run time of each kernel
c     is adjusted such that it weights the total time in proportion
c     to the actual usage of that catagory of computation in the
c     total workload.
c
c
c
c
c
c   1. Assignment Of Weights To Floating-Point Operations
c
c     Weights are assigned to different kinds of floating-point
c     operations to normalize their hardware execution time to
c     addition time so that the flop rates computed for various
c     Fortran Kernels will be commensurable.
c
c                           +,-,*   1
c                          /,SQRT   4
c                     EXP,SIN,ETC.  8
c                     IF(X.REL.Y)   1
c
c
c     Each Kernel flop-count is the weighted number of flops required for
c     serial execution.  The scalar version defines the NECESSARY computation
c     generally, in the absence of proof to the contrary.  The vector
c     or parallel executions are only credited with executing the same
c     necessary computation.  If the parallel methods do more computation
c     than is necessary then the extra flops are not counted as through-put.
c
c
c    2. SAMPLE OUTPUT:               CDC-7600/FTN-4.4
c
c                 KERNEL  FLOPS   TIME   MFLOPS
c                      1    500    94.4    5.30
c                      2    300    45.3    6.62
c                      3    100    21.9    4.57
c                      4    300   109.3    2.75
c                      5    100    25.6    3.91
c                      6    100    27.8    3.60
c                      7    640    88.2    7.25
c                      8   1440   249.0    5.78
c                      9    680   123.2    5.52
c                     10    360   102.8    3.50
c                     11     49    34.8    1.41
c                     12     49    18.3    2.68
c                     13    224   107.7    2.08
c                     14   3300   809.3    4.08
c                     15   3960  1769.5    2.24
c                     16    530   320.3    1.65
c                     17    405    92.2    4.39
c                     18   6600  1121.5    5.88
c                     19    540   105.8    5.11
c                     20   1300   266.0    4.89
c                     21   1250   370.9    3.37
c                     22   1700   601.9    2.82
c                     23   1650   362.4    4.55
c                     24    200   171.7    1.16
c
c                      AVERAGE  RATE =     3.96 MEGA-FLOPS/SEC.
c                      MEDIAN   RATE =     4.08 MEGA-FLOPS/SEC.
c                      HARMONIC MEAN =     3.15 MEGA-FLOPS/SEC.
c                      STANDARD DEV. =     1.61 MEGA-FLOPS/SEC.
c
c                                                    F.H.MCMAHON  1972
c
c
c
c
c
c
c    3. INTERPRETATION OF OUTPUT FILE FROM SUBROUTINE REPORT:
c
c
c
c  The highly instrumented LFK test program measures the effective cpu
c  performance range and has sufficient timed samples for many statisical
c  analyses thus avoiding the PERIL of a SINGLE performance "rating".
c  A COMPLETE REPORT OF LFK TEST RESULTS MUST QUOTE THE PERFORMANCE RANGE
c  STATISTICS BASED ON THE SUMMARY OF 72 TIMED SAMPLES:  the minium,
c  the equi-weighted harmonic, geometric, and arithmetic means and the maximum
c  rates.  The standard deviation must also be quoted to show the variance
c  in performance rates.  NO SINGLE RATE QUOTATION IS SUFFICIENT OR HONEST.
c
c  The LFK test (Livermore loops) outputs data for three benchmarking contexts
c  following print-outs of cpu clock checks and experimental timing errors:
c
c
c
c  1. Conventional "Balanced"  Cpus,  e.g. PCs, DEC-VAXs, IBM-370s.
c
c    1.1. [Refer to SUMMARY of 72 timings on pp.9-10 of LFK test OUTPUT file.
c         The bottom line is the set of nine performance range statistics
c         min thru max plus standard deviation listed after SUMMARY table.
c         These statistics may be used for computer comparisons as shown
c         in figure 11, p.24 of the LFK report UCRL-53745.  Ratios of the
c         range statistics from two computers show the range of speed-ups.]
c
c    1.2. An all-scalar coded LFK test (NOVECTOR) measures the basal scalar,
c         mono-processor computing capability.
c
c
c
c  2. Vector "Unbalanced"  Cpus,  e.g. CRAY, NEC, IBM-3090.
c
c    2.1. [Pages 2-8 of the LFK test OUTPUT file analyzes three different
c         runs of the 24 Livermore loops with short, medium, and long DO
c         loop spans (vector lengths).  The performance range statistics
c         for each of these three runs on vector computers should be compared
c         as shown in figure 12, p.25 of the LFK report UCRL-53745.]
c
c    2.2  The performance rates of most applications on vector computers are
c         observed in a sub-range from approximately the harmonic mean through
c         the mean rate of the 24 LFK samples (thru the two middle quartiles).
c
c         2.2.1  The equi-weighted arithmetic mean (AM) of 72 LFK rates
c                correlates with highly vectorised applications in the workload,
c                (80%-90% of flops) because the average is dominated by the high
c                vector rates.  Very highly vectorised applications (95%-99%+)
c                may run several times the average rate (figure 10, p21, ibid).
c
c         2.2.2  The equi-weighted harmonic mean (HM) of 72 LFK rates
c                correlates with poorly vectorised applications in the workload,
c                (30%-40% of flops) because the HM is dominated by the low
c                scalar rates.  An all-scalar coded LFK test (NOVECTOR)
c                measures the basal scalar, mono-processor computing capability.
c
c         2.2.3  The best central measure is the Geometric Mean(GM) of 72 rates,
c                because it is least biased by outliers.  CRAY hardware monitors
c                have demonstrated net Mflop rates for the LLNL and UCSD
c                workloads are closest to the 72 LFK test geometric mean rate.
c
c
c
c  3. Parallel "Unbalanced"  Cpus,  e.g. CRAY, NEC, IBM-3090.
c
c    3.1. The lower,   uni-processor bound of an MP system is given by 1.2.
c
c    3.2. The upper, multi-processor bound of an MP system is estimated by
c         multiplying the LFK performance statistics from 1.2 or from 2.2.
c         by N, the number of processors.
c
c
c
c
c      Comparision of two or more computers should make use of all the
c  performance range statistics in the tables below ( DO span= 167):
c  the extrema, the mean rates, and the standard deviation.
c  NO SINGLE MFLOPS RATE QUOTATION IS SUFFICIENT OR HONEST.
c  If the performance range is very large the causes and implications should
c  be fully explored.  Use of a single mean statistic is insufficient
c  but may be valid if the three mean rates are close in value and the
c  standard deviation is relatively small.  The geometric mean is a
c  better central measure than the median which depends on one value
c  in a small set.  The least biased central measure is the geometric
c  mean because it is less sensitive to outliers than either the average
c  or the harmonic mean.  When the computer performance range is very
c  large the net Mflops rates of many Fortran programs and workloads
c  have been observed to be in the sub-range between the equi-weighted
c  harmonic and arithmetic means depending on the degree of code
c  parallelism and optimization(Ref. 1).  Note that LFK mean Mflops rates
c  also imply the average efficiency of a computing system since
c  the peak rate is a well known constant.
c
c      The performance data shown for the computers below will be subject to
c  change with time.  Effective Cpu performance may improve as the programming
c  system software matures or effective performance may regress when the system
c  is oversubscribed.  We have observed degraded performance for the LFK test
c  in virtual storage systems when the working set size was too small, and in
c  multiprogramming or multiprocessing systems which were either immature or
c  very active.  In these active environments the LFK test measures a real
c  Cpu degradation in the effectiveness of caching data and data access
c  generally.  It is necessary to run the LFK test stand-alone to have
c  reproducable performance measurements.
c
c      The performance data sets tabulated below which have 72 sample
c  timings are a combination of three 24 sample sets produced by the
c  LFK test.  Statistics on the 72 sample data set are more significant
c  and these statistics should be quoted ( DO span= 167).
c
c
c
c
c
c
c                          REFERENCES
c
c         F.H.McMahon,   The Livermore Fortran Kernels:
c         A Computer Test Of The Numerical Performance Range,
c         Lawrence Livermore National Laboratory,
c         Livermore, California, UCRL-53745, December 1986.
c
c  from:  National Technical Information Service
c         U.S. Department of Commerce
c         5285 Port Royal Road
c         Springfield, VA.  22161
c
c
c         F.H.McMahon, "The Livermore Fortran Kernels Test of the Numerical
c         Performance Range", in Performance Evaluation of Supercomputers
c         (J.L.Martin, ed., North Holland, Amsterdam), 143-186(1988).
c
c
c         J.T. Feo,  An Analysis Of The Computational And Parallel
c         Complexity Of The Livermore Loops, PARALLEL COMPUTING
c         (North Holland), Vol 7(2), 163-185, (1988).
c
c
c         F.H.McMahon, "Measuring the Performance of Supercomputers",
c         in Energy and Technology Review (A.J.Poggio,ed.),
c         Lawrence Livermore National Laboratory, UCRL-52000-88-5, (1988).
c
c
c
c
c    The range of speed-ups shown below as ratios of the performance
c    statistics has a small variance compared to the enormous
c    performance ranges; the range of speed-ups are convergent estimates.
c    Report all nine performance range statistics on 72 samples, e.g.:
c
c
c
c
c D.117 LFK Test   117.1      117.2      117.3      117.4      117.5      117.6
c ------------- ---------- ---------- ---------- ---------- ---------- ---------
c   Vendor       CRAY RI    CRAY RI    CRAY RI    CDC        IBM        NEC
c   Model        XMP1 8.5   YMP1       2          ETA10-G    3090S180   SX-2
c   OSystem      COS 1.16   COS 1.16   UNICOS     EOS1.2J2   MVS2.2.0   SXOS1.21
c   Compiler     CFT771.2   CFT771.2   CFT771.3   F200 690   VSF2.3.0   F77/SX24
c   OptLevel     Vector     Vector     Vector     VAST2.25   Vector     Vector
c   NR.Procs          1          1          1          1          1          1
c   Samples          72         72         72         72         72         72
c   WordSize         64         64         64         64         64         64
c   DO Span         167        167        167        167        167        167
c   Year           1987       1988       1988       1988       1989       1986
c   Kernel/MFlops--------- ---------- ---------- ---------- ---------- ---------
c          1       183.57     258.64     160.17     405.57      56.03     800.05
c          2        42.49      67.09      21.61      12.55       8.88      49.94
c          3       173.19     236.67     111.93     233.09      53.66     528.67
c          4        65.68      95.05      47.45      59.48      40.72     164.18
c          5        15.89      18.69      13.01      11.86       8.83      11.26
c          6        12.91      20.58      13.07      13.13       8.57      29.30
c          7       207.28     295.48     228.00     488.07      62.08    1042.33
c          8       149.44     232.41     189.47     242.77      46.19     415.68
c          9       178.50     251.07     195.24     186.88      61.70     705.28
c         10        78.50     111.42      73.20      82.68       8.57     120.75
c         11        12.02      16.52      12.39       7.11       6.84       8.32
c         12        81.14     112.50      57.52     227.40      18.18     242.80
c         13         5.89       7.35       4.83       5.66       4.12      16.78
c         14        22.48      31.90      19.08      11.56      11.08      25.79
c         15         6.24       7.78       7.58      75.87       4.93       8.73
c         16         7.28       8.62       5.06       2.53       5.27       9.85
c         17        11.70      14.92      10.29       8.38      10.65      17.89
c         18       126.84     203.76     127.63     160.39      37.13     349.42
c         19        16.74      20.63      13.70       9.69      11.58      13.40
c         20        14.56      18.76      13.51       8.13       9.75      16.12
c         21       117.63     168.79      58.97     138.42      19.62     253.03
c         22        75.96     103.46      95.34      54.32      17.04     183.34
c         23        15.34      17.71      10.46      20.22      13.97      20.52
c         24         3.60       4.58       2.66      28.60       3.95       4.59
c -------------      ....       ....       ....       ....       ....       ....
c PM Correlation =   1.00       1.00       0.97       0.90       0.95       0.93
c Standard  Dev. =  59.92      86.75      61.18      89.09      16.32     219.72
c
c Maximum   Rate = 207.28     295.48     228.00     488.07      62.08    1042.33
c Quartile  Q3   =  78.59     111.42      73.20      78.61      19.20     156.56
c Average   Rate =  55.39      78.23      49.70      64.38      17.56     139.95
c Geometric Mean =  27.57      36.63      22.61      26.39      12.23      43.94
c Median    Q2   =  16.74      20.63      13.77      19.82      10.06      24.16
c Harmonic  Mean =  13.95      17.66      11.26      12.25       9.02      19.07
c Quartile  Q1   =  11.70      14.75       8.34       8.39       6.99      11.44
c Minimum   Rate =   2.20       2.85       2.01       2.25       2.43       4.47
c
c Maxima    Ratio=   1.00       1.43       1.10       2.35       0.30       5.03
c Average   Ratio=   1.00       1.41       0.90       1.16       0.32       2.53
c Geometric Ratio=   1.00       1.33       0.82       0.96       0.44       1.59
c Harmonic  Ratio=   1.00       1.27       0.81       0.88       0.65       1.37
c Minima    Ratio=   1.00       1.30       0.91       1.02       1.10       2.03
c
c           The range of speed-ups shown above as ratios of the performance
c           statistics has a small variance compared to the enormous
c           performance ranges; the range of speed-ups are convergent estimates.
c           More accurate projection of a cpu workload rate may be
c           computed by assigning appropriate weights for each kernel.
c
c           The upper bound for Fortran performance of a parallel
c           N-processor system is given by multiplying the seven range
c           statistics from a uni-processor LFK test (2.2) by N.
c
c D.118 LFK Test  118.1      118.2      118.3      118.4      118.5      118.6
c ------------- ---------- ---------- ---------- ---------- ---------- ---------
c   Vendor       CRAY RI    CRAY RI    CRAY RI    CRAY RI    CRAY RI    CRAY RI
c   Model        YMP1modY   YMP1modY   YMP/832    YMP/832    YMP/832    YMP/832
c   OSystem      NLTSS      NLTSS      UNICOS     UNICOS     UNICOS     UNICOS
c   Compiler     CFT77 3.   CFT77 3.   CF77 4.0   CF77 4.0   CF77 4.0   CF77 4.0
c   OptLevel     Scalar     Vector     vector     vector     vector     vector
c   NR.Procs            1          1          1          2          4          8
c   Samples            72         72         72         72         72         72
c   WordSize           64         64         64         64         64         64
c   DO Span           167        167        167        167        167        167
c   Year             1989       1989       1990       1990       1990       1990
c   Kernel/MFlops--------- ---------- ---------- ---------- ---------- ---------
c          1        23.33     258.08     188.23     364.86     535.99     581.75
c          2        14.26      68.12      64.45      64.86      65.59      64.08
c          3        25.05     232.20     236.81     236.93     233.45     236.86
c          4        22.92      92.14      89.72     110.24     160.77     156.70
c          5        19.44      19.59      19.30      19.64      19.59      19.65
c          6         9.24      21.15      20.76      21.07      20.93      20.86
c          7        32.83     291.31     274.07     521.69     896.68    1308.07
c          8        30.00     229.89     188.78     264.72     262.94     266.89
c          9        31.23     240.88     169.97     225.10     219.31     243.47
c         10        18.53     108.73     106.66     112.78     108.76     108.58
c         11        19.73      19.75      37.87      38.66      38.52      37.93
c         12        16.95     135.81     126.99     130.52     125.68     130.49
c         13         6.73       6.74      20.77      21.16      20.89      21.18
c         14         9.71      29.98      29.04      35.15      38.77      40.81
c         15         7.55       7.55      32.53      52.47      73.84     127.58
c         16         8.42       8.34       8.38       8.44       8.34       8.44
c         17        13.47      13.84      15.70      15.89      15.88      15.89
c         18        24.84     199.36     179.98     293.24     410.87     526.41
c         19        20.28      20.34      20.07      20.37      20.27      20.37
c         20        18.27      18.50      17.84      17.98      18.03      18.05
c         21        20.53     160.40     278.54     439.90     776.14    1268.94
c         22         8.74     106.25      86.52     132.58     131.62     129.79
c         23        19.53      20.16      36.35      36.65      36.75      36.79
c         24         3.85       3.93      38.04      38.82      38.58      38.80
c -------------      ....       ....       ....       ....       ....       ....
c Standard  Dev. =   7.93      85.18      78.37     113.05     171.34     246.63
c
c Maximum   Rate =  32.83     291.31     278.54     521.69     896.68    1308.07
c Quartile  Q3   =  20.53     109.15     116.42     120.27     125.70     128.95
c Average   Rate =  16.65      77.30      75.88      92.90     113.89     136.32
c Geometric Mean =  14.58      36.50      41.43      45.11      48.00      49.09
c Median    Q2   =  16.95      21.15      32.25      36.53      36.75      36.79
c Harmonic  Mean =  12.40      17.27      22.69      23.48      24.12      23.58
c Quartile  Q1   =   8.76      13.84      16.56      16.74      17.94      17.13
c Minimum   Rate =   3.73       2.90       2.82       2.87       2.86       2.83
c
c Maxima    Ratio=   1.00       8.87       8.48      15.89      27.31      39.84
c Average   Ratio=   1.00       4.64       4.56       5.58       6.84       8.19
c Geometric Ratio=   1.00       2.50       2.84       3.09       3.29       3.37
c Harmonic  Ratio=   1.00       1.39       1.83       1.89       1.95       1.90
c Minima    Ratio=   1.00       0.78       0.76       0.77       0.77       0.76
c
c
c
c
c           The parallel complexity and parallel techniques for the LFK
c           test are described in the following reference:
c
c           J.T.Feo,  An Analysis of the Computational and
c           Parallel Complexity of the Livermore Loops,
c           PARALLEL COMPUTING 7(2), 163-185(1988).
c
c
c
c
c
c
c
c
c    4. SAMPLE OUTPUT FILE FROM SUBROUTINe REPORT: (CRAY-C90/1/CFT77 Compiler)
c                                                                       aus
c
c         Output file for the Mono-processed Standard Benchmark Test:
c         The following output was uni-processed on CRAY-C90/1 in a
c         fully loaded, multi-processing, multi-programming system:
c
c
c
c  *********************************************
c  THE LIVERMORE  FORTRAN KERNELS "MFLOPS" TEST:
c  *********************************************
c  >>> USE 72 SAMPLES LFK TEST RESULTS SUMMARY (line 330+)
c  >>> USE ALL RANGE STATISTICS FOR OFFICIAL QUOTATIONS.
c SECOVT:     16000  0.1970E-05     1.0000
c SECOVT:     32000  0.1971E-05     0.0002
c VERIFY:       200  0.2177E-05 =  Time Resolution of Cpu-timer
c
c         VERIFY ADEQUATE Loop SIZE VERSUS CPU CLOCK ACCURACY
c         -----     -------     -------    -------   --------
c         EXTRA     MAXIMUM     DIGITAL    DYNAMIC   RELATIVE
c         Loop      CPUTIME     CLOCK      CLOCK     TIMING
c         SIZE      SECONDS     ERROR      ERROR     ERROR
c         -----     -------     -------    -------   --------
c           256  0.1129E-03       0.00%      0.20%      0.78%
c           512  0.2256E-03       0.00%      0.23%      0.95%
c          1024  0.4505E-03       0.00%      0.19%      1.04%
c          1360       Repetition Count = MULTI * Loops2 =       10.000
c          2048  0.9065E-03       0.00%      0.32%      0.68%
c          4096  0.1820E-02       0.00%      0.09%      0.14%
c          8192  0.3669E-02       0.00%      0.49%      0.47%
c         16384  0.7279E-02       0.00%      0.33%      0.24%
c         32768  0.1458E-01       0.00%      0.31%      0.03%
c         65536  0.2952E-01       0.00%      0.88%      0.03%
c         -----     -------     -------    -------   --------
c
c
c  CLOCK CALIBRATION TEST OF INTERNAL CPU-TIMER: SECOND
c  MONOPROCESS THIS TEST, STANDALONE, NO TIMESHARING.
c  VERIFY TIMED INTERVALS SHOWN BELOW USING EXTERNAL CLOCK
c  START YOUR STOPWATCH NOW !
c
c            Verify  T or DT  observe external clock(sec):
c
c            -------     -------      ------      -----
c            Total T ?   Delta T ?    Mflops ?    Flops
c            -------     -------      ------      -----
c      1       10.15       10.15      222.33    0.22568E+10
c      2       20.29       10.13      222.51    0.45136E+10
c      3       30.42       10.14      222.54    0.67703E+10
c      4       40.54       10.12      222.66    0.90271E+10
c            -------     -------      ------      -----
c  END CALIBRATION TEST.
c
c
c  ESTIMATED TOTAL JOB CPU-TIME:=    45.724 sec.  ( Nruns=       7 Trials)
c
c  Trial=      1             ChkSum=  797    Pass=      0     Fail=      0
c  Trial=      2             ChkSum=  797    Pass=      1     Fail=      0
c  Trial=      3             ChkSum=  797    Pass=      2     Fail=      0
c  Trial=      4             ChkSum=  797    Pass=      3     Fail=      0
c  Trial=      5             ChkSum=  797    Pass=      4     Fail=      0
c  Trial=      6             ChkSum=  797    Pass=      5     Fail=      0
c  Trial=      7             ChkSum=  797    Pass=      6     Fail=      0
c 1
c
c
c  time TEST overhead (t err):
c
c       RUN        AVERAGE        STANDEV        MINIMUM        MAXIMUM
c  TICK   1   0.693376E-06   0.341353E-09
c  TICK   2   0.693154E-06   0.251747E-09
c  TICK   3   0.693600E-06   0.959460E-09
c  TICK   4   0.694089E-06   0.598360E-09
c  TICK   5   0.693888E-06   0.898132E-09
c  TICK   6   0.695235E-06   0.385844E-10
c  TICK   7   0.694262E-06   0.200605E-09
c  DATA   7   0.999866E-01   0.543319E-06   0.999856E-01   0.999877E-01
c  DATA   7   0.999859E-01   0.862079E-06   0.999843E-01   0.999875E-01
c  TICK   7   0.693943E-06   0.638885E-09   0.693154E-06   0.695235E-06
c
c
c  THE EXPERIMENTAL TIMING ERRORS FOR ALL  7 RUNS
c  --  ---------  ---------  --------- -----  -----   ---
c   k   T min      T avg      T max    T err   tick   P-F
c  --  ---------  ---------  --------- -----  -----   ---
c   1 0.4935E-03 0.4946E-03 0.4970E-03  0.23%  0.00%     0
c   2 0.2459E-02 0.2463E-02 0.2469E-02  0.13%  0.00%     0
c   3 0.3627E-03 0.3636E-03 0.3662E-03  0.32%  0.00%     0
c   4 0.9722E-03 0.9766E-03 0.9841E-03  0.40%  0.00%     0
c   5 0.5913E-02 0.5917E-02 0.5922E-02  0.05%  0.00%     0
c   6 0.3719E-02 0.3727E-02 0.3741E-02  0.19%  0.00%     0
c   7 0.7694E-03 0.7709E-03 0.7748E-03  0.24%  0.00%     0
c   8 0.1190E-02 0.1194E-02 0.1200E-02  0.28%  0.00%     0
c   9 0.1018E-02 0.1022E-02 0.1037E-02  0.61%  0.00%     0
c  10 0.1047E-02 0.1059E-02 0.1084E-02  1.10%  0.00%     0
c  11 0.3269E-02 0.3270E-02 0.3271E-02  0.02%  0.00%     0
c  12 0.3852E-03 0.3882E-03 0.3994E-03  1.19%  0.00%     0
c  13 0.2562E-02 0.2573E-02 0.2594E-02  0.41%  0.00%     0
c  14 0.2188E-02 0.2203E-02 0.2224E-02  0.46%  0.00%     0
c  15 0.1263E-02 0.1267E-02 0.1276E-02  0.32%  0.00%     0
c  16 0.1231E-01 0.1232E-01 0.1233E-01  0.05%  0.00%     0
c  17 0.1450E-01 0.1451E-01 0.1452E-01  0.03%  0.00%     0
c  18 0.7822E-03 0.7866E-03 0.7986E-03  0.65%  0.00%     0
c  19 0.6595E-02 0.6599E-02 0.6613E-02  0.09%  0.00%     0
c  20 0.8131E-02 0.8133E-02 0.8136E-02  0.02%  0.00%     0
c  21 0.1578E-02 0.1581E-02 0.1585E-02  0.13%  0.00%     0
c  22 0.1005E-02 0.1006E-02 0.1010E-02  0.17%  0.00%     0
c  23 0.5906E-02 0.5915E-02 0.5920E-02  0.09%  0.00%     0
c  24 0.5960E-03 0.5987E-03 0.6037E-03  0.46%  0.00%     0
c  --  ---------  ---------  --------- -----  -----   ---
c
c
c  NET CPU TIMING VARIANCE (T err);  A few % is ok:
c
c                  AVERAGE        STANDEV        MINIMUM        MAXIMUM
c      Terr          0.32%          0.31%          0.02%          1.19%
c
c
c
c
c 1
c  ********************************************
c  THE LIVERMORE  FORTRAN KERNELS:  * SUMMARY *
c  ********************************************
c
c               Computer : CRAY-YMP C90 (240 MHz)
c               System   : UNICOS 7.C, loaded
c               Compiler : CFT77 5.0.1.17
c               Date     : 92.02.18
c               Testor   : Charles Grassl, CRI
c
c          When the computer performance range is very large
c          the net Mflops rate of many Fortran programs and
c          workloads will be in the sub-range between the equi-
c          weighted Harmonic and Arithmetic means depending
c          on the degree of code parallelism and optimization.
c          The least biased central measure is the Geometric
c          Mean of 72 rates,  quoted +- a standard deviation.
c          Mean Mflops rates imply the average efficiency of a
c          computing system since the peak rate is well known.
c          LFK test measures a lower bound for a Multi-processor
c          and N * LFK rates project an upper bound for N-procs.
c
c  KERNEL  FLOPS   MICROSEC   MFLOP/SEC SPAN WEIGHT  CHECK-SUMS             OK
c  ------  -----   --------   --------- ---- ------  ---------------------- --
c   1 3.0240E+05 1.3005E+03    232.5310   27   1.00  2.6985731517477143E+02 12
c   2 1.6192E+05 6.5410E+03     24.7546   15   1.00  2.7673078908499883E+02 11
c   3 1.5984E+05 2.9562E+03     54.0693   27   1.00  1.8895163625590286E+00 11
c   4 9.1200E+04 1.1104E+04      8.2135   27   1.00  4.1994754168412953E+00 12
c   5 1.6640E+05 7.7860E+03     21.3717   27   1.00  2.2278306739568734E+01 11
c   6 8.0640E+04 1.1872E+04      6.7925    8   1.00  7.8421657545539745E+00 10
c   7 5.3760E+05 1.5594E+03    344.7549   21   1.00  1.9920041523528744E+02 12
c   8 6.7392E+05 3.1846E+03    211.6168   14   1.00  2.0723805676611606E+04 10
c   9 5.3040E+05 3.0473E+03    174.0534   15   1.00  1.8367779226210434E+04 11
c  10 2.7000E+05 2.3486E+03    114.9621   15   1.00  1.1559038593948761E+04 11
c  11 9.5680E+04 7.6937E+03     12.4361   27   1.00  4.5858129351337848E+03 11
c  12 9.9840E+04 2.6005E+03     38.3928   26   1.00  1.3604052417459656E-05  7
c  13 1.3888E+05 1.2600E+04     11.0223    8   1.00  3.3286478760637817E+09 12
c  14 1.9008E+05 2.9941E+03     63.4846   27   1.00  1.6283981735038579E+07 10
c  15 1.8480E+05 4.3720E+03     42.2689   15   1.00  7.7629810171017889E+03 11
c  16 1.2320E+05 1.1063E+04     11.1365   15   1.00  1.8043200000000000E+05 16
c  17 2.8080E+05 1.2362E+04     22.7143   15   1.00  2.0631580330880024E+02 10
c  18 4.5760E+05 3.1121E+03    147.0397   14   1.00  6.7904523488866980E+03 10
c  19 2.0160E+05 1.0817E+04     18.6377   15   1.00  8.8776148867716529E+01 10
c  20 3.7856E+05 1.1965E+04     31.6382   26   1.00  4.1913992744349525E+03 10
c  21 2.0000E+06 6.0355E+03    331.3725   20   1.00  1.7618090567769289E+07 11
c  22 1.6320E+05 1.7162E+03     95.0947   15   1.00  4.2769781098294516E+01 11
c  23 4.0040E+05 1.1189E+04     35.7852   14   1.00  3.3952384220209933E+03 11
c  24 4.7840E+04 4.8385E+03      9.8874   27   1.00  9.1000000000000000E+01 16
c   1 4.0400E+05 8.3869E+02    481.7017  101   2.00  3.6773413452582608E+03 12
c   2 3.1040E+05 2.9449E+03    105.4019  101   2.00  1.0778052681746078E+04 11
c   3 2.1412E+05 1.5697E+03    136.4098  101   2.00  7.0681900561830844E+00 11
c   4 1.6800E+05 5.3932E+03     31.1501  101   2.00  4.1994754168412953E+00 12
c   5 2.2000E+05 6.9601E+03     31.6085  101   2.00  3.2123223577842145E+02 11
c   6 1.3440E+05 6.2436E+03     21.5260   32   2.00  6.0421729524445254E+02 10
c   7 7.1104E+05 9.8050E+02    725.1814  101   2.00  4.4419104210539372E+03 12
c   8 8.5536E+05 1.4317E+03    597.4570  100   2.00  1.0508876040143967E+06 10
c   9 7.2114E+05 1.1952E+03    603.3716  101   2.00  8.3261052698823810E+05 11
c  10 3.4542E+05 1.1886E+03    290.6114  101   2.00  5.1172588490649126E+05 11
c  11 1.2800E+05 4.4541E+03     28.7379  101   2.00  2.4034922852549423E+05 11
c  12 1.3600E+05 1.0559E+03    128.7950  100   2.00  4.9892992076472353E-05  7
c  13 1.8368E+05 3.1580E+03     58.1634   32   2.00  1.6277232613741943E+10 13
c  14 2.2220E+05 2.4170E+03     91.9337  101   2.00  2.0960155135011959E+08 10
c  15 3.3000E+05 2.5339E+03    130.2320  101   2.00  2.7606716832616553E+05 11
c  16 1.5120E+05 1.4211E+04     10.6397   40   2.00  2.2708700000000000E+05 16
c  17 3.6360E+05 1.6580E+04     21.9302  101   2.00  7.8024924106551043E+03 10
c  18 4.3560E+05 7.8748E+02    553.1564  100   2.00  3.6159377876652405E+05 10
c  19 2.7876E+05 7.7803E+03     35.8289  101   2.00  3.7952718722807331E+03 10
c  20 4.1600E+05 1.3045E+04     31.8893  100   2.00  2.1883436249202024E+05 10
c  21 1.2500E+06 2.2076E+03    566.2378   50   2.00  2.7905717956063032E+07 11
c  22 2.4038E+05 1.2824E+03    187.4462  101   2.00  2.0570230636183987E+03 11
c  23 5.4450E+05 7.3953E+03     73.6275  100   2.00  2.4849262269833777E+05 10
c  24 6.2000E+04 2.8275E+03     21.9275  101   2.00  3.5000000000000000E+02 16
c   1 3.5035E+05 4.9458E+02    708.3752 1001   1.00  3.5802568852590770E+05 12
c   2 2.5996E+05 2.4626E+03    105.5643  101   1.00  1.0778052681746078E+04 11
c   3 1.8018E+05 3.6356E+02    495.6051 1001   1.00  7.0052001816080974E+01 11
c   4 1.6800E+05 9.7664E+02    172.0182 1001   1.00  4.1994754168412953E+00 12
c   5 2.0000E+05 5.9169E+03     33.8016 1001   1.00  3.1842101497423719E+04 11
c   6 1.1904E+05 3.7266E+03     31.9435   64   1.00  3.0625814420367242E+04 10
c   7 6.3680E+05 7.7086E+02    826.0859  995   1.00  4.2729757526330464E+05 12
c   8 7.1280E+05 1.1942E+03    596.8785  100   1.00  1.0508876040143967E+06 10
c   9 6.1812E+05 1.0225E+03    604.5414  101   1.00  8.3261052698823810E+05 11
c  10 3.0906E+05 1.0591E+03    291.8238  101   1.00  5.1172588490649126E+05 11
c  11 1.1000E+05 3.2701E+03     33.6382 1001   1.00  2.3400376808760548E+08 11
c  12 1.2000E+05 3.8817E+02    309.1466 1000   1.00  2.0350071076613574E-04  5
c  13 1.6128E+05 2.5726E+03     62.6904   64   1.00  2.8399773178734131E+10 13
c  14 2.2022E+05 2.2027E+03     99.9773 1001   1.00  2.2155216882543457E+10 10
c  15 1.6500E+05 1.2668E+03    130.2491  101   1.00  2.7606716832616553E+05 11
c  16 1.3250E+05 1.2315E+04     10.7591   75   1.00  1.9828200000000000E+05 16
c  17 3.1815E+05 1.4508E+04     21.9295  101   1.00  7.8024924106551043E+03 10
c  18 4.3560E+05 7.8658E+02    553.7869  100   1.00  3.6159377876652405E+05 10
c  19 2.3634E+05 6.5990E+03     35.8145  101   1.00  3.7952718722807331E+03 10
c  20 2.6000E+05 8.1326E+03     31.9699 1000   1.00  2.1284510356243229E+08  9
c  21 1.2625E+06 1.5807E+03    798.7219  101   1.00  5.8026253852328300E+07 10
c  22 1.8887E+05 1.0064E+03    187.6600  101   1.00  2.0570230636183987E+03 11
c  23 4.3560E+05 5.9148E+03     73.6464  100   1.00  2.4848841798518039E+05 10
c  24 5.0000E+04 5.9870E+02     83.5138 1001   1.00  3.5000000000000000E+03 16
c  ------  -----   --------   --------- ---- ------  ---------------------- --
c  72 0.2421E+08 0.3427E+06     70.6599  167                               797
c
c          MFLOPS    RANGE:             REPORT ALL RANGE STATISTICS:
c          Mean DO Span   =  167
c          Code Samples   =   72
c
c          Maximum   Rate =    826.0859 Mega-Flops/Sec.
c          Quartile  Q3   =    261.5712 Mega-Flops/Sec.
c          Average   Rate =    190.5636 Mega-Flops/Sec.
c          Geometric Mean =     86.2649 Mega-Flops/Sec.
c          Median    Q2   =     83.5138 Mega-Flops/Sec.
c          Harmonic  Mean =     40.7302 Mega-Flops/Sec.
c          Quartile  Q1   =     31.1501 Mega-Flops/Sec.
c          Minimum   Rate =      6.7925 Mega-Flops/Sec.
c
c
c          Standard  Dev. =    227.2457 Mega-Flops/Sec.
c          Avg Efficiency =     10.44%  Program & Processor
c          Mean Precision =     11.07   Decimal Digits
c  <<<<<<<<<<<<<<<<<<<<<<<<<<<*>>>>>>>>>>>>>>>>>>>>>>>>>>>
c  < BOTTOM-LINE:   72 SAMPLES LFK TEST RESULTS SUMMARY. >
c  < USE RANGE STATISTICS ABOVE FOR OFFICIAL QUOTATIONS. >
c  <<<<<<<<<<<<<<<<<<<<<<<<<<<*>>>>>>>>>>>>>>>>>>>>>>>>>>>
c
c
c
c
c
c                     SENSITIVITY ANALYSIS
c
c
c          The sensitivity of the harmonic mean rate (Mflops)
c          to various weightings is shown in the table below.
c          Seven work distributions are generated by assigning
c          two distinct weights to ranked kernels by quartiles.
c          Forty nine possible cpu workloads are then evaluated
c          using seven sets of values for the total weights:
c
c
c              ------ ------ ------ ------ ------ ------ ------
c    1st QT:       O      O      O      O      O      X      X
c    2nd QT:       O      O      O      X      X      X      O
c    3rd QT:       O      X      X      X      O      O      O
c    4th QT:       X      X      O      O      O      O      O
c              ------ ------ ------ ------ ------ ------ ------
c    Total
c    Weights                    Net Mflops:
c     X    O
c   ---- ----
c
c   1.00 0.00   14.37  21.21  40.49  61.64 129.01 203.46 481.18
c
c   0.95 0.05   14.99  22.20  40.35  58.12 111.48 142.31 272.09
c
c   0.90 0.10   15.67  23.29  40.20  54.99  98.14 109.42 189.67
c
c   0.80 0.20   17.24  25.84  39.92  49.63  79.20  74.84 118.11
c
c   0.70 0.30   19.16  29.00  39.63  45.23  66.38  56.86  85.76
c
c   0.60 0.40   21.57  33.05  39.35  41.54  57.14  45.85  67.32
c
c   0.50 0.50   24.65  38.41  39.08  38.41  50.15  38.41  55.40
c   ---- ----
c              ------ ------ ------ ------ ------ ------ ------
c
c
c
c
c
c
c SENSITIVITY OF NET MFLOPS RATE TO USE OF OPTIMAL FORTRAN CODE(SISD/SIMD MODEL)
c
c   22.54  27.85  36.44  52.70  67.82   95.12  159.22  240.13  488.20
c
c    0.00   0.20   0.40   0.60   0.70    0.80    0.90    0.95    1.00
c    Fraction Of Operations Run At Optimal Fortran Rates
c
c
c1
c
c
c
c
c           TABLE OF SPEED-UP RATIOS OF LIVERMORE LOOPS MEAN RATES (72 Samples)
c
c
c           The range of speed-ups shown below as ratios of the performance
c           statistics has a very small variance compared to the enormous
c           performance ranges; the range of speed-ups are convergent estimates.
c
c
c           Arithmetic, Geometric, Harmonic Means (AM,GM,HM)
c           The Geometric Mean is the least biased statistic.
c
c --------  ----  ------   -------- -------- -------- -------- -------- --------
c SYSTEM    MEAN  MFLOPS   SX-3/14  VP2600   Y16/1C90 6000/560 9000/730 i486/25
c --------  ----  ------   -------- -------- -------- -------- -------- --------
c
c
c NEC       AM=  311.820 :    1.000    1.054    1.636   11.485   17.030  271.148
c SX-3/14   GM=   95.590 :    1.000    1.028    1.108    4.730    6.081   91.038
c F77v.012  HM=   38.730 :    1.000    0.916    0.951    2.667    2.916   42.098
c           SD=  499.780
c
c
c FUJITSU   AM=  295.790 :    0.949    1.000    1.552   10.895   16.155  257.209
c VP2600    GM=   93.030 :    0.973    1.000    1.078    4.603    5.918   88.600
c F77  V12  HM=   42.260 :    1.091    1.000    1.038    2.910    3.182   45.935
c           SD=  514.490
c
c
c CRAY      AM=  190.560 :    0.611    0.644    1.000    7.019   10.407  165.704
c Y16/1C90  GM=   86.270 :    0.903    0.927    1.000    4.269    5.488   82.162
c CF77 5.0  HM=   40.730 :    1.052    0.964    1.000    2.805    3.067   44.272
c           SD=  227.250
c
c
c IBM       AM=   27.150 :    0.087    0.092    0.142    1.000    1.483   23.609
c 6000/560  GM=   20.210 :    0.211    0.217    0.234    1.000    1.286   19.248
c XLF 2.2.  HM=   14.520 :    0.375    0.344    0.356    1.000    1.093   15.783
c           SD=   20.150
c
c
c HP        AM=   18.310 :    0.059    0.062    0.096    0.674    1.000   15.922
c 9000/730  GM=   15.720 :    0.164    0.169    0.182    0.778    1.000   14.971
c f77 8.05  HM=   13.280 :    0.343    0.314    0.326    0.915    1.000   14.435
c           SD=    9.680
c
c
c COMPAQ    AM=    1.150 :    0.004    0.004    0.006    0.042    0.063    1.000
c i486/25   GM=    1.050 :    0.011    0.011    0.012    0.052    0.067    1.000
c           HM=    0.920 :    0.024    0.022    0.023    0.063    0.069    1.000
c           SD=    0.480
c
c
c Version: 22/DEC/86  mf523           6094
c CHECK FOR CLOCK CALIBRATION ONLY:
c Total Job    Cpu Time =     4.58798E+01 Sec.
c Total 24 Kernels Time =     2.39868E+00 Sec.
c Total 24 Kernels Flops=     1.69491E+08 Flops
c
c
c
c
c**********************************************************************
c
c
c    5. SAMPLE OUTPUT FILE FROM SUBROUTINe REPORT: (IBM RS6000 Model 590)
c
c
c  
c *********************************************
c  THE LIVERMORE  FORTRAN KERNELS "MFLOPS" TEST:
c  *********************************************
c  >>> USE 72 SAMPLES LFK TEST RESULTS SUMMARY (line 330+)
c  >>> USE ALL RANGE STATISTICS FOR OFFICIAL QUOTATIONS.  
c SECOVT:     16000   .5033E-06     1.0000
c SECOVT:     32000   .5026E-06      .0013
c VERIFY:       200   .6324E-06 =  Time Resolution of Cpu-timer
c 
c         VERIFY ADEQUATE Loop SIZE VERSUS CPU CLOCK ACCURACY
c         -----     -------     -------    -------   --------
c         EXTRA     MAXIMUM     DIGITAL    DYNAMIC   RELATIVE
c         Loop      CPUTIME     CLOCK      CLOCK     TIMING  
c         SIZE      SECONDS     ERROR      ERROR     ERROR   
c         -----     -------     -------    -------   --------
c           256   .8203E-03        .00%      1.96%       .74%
c           512   .1563E-02        .00%       .01%       .26%
c          1024   .3194E-02        .00%       .85%       .35%
c          2048   .6300E-02        .00%       .24%       .13%
c          4096   .1258E-01        .00%       .17%       .03%
c          8192   .2508E-01        .00%       .04%       .02%
c         13600       Repetition Count = MULTI * Loops2 =      100.000
c         16384   .5016E-01        .00%       .03%       .01%
c         32768   .1004E+00        .00%       .04%       .00%
c         65536   .2006E+00        .00%       .01%       .00%
c         -----     -------     -------    -------   --------
c 
c 
c  CLOCK CALIBRATION TEST OF INTERNAL CPU-TIMER: SECOND
c  MONOPROCESS THIS TEST, STANDALONE, NO TIMESHARING.
c  VERIFY TIMED INTERVALS SHOWN BELOW USING EXTERNAL CLOCK
c  START YOUR STOPWATCH NOW !
c 
c            Verify  T or DT  observe external clock:
c 
c            -------     -------      ------      -----
c            Total T ?   Delta T ?    Mflops ?    Flops
c            -------     -------      ------      -----
c      1       10.00       10.00       32.67     .32673E+09
c      2       20.00       10.00       32.67     .65345E+09
c      3       30.02       10.01       32.66     .98018E+09
c      4       40.02       10.00       32.66     .13069E+10
c            -------     -------      ------      -----
c  END CALIBRATION TEST.
c 
c 
c  ESTIMATED TOTAL JOB CPU-TIME:=    85.926 sec.  ( Nruns=       7 Trials)
c 
c  Trial=      1             ChkSum=  911    Pass=      0     Fail=      0
c  Trial=      2             ChkSum=  911    Pass=      1     Fail=      0
c  Trial=      3             ChkSum=  911    Pass=      2     Fail=      0
c  Trial=      4             ChkSum=  911    Pass=      3     Fail=      0
c  Trial=      5             ChkSum=  911    Pass=      4     Fail=      0
c  Trial=      6             ChkSum=  911    Pass=      5     Fail=      0
c  Trial=      7             ChkSum=  911    Pass=      6     Fail=      0
c 1
c 
c 
c  time TEST overhead (t err): 
c 
c       RUN        AVERAGE        STANDEV        MINIMUM        MAXIMUM
c  TICK   1    .377055E-06    .299276E-09
c  TICK   2    .376780E-06    .692777E-10
c  TICK   3    .376781E-06    .677881E-10
c  TICK   4    .377010E-06    .150502E-09
c  TICK   5    .376781E-06    .707672E-10
c  TICK   6    .376797E-06    .605503E-09
c  TICK   7    .376780E-06    .692777E-10
c  DATA   7    .999866E-01    .543320E-06    .999856E-01    .999877E-01
c  DATA   7    .999859E-01    .862081E-06    .999843E-01    .999875E-01
c  TICK   7    .376855E-06    .113055E-09    .376780E-06    .377055E-06
c 
c 
c  THE EXPERIMENTAL TIMING ERRORS FOR ALL  7 RUNS
c  --  ---------  ---------  --------- -----  -----   ---
c   k   T min      T avg      T max    T err   tick   P-F
c  --  ---------  ---------  --------- -----  -----   ---
c   1  .3173E-01  .3176E-01  .3180E-01   .09%   .00%     0
c   2  .2810E-01  .2811E-01  .2815E-01   .06%   .00%     0
c   3  .8709E-02  .8712E-02  .8713E-02   .01%   .00%     0
c   4  .1354E-01  .1356E-01  .1358E-01   .10%   .00%     0
c   5  .9017E-01  .9022E-01  .9027E-01   .04%   .00%     0
c   6  .3785E-01  .3790E-01  .3797E-01   .09%   .00%     0
c   7  .3120E-01  .3121E-01  .3125E-01   .07%   .00%     0
c   8  .3995E-01  .3995E-01  .3997E-01   .02%   .00%     0
c   9  .4084E-01  .4087E-01  .4091E-01   .05%   .00%     0
c  10  .6574E-01  .6576E-01  .6581E-01   .04%   .00%     0
c  11  .4339E-01  .4342E-01  .4344E-01   .04%   .00%     0
c  12  .1479E-01  .1481E-01  .1483E-01   .11%   .00%     0
c  13  .2233E+00  .2233E+00  .2234E+00   .02%   .00%     0
c  14  .1276E+00  .1276E+00  .1277E+00   .03%   .00%     0
c  15  .1065E+00  .1065E+00  .1066E+00   .03%   .00%     0
c  16  .8737E-01  .8740E-01  .8748E-01   .05%   .00%     0
c  17  .1164E+00  .1165E+00  .1165E+00   .02%   .00%     0
c  18  .5683E-01  .5689E-01  .5695E-01   .06%   .00%     0
c  19  .7101E-01  .7104E-01  .7109E-01   .03%   .00%     0
c  20  .1417E+00  .1417E+00  .1418E+00   .02%   .00%     0
c  21  .5630E-01  .5637E-01  .5654E-01   .13%   .00%     0
c  22  .1247E+00  .1248E+00  .1248E+00   .02%   .00%     0
c  23  .6805E-01  .6808E-01  .6812E-01   .04%   .00%     0
c  24  .3823E-01  .3826E-01  .3832E-01   .08%   .00%     0
c  --  ---------  ---------  --------- -----  -----   ---
c 
c 
c  NET CPU TIMING VARIANCE (T err);  A few % is ok: 
c 
c                  AVERAGE        STANDEV        MINIMUM        MAXIMUM
c      Terr           .05%           .03%           .01%           .13%
c  
c 
c 
c  
c  ********************************************
c  THE LIVERMORE  FORTRAN KERNELS:  * SUMMARY *
c  ********************************************
c 
c               Computer : IBM RS/6000-590         
c               System   : AIX 3.2.5 beta          
c               Compiler : XLF 3.1 beta/Vast 4.3H18
c               Date     : 93.09.16                
c               Testor   : Jacob Thomas, IBM Austin
c 
c          When the computer performance range is very large 
c          the net Mflops rate of many Fortran programs and    
c          workloads will be in the sub-range between the equi-
c          weighted Harmonic and Arithmetic means depending    
c          on the degree of code parallelism and optimization. 
c          The least biased central measure is the Geometric 
c          Mean of 72 rates,  quoted +- a standard deviation.
c          Mean Mflops rates imply the average efficiency of a
c          computing system since the peak rate is well known.
c          LFK test measures a lower bound for a Multi-processor
c          and N * LFK rates project an upper bound for N-procs.
c 
c  KERNEL  FLOPS   MICROSEC   MFLOP/SEC SPAN WEIGHT  CHECK-SUMS             OK
c  ------  -----   --------   --------- ---- ------  ---------------------- --
c   1 3.0240E+06 2.8394E+04    106.4995   27   1.00  2.6985731517464893E+02 14
c   2 1.6192E+06 2.8855E+04     56.1147   15   1.00  2.7673078908323839E+02 13
c   3 1.5984E+06 1.3863E+04    115.2964   27   1.00  1.8895163625249363E+00 13
c   4 9.1200E+05 4.8647E+04     18.7473   27   1.00  4.1994754168318593E+00 13
c   5 1.6640E+06 6.6596E+04     24.9866   27   1.00  2.2278306739138266E+01 13
c   6 8.0640E+05 3.5215E+04     22.8994    8   1.00  7.8421657542732719E+00 12
c   7 5.3760E+06 2.8743E+04    187.0345   21   1.00  1.9920041523508445E+02 14
c   8 6.7392E+06 3.8995E+04    172.8203   14   1.00  2.0723805675143576E+04 12
c   9 5.3040E+06 4.2355E+04    125.2263   15   1.00  1.8367779226121049E+04 13
c  10 2.7000E+06 6.2129E+04     43.4581   15   1.00  1.1559038593888716E+04 13
c  11 9.5680E+05 3.6116E+04     26.4923   27   1.00  4.5858129350926083E+03 13
c  12 9.9840E+05 1.6807E+04     59.4047   26   1.00  1.3604051878501888E-05  9
c  13 1.3888E+06 1.9269E+05      7.2075    8   1.00  8.1274474595467102E+10  9
c  14 1.9008E+06 1.1365E+05     16.7252   27   1.00  1.8264361845046304E+07 11
c  15 1.8480E+06 1.1585E+05     15.9520   15   1.00  7.7629810169455432E+03 13
c  16 1.2320E+06 7.7707E+04     15.8544   15   1.00  1.8033120000000000E+06 16
c  17 2.8080E+06 9.8517E+04     28.5026   15   1.00  2.0631580330135154E+02 12
c  18 4.5760E+06 6.1675E+04     74.1951   14   1.00  6.7904523486391399E+03 12
c  19 2.0160E+06 5.9486E+04     33.8904   15   1.00  8.8776148863622851E+01 12
c  20 3.7856E+06 2.0545E+05     18.4257   26   1.00  4.1913992746301947E+03 12
c  21 2.0000E+07 9.3011E+04    215.0286   20   1.00  1.7539193044911301E+08 12
c  22 1.6320E+06 1.0735E+05     15.2033   15   1.00  4.2769781097853965E+01 13
c  23 4.0040E+06 6.3337E+04     63.2169   14   1.00  3.3952384219262103E+03 12
c  24 4.7840E+05 5.5553E+04      8.6116   27   1.00  9.1000000000000000E+01 16
c   1 4.0400E+06 3.6931E+04    109.3938  101   2.00  3.6773413452566024E+03 14
c   2 3.1040E+06 3.3557E+04     92.4983  101   2.00  1.0778052681679619E+04 13
c   3 2.1412E+06 1.2800E+04    167.2864  101   2.00  7.0681900560544264E+00 13
c   4 1.6800E+06 3.0418E+04     55.2301  101   2.00  4.1994754168318593E+00 13
c   5 2.2000E+06 9.6565E+04     22.7825  101   2.00  3.2123223577215447E+02 13
c   6 1.3440E+06 4.4997E+04     29.8689   32   2.00  6.0421729517423250E+02 12
c   7 7.1104E+06 3.5111E+04    202.5141  101   2.00  4.4419104210489095E+03 14
c   8 8.5536E+06 4.7954E+04    178.3717  100   2.00  1.0508876039390103E+06 12
c   9 7.2114E+06 4.7690E+04    151.2144  101   2.00  8.3261052698256506E+05 13
c  10 3.4542E+06 7.3439E+04     47.0347  101   2.00  5.1172588490282180E+05 13
c  11 1.2800E+06 5.0583E+04     25.3049  101   2.00  2.4034922852333321E+05 13
c  12 1.3600E+06 1.9936E+04     68.2197  100   2.00  4.9892984047186784E-05  9
c  13 1.8368E+06 2.5437E+05      7.2209   32   2.00  2.4799100425830582E+11 12
c  14 2.2220E+06 1.2957E+05     17.1492  101   2.00  2.1111605770909780E+08 12
c  15 3.3000E+06 2.1298E+05     15.4945  101   2.00  2.7606716832466237E+05 13
c  16 1.5120E+06 1.0039E+05     15.0620   40   2.00  2.2682870000000000E+06 16
c  17 3.6360E+06 1.3312E+05     27.3139  101   2.00  7.8024924103217118E+03 12
c  18 4.3560E+06 5.6873E+04     76.5915  100   2.00  5.2551705030608980E+05 12
c  19 2.7876E+06 8.3822E+04     33.2564  101   2.00  3.7952718721053016E+03 12
c  20 4.1600E+06 2.2651E+05     18.3658  100   2.00  2.1883436251676382E+05 12
c  21 1.2500E+07 5.5809E+04    223.9802   50   2.00  2.7413199222161126E+08 12
c  22 2.4038E+06 1.5875E+05     15.1416  101   2.00  2.0570230635969879E+03 13
c  23 5.4450E+06 8.5124E+04     63.9656  100   2.00  2.4849303510963483E+05 12
c  24 6.2000E+05 5.3257E+04     11.6416  101   2.00  3.5000000000000000E+02 16
c   1 3.5035E+06 3.1760E+04    110.3121 1001   1.00  3.5802568852572941E+05 14
c   2 2.5996E+06 2.8108E+04     92.4866  101   1.00  1.0778052681679619E+04 13
c   3 1.8018E+06 8.7117E+03    206.8264 1001   1.00  7.0052001814664635E+01 13
c   4 1.6800E+06 1.3564E+04    123.8538 1001   1.00  4.1994754168318593E+00 13
c   5 2.0000E+06 9.0216E+04     22.1690 1001   1.00  3.1842101496720381E+04 13
c   6 1.1904E+06 3.7904E+04     31.4059   64   1.00  3.0625814413202374E+04 12
c   7 6.3680E+06 3.1211E+04    204.0320  995   1.00  4.2729757526224764E+05 13
c   8 7.1280E+06 3.9950E+04    178.4229  100   1.00  1.0508876039390103E+06 12
c   9 6.1812E+06 4.0865E+04    151.2590  101   1.00  8.3261052698256506E+05 13
c  10 3.0906E+06 6.5765E+04     46.9947  101   1.00  5.1172588490282180E+05 13
c  11 1.1000E+06 4.3416E+04     25.3364 1001   1.00  2.3400376808553928E+08 13
c  12 1.2000E+06 1.4810E+04     81.0278 1000   1.00  2.0349990012896724E-04  7
c  13 1.6128E+06 2.2331E+05      7.2222   64   1.00  3.4706712065086060E+11 13
c  14 2.2022E+06 1.2764E+05     17.2533 1001   1.00  2.2156947925794350E+10 12
c  15 1.6500E+06 1.0651E+05     15.4917  101   1.00  2.7606716832466237E+05 13
c  16 1.3250E+06 8.7405E+04     15.1593   75   1.00  1.9780320000000000E+06 16
c  17 3.1815E+06 1.1646E+05     27.3175  101   1.00  7.8024924103217118E+03 12
c  18 4.3560E+06 5.6887E+04     76.5731  100   1.00  5.2551705030608980E+05 12
c  19 2.3634E+06 7.1037E+04     33.2700  101   1.00  3.7952718721053016E+03 12
c  20 2.6000E+06 1.4175E+05     18.3424 1000   1.00  2.1284510375215888E+08 11
c  21 1.2625E+07 5.6370E+04    223.9662  101   1.00  5.6017393194650853E+08 12
c  22 1.8887E+06 1.2476E+05     15.1389  101   1.00  2.0570230635969879E+03 13
c  23 4.3560E+06 6.8081E+04     63.9824  100   1.00  2.4849303510963483E+05 12
c  24 5.0000E+05 3.8261E+04     13.0682 1001   1.00  3.5000000000000000E+03 16
c  ------  -----   --------   --------- ---- ------  ---------------------- --
c  72  .2421E+09  .5436E+07     44.5395  167                               911
c 
c          MFLOPS    RANGE:             REPORT ALL RANGE STATISTICS:
c          Mean DO Span   =  167
c          Code Samples   =   72
c 
c          Maximum   Rate =    223.9802 Mega-Flops/Sec.
c          Quartile  Q3   =    106.4995 Mega-Flops/Sec.
c          Average   Rate =     68.9845 Mega-Flops/Sec.
c          Geometric Mean =     43.2106 Mega-Flops/Sec.
c          Median    Q2   =     33.2700 Mega-Flops/Sec.
c          Harmonic  Mean =     27.8395 Mega-Flops/Sec.
c          Quartile  Q1   =     17.2533 Mega-Flops/Sec.
c          Minimum   Rate =      7.2075 Mega-Flops/Sec.
c 
c 
c          Standard  Dev. =     65.1276 Mega-Flops/Sec.
c          Avg Efficiency =     19.29%  Program & Processor
c          Mean Precision =     12.65   Decimal Digits
c  <<<<<<<<<<<<<<<<<<<<<<<<<<<*>>>>>>>>>>>>>>>>>>>>>>>>>>>
c  < BOTTOM-LINE:   72 SAMPLES LFK TEST RESULTS SUMMARY. >
c  < USE RANGE STATISTICS ABOVE FOR OFFICIAL QUOTATIONS. >
c  <<<<<<<<<<<<<<<<<<<<<<<<<<<*>>>>>>>>>>>>>>>>>>>>>>>>>>>
c 1
c 
c 
c 
c 
c 
c 
c                     SENSITIVITY ANALYSIS
c 
c 
c          The sensitivity of the harmonic mean rate (Mflops)  
c          to various weightings is shown in the table below.  
c          Seven work distributions are generated by assigning 
c          two distinct weights to ranked kernels by quartiles.
c          Forty nine possible cpu workloads are then evaluated
c          using seven sets of values for the total weights:   
c 
c 
c              ------ ------ ------ ------ ------ ------ ------
c    1st QT:       O      O      O      O      O      X      X
c    2nd QT:       O      O      O      X      X      X      O
c    3rd QT:       O      X      X      X      O      O      O
c    4th QT:       X      X      O      O      O      O      O
c              ------ ------ ------ ------ ------ ------ ------
c    Total
c    Weights                    Net Mflops:
c     X    O
c   ---- ----
c 
c   1.00  .00   12.37  16.40  24.33  34.87  61.54  88.84 159.66
c 
c    .95  .05   12.84  17.10  24.53  33.99  56.90  72.77 121.16
c 
c    .90  .10   13.35  17.86  24.73  33.15  52.91  61.62  97.62
c 
c    .80  .20   14.51  19.60  25.15  31.59  46.41  47.17  70.30
c 
c    .70  .30   15.88  21.71  25.57  30.18  41.33  38.21  54.93
c 
c    .60  .40   17.55  24.34  26.01  28.88  37.25  32.11  45.08
c 
c    .50  .50   19.60  27.69  26.47  27.69  33.90  27.69  38.22
c   ---- ----
c              ------ ------ ------ ------ ------ ------ ------
c 
c 
c 
c 
c 
c 
c  SENSITIVITY OF NET MFLOPS RATE TO USE OF OPTIMAL FORTRAN CODE(SISD/SIMD MODEL
c 
c    16.48  20.08  25.71  35.70  44.32   58.41   85.66  111.72  160.56
c 
c      .00    .20    .40    .60    .70     .80     .90     .95    1.00
c     Fraction Of Operations Run At Optimal Fortran Rates
c 
c 
c 1
c 
c 
c                           TABLE OF SPEED-UP RATIOS OF MEAN RATES (72 Samples)
c 
c                           Arithmetic, Geometric, Harmonic Means (AM,GM,HM)
c                           The Geometric Mean is the least biased statistic.
c 
c  --------  ----  ------   -------- -------- -------- -------- -------- -------
c  SYSTEM    MEAN  MFLOPS   SX-3/14  RS6K-590 YMP/1    9000/730 6000/540 i486/25
c  --------  ----  ------   -------- -------- -------- -------- -------- -------
c 
c 
c  NEC       AM=  311.820 :    1.000    4.520    3.986   17.030   22.006  271.14
c  SX-3/14   GM=   95.590 :    1.000    2.212    2.610    6.081    8.909   91.03
c  F77v.012  HM=   38.730 :    1.000    1.391    2.193    2.916    5.199   42.09
c            SD=  499.780
c 
c 
c  IBM       AM=   68.984 :     .221    1.000     .882    3.768    4.868   59.98
c  RS6K-590  GM=   43.211 :     .452    1.000    1.180    2.749    4.027   41.15
c  XLF 3.1   HM=   27.839 :     .719    1.000    1.576    2.096    3.737   30.26
c            SD=   65.128
c 
c 
c  CRAY      AM=   78.230 :     .251    1.134    1.000    4.273    5.521   68.02
c  YMP/1     GM=   36.630 :     .383     .848    1.000    2.330    3.414   34.88
c  CFT771.2  HM=   17.660 :     .456     .634    1.000    1.330    2.370   19.19
c            SD=   86.750
c 
c 
c  HP        AM=   18.310 :     .059     .265     .234    1.000    1.292   15.92
c  9000/730  GM=   15.720 :     .164     .364     .429    1.000    1.465   14.97
c  f77 8.05  HM=   13.280 :     .343     .477     .752    1.000    1.783   14.43
c            SD=    9.680
c 
c 
c  IBM       AM=   14.170 :     .045     .205     .181     .774    1.000   12.32
c  6000/540  GM=   10.730 :     .112     .248     .293     .683    1.000   10.21
c  XL v0.90  HM=    7.450 :     .192     .268     .422     .561    1.000    8.09
c            SD=    9.590
c 
c 
c  COMPAQ    AM=    1.150 :     .004     .017     .015     .063     .081    1.00
c  i486/25   GM=    1.050 :     .011     .024     .029     .067     .098    1.00
c            HM=     .920 :     .024     .033     .052     .069     .123    1.00
c            SD=     .480
c 1
c 
c  Version: 22/DEC/86  mf523           6094
c  CHECK FOR CLOCK CALIBRATION ONLY: 
c  Total Job    Cpu Time =     8.74765E+01 Sec.
c  Total 24 Kernels Time =     3.80540E+01 Sec.
c  Total 24 Kernels Flops=     1.69491E+09 Flops
c 
c
c**********************************************************************
c
c
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
cout  DOUBLE  PRECISION  sum                                            REDUNDNT
c
c/      PARAMETER( kn= 47, kn2= 95, np= 3, ls= 3*47, krs= 24)
c/      PARAMETER( nk= 47, nl= 3, nr= 8 )
      parameter(  nt= 4 )
c
      CHARACTER  NAME*8
      CHARACTER  Komput*24, Kontrl*24, Kompil*24, Kalend*24, Identy*24
c
      COMMON /SYSID/ Komput, Kontrl, Kompil, Kalend, Identy
      COMMON /ALPHA/ mk,ik,im,ml,il,Mruns,Nruns,jr,iovec,NPFS(8,3,47)
      COMMON /BETA / tic, TIMES(8,3,47), SEE(5,3,8,3),
     1              TERRS(8,3,47), CSUMS(8,3,47),
     2              FOPN(8,3,47), DOS(8,3,47)
c
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), ticks,
     1                FR(9), TERR1(47), SUMW(7), START,
     2              SKALE(47), BIAS(47), WS(95), TOTAL(47), FLOPN(47),
     3                IQ(7), NPF, NPFS1(47)
c
      COMMON /SPACEI/ WTP(3), MUL(3), ISPAN(47,3), IPASS(47,3)
c
      DIMENSION  NAME(nt), RATE(nt)
      DIMENSION  FLOPS(141), TR(141), RATES(141)
      DIMENSION  LSPAN(141), WG(141), OSUM (141), ID(141)
      DIMENSION  HM(12), LVL(10)
      DIMENSION  LQ(5), STAT1(20), STAT2(20)
      DIMENSION  IN(141), CSUM1(141), TV4(141), TV5(141)
      DIMENSION  MAP1(141), MAP2(141), MAP3(141), IN2(141), VL1(141)
      DIMENSION  MAP(141), VL(141), TV(141), TV1(141), TV2(141)
      DIMENSION  FLOPS1(141), RT1(141), ISPAN1(141), WT1(141)
      DIMENSION  FLOPS2(141), RT2(141), ISPAN2(141), WT2(141)
      SAVE    kall,  LVL
c
       MODI(i,mm)= (MOD( ABS(i)-1, mm) + 1)
c
      DATA  kall/0/
c
      CALL TRACE ('REPORT  ')
c
          IF( iou.LT.0) GO TO 73
c
            meff= 0
            neff= 0
            fuzz= 1.0d-9
       DO 1000 k= 1,ntk
           VL(k)= LSPAN(k)
 1000  CONTINUE
c
              bl= 1.0d-5
              bu= 1.0d+5
            CALL  VALID( TV,MAP,neff,  bl, RATES, bu, ntk)
c
c      Compress valid data sets mapping on MAP.
c
              nd= 0
        DO  1  k= 1,neff
         MAP1(k)=  MODI( MAP(k),nek)
       FLOPS1(k)= FLOPS( MAP(k))
          RT1(k)=    TR( MAP(k))
          VL1(k)=    VL( MAP(k))
       ISPAN1(k)= LSPAN( MAP(k))
          WT1(k)=    WG( MAP(k))
          TV1(k)= RATES( MAP(k))
        CSUM1(k)=  OSUM( MAP(k))
              nd=    ID( MAP(k)) + nd
    1  continue
              IF( nd .LE. 8*neff )  nd= nd - 16*((neff-1+24)/24)
          precis= REAL(nd)/( REAL(neff) + fuzz)
c
             som= 0.00d0
             sum= 0.00d0
        DO 11  k= 1,neff
             som= som + FLOPS1(k)
             sum= sum + RT1(k)
   11  continue
           rneto= som/(sum + fuzz)
c
            CALL  STATW( STAT1,TV,IN, VL1,WT1,neff)
              lv= STAT1(1)
c
            CALL  STATW( STAT1,TV,IN, TV1,WT1,neff)
             twt= STAT1(6)
c                             compute average efficiency= GM/Max
            kall= kall +  1
            peak= 0.00d0
              if( kall.LE.1 .OR. il.EQ.im ) then
                  peak= STAT1(4)
              endif
          avgeff= (100.0d0* STAT1(10))/( peak + fuzz)
c
          WRITE ( iou,7001)
          WRITE ( iou,7001)
          WRITE ( iou,7001)
          WRITE ( iou,7001)
          WRITE ( iou,7001)
          WRITE ( iou,7001)
       CALL PAGE( iou)
          WRITE ( iou,7002)
c
      IF( ntk .EQ. nek )  THEN
          WRITE ( iou,7003)
      ELSE
          WRITE ( iou,7090)
      ENDIF
c
          WRITE ( iou,7002)
          WRITE ( iou,7007)  Komput
          WRITE ( iou,7057)  Kontrl
          WRITE ( iou,7008)  Kompil
          WRITE ( iou,7038)  Kalend
          WRITE ( iou,7039)  Identy
          WRITE ( iou,7061)
          WRITE ( iou,7062)
          WRITE ( iou,7063)
          WRITE ( iou,7064)
          WRITE ( iou,7065)
          WRITE ( iou,7066)
          WRITE ( iou,7067)
          WRITE ( iou,7071)
          WRITE ( iou,7072)
          WRITE ( iou,7068)
          WRITE ( iou,7069)
c         WRITE ( iou,7001)
          WRITE ( iou,7004)
          WRITE ( iou,7005)
          WRITE ( iou,7011) (MAP1(k),  FLOPS1(k), RT1(k), TV1(k),
     1                    ISPAN1(k), WT1(k), CSUM1(k), ID(k), k=1,neff)
          WRITE ( iou,7005)
c
          WRITE ( iou,7023)  neff, som, sum, rneto, lv, nd
          WRITE ( iou,7022)
          WRITE ( iou,7009)  lv
          WRITE ( iou,7010)  ntk
          WRITE ( iou,7041)  STAT1( 4)
          WRITE ( iou,7037)  STAT1(14)
          WRITE ( iou,7033)  STAT1( 1)
          WRITE ( iou,7043)  STAT1(10)
          WRITE ( iou,7030)  STAT1( 7)
          WRITE ( iou,7055)  STAT1( 5)
          WRITE ( iou,7036)  STAT1(13)
          WRITE ( iou,7042)  STAT1( 3)
          WRITE ( iou,7001)
          WRITE ( iou,7044)  STAT1( 2)
          WRITE ( iou,7091)  avgeff
          WRITE ( iou,7034)  precis
c
      IF( ntk .NE. nek )  THEN
          WRITE (   *,7001)
          WRITE (   *,7002)
          WRITE (   *,7090)
          WRITE (   *,7002)
          WRITE (   *,7007)  Komput
          WRITE (   *,7057)  Kontrl
          WRITE (   *,7008)  Kompil
          WRITE (   *,7038)  Kalend
          WRITE (   *,7039)  Identy
          WRITE (   *,7022)
          WRITE (   *,7009)  lv
          WRITE (   *,7010)  ntk
          WRITE (   *,7041)  STAT1( 4)
          WRITE (   *,7037)  STAT1(14)
          WRITE (   *,7033)  STAT1( 1)
          WRITE (   *,7043)  STAT1(10)
          WRITE (   *,7030)  STAT1( 7)
          WRITE (   *,7055)  STAT1( 5)
          WRITE (   *,7036)  STAT1(13)
          WRITE (   *,7042)  STAT1( 3)
          WRITE (   *,7001)
          WRITE (   *,7044)  STAT1( 2)
          WRITE (   *,7091)  avgeff
          WRITE (   *,7034)  precis
      ENDIF
c
c         WRITE ( iou,7031)  STAT1( 9)
c         WRITE ( iou,7032)  STAT1(15)
c
 7001 FORMAT(/)
 7002 FORMAT(  ' ********************************************' )
 7003 FORMAT(  ' THE LIVERMORE  FORTRAN KERNELS:  M F L O P S'  )
 7090 FORMAT(  ' THE LIVERMORE  FORTRAN KERNELS:  * SUMMARY *'  )
 7004 FORMAT(/,' KERNEL  FLOPS   MICROSEC   MFLOP/SEC SPAN WEIGHT  CH',
     1'ECK-SUMS             OK' )
 7005 FORMAT(  ' ------  -----   --------   --------- ---- ------  --',
     1'-------------------- --' )
 7007 FORMAT(/,9X,'     Computer : ' ,A )                               f77
 7057 FORMAT(  9X,'     System   : ' ,A )                               f77
 7008 FORMAT(  9X,'     Compiler : ' ,A )                               f77
 7038 FORMAT(  9X,'     Date     : ' ,A )                               f77
 7039 FORMAT(  9X,'     Testor   : ' ,A )                               f77
c7007 FORMAT(/,9X,16H     Computer :  ,A8)                               f66
c7057 FORMAT(  9X,16H     System   :  ,A8)                               f66
c7008 FORMAT(  9X,16H     Compiler :  ,A8)                               f66
c7038 FORMAT(  9X,16H     Date     :  ,A8)                               f66
 7009 FORMAT(  9X,'Mean DO Span   ='  ,I5)
 7010 FORMAT(  9X,'Code Samples   ='  ,I5)
 7011 FORMAT(1X,i2,1PE11.4,E11.4,0PF12.4,1X,I4,1X,F6.2,1PE24.16,1X,I2)
c7011 FORMAT(1X,i2,E11.4,E11.4,F12.4,1X,I4,1X,F6.2,E35.25,1X,I2)
 7012 FORMAT(1X,i2,E11.4,E11.4,F12.4,1X,I4,1X,F6.2)
 7023 FORMAT(1X,i2,E11.4,E11.4,F12.4,1X,I4,30X,I4)
c7022 FORMAT(/,15H MFLOPS  RANGE:,23X,28HREPORT ALL RANGE STATISTICS: )  f66
 7022 FORMAT(/,9X,'MFLOPS    RANGE:',13X,'REPORT ALL RANGE STATISTICS:') f77
 7041 FORMAT(/,9X,'Maximum   Rate ='  ,F12.4,' Mega-Flops/Sec.' )
 7037 FORMAT(  9X,'Quartile  Q3   ='  ,F12.4,' Mega-Flops/Sec.' )
 7033 FORMAT(  9X,'Average   Rate ='  ,F12.4,' Mega-Flops/Sec.' )
 7043 FORMAT(  9X,'GEOMETRIC MEAN ='  ,F12.4,' Mega-Flops/Sec.' )
 7030 FORMAT(  9X,'Median    Q2   ='  ,F12.4,' Mega-Flops/Sec.' )
 7055 FORMAT(  9X,'Harmonic  Mean ='  ,F12.4,' Mega-Flops/Sec.' )
 7036 FORMAT(  9X,'Quartile  Q1   ='  ,F12.4,' Mega-Flops/Sec.' )
 7042 FORMAT(  9X,'Minimum   Rate ='  ,F12.4,' Mega-Flops/Sec.' )
 7044 FORMAT(  9X,'Standard  Dev. ='  ,F12.4,' Mega-Flops/Sec.' )
c7031 FORMAT(  9X,16HMedian    Dev. =  ,F12.4,16H Mega-Flops/Sec. )
c7032 FORMAT(  9X,16HGeom.Mean Dev. =  ,F12.4,16H Mega-Flops/Sec. )
 7091 FORMAT(  9X,'Avg Efficiency ='  ,F10.2,'%  Program & Processor')
 7034 FORMAT(  9X,'Mean Precision ='  ,F10.2,'   Decimal Digits' )
 7053 FORMAT(/,9X,'Frac.  Weights ='  ,F12.4)
 7104 FORMAT(/,' KERNEL  FLOPS   MICROSEC   MFLOP/SEC SPAN WEIGHT '  )
 7105 FORMAT(  ' ------  -----   --------   --------- ---- ------ '  )
c
 7061 FORMAT(/,9X,'When the computer performance range is very large ')
 7062 FORMAT(9X,'the net Mflops rate of many Fortran programs and    ')
 7063 FORMAT(9X,'workloads will be in the sub-range between the equi-')
 7064 FORMAT(9X,'weighted Harmonic and Arithmetic means depending    ')
 7065 FORMAT(9X,'on the degree of code parallelism and optimization. ')
c7066 FORMAT(9X,52HMore accurate estimates of cpu workload rates depend)
c7067 FORMAT(9X,52Hon assigning appropriate weights for each kernel.   )
c7066 FORMAT(9X,52HThe best central measure is the Geometric Mean of 72)
c7067 FORMAT(9X,52Hrates which must be quoted +- a standard deviation. )
 7066 FORMAT(9X,'The least biased central measure is the Geometric ')
 7067 FORMAT(9X,'Mean of 72 rates,  quoted +- a standard deviation.')
 7068 FORMAT(9X,'LFK test measures a lower bound for a Multi-processor')
 7069 FORMAT(9X,'and N * LFK rates project an upper bound for N-procs.')
 7071 FORMAT(9X,'Mean Mflops rates imply the average efficiency of a')
 7072 FORMAT(9X,'computing system since the peak rate is well known.')
c
      NAME(1)= Komput
      NAME(2)= Komput
      NAME(3)= Kompil
      RATE(1)= STAT1(1)
      RATE(2)= STAT1(10)
      RATE(3)= STAT1(5)
      RATE(4)= STAT1(2)
c
      IF( ntk .NE. nek )  THEN
      WRITE( iou,7099)
      WRITE( iou,7097)
      WRITE( iou,7098)
      WRITE( iou,7099)
 7097 FORMAT(' < BOTTOM-LINE:   72 SAMPLES LFK TEST RESULTS SUMMARY. >')
 7098 FORMAT(' < USE RANGE STATISTICS ABOVE FOR OFFICIAL QUOTATIONS. >')
 7099 FORMAT(' <<<<<<<<<<<<<<<<<<<<<<<<<<<*>>>>>>>>>>>>>>>>>>>>>>>>>>>')
       CALL PAGE( iou)
c
       IF( iovec.EQ.1 ) THEN
          WRITE ( iou,7070)
 7070 FORMAT(//,' TOP QUARTILE: BEST ARCHITECTURE/APPLICATION MATCH' )
c
c      Compute compression index-list MAP1:  Non-zero weights.
c
              bl= 1.0d-6
              bu= 1.0d+6
            CALL  VALID( TV,MAP1,meff,  bl, WT1, bu, neff)
c
c      Re-order data sets mapping on IN (descending order of MFlops).
c
        DO  2  k= 1,meff
         MAP3(k)=     IN( MAP1(k))
    2  continue
c
          IF( meff.GT.0 )  THEN
              CALL TRAP( MAP3, ' REPORT ' , 1, neff,meff)
          ENDIF
c
        DO  3  k= 1,meff
               i=   MAP3(k)
        FLOPS2(k)=  FLOPS1(i)
          RT2(k)=    RT1(i)
       ISPAN2(k)= ISPAN1(i)
          WT2(k)=    WT1(i)
          TV2(k)=    TV1(i)
         MAP2(k)=   MODI( MAP(i),nek)
    3  continue
c                             Sort kernels by performance into quartiles
              nq= meff/4
              lo= meff -4*nq
           LQ(1)= nq
           LQ(2)= nq + nq + lo
           LQ(3)= nq
              i2= 0
c
         DO 5  j= 1,3
              i1= i2 + 1
              i2= i2 + LQ(j)
              ll= i2 - i1 + 1
            CALL  STATW( STAT2,TV,IN2, TV2(i1),WT2(i1),ll)
            frac= STAT2(6)/( twt +fuzz)
c
          WRITE ( iou,7001)
          WRITE ( iou,7104)
          WRITE ( iou,7105)
          WRITE ( iou,7012) ( MAP2(k),  FLOPS2(k), RT2(k), TV2(k),
     1                         ISPAN2(k), WT2(k),  k=i1,i2 )
          WRITE ( iou,7105)
c
          WRITE ( iou,7053)  frac
          WRITE ( iou,7033)  STAT2(1)
          WRITE ( iou,7055)  STAT2(5)
          WRITE ( iou,7044)  STAT2(2)
    5 continue
c
       ENDIF
c
      ENDIF
c
c           Sensitivity analysis of harmonic mean rate to 49 workloads
c
      CALL  SENSIT(   iou,RATES,WG,IQ,SUMW, MAP,TV,TV4,TV2,TV5, ntk)
c
c
c           Sensitivity analysis of harmonic mean rate to SISD/SIMD model
c
      CALL  SIMD( HM, iou,RATES,WG,FR,9,    MAP,TV,TV4,TV2, ntk)
c
c
      IF( ntk .NE. nek )  THEN
        IF( iovec.EQ.1 )  THEN
               CALL  PAGE( iou)
               mrl= Nruns
                IF( Nruns.gt.8) mrl= 8
c
      DO  8      k= 1,mk
      DO  8      j= im,ml
               sum= 0.0d0
      DO  8      i= 1,mrl
               sum= sum + CSUMS(i,j,k)
      CSUMS(i,j,k)= sum
    8 continue
c
      DO  10     i= 1,mrl
                IF( (i.NE.1).AND.(i.NE.mrl))  GO TO 10
             WRITE( iou,76) i
             WRITE( iou,77)  ( LVL(j), j= 1,3 )
   76       FORMAT( //,'  Cumulative Checksums:  RUN=',i5)
   77       FORMAT( /,'  k    VL=',i5,3i24)
c
      DO  9      k= 1,mk
             WRITE( iou,78)  k, ( CSUMS(i,j,k), j= 1,3)
   78       FORMAT( 1X,I2,4E24.16)
    9 continue
   10 continue
        ENDIF
c
      CALL SPEDUP( iou, NAME, RATE )
      ENDIF
          LVL(il)= lv
   73 CONTINUE
      CALL TRACK ('REPORT  ')
      RETURN
c
      END
c**********************************************
      SUBROUTINE RESULT( iou,FLOPS,TR,RATES,LSPAN,WG,OSUM,TERR,ID)
c***********************************************************************
c                                                                      *
c     RESULT -  Computes timing Results into pushdown store.           *
c                                                                      *
c      iou   -  Input   IO unit number for print output                *
c     FLOPS  - Out.Ary  Number of Flops executed by each kernel        *
c     TR     - Out.Ary  Time of execution of each kernel(microsecs)    *
c     RATES  - Out.Ary  Rate of execution of each kernel(megaflops/sec)*
c     LSPAN  - Out.Ary  Span of inner DO loop in each kernel           *
c     WG     - Out.Ary  Weight assigned to each kernel for statistics  *
c     OSUM   - Out.Ary  Checksums of the results of each kernel        *
c     TERR   - Out.Ary  Experimental timing errors per kernel          *
c     ID     - Out.Ary  Number of valid digits in checksum.            *
c                                                                      *
c***********************************************************************
c
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
cout  DOUBLE  PRECISION  SUMS, cs                                       REDUNDNT
c
c/      PARAMETER( kn= 47, kn2= 95, np= 3, ls= 3*47, krs= 24)
c/      PARAMETER( nk= 47, nl= 3, nr= 8 )
c
      COMMON /ALPHA/ mk,ik,im,ml,il,Mruns,Nruns,jr,iovec,NPFS(8,3,47)
      COMMON /TAU/   tclock, tsecov, testov, cumtim(4)
      COMMON /BETA / tic, TIMES(8,3,47), SEE(5,3,8,3),
     1              TERRS(8,3,47), CSUMS(8,3,47),
     2              FOPN(8,3,47), DOS(8,3,47)
c
      DIMENSION  FLOPS(141), TR(141), RATES(141), ID(141)
      DIMENSION  LSPAN(141), WG(141), OSUM (141), TERR(141)
c
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), ticks,
     1                FR(9), TERR1(47), SUMW(7), START,
     2              SKALE(47), BIAS(47), WS(95), TOTAL(47), FLOPN(47),
     3                IQ(7), NPF, NPFS1(47)
c
      COMMON /SPACEI/ WTP(3), MUL(3), ISPAN(47,3), IPASS(47,3)
c
      COMMON /SPACES/ ion,j5,k2,k3,MULTI,laps,Loop,m,kr,LP,n13h,ibuf,nx,
     1 L,npass,nfail,n,n1,n2,n13,n213,n813,n14,n16,n416,n21,nt1,nt2,
     2 last,idebug,mpy,Loops2,mucho,mpylim, intbuf(16)
c
      COMMON /PROOF/  SUMS(24,3,8)
c
c
      CALL TRACE ('RESULT  ')
c
           CALL  TALLY( iou, 1 )
c
c                             Push Result Arrays Down before entering new result
            isum= 0
           limit= 141 - mk
              j = 141
      DO 1001 k = limit,1,-1
        FLOPS(j)= FLOPS(k)
           TR(j)=    TR(k)
        RATES(j)= RATES(k)
        LSPAN(j)= LSPAN(k)
           WG(j)=    WG(k)
         OSUM(j)=  OSUM(k)
         TERR(j)=  TERR(k)
           ID(j)=    ID(k)
              j = j - 1
 1001 CONTINUE
c
c                             CALCULATE MFLOPS FOR EACH KERNEL
c                          setting RATES(k)= 0. deletes kernel k from REPORT.
            tmin= 1.0d0*tsecov
      DO 1010 k = 1,mk
        FLOPS(k)= FLOPN(k)*TOTAL(k)
           TR(k)=  TIME(k) * 1.0d+6
        RATES(k)= 0.0d0
              IF( TR(k).NE. 0.0d0)   RATES(k)= FLOPS(k)/TR(k)
              IF( WT(k).LE. 0.0d0)   RATES(k)= 0.0d0
              IF( TIME(k).LT.tmin)   RATES(k)= 0.0d0
              IF( TIME(k).LE. 0.0d0) RATES(k)= 0.0d0
        LSPAN(k)= ISPAN(k,il)
           WG(k)= WT(k)*WTP(il)
         OSUM(k)= CSUM(k)
         TERR(k)= TERR1(k)
c
c                 compute relative error and digits of precision in CSUM
c
c
                           ijk= 4
      IF( MULTI.LE.   1 )  ijk= 1
      IF( MULTI.EQ.  10 )  ijk= 2
      IF( MULTI.EQ.  50 )  ijk= 3
      IF( MULTI.GE. 100 )  ijk= 4
              cs= REAL( Nruns) * SUMS(k,il,ijk)
        TERR1(k)= cs
 1010 CONTINUE
c
      CALL SEQDIG( ID, isum, TERR1, CSUM, mk)
c
      CALL TRACK ('RESULT  ')
      RETURN
      END
c
c
c**********************************************
      FUNCTION   SECND2( OLDSEC)
c***********************************************
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
cLOX  REAL*8 SECOND
c
c     SECOND= Cumulative CPU time for job in seconds.  MKS unit is seconds.
c             Clock resolution should be less than 2% of Kernel 11 run-time.
c             ONLY CPU time should be measured, NO system or I/O time included.
c             In VM systems, page-fault time must be avoided (Direction 8).
c             SECOND accuracy may be tested by calling: CALIBR test.
c
c     IF your system provides a timing routine that satisfies
c     the definition above; THEN simply delete this function.
c
c     ELSE this function must be programmed using some
c     timing routine available in your system.
c     Timing routines with CPU-clock resolution are always  sufficient.
c     Timing routines with microsec. resolution are usually sufficient.
c
c     Timing routines with much less resolution have required the use
c     of multiple-pass loops around each kernel to make the run time
c     at least 50 times the tick-period of the timing routine.
c     Function SECOVT measures the overhead time for a call to SECOND.
c
c     If no CPU timer is available, then you can time each kernel by
c     the wall clock using the PAUSE statement at the end of func. TEST.
c
c     An independent calibration of the running time may be wise.
c     Compare the Total Job Cpu Time printout at end of the LFK output file
c     with the job Cpu time charged by your operating system.
c
c     Default, uni-processor tests measure job  Cpu-time in SECOND (TSS mode).
c     Parallel processing tests should measure Real-time in stand-alone mode.
c
c     The following statement is deliberately incomplete:
c
c      SECOND=                                                            sdef
c               USE THE HIGHEST RESOLUTION CPU-TIMER FUNCTION AVAILABLE
c*******************************************************************************
c
c     The following statements were used on  UNIX 4.2bsd systems, e.g.  SUN
c     Time Resolution of ETIME is poor= 0.01 Sec.
c
c
c        REAL*4 CPUTYM(4), ETIME                                          unix
c        XT= ETIME( CPUTYM)                                               unix
c        SECOND=    CPUTYM(1)                                             unix
         REAL*4 CPUTYM(4)
         REAL*8 DTIME
         EXTERNAL DTIME
         TX=DTIME( CPUTYM)
         SECND2= TX
c
c        REAL*8 CPUTYM(4), MYETIME                                        unix
c        SECOND=    MYETIME(CPUTYM(1))                                    unix
c
c#include <sys/types.h>
c#include <sys/time.h>
c
cdouble myetime_(arg)
cdouble *arg;
c{
c   clock_t clock();  /* Returns Micro-Seconds. */
c   double foo;
c
c   foo = (double)clock()/(double)CLOCKS_PER_SEC;
c   return( foo );
c}
c
c
c or
c        REAL*4 XTIME(4)                                                  unix
c        INTEGER    CLOCK                                                 unix
c        EXTERNAL   CLOCK                                                 unix
c        XT = REAL( CLOCK( XTIME)) * 1.00d-6                              unix
c        SECOND=  XT                                                      unix
c
c*******************************************************************************
c
c     The following statements were used on the DEC  VAX/780  VMS 3.0 .
c     Enable page-fault tallys in TEST by un-commenting LIB$STAT_TIMER calls.
c     Clock resolution is 0.01 Sec.
c
c       DATA  INITIA   /123/
c       IF(   INITIA.EQ.123 )  THEN
c             INITIA= 1
c             NSTAT = LIB$INIT_TIMER()
c       ELSE
c             NSTAT = LIB$STAT_TIMER(2,ISEC)
c             SECOND= REAL(ISEC)*0.01 - OLDSEC
c       ENDIF
c
c* OR less accurately:
c*        REAL    SECNDS
c*        SECOND= SECNDS( OLDSEC)
c
c*****************************************************************************
c
c     The following statements were used on the IBM RS/6000
c     Contrary to what the manual states, INTEGER FUNCTION MCLOCK()
c     returns the number of ticks with 100 ticks being one second.
c
cIBMRS          integer itemp, MCLOCK
cIBMRS          external MCLOCK
c
cIBMRS          itemp = MCLOCK()
cIBMRS          SECOND= REAL(itemp)/100.00d0
c
c*******************************************************************************
c     The following statements were used on the DEC PDP-11/23 RT-11 system.
c
c*       DIMENSION JT(2)
c*       CALL GTIM(JT)
c*       TIME1 = JT(1)
c*       TIME2 = JT(2)
c*       TIME = TIME1 * 65768. + TIME2
c*       SECOND=TIME/60. - OLDSEC
c*******************************************************************************
c
c     The following statements were used on the Hewlett-Packard HP 9000
c
c*       INTEGER*4 ITIME(4)
c*       CALL TIMES( ITIME(4))
c*       TIMEX= ITIME(1) + ITIME(2) + ITIME(3) + ITIME(4)
c*       SECOND= TIMEX/60. - OLDSEC
c
c*******************************************************************************
c
c     FOR THE GOULD 32/87 WITH MPX 3.2  (et seq. gratis D.Lindsay)
c
c     INTEGER*4 NSEC, NCLICK
c     REAL*8 CPUTIM
c
c      CALL M:CLOCK (NSEC, NCLICK)
c      CPUTIM = FLOAT(NSEC)
c      SECOND = CPUTIM + FLOAT(NCLICK)/60.
c
c*******************************************************************************
c
c  FOR THE HP 1000 RUNNING FORTRAN 77.
c  note that since the hp operating system has no facility for
c  returning cpu time, this routine only measures elapsed time.
c  therefore, the tests must be run stand-alone.
c
c     REAL*8 TOTIME
c     INTEGER*2 TIMEA(5)
c
c     CALL EXEC (11, TIMEA)
c     TOTIME = DBLE (TIMEA(1))/100.
c     TOTIME = TOTIME + DBLE (TIMEA(2))
c     TOTIME = TOTIME + DBLE (TIMEA(3)) * 60.
c     SECOND = TOTIME + DBLE (TIMEA(4)) * 3600.
c
c*******************************************************************************
c
c     FOR THE PR1ME SYSTEM UNDER PRIMOS
c
c     REAL*8 CPUTIM
c     INTEGER*2 TIMERS (28)
c
c     CALL TMDAT (TIMERS)
c     SECOND = DBLE (TIMERS(7))
c    .+ DBLE(TIMERS(8)) / DBLE(TIMERS(11))
c
c*******************************************************************************
c
c     The following statements were used on the Stellar
c
c      REAL DUMMY(8)
c      INTEGER*4 TIMES$
c      SAVE IOFSET
c      ITIME= TIMES$( DUMMY)
c      IF( IOFSET.EQ.0 )  IOFSET= ITIME
c      SECOND= (ITIME - IOFSET)/100.0  - OLDSEC
c*******************************************************************************
c
c     The following statements were used on the IBM 3090 VM system.
c     Clock resolution is 1 microsec.
c
c      SECOND= IOCPU(0.0d0)* 1.0d-6
c
c*******************************************************************************
c
c     The following statement was used on the IBM 3090  MVS
c
c**   CALL TODD( xtime)
c     TODD returns microsecs in REAL*8 form
c     TODD provides 1/16th of a microsecond precision
c**   xtime = xtime * 1.0D-6
c     SECOND= xtime - oldsec
c
c********************************
c     REAL*4 TIME(4)
c     xtime = 0.0D-6
c     CALL VCLOCK(time(1))
c     xtime = time(1)
c     SECOND= xtime - oldsec
c
c********************************
c     The following statement was used on the IBM 4381, 9370
c
c     real*8 elapsed(2),cpu(2)
c     call timer(elapsed,cpu)
c     second = cpu(1) - oldsec
c
c
c*******************************************************************************
c
c     The following statements were used on the IBM PC Professional Fortran.
c     Clock resolution is 0.01 Sec.
c
c      INTEGER*2 IHR,IMIN,ISEC,IS100
c      CALL GETTIM(IHR,IMIN,ISEC,IS100)
c      ISECT=(JFIX(IHR)*60+JFIX(IMIN))*60+JFIX(ISEC)
c      SECOND=FLOAT(ISECT)+FLOAT(IS100)/100.0
c
c*******************************************************************************
c
c     THE FOLLOWING STATEMENTS ARE USED ON IBM-PC WITH LAHEY COMPILER
c**   SECOND= REAL( MOD( ITICKS, 1000000)) * 1.0D-2
c
c**   INTEGER*4   ITICKS
c**   CALL TIMER( ITICKS)
c**   SECOND= REAL( ITICKS ) * 1.0D-2
c
c      INTEGER*4  I1, ITICK0, ITICKS
c      SAVE I1, ITICK0
c      DATA I1/-357/, ITICK0/0/
cC
c      IF(  I1.EQ.(-357)) THEN
c         CALL  TIMER( ITICK0)
c      ENDIF
c           I1 = 7
c         CALL  TIMER( ITICKS)
c       SECOND = REAL( ITICKS - ITICK0 ) * 1.0D-2
c
c
c*******************************************************************************
c
c  FOR THE IBM PC.
c  note that the pc's operating system has no facility for
c  returning cpu time; this routine only measures elapsed time.
c  also, the pc does not have real*8.  Remove all references to real*8
c
c      IMPLICIT INTEGER*4 (I-N)
c      LOGICAL FIRST
c      DATA FIRST /.TRUE./
c
c      CALL GETTIM (IYEAR, IMONTH, IDAY, IHOUR, IMIN, ISEC, IFRACT)
c
c  ifract is integer fractions of a second
c  in units of 1/32,768 seconds
c
c      IF (.NOT. FIRST) GO TO 10
c        FIRST = .FALSE.
c
c        LASTHR = IHOUR
c        BASETM = 0.
c10    CONTINUE
c
c  because of limited precision, do not include the time of day
c  in hours in the total time.  but correct for an hour change.
c
c      IF (LASTHR .EQ. IHOUR) GO TO 20
c        BASETM = BASETM + 3600.
c        LASTHR = IHOUR
c
c20    TOTIME = FLOAT(IMIN) * 60
c    . + FLOAT(ISEC)
c    . + FLOAT(IFRACT)/32768.
c      SECOND = TOTIME + BASETM
c
c
      RETURN
      END
c
c
c
c
c***********************************************************************
      FUNCTION  SECOVT( iou )
c***********************************************************************
c                                                                      *
c     SECOVT  - Measures the Overhead time for calling SECOND
c      toler  - tolerance for convergence= Relative error :  0.02
c        iou  - I/O unit number
c***********************************************************************
c
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
cLOX  REAL*8 SECOND
c
      DIMENSION   TIM(20), TER(20), TMX(20), INX(20)
      COMMON /FAKE1/ t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11(20),t12(20)
      COMMON /FAKE2/ tcum(20)
c
      CALL TRACE ('SECOVT  ')
c
c***********************************************************************
c     Measure  tsecov:  Overhead time for calling SECOND
c***********************************************************************
c
         tseco= 0.000d0
           klm= 1600
            io= ABS(iou)
            jj= 0
c
      DO 820 j= 1,15
c
      DO 803 i= 1,10
        t12(i)= 0.000d0
  803 continue
       tcum(1)= 0.000d0
            t0= SECND2( tcum(1))
c                       assure that 10 calls to SECOND are NOT optimized
      DO 810 k= 1,klm
      DO 805 i= 1,10
       tcum(i)= t12(i)
  805 continue
            t1= SECND2( tcum(1))
            t2= SECND2( tcum(2))
            t3= SECND2( tcum(3))
            t4= SECND2( tcum(4))
            t5= SECND2( tcum(5))
            t6= SECND2( tcum(6))
            t7= SECND2( tcum(7))
            t8= SECND2( tcum(8))
            t9= SECND2( tcum(9))
           t10= SECND2( tcum(10))
  810 continue
        elapst= t10 - t0
         tseco= elapst/( REAL(10*klm) + 1.0e-9)
         toler= 0.020d0
          rerr= 1.000d0
c
c                                  Convergence test:  Rel.error .LT. 1%
            IF( elapst.GT. 1.00d04 ) GO TO 911
            IF( elapst.LT. 1.00d-10 .AND. j.GT.10 ) GO TO 911
            IF( elapst.GT. 1.00d-9 ) THEN
                     jj= jj + 1
                TIM(jj)= tseco
                     IF( jj.GT.1 ) THEN
                         rerr= RELERR( TIM(jj), TIM(jj-1))
                     ENDIF
                TER(jj)= rerr
            ENDIF
c
            IF( iou.GT.0 ) THEN
         WRITE( iou,64) 10*klm,  tseco, rerr
            ENDIF
            IF( rerr  .LT. toler   ) GO TO 825
            IF( elapst.GT. 10.00d0 ) GO TO 822
           klm= klm + klm
  820 continue
c                                  Poor accuracy on exit from loop
  822     IF( j .LE. 1 )  GO TO 911
          IF( jj.LT. 1 )  GO TO 911
         CALL SORDID( INX,TMX,  TER,jj,1)
c
           i= 0
  823      i= i + 1
       tseco= TIM( INX(i))
        rerr= TMX(i)
          IF( tseco.LE. 0.00d0 .AND. i.LT.jj ) GO TO 823
c
          IF(  rerr.GT. 0.100d0 ) THEN
               WRITE( io,63)  100.00d0 * rerr
          ENDIF
c                                  Good convergence, satifies 1% error tolerence
  825 SECOVT = tseco
c
      CALL TRACK ('SECOVT  ')
      RETURN
c
  911         WRITE( io,61)
              WRITE( io,62) elapst, j
              CALL WHERE(0)
c
   61 FORMAT(1X,'FATAL(SECOVT): cant measure overhead time subr SECOND')
   62 FORMAT(/,13X,'using SECOND:  elapst=',1E20.8,6X,'J=',I4)
   63 FORMAT(1X,'WARNING(SECOVT): SECOND overhead time relerr',f9.4,'%')
   64 FORMAT('SECOVT:',I10,E12.4,F11.4)
      END
c
c***********************************************************************
      SUBROUTINE  SENSIT( iou, RATES,WG,IQ,SUMW,  MAP,TV,TV1,TV2,TV3,n)
c***********************************************************************
c                                                                      *
c     SENSIT  - Sensitivity Of Harmonic Mean Rate(Mflops) 49 Workloads *
c                                                                      *
c     iou     - input scalar,  i/o unit number                         *
c     RATES   - input array ,  execution rates (Mflops)                *
c     WG      - input array ,  weights paired with RATES               *
c     IQ      - input array ,  1 or 2 quartiles specifier              *
c     SUMW    - input array ,  workload fractions.                     *
c                                                                      *
c     MAP,TV,TV1,TV2,TV3    -  output temporary arrays                 *
c     n       - input scalar,  number of rates, etc.                   *
c                                                                      *
c***********************************************************************
c
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
c
c In
      DIMENSION  RATES(n), WG(n), IQ(7), SUMW(7)
c Temp
      DIMENSION  MAP(n), TV(n), TV1(n), TV2(n), TV3(n)
      DIMENSION  NR1(10), NR2(10), STAT2(20)
 
c     DIMENSION  TAG(4)                                                  f66
      CHARACTER*8  TAG(4)                                                f77
      SAVE  TAG
c
      DATA  ( TAG(i), i= 1,4)
     1 /'1st QT: ',  '2nd QT: ',  '3rd QT: ',  '4th QT: '/               f77
c    ./8H1st QT:  , 8H2nd QT:  , 8H3rd QT:  , 8H4th QT:  /               f66
c
      CALL TRACE ('SENSIT  ')
c
c                 Compress valid data sets RATES,  mapping on MAP.
 
            meff= 0
            neff= 0
              bl= 1.0d-5
              bu= 1.0d+5
            CALL  VALID( TV1,MAP,neff,  bl, RATES, bu, n)
 
        DO  1  k= 1,neff
          TV3(k)=    WG( MAP(k))
    1  continue
 
 
c                 Compress valid data sets WG,  mapping on MAP.
 
            CALL  VALID( TV3,MAP,meff,  bl, TV3, bu, neff)
 
        DO  3  k= 1,meff
           TV(k)=TV1( MAP(k))
    3  continue
c
c                 Sort selected rates into descending order
 
            CALL  SORDID( MAP,TV2,   TV,meff,2)
 
c
c
       CALL PAGE( iou)
          WRITE ( iou,7001)
c
 7001 FORMAT(/)
 7301 FORMAT(9X,'           SENSITIVITY ANALYSIS' )
 7302 FORMAT(9X,'The sensitivity of the harmonic mean rate (Mflops)  ')
 7303 FORMAT(9X,'to various weightings is shown in the table below.  ')
 7304 FORMAT(9X,'Seven work distributions are generated by assigning ')
 7305 FORMAT(9X,'two distinct weights to ranked kernels by quartiles.')
 7306 FORMAT(9X,'Forty nine possible cpu workloads are then evaluated')
 7307 FORMAT(9X,'using seven sets of values for the total weights:   ')
 7341 FORMAT(3X,A ,6X,'O      O      O      O      O      X      X')    f77
 7342 FORMAT(3X,A ,6X,'O      O      O      X      X      X      O')    f77
 7343 FORMAT(3X,A ,6X,'O      X      X      X      O      O      O')    f77
 7344 FORMAT(3X,A ,6X,'X      X      O      O      O      O      O')    f77
c7341 FORMAT(3X,A7,6X,43HO      O      O      O      O      X      X)    f66
c7342 FORMAT(3X,A7,6X,43HO      O      O      X      X      X      O)    f66
c7343 FORMAT(3X,A7,6X,43HO      X      X      X      O      O      O)    f66
c7344 FORMAT(3X,A7,6X,43HX      X      O      O      O      O      O)    f66
 7346 FORMAT(13X,  '------ ------ ------ ------ ------ ------ ------')
 7348 FORMAT(3X,'Total',/,3X,'Weights',20X,'Net Mflops:',/,4X,'X    O')
 7349 FORMAT(2X,'---- ----' )
 7220 FORMAT(/,1X,2F5.2,1X,7F7.2)
c
          WRITE ( iou,7001)
          WRITE ( iou,7001)
          WRITE ( iou,7301)
          WRITE ( iou,7001)
          WRITE ( iou,7302)
          WRITE ( iou,7303)
          WRITE ( iou,7304)
          WRITE ( iou,7305)
          WRITE ( iou,7306)
          WRITE ( iou,7307)
          WRITE ( iou,7001)
          WRITE ( iou,7346)
          WRITE ( iou,7341)   TAG(1)
          WRITE ( iou,7342)   TAG(2)
          WRITE ( iou,7343)   TAG(3)
          WRITE ( iou,7344)   TAG(4)
          WRITE ( iou,7346)
          WRITE ( iou,7348)
          WRITE ( iou,7349)
c
            IF( meff .LE. 0 )  GO TO 73
          fuzz= 1.0d-9
             r= meff
            mq= (meff+3)/4
             q= mq
             j= 1
      DO 21  i= 8,2,-2
      NR1(i  )= j
      NR1(i+1)= j
      NR2(i  )= j + mq + mq - 1
      NR2(i+1)= j + mq - 1
             j= j + mq
   21  continue
c
       DO 29 j= 1,7
          sumo= 1.0d0 - SUMW(j)
       DO 27 i= 1,7
             p= IQ(i)*q
            xt= SUMW(j)/(p + fuzz)
            ot= sumo   /(r - p + fuzz)
       DO 23 k= 1,meff
        TV3(k)= ot
   23  continue
            k1= NR1(i+2)
            k2= NR2(i+2)
       DO 25 k= k1,k2
        TV3(k)= xt
   25  continue
          CALL  STATW( STAT2,TV,MAP, TV2,TV3,meff)
        TV1(i)= STAT2(5)
   27  continue
        WRITE ( iou,7220) SUMW(j), sumo, ( TV1(k), k=1,7)
   29  continue
c
           WRITE ( iou,7349)
           WRITE ( iou,7346)
c
c
   73 CONTINUE
      CALL TRACK ('SENSIT  ')
      RETURN
      END
c
c***************************************
      SUBROUTINE SEQDIG( ND, isum, A, B, nr)
c***********************************************************************
c                                                                      *
c     SEQDIG - compute relative error and significant digits of precisi*
c                                                                      *
c     ND     - Out.Ary  Number of Significant Equal Digits in A.eq.B   *
c     isum   - Result   Total number of Equal digits.                  *
c     A      -  In.Ary  Reference values for comparison.               *
c     B      -  In.Ary  New values of unknown precision.               *
c     nr     -  Input   number of results in ND                        *
c***********************************************************************
c
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
c
      parameter( maxsd= 16 )
c/    parameter( maxsd= 30 )
      DIMENSION  ND(nr), A(nr), B(nr)
c
      ISIGDG(reler)= INT( ABS( LOG10( ABS( reler))) + 0.500d0 )
      CALL TRACE ('SEQDIG  ')
c
c     Try to determine floating-point precision used: Max Sig Digits
c
        maxs= maxsd
         one= 1.00d0
          sd= 1073741824.00d0
         sum= sd + one
          IF( sum .EQ. sd )  maxs= 8
c
        isum= 0
      DO 1 k= 1,nr
          se= SIGN( one, A(k)) * SIGN( one, B(k))
          IF( se .LT. 0.0)  THEN
              ND(k)= 0
          ELSE
c
c             compute relative error and digits of precision in B.
c
                  re=  RELERR( A(k), B(k))
              IF((re.GT. 0.0d0 ) .AND. (re.LT. 1.0d0))  THEN
                       ND(k)= ISIGDG(re)
              ELSEIF( re .EQ. 0.0d0 )  THEN
                       ND(k)= maxs
              ELSEIF( re .GE. 1.0d0 )  THEN
                       ND(k)= 0
              ENDIF
              IF( ND(k).GT. maxs  )  ND(k)= maxs
          ENDIF
        isum= isum + ND(k)
    1 CONTINUE
c
      CALL TRACK ('SEQDIG  ')
      RETURN
      END
c
c
c***********************************************
      SUBROUTINE  SIGNEL( V, SCALE,BIAS, n)
c***********************************************
c
c    SIGNEL GENERATES VERY FRIENDLY FLOATING-POINT NUMBERS NEAR 1.0
c                     WHEN SCALE= 1.0 AND BIAS= 0.
c
c     V      - result array,  floating-point test data
c     SCALE  - input scalar,  scales magnitude of results
c     BIAS   - input scalar,  offsets magnitude of results
c     n      - input integer, number of results in V.
c
c***********************************************
                DOUBLE PRECISION  V, SCALE, BIAS
cIBM  REAL*8            V, SCALE, BIAS
c
c     Use the following Double Precision declaration to improve Real*4 tests.
c     Use the following Real*16          declaration to improve Real*8 tests.
c
         DOUBLE     PRECISION  SCALED,BIASED,FUZZ,BUZZ,FIZZ,ONE
cIBM  REAL*16           SCALED,BIASED,FUZZ,BUZZ,FIZZ,ONE
      DIMENSION  V(n)
c
      CALL TRACE ('SIGNEL  ')
c
        SCALED= SCALE
        BIASED= BIAS
c
        SCALED= 10.00d0
        SCALED=  1.00d0/SCALED
        BIASED=  0.00d0
c
c         FUZZ= 1.234500d-9
          FUZZ= 1.234500d-3
          BUZZ= 1.000d0  + FUZZ
          FIZZ= 1.100d0  * FUZZ
           ONE= 1.000d0
c
        DO 1 k= 1,n
          BUZZ= (ONE - FUZZ)*BUZZ +FUZZ
          FUZZ= -FUZZ
c         V(k)=((BUZZ- FIZZ) -BIASED)*SCALED
          V(k)= (BUZZ- FIZZ)*SCALED
    1 CONTINUE
c
      CALL TRACK ('SIGNEL  ')
      RETURN
      END
c
c
c***********************************************************************
      SUBROUTINE SIMD( HM,  iou,RATES,WG,FR,m,  MAP,TV1,TV2,TV3,n)
c***********************************************************************
c                                                                      *
c     SIMD  - Sensitivity Of Harmonic Mean Rate(Mflops) SISD/SIMD Model*
c                                                                      *
c     HM      - result array,  Harmonic Mean Rates(k)= f( FR(k))       *
c     iou     - input scalar,  i/o unit number                         *
c     RATES   - input array ,  execution rates (Mflops)                *
c     WG      - input array ,  weights paired with RATES               *
c     FR      - input array ,  fractions of flops executed SIMD        *
c     m       - input scalar,  number of fractions                     *
c                                                                      *
c     MAP,TV,TV1,TV2,TV3    -  output temporary arrays                 *
c     n       - input scalar,  number of rates, etc.                   *
c                                                                      *
c***********************************************************************
c
c
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
c
c SENSITIVITY OF NET MFLOPS RATE TO USE OF OPTIMAL FORTRAN CODE(SISD/SIMD MODEL)
c Out
      DIMENSION  HM(m)
c In
      DIMENSION  FR(m), RATES(n), WG(n)
c Temp
      DIMENSION  MAP(n), TV1(n), TV2(n), TV3(n), STAT2(20)
c
      CALL TRACE ('SIMD    ')
 
c                 Compress valid data sets RATES,  mapping on MAP.
 
            meff= 0
            neff= 0
              bl= 1.0d-5
              bu= 1.0d+5
            CALL  VALID( TV1,MAP,neff,  bl, RATES, bu, n)
 
        DO  1  k= 1,neff
          TV3(k)=    WG( MAP(k))
    1  continue
 
 
c                 Compress valid data sets WG,  mapping on MAP.
 
            CALL  VALID( TV3,MAP,meff,  bl, TV3, bu, neff)
 
        DO  3  k= 1,meff
          TV2(k)= TV1( MAP(k))
    3  continue
 
c                 Sort RATES,WT into descending order.
 
            CALL  STATW( STAT2,TV1,MAP, TV2, TV3, meff)
             med= meff + 1 - INT(STAT2(8))
              lh= meff + 1 - med
 
        DO  5  k= 1,meff
          TV2(k)= TV3( MAP(k))
    5  continue
 
 
c                 Estimate vector rate= HMean of top LFK quartile.
 
              nq= meff/4
            CALL  STATW( STAT2,TV3,MAP, TV1,TV2,nq)
             vmf= STAT2(5)
 
c                 Estimate scalar rate= HMean of lowest two LFK quartiles.
 
            CALL  STATW( STAT2,TV3,MAP, TV1(med),TV2(med),lh)
             smf= STAT2(5)
            fuzz= 1.0d-9
 
               g= 1.0d0 -   smf/( vmf + fuzz)
           HM(1)= smf
 
          DO 7 k= 2,m
           HM(k)=   smf/( 1.0d0 - FR(k)*g + fuzz)
   7      continue
c
      IF( iou .GT. 0)  THEN
c
          WRITE ( iou,7001)
          WRITE ( iou,7001)
          WRITE ( iou,7001)
          WRITE ( iou,7101)
          WRITE ( iou,7102) ( HM(k), k= 1,9)
          WRITE ( iou,7102) ( FR(k), k= 1,9)
          WRITE ( iou,7103)
          WRITE ( iou,7001)
 7001 FORMAT(/)
 7101 FORMAT(' SENSITIVITY OF NET MFLOPS RATE TO USE OF OPTIMAL FORTRAN
     1CODE(SISD/SIMD MODEL)' )
 7102 FORMAT(/,1X,5F7.2,4F8.2)
 7103 FORMAT(3x,' Fraction Of Operations Run At Optimal Fortran Rates')
c
      ENDIF
c
      CALL TRACK ('SIMD    ')
      RETURN
c
      END
c
c
c***********************************************
      SUBROUTINE SIZES(i)
c***********************************************
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
c
c            SIZES      test and set the loop controls before each kernel test
c
c     i    :=  kernel number
c
c     mk    :=  number of kernels to test
c     Nruns :=  number of timed runs of complete test.
c     tclock:=  cpu clock resolution or minimum time in seconds.
c     Loop  :=  multiple pass control to execute kernel long enough to time.
c     n     :=  DO loop control for each kernel.
c     ******************************************************************
c
c
c/      PARAMETER( l1= 1001, l2=  101, l1d= 2*1001 )
c/      PARAMETER( l13=  64, l13h= l13/2, l213= l13+l13h, l813= 8*l13 )
c/      PARAMETER( l14=2048, l16=  75, l416= 4*l16 , l21= 25 )
c
c/      PARAMETER( l1=   27, l2=   15, l1d= 2*1001 )
c/      PARAMETER( l13= 8, l13h= 8/2, l213= 8+4, l813= 8*8 )
c/      PARAMETER( l14=  16, l16= 15, l416= 4*15 , l21= 15)
c
c/      PARAMETER( l1=   1001, l2=   101, l1d= 2*1001 )
c/      PARAMETER( l13= 64, l13h= 64/2, l213= 64+32, l813= 8*64 )
c/      PARAMETER( l14= 2048, l16= 75, l416= 4*75 , l21= 25)
c
c/      PARAMETER( kn= 47, kn2= 95, np= 3, ls= 3*47, krs= 24)
c
c/      PARAMETER( NNI=  2*l1 +2*l213 +l416 )
c/      PARAMETER( NN1= 16*l1 +13*l2 +2*l416 + l14 )
c/      PARAMETER( NN2= 4*l813 + 3*l21*l2 +121*l2 +3*l13*l13 )
c/      PARAMETER( Nl1= 19*l1, Nl2= 131*l2 +3*l21*l2 )
c/      PARAMETER( Nl13= 3*l13*l13 +34*l13 +32)
c
c/      PARAMETER( nk= 47, nl= 3, nr= 8 )
c
      COMMON /ALPHA/ mk,ik,im,ml,il,Mruns,Nruns,jr,iovec,NPFS(8,3,47)
      COMMON /TAU/   tclock, tsecov, testov, cumtim(4)
      COMMON /BETA / tic, TIMES(8,3,47), SEE(5,3,8,3),
     1              TERRS(8,3,47), CSUMS(8,3,47),
     2              FOPN(8,3,47), DOS(8,3,47)
c
c
      COMMON /SPACES/ ion,j5,k2,k3,MULTI,laps,Loop,m,kr,LP,n13h,ibuf,nx,
     1 L,npass,nfail,n,n1,n2,n13,n213,n813,n14,n16,n416,n21,nt1,nt2,
     2 last,idebug,mpy,Loops2,mucho,mpylim, intbuf(16)
c
      COMMON /SPACER/ A11,A12,A13,A21,A22,A23,A31,A32,A33,
     1                AR,BR,C0,CR,DI,DK,
     2  DM22,DM23,DM24,DM25,DM26,DM27,DM28,DN,E3,E6,EXPMAX,FLX,
     3  Q,QA,R,RI,S,SCALE,SIG,STB5,T,XNC,XNEI,XNM
c
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), ticks,
     1                FR(9), TERR1(47), SUMW(7), START,
     2              SKALE(47), BIAS(47), WS(95), TOTAL(47), FLOPN(47),
     3                IQ(7), NPF, NPFS1(47)
c
      COMMON /SPACEI/ WTP(3), MUL(3), ISPAN(47,3), IPASS(47,3)
c
c     ******************************************************************
c
      CALL TRACE ('SIZES   ')
c
      nif= 0
c                        Set  mk .LE. 47  number of kernels to test.
             mk= 24
             im= 1
             ml= 3
c                        Set  Nruns .LT. 8  number of timed runs of KERNEL test
c                        Set  Nruns= 1   to REDUCE RUN TIME for debug runs.
          Nruns= 1
c                        Set  Nruns= 7   for Standard BENCHMARK Test. Maximum.
          Nruns= 7
             IF( Nruns.GT. 7) Nruns= 7
c
c                        Set  Mruns= 7   for Standard BENCHMARK Test.
          Mruns= Nruns
c
c****************************************************************************
c         OPTIONAL LONG ENDURANCE TEST FOR NEW HARDWARE ACCEPTANCE TESTING.
c         OPTIONAL       Set  Mruns=     for Hardware ENDURANCE TRIAL
c
c         Mruns= Nruns * ( Desired Trial Time(sec) / totjob Time(sec))
c                          where totjob-time is LFK Standard benchmark
c                          test Job-time printed at end of output file.
c
c   e.g.  12 Hour run on CRAY-XMP :   laps = 43200./ 17.5 = 2468
c         12 Hour run on VaxS3500 :   laps = 43200./478.4 =   90
c
c          laps= 1
c****************************************************************************
c
          Mruns= Nruns * laps
      IF( Mruns.LT.Nruns .OR. Mruns.GT.500000 ) Mruns= Nruns
c
      IF( i.EQ.-1)  GO TO 73
c
c****************************************************************************
c     Domain tests follow to detect overstoring of controls for array opns.
c****************************************************************************
c
      nif= 1
      iup= 999000
      IF( iup.LT.65000 ) iup= 65000
      IF( i.LT.1 .OR.  (i-1).GT.  24)      GO TO 911
      IF( n.LT.0 .OR.  n.GT.   1001)          GO TO 911
      IF(Loop.LT.0 .OR. Loop.GT.iup)        GO TO 911
c
      nif= 2
      IF(  il.LT.1 .OR. il.GT.3 )  GO TO 911
                 n= ISPAN(i,il)
      Loop        = IPASS(i,il) * MUL(il)
      Loop = MULTI * Loop
      LP   = Loop
c
c
c
c MULTI= 10
c        ------    ------    ------   -------   -------   ------------
c        kernel    L:Loop    n:loop   flops*1   flops*n   flops*n*Loop
c        ------    ------    ------   -------   -------   ------------
c   il= 1     1        70      1001         5      5005    350350
c             2       670        97         4       388    259960
c             3        90      1001         2      2002    180180
c             4       140       600         2      1200    168000
c             5       100      1000         2      2000    200000
c             6        30      1984         2      3968    119040
c             7        40       995        16     15920    636800
c             8       100       198        36      7128    712800
c             9       360       101        17      1717    618120
c            10       340       101         9       909    309060
c            11       110      1000         1      1000    110000
c            12       120      1000         1      1000    120000
c            13       360        64         7       448    161280
c            14        20      1001        11     11011    220220
c            15        10       500        33     16500    165000
c            16       250        53        10       530    132500
c            17       350       101         9       909    318150
c            18        20       495        44     21780    435600
c            19       390       101         6       606    236340
c            20        10      1000        26     26000    260000
c            21        10     63125         2    126250   1262500
c            22       110       101        17      1717    188870
c            23        80       495        11      5445    435600
c            24        50      1000         1      1000     50000
c   il= 2     1       800       101         5       505    404000
c             2       800        97         4       388    310400
c             3      1060       101         2       202    214120
c             4      1400        60         2       120    168000
c             5      1100       100         2       200    220000
c             6       140       480         2       960    134400
c             7       440       101        16      1616    711040
c             8       120       198        36      7128    855360
c             9       420       101        17      1717    721140
c            10       380       101         9       909    345420
c            11      1280       100         1       100    128000
c            12      1360       100         1       100    136000
c            13       820        32         7       224    183680
c            14       200       101        11      1111    222200
c            15        20       500        33     16500    330000
c            16       540        28        10       280    151200
c            17       400       101         9       909    363600
c            18        20       495        44     21780    435600
c            19       460       101         6       606    278760
c            20       160       100        26      2600    416000
c            21        20     31250         2     62500   1250000
c            22       140       101        17      1717    240380
c            23       100       495        11      5445    544500
c            24       620       100         1       100     62000
c   il= 3     1      2240        27         5       135    302400
c             2      3680        11         4        44    161920
c             3      2960        27         2        54    159840
c             4      3040        15         2        30     91200
c             5      3200        26         2        52    166400
c             6      1680        24         2        48     80640
c             7      1600        21        16       336    537600
c             8       720        26        36       936    673920
c             9      2080        15        17       255    530400
c            10      2000        15         9       135    270000
c            11      3680        26         1        26     95680
c            12      3840        26         1        26     99840
c            13      2480         8         7        56    138880
c            14       640        27        11       297    190080
c            15        80        70        33      2310    184800
c            16      1120        11        10       110    123200
c            17      2080        15         9       135    280800
c            18       160        65        44      2860    457600
c            19      2240        15         6        90    201600
c            20       560        26        26       676    378560
c            21        80     12500         2     25000   2000000
c            22       640        15        17       255    163200
c            23       560        65        11       715    400400
c            24      1840        26         1        26     47840
c
computers with high resolution clocks tic= O(microsec.) should use Loop= 1
c     to show un-initialized as well as encached execution rates.
c
c     Loop= 1
c
      IF( Loop.LT. 1)   Loop= 1
      LP  = Loop
      L   = 1
      mpy = 1
      nif = 3
      IF( n.LT.0 .OR.  n.GT.   1001)  GO TO 911
      IF(Loop.LT.0 .OR. Loop.GT.iup)  GO TO 911
      n1  = 1001
      n2  = 101
      n13 = 64
      n13h= 32
      n213= 96
      n813= 512
      n14 = 2048
      n16 = 75
      n416= 300
      n21 = 25
c
      nt1= 16*1001 +13*101 +2*300 + 2048
      nt2= 4*512 + 3*25*101 +121*101 +3*64*64
c
   73 CONTINUE
      CALL TRACK ('SIZES   ')
      RETURN
c
c
  911 io= ABS( ion)
      IF( io.LE.0 .OR. io.GT.10 ) io=6
      WRITE( io,913) i, nif, n, Loop, il
  913 FORMAT('1',///,' FATAL OVERSTORE/ DATA LOSS.  TEST=  ',6I6)
      CALL WHERE(0)
c
      END
c***********************************************
      SUBROUTINE SORDID( I,W, V,n,KIND)
c***********************************************
c                    QUICK AND DIRTY PORTABLE SORT.
c
c                I - RESULT INDEX-LIST. MAPS V TO SORTED W.
c                W - RESULT ARRAY, SORTED V.
c
c                V - INPUT  ARRAY SORTED IN PLACE.
c                n - INPUT  NUMBER OF ELEMENTS IN V
c             KIND - SORT ORDER:   = 1  ASCENDING MAGNITUDE
c                                  = 2 DESCENDING MAGNITUDE
c
c***********************************************
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
c
      DIMENSION  I(n), W(n), V(n)
c
      CALL TRACE ('SORDID  ')
c
            IF( n.LE.0 )  GO TO 73
      DO  1  k= 1,n
          W(k)= V(k)
    1     I(k)= k
c
      IF( KIND.EQ.1)  THEN
c
          DO  3  j= 1,n-1
                 m= j
          DO  2  k= j+1,n
                IF( W(k).LT.W(m)) m= k
    2     CONTINUE
                 X= W(j)
                 k= I(j)
              W(j)= W(m)
              I(j)= I(m)
              W(m)= X
              I(m)= k
    3     CONTINUE
c
c
      ELSE
c
          DO  6  j= 1,n-1
                 m= j
          DO  5  k= j+1,n
                IF( W(k).GT.W(m)) m= k
    5     CONTINUE
                 X= W(j)
                 k= I(j)
              W(j)= W(m)
              I(j)= I(m)
              W(m)= X
              I(m)= k
    6     CONTINUE
      ENDIF
c
      IF( n.GT.0 )  THEN
          CALL TRAP( I, ' SORDID ' , 1, n,n)
      ENDIF
c
   73 CONTINUE
      CALL TRACK ('SORDID  ')
      RETURN
      END
c
c***********************************************
      SUBROUTINE  SPACE
c***********************************************
c
c            SPACE      sets memory pointers for array variables.  optional.
c
c     Subroutine Space dynamically allocates physical memory space
c     for the array variables in KERNEL by setting pointer values.
c     The POINTER declaration has been defined in the IBM PL1 language
c     and defined as a Fortran extension in Livermore and CRAY compilers.
c
c     In general, large FORTRAN simulation programs use a memory
c     manager to dynamically allocate arrays to conserve high speed
c     physical memory and thus avoid slow disk references (page faults).
c
c     It is sufficient for our purposes to trivially set the values
c     of pointers to the location of static arrays used in common.
c     The efficiency of pointered (indirect) computation should be measured
c     if available.
c
c***********************************************
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
c
c
c/      PARAMETER( l1=   1001, l2=   101, l1d= 2*1001 )
c/      PARAMETER( l13= 64, l13h= 64/2, l213= 64+32, l813= 8*64 )
c/      PARAMETER( l14= 2048, l16= 75, l416= 4*75 , l21= 25)
c
      INTEGER    E,F,ZONE
      COMMON /ISPACE/ E(96), F(96),
     1  IX(1001), IR(1001), ZONE(300)
c
      COMMON /SPACE1/ U(1001), V(1001), W(1001),
     1  X(1001), Y(1001), Z(1001), G(1001),
     2  DU1(101), DU2(101), DU3(101), GRD(1001), DEX(1001),
     3  XI(1001), EX(1001), EX1(1001), DEX1(1001),
     4  VX(1001), XX(1001), RX(1001), RH(2048),
     5  VSP(101), VSTP(101), VXNE(101), VXND(101),
     6  VE3(101), VLR(101), VLIN(101), B5(101),
     7  PLAN(300), D(300), SA(101), SB(101)
c
      COMMON /SPACE2/ P(4,512), PX(25,101), CX(25,101),
     1  VY(101,25), VH(101,7), VF(101,7), VG(101,7), VS(101,7),
     2  ZA(101,7)  , ZP(101,7), ZQ(101,7), ZR(101,7), ZM(101,7),
     3  ZB(101,7)  , ZU(101,7), ZV(101,7), ZZ(101,7),
     4  B(64,64), C(64,64), H(64,64),
     5  U1(5,101,2),  U2(5,101,2),  U3(5,101,2)
c
c     ******************************************************************
c
c//      COMMON /POINT/ ME,MF,MU,MV,MW,MX,MY,MZ,MG,MDU1,MDU2,MDU3,MGRD,
c//     1  MDEX,MIX,MXI,MEX,MEX1,MDEX1,MVX,MXX,MIR,MRX,MRH,MVSP,MVSTP,
c//     2  MVXNE,MVXND,MVE3,MVLR,MVLIN,MB5,MPLAN,MZONE,MD,MSA,MSB,
c//     3  MP,MPX,MCX,MVY,MVH,MVF,MVG,MVS,MZA,MZP,MZQ,MZR,MZM,MZB,MZU,
c//     4  MZV,MZZ,MB,MC,MH,MU1,MU2,MU3
c//C
c//CLLL. LOC(X) =.LOC.X
c//C
           CALL TRACE ('SPACE   ')
c//      ME     = LOC( E )
c//      MF     = LOC( F )
c//      MU     = LOC( U )
c//      MV     = LOC( V )
c//      MW     = LOC( W )
c//      MX     = LOC( X )
c//      MY     = LOC( Y )
c//      MZ     = LOC( Z )
c//      MG     = LOC( G )
c//      MDU1   = LOC( DU1 )
c//      MDU2   = LOC( DU2 )
c//      MDU3   = LOC( DU3 )
c//      MGRD   = LOC( GRD )
c//      MDEX   = LOC( DEX )
c//      MIX    = LOC( IX )
c//      MXI    = LOC( XI )
c//      MEX    = LOC( EX )
c//      MEX1   = LOC( EX1 )
c//      MDEX1  = LOC( DEX1 )
c//      MVX    = LOC( VX )
c//      MXX    = LOC( XX )
c//      MIR    = LOC( IR )
c//      MRX    = LOC( RX )
c//      MRH    = LOC( RH )
c//      MVSP   = LOC( VSP )
c//      MVSTP  = LOC( VSTP )
c//      MVXNE  = LOC( VXNE )
c//      MVXND  = LOC( VXND )
c//      MVE3   = LOC( VE3 )
c//      MVLR   = LOC( VLR )
c//      MVLIN  = LOC( VLIN )
c//      MB5    = LOC( B5 )
c//      MPLAN  = LOC( PLAN )
c//      MZONE  = LOC( ZONE )
c//      MD     = LOC( D )
c//      MSA    = LOC( SA )
c//      MSB    = LOC( SB )
c//      MP     = LOC( P )
c//      MPX    = LOC( PX )
c//      MCX    = LOC( CX )
c//      MVY    = LOC( VY )
c//      MVH    = LOC( VH )
c//      MVF    = LOC( VF )
c//      MVG    = LOC( VG )
c//      MVS    = LOC( VS )
c//      MZA    = LOC( ZA )
c//      MZP    = LOC( ZP )
c//      MZQ    = LOC( ZQ )
c//      MZR    = LOC( ZR )
c//      MZM    = LOC( ZM )
c//      MZB    = LOC( ZB )
c//      MZU    = LOC( ZU )
c//      MZV    = LOC( ZV )
c//      MZZ    = LOC( ZZ )
c//      MB     = LOC( B )
c//      MC     = LOC( C )
c//      MH     = LOC( H )
c//      MU1    = LOC( U1 )
c//      MU2    = LOC( U2 )
c//      MU3    = LOC( U3 )
c
      CALL TRACK ('SPACE   ')
      RETURN
      END
c
c***********************************************************************
      SUBROUTINE  SPEDUP( iou, NAME, RATE )
c***********************************************************************
c                                                                      *
c     SPEDUP  - Computes Speed-ups: A circumspect method of comparison.*
c               Computers are ranked by their Geometric Mean Rates.    *
c                                                                      *
c     iou     - input scalar,  i/o unit number                         *
c     NAME    - input array ,  system name                             *
c     RATE    - input array ,  execution rates (Mflops)                *
c                                                                      *
c***********************************************************************
c
c
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
c
      parameter( nsys= 5, ns= nsys+1, nd= 11, nt= 4 )
      CHARACTER  NAME*8, NAMES*8, ijk*8
      DIMENSION  RATE(nt), NAME(nt), RATIO(nd)
      CHARACTER*8  IT(nt)
      COMMON /TAGS/  NAMES(nd,nt)
      COMMON /RATS/  RATED(nd,nt)
c
      CALL TRACE ('SPEDUP  ')
c                            Rank computer NAME by its Geometric Mean.
      DO  2  k= 1,nsys
            IF( RATE(2) .GT. RATED(k,2))  GO TO 4
    2 continue
    4   insert= k
c                            Pushdown Tables to allow insertion.
      DO  8  i= nd, insert+1, -1
      DO  6  j= 1,nt
      NAMES(i,j)=  NAMES(i-1,j)
      RATED(i,j)=  RATED(i-1,j)
    6 continue
    8 continue
c                            Insert new computer NAME
      DO 10  j= 1,nt
      NAMES(insert,j)=  NAME(j)
      RATED(insert,j)=  RATE(j)
   10 continue
c                            Print Table of Speed-ups of Mean Rates.
      CALL PAGE( iou)
      IT(1)= 'AM='
      IT(2)= 'GM='
      IT(3)= 'HM='
      ijk  = '--------'
      fuzz = 1.0d-9
      WRITE( iou,111)
      WRITE( iou,104)
  104 FORMAT(26X,'TABLE OF SPEED-UP RATIOS OF MEAN RATES (72 Samples)')
      WRITE( iou,105)
  105 FORMAT(/,26X,'Arithmetic, Geometric, Harmonic Means (AM,GM,HM)')
      WRITE( iou,106)
  106 FORMAT(26X,'The Geometric Mean is the least biased statistic.',/)
      WRITE( iou,109) ( ijk, m= 1,ns)
  109 FORMAT(1X,'--------  ----  ------  ',11(1X,A ))
      WRITE( iou,110) ( NAMES(m,2), m= 1,ns)
  110 FORMAT(1X,'SYSTEM    MEAN  MFLOPS',2X,11(1X,A ))
      WRITE( iou,109) ( ijk, m= 1,ns)
c
      DO 40  i= 1,ns
      WRITE( iou,111)
  111 FORMAT(/)
c
      DO 26  j= 1,nt-1
c
      DO 22  m= 1,ns
      RATIO(m)= RATED(i,j) / (RATED(m,j) + fuzz)
   22 continue
c
      WRITE( iou,112) NAMES(i,j), IT(j), RATED(i,j), (RATIO(m), m=1,ns)
  112 FORMAT(1X,A ,2X,A3,F9.3,' :',11F9.3)
   26 continue
c
      WRITE( iou,114)  RATED(i,4)
  114 FORMAT(11X,'SD=',F9.3)
   40 continue
c
      CALL TRACK ('SPEDUP  ')
      RETURN
      END
c***********************************************
      SUBROUTINE STATS( STAT, X,n)
c***********************************************
c
c     UNWEIGHTED STATISTICS: MEAN, STADEV, MIN, MAX, HARMONIC MEAN.
c
c     STAT(1)= THE MEAN OF X.
c     STAT(2)= THE STANDARD DEVIATION OF THE MEAN OF X.
c     STAT(3)= THE MINIMUM OF X.
c     STAT(4)= THE MAXIMUM OF X.
c     STAT(5)= THE HARMONIC MEAN
c     X       IS THE ARRAY  OF INPUT VALUES.
c     n       IS THE NUMBER OF INPUT VALUES IN X.
c
c***********************************************
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
c
      DIMENSION X(n), STAT(20)
cLLL. OPTIMIZE LEVEL G
c
      CALL TRACE ('STATS   ')
c
      DO 10   k= 1,9
   10 STAT(k)= 0.0
c
      IF(n.LE.0)  GO TO 73
c                             CALCULATE MEAN OF X.
      S= 0.0
      DO 1 k= 1,n
    1 S= S + X(k)
      A= S/n
      STAT(1)= A
c                             CALCULATE STANDARD DEVIATION OF X.
      D= 0.0
      DO 2 k= 1,n
    2 D= D + (X(k)-A)**2
      D= D/n
      STAT(2)= SQRT(D)
c                             CALCULATE MINIMUM OF X.
      U= X(1)
      DO 3 k= 2,n
    3 U= MIN(U,X(k))
      STAT(3)= U
c                             CALCULATE MAXIMUM OF X.
      V= X(1)
      DO 4 k= 2,n
    4 V= MAX(V,X(k))
      STAT(4)= V
c                             CALCULATE HARMONIC MEAN OF X.
      H= 0.0
      DO 5 k= 1,n
          IF( X(k).NE.0.0) H= H + 1.0/X(k)
    5 CONTINUE
          IF( H.NE.0.0) H= REAL(n)/H
      STAT(5)= H
c
   73 CONTINUE
      CALL TRACK ('STATS   ')
      RETURN
      END
c***********************************************
      SUBROUTINE STATW( STAT,OX,IX, X,W,n)
c***********************************************
c
c     WEIGHTED STATISTICS: MEAN, STADEV, MIN, MAX, HARMONIC MEAN, MEDIAN.
c
c     STAT( 1)=  THE MEAN OF X.
c     STAT( 2)=  THE STANDARD DEVIATION OF THE MEAN OF X.
c     STAT( 3)=  THE MINIMUM OF X.
c     STAT( 4)=  THE MAXIMUM OF X.
c     STAT( 5)=  THE HARMONIC MEAN
c     STAT( 6)=  THE TOTAL WEIGHT.
c     STAT( 7)=  THE MEDIAN.
c     STAT( 8)=  THE MEDIAN INDEX, ASCENDING.
c     STAT( 9)=  THE ROBUST MEDIAN ABSOLUTE DEVIATION.
c     STAT(10)=  THE GEOMETRIC MEAN
c     STAT(11)=  THE MOMENTAL SKEWNESS
c     STAT(12)=  THE KURTOSIS
c     STAT(13)=  THE LOWER QUARTILE BOUND Q1/Q2 VALUE
c     STAT(14)=  THE UPPER QUARTILE BOUND Q3/Q4 VALUE
c     STAT(15)=  THE DEVIATION OF THE GEOMETRIC MEAN OF X
c
c     OX      IS THE ARRAY  OF ORDERED (DECENDING) Xs.
c     IX      IS THE ARRAY  OF INDEX LIST MAPS X TO OX.
c
c     X       IS THE ARRAY  OF INPUT VALUES.
c     W       IS THE ARRAY  OF INPUT WEIGHTS.
c     n       IS THE NUMBER OF INPUT VALUES IN X.
c
c***********************************************
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
c
      DIMENSION STAT(20), OX(n), IX(n), X(n), W(n)
cLLL. OPTIMIZE LEVEL G
c
      CALL TRACE ('STATW   ')
         stin09= 0.00d0
         stin13= 0.00d0
         stin14= 0.00d0
c
      DO 50   k= 1,15
   50 STAT(k)= 0.0d0
c
      IF( n.LE.0 )  GO TO 73
c
      IF( n.EQ.1 )  THEN
          STAT( 1)= X(1)
          STAT( 3)= X(1)
          STAT( 4)= X(1)
          STAT( 5)= X(1)
          STAT( 6)= W(1)
          STAT( 7)= X(1)
          STAT( 8)= 1.0d0
          STAT(10)= X(1)
          GO TO 73
      ENDIF
c
c
c                             CALCULATE MEAN OF X.
      A= 0.0d0
      S= 0.0d0
      T= 0.0d0
c
      DO 1 k= 1,n
      S= S + W(k)*X(k)
    1 T= T + W(k)
          IF( T.NE.0.0d0) A= S/T
      STAT(1)= A
c                             CALCULATE STANDARD DEVIATION OF X.
      D= 0.0d0
      E= 0.0d0
      F= 0.0d0
      Q= 0.0d0
      U= 0.0d0
c
      DO 2 k= 1,n
      B= W(k) *( X(k) -A)**2
      D= D + B
      E= E + B*( X(k) -A)
    2 F= F + B*( X(k) -A)**2
          IF( T.NE.0.0d0) Q= 1.0d0/T
                          D= D*Q
                          E= E*Q
                          F= F*Q
          IF( D.GE.0.0d0) U= SQRT(D)
      STAT(2)= U
c                             CALCULATE MINIMUM OF X.
      U= X(1)
      DO 3 k= 2,n
    3 U= MIN(U,X(k))
      STAT(3)= U
c                             CALCULATE MAXIMUM OF X.
      V= X(1)
      DO 4 k= 2,n
    4 V= MAX(V,X(k))
      STAT(4)= V
c                             CALCULATE HARMONIC MEAN OF X.
      H= 0.0d0
      DO 5 k= 1,n
          IF( X(k).NE.0.0d0) H= H + W(k)/X(k)
    5 CONTINUE
          IF( H.NE.0.0d0) H= T/H
      STAT(5)= H
      STAT(6)= T
c                             CALCULATE WEIGHTED MEDIAN
      CALL SORDID( IX, OX, X, n, 1)
c
           ew= 0.0d0
      DO 7  k= 2,n
           IF( W(1) .NE. W(k))  GO TO 75
    7 continue
           ew= 1.0d0
   75 continue
c
        qt= 0.500d0
      CALL  TILE( STAT( 7), STAT(8), OX,IX,W,ew,T, qt,n)
c
        qt= 0.250d0
      CALL  TILE( STAT(13),  stin13, OX,IX,W,ew,T, qt,n)
c
        qt= 0.750d0
      CALL  TILE( STAT(14),  stin14, OX,IX,W,ew,T, qt,n)
c
c
c                           CALCULATE ROBUST MEDIAN ABSOLUTE DEVIATION (MAD)
      DO 90 k= 1,n
   90   OX(k)= ABS( X(k) - STAT(7))
c
      CALL SORDID( IX, OX, OX, n, 1)
c
        qt= 0.700d0
      CALL  TILE( STAT( 9),  stin09, OX,IX,W,ew,T, qt,n)
c
c                             CALCULATE GEOMETRIC MEAN
            R= 0.0d0
      DO 10 k= 1,n
           IF( X(k).LE. 0.0d0)  GO TO 10
            R= R + W(k) *LOG10( X(k))
   10 CONTINUE
             U= R*Q
             G= 10.0d0
            IF( U.LT. 0.0d0)  G= 0.1D0
        POWTEN= 50.0d0
            IF( ABS(U) .GT. POWTEN)  U= SIGN( POWTEN, U)
      STAT(10)=  G** ABS(U)
c
c                             CALCULATE MOMENTAL SKEWNESS
             G= 0.0d0
           DXD= D*D
            IF( DXD.NE.0.0d0) G= 1.0d0/(DXD)
      STAT(11)= 0.50d0*E*G*STAT(2)
c
c                             CALCULATE KURTOSIS
      STAT(12)= 0.50d0*( F*G -3.0d0)
c
c                             CALCULATE DEVIATION OF GEOMETRIC MEAN
      D= 0.0d0
      Q= 0.0d0
      U= 0.0d0
      GM= STAT(10)
c
      DO 15 k= 1,n
      B= W(k) *( X(k) -GM)**2
   15 D= D + B
          IF( T.NE.0.0d0) Q= 1.0d0/T
                          D= D*Q
          IF( D.GE.0.0d0) U= SQRT(D)
      STAT(15)= U
c
c                             CALCULATE DESCENDING ORDERED X.
      CALL SORDID( IX, OX, X, n, 2)
c
   73 CONTINUE
      CALL TRACK ('STATW   ')
      RETURN
      END
c
c***********************************************
      FUNCTION SUMO( V,n)
c***********************************************
c
c     CHECK-SUM WITH ORDINAL DEPENDENCY.
c
c     V   - input array,   floating-point numbers
c     n   - input integer, number of elements in V.
c
c***********************************************
                DOUBLE PRECISION  SUMO, V
cIBM  REAL*8            SUMO, V
c
c     Use the following Double Precision declaration to improve Real*4 tests.
c     Use the following Real*16          declaration to improve Real*8 tests.
c
         DOUBLE     PRECISION  S
cIBM  REAL*16           S
c
      DIMENSION  V(n)
c
      CALL TRACE ('SUMO    ')
           S= 0.00d0
c
      DO 1 k= 1,n
    1      S= S + REAL(k)*V(k)
       SUMO = S
      CALL TRACK ('SUMO    ')
      RETURN
      END
c
c***********************************************
      SUBROUTINE  SUPPLY(i)
c***********************************************
c
c            SUPPLY     initializes common blocks containing type real arrays.
c
c     i    :=  kernel number
c
c****************************************************************************
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
cout  DOUBLE  PRECISION  DS, DW                                         REDUNDNT
c
c/      PARAMETER( l1=   1001, l2=   101, l1d= 2*1001 )
c/      PARAMETER( l13= 64, l13h= 64/2, l213= 64+32, l813= 8*64 )
c/      PARAMETER( l14= 2048, l16= 75, l416= 4*75 , l21= 25)
c
c/      PARAMETER( kn= 47, kn2= 95, np= 3, ls= 3*47, krs= 24)
c/C
c/C/      PARAMETER( NN0= 39 )
c/C/      PARAMETER( NNI=  2*l1 +2*l213 +l416 )
c/C/      PARAMETER( NN1= 16*l1 +13*l2 +2*l416 + l14 )
c/C/      PARAMETER( NN2= 4*512 + 3*25*101 +121*101 +3*64*64 )
c
      COMMON /ALPHA/ mk,ik,im,ml,il,Mruns,Nruns,jr,iovec,NPFS(8,3,47)
      COMMON /SPACES/ ion,j5,k2,k3,MULTI,laps,Loop,m,kr,LP,n13h,ibuf,nx,
     1 L,npass,nfail,n,n1,n2,n13,n213,n813,n14,n16,n416,n21,nt1,nt2,
     2 last,idebug,mpy,Loops2,mucho,mpylim, intbuf(16)
c
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), ticks,
     1                FR(9), TERR1(47), SUMW(7), START,
     2              SKALE(47), BIAS(47), WS(95), TOTAL(47), FLOPN(47),
     3                IQ(7), NPF, NPFS1(47)
      COMMON /CKSUMS/ cksumu,ckoldu, cksump,ckoldp, cksuma,ckolda
c
c/      COMMON /SPACE1/ U(NN1)
c/      COMMON /SPACE2/ P(NN2)
c/      COMMON /SPACER/ A11(NN0)
c/C
        COMMON /SPACE1/ U(19977)
        COMMON /SPACE2/ P(34132)
        COMMON /SPACER/ A11(39)
c/
c
c***********************************************************************
c           Method 1:  Least space and most cpu time (D.P. SIGNEL arith)
c***********************************************************************
c
csmall      CALL TRACE ('SUPPLY  ')
csmall      IP1= i+1
csmall      nt0= 39
csmallC
csmall      CALL SIGNEL(  U, SKALE(IP1), BIAS(IP1), nt1)
csmall      CALL SIGNEL(  P, SKALE(IP1), BIAS(IP1), nt2)
csmall      CALL SIGNEL(A11, SKALE(IP1), BIAS(IP1), nt0)
csmall      CALL TRACK ('SUPPLY  ')
csmall      RETURN
c
c***********************************************************************
c           Method 2:  Double space and least cpu time
c***********************************************************************
c
        COMMON /BASE1/ BUFU(19977)
        COMMON /BASE2/ BUFP(34132)
        COMMON /BASER/ BUFA(39)
      DIMENSION P0(4,512)
      EQUIVALENCE(BUFP,P0)
c
c/C kleiner
c/      COMMON /BASE1/ BUFU( 2136)
c/      COMMON /BASE2/ BUFP( 2938)
c
      CALL TRACE ('SUPPLY  ')
c
      IP1= i
      nt0= 39
c               Execute SIGNEL calls only once; re-use generated data.
          ibuf= ibuf+1
      IF( ibuf.EQ. 1) THEN
          CALL SIGNEL(  BUFU, SKALE(IP1), BIAS(IP1), nt1)
          CALL SIGNEL(  BUFP, SKALE(IP1), BIAS(IP1), nt2)
          CALL SIGNEL(  BUFA, SKALE(IP1), BIAS(IP1), nt0)
                   DS= 1.000d0
                   DW= 0.500d0
             DO 205 j= 1,4
             DO 205 k= 1,512
             P0(j,k) = DS
                   DS= DS + DW
  205        CONTINUE
      ENDIF
c
c                                       Test for Trashing Data in BUF
               idebug=   0
      IF(      idebug.EQ.1
     1    .OR. ibuf  .EQ.1
     2    .OR. i     .EQ.(24-1))  THEN
c
           cksumu= SUMO( BUFU, nt1)
           cksump= SUMO( BUFP, nt2)
           cksuma= SUMO( BUFA, nt0)
c
           IF( ibuf.EQ. 1) THEN
                ckoldu= cksumu
                ckoldp= cksump
                ckolda= cksuma
           ELSEIF(      cksumu.NE.ckoldu
     1             .OR. cksump.NE.ckoldp
     2             .OR. cksuma.NE.ckolda )  THEN
                iou= ABS(ion)
                WRITE( iou,111) jr, il, ik
                WRITE( iou,112) ckoldu, ckoldp, ckolda
                WRITE( iou,113) cksumu, cksump, cksuma
  111 FORMAT(' SUPPLY: OVERSTORED! Trial=',I2,' Pass=',I2,' Kernel=',I3)
  112 FORMAT(' ckold:',3E24.15)
  113 FORMAT(' cksum:',3E24.15)
           ENDIF
      ENDIF
c                             Refill Work-Space from copies in Buffers
      DO 1 k= 1,nt0
    1 A11(k)= BUFA(k)
      DO 2 k= 1,nt1
    2   U(k)= BUFU(k)
      DO 3 k= 1,nt2
    3   P(k)= BUFP(k)
c
      CALL TRACK ('SUPPLY  ')
      RETURN
      END
c
c***********************************************************************
      SUBROUTINE  TALLY( iou, mode )
c***********************************************************************
c                                                                      *
c    TALLY      computes average and minimum Cpu timings and variances.*
c                                                                      *
c               iou -  i/o unit number                                 *
c                                                                      *
c              mode -  = 1 selects average run time: Preferred mode.   *
c                      = 2 selects minimum run time: Less accurate mode*
c                                                                      *
c***********************************************************************
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
cout  DOUBLE  PRECISION  cs                                             REDUNDNT
c
c/      PARAMETER( nk= 47, nl= 3, nr= 8 )
c
      COMMON /ALPHA/ mk,ik,im,ml,il,Mruns,Nruns,jr,iovec,NPFS(8,3,47)
      COMMON /BETA / tic, TIMES(8,3,47), SEE(5,3,8,3),
     1              TERRS(8,3,47), CSUMS(8,3,47),
     2              FOPN(8,3,47), DOS(8,3,47)
c
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), ticks,
     1                FR(9), TERR1(47), SUMW(7), START,
     2              SKALE(47), BIAS(47), WS(95), TOTAL(47), FLOPN(47),
     3                IQ(7), NPF, NPFS1(47)
c
      DIMENSION  S1(20), S2(20), S3(20), S4(20)
      DIMENSION  T1(47), T4(47)
      DIMENSION  PSUM(8,47)
c
      CALL TRACE ('TALLY   ')
c
           CALL  SIZES(-1)
c
      m= 1
      IF( mode .EQ. 2 )  m= 3
      CALL  PAGE(iou)
      WRITE( iou, 99)
      WRITE( iou,100)
c                        Checks valid domain for min and max of data sets
      DO 2 j= 1,Nruns
      WRITE( iou,102)  j, ( SEE(k,1,j,il), k= 1,2)
      T1(j)= SEE(1,1,j,il)
      i= 0
      IF( (SEE(3,2,j,il).LT. 0.01) .OR. (SEE(4,2,j,il).GT. 1.0))  i= i+1
      IF( (SEE(3,3,j,il).LT. 0.01) .OR. (SEE(4,3,j,il).GT. 1.0))  i= i+1
      IF( i.GT.0 )  THEN
      WRITE( iou,131)  j, il
      ENDIF
      IF( ( j.EQ.Nruns ) .OR. ( i.GT.0 ))  THEN
      WRITE( iou,104)  j, ( SEE(k,2,j,il), k= 1,4)
      WRITE( iou,104)  j, ( SEE(k,3,j,il), k= 1,4)
      ENDIF
    2 continue
c
      CALL STATS( S1, T1, Nruns)
      WRITE( iou,102)  Nruns, ( S1(k), k= 1,4)
c
c
c
      WRITE( iou,120) Nruns
      WRITE( iou,122)
      WRITE( iou,121)
      WRITE( iou,122)
c                        Computes and Checks experimental timing errors
      DO 8 k= 1,mk
        npft= 0
          cs= 0.0d0
c
      DO 4 j= 1,Nruns
        npft= npft +  NPFS(j,il,k)
          cs= cs   + CSUMS(j,il,k)
      PSUM(j,k)= cs                                     
    4 continue
c
      CALL  STATS( S2, TIMES(1,il,k), Nruns)
      TIME(k)= S2(m)
      CSUM(k)= cs
      TERR1(k)= 100.0d0*( S2(2)/( S2(1) + 1.0d-9))
      T4(k)= TERR1(k)
c
c
c     If this clock resolution test fails, you must increase Loop (Subr. SIZES)
c
      CALL  STATS( S3, TERRS(1,il,k), Nruns)
         IF( S3(1) .GT. 15.0)  THEN
            WRITE( iou,113) k
         ENDIF
c
      WRITE( iou,123) k, S2(3), S2(1), S2(4), TERR1(k), S3(1), npft
      TERR1(k)= MAX( TERR1(k), S3(1))
      CALL  STATS( S1, DOS(1,il,k), Nruns)
      TOTAL(k)= S1(1)
           IF( (S1(1).LE.0.0d0) .OR. (ABS(S1(3)-S1(4)).GT.1.0d-5)) THEN
           WRITE( iou,131) il, k, ( S1(k4), k4= 1,4)
           ENDIF
      CALL  STATS( S4, FOPN(1,il,k), Nruns)
      FLOPN(k)= S4(1)
           IF( (S4(1).LE.0.0d0) .OR. (ABS(S4(3)-S4(4)).GT.1.0d-5)) THEN
           WRITE( iou,131) il, k, ( S4(k4), k4= 1,4)
           ENDIF
    8 continue
c
      WRITE( iou,122)
      CALL  STATS( S4, T4, mk)
      WRITE(   *,124)
      WRITE(   *,133)
      WRITE(   *,125)  ( S4(k), k= 1,4)
      WRITE( iou,124)
      WRITE( iou,133)
      WRITE( iou,125)  ( S4(k), k= 1,4)
c
c
c      WRITE( iou,7783)     ( PSUM(1,k),  k= 1,krs )                      
c      WRITE( iou,7783)     ( CSUMS(j,il,18), j= 1,Nruns)
c      WRITE( iou,7783)     ( PSUM(k,18), k= 1,Nruns )   
c7783  format(5X,'&',E21.15,',',E21.15,',',E21.15,',') 
c 
c 
c
      CALL TRACK ('TALLY   ')
      RETURN
c
   99 FORMAT(//,' time TEST overhead (t err): ' )
  100 FORMAT(/,6X,'RUN',8X,'AVERAGE',8X,'STANDEV',8X,'MINIMUM',8X,
     1 'MAXIMUM' )
  102 FORMAT(1X,'TICK ',I3,4E15.6)
  104 FORMAT(1X,'DATA ',I3,4E15.6)
  113 FORMAT(/,1X,I2,' POOR CPU CLOCK RESOLUTION; NEED LONGER RUN. ')
  120 FORMAT(//,' THE EXPERIMENTAL TIMING ERRORS FOR ALL',I3,' RUNS')
  121 FORMAT('  k   T min      T avg      T max    T err   tick   P-F')
  122 FORMAT(' --  ---------  ---------  --------- -----  -----   ---')
  123 FORMAT(1X,I2,3E11.4,F6.2,'%',F6.2,'%',1X,I5)
  124 FORMAT(//,' NET CPU TIMING VARIANCE (T err);  A few % is ok: ')
  125 FORMAT(4X,' Terr',4(F14.2,'%'))
  131 FORMAT(1X,'**  TALLY: ERROR INVALID DATA** ',2I6,4E14.6)
  133 FORMAT(/,17X,'AVERAGE',8X,'STANDEV',8X,'MINIMUM',8X,'MAXIMUM' )
      END
c
c***********************************************
      SUBROUTINE TDIGIT( derr, nzd, s )
c***********************************************************************
c                                                                      *
c     TDIGIT  -  Count Lead Digits Followed By Trailing Zeroes.        *
c                                                                      *
c       derr  -  Result,  Digital Error in percent.                    *
c        nzd  -  Result,  Number Of Lead Digits                        *
c          s  -  Input ,  A Floated Integer                            *
c                                                                      *
c***********************************************************************
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
cout  DOUBLE  PRECISION  frac, fuzz, x, y, v, z                         REDUNDNT
c
c     frac(z)= (SIGN((ABS(z) - AINT(ABS(z))),z))
      frac(z)= ( ABS( ABS(z) - AINT(ABS(z))))
c
      CALL TRACE ('TDIGIT  ')
c
            x= 0.00d0
            n= 14
            x= ABS(s)
         fuzz= 1.0d-6
         derr= 100.0d0
          nzd= 0
           IF( x.EQ. 0.0d0)  GO TO 73
c                                  Normalize x
            y= LOG10(x)
            v= REAL( 10**( ABS( INT(y)) + 1 ))
c
           IF( (y.GE. 0.0d0) .AND. (v.NE. 0.0d0))  THEN
            x= (x/v) * 10.0d0
           ELSE
            x= x*v
           ENDIF
c                                  Multiply x Until Trailing Digits= Fuzz
       DO 1 k= 1,n
           IF( ((1.0d0-frac(x)).LE.fuzz) .OR. (frac(x).LE.fuzz)) GO TO 2
            x= 10.0d0*x
    1 continue
c
    2      IF( x.NE. 0.0d0)  THEN
               derr= 50.0d0/x
                nzd= INT( LOG10( ABS( 9.999999990d0*x )))
           ENDIF
c
   73 CONTINUE
      CALL TRACK ('TDIGIT  ')
      RETURN
      END
c
c*************************************************
      INTEGER FUNCTION  TEST( i )
c***********************************************************************
c                                                                      *
c              REPEAT AND TIME THE EXECUTION OF KERNEL i               *
c                                                                      *
c                    i  - Input integer;   Test Kernel Serial Number   *
c                 TEST  - Repetition Loop Counter, decremented to 0    *
c                                                                      *
c***********************************************************************
c
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
c
cLOX  REAL*8 SECOND
c
c/      PARAMETER( l1=   1001, l2=   101, l1d= 2*1001 )
c/      PARAMETER( l13= 64, l13h= 64/2, l213= 64+32, l813= 8*64 )
c/      PARAMETER( l14= 2048, l16= 75, l416= 4*75 , l21= 25)
c
c/      PARAMETER( kn= 47, kn2= 95, np= 3, ls= 3*47, krs= 24)
c
      COMMON /SPACES/ ion,j5,k2,k3,MULTI,laps,Loop,m,kr,LP,n13h,ibuf,nx,
     1 L,npass,nfail,n,n1,n2,n13,n213,n813,n14,n16,n416,n21,nt1,nt2,
     2 last,idebug,mpy,Loops2,mucho,mpylim, intbuf(16)
c
      INTEGER    E,F,ZONE
      COMMON /ISPACE/ E(96), F(96),
     1  IX(1001), IR(1001), ZONE(300)
c
      COMMON /SPACER/ A11,A12,A13,A21,A22,A23,A31,A32,A33,
     1                AR,BR,C0,CR,DI,DK,
     2  DM22,DM23,DM24,DM25,DM26,DM27,DM28,DN,E3,E6,EXPMAX,FLX,
     3  Q,QA,R,RI,S,SCALE,SIG,STB5,T,XNC,XNEI,XNM
c
      DIMENSION     ZX(1023), XZ(1500)
      EQUIVALENCE ( ZX(1), Z(1)), ( XZ(1), X(1))
c
      COMMON /SPACE1/ U(1001), V(1001), W(1001),
     1  X(1001), Y(1001), Z(1001), G(1001),
     2  DU1(101), DU2(101), DU3(101), GRD(1001), DEX(1001),
     3  XI(1001), EX(1001), EX1(1001), DEX1(1001),
     4  VX(1001), XX(1001), RX(1001), RH(2048),
     5  VSP(101), VSTP(101), VXNE(101), VXND(101),
     6  VE3(101), VLR(101), VLIN(101), B5(101),
     7  PLAN(300), D(300), SA(101), SB(101)
c
      COMMON /SPACE2/ P(4,512), PX(25,101), CX(25,101),
     1  VY(101,25), VH(101,7), VF(101,7), VG(101,7), VS(101,7),
     2  ZA(101,7)  , ZP(101,7), ZQ(101,7), ZR(101,7), ZM(101,7),
     3  ZB(101,7)  , ZU(101,7), ZV(101,7), ZZ(101,7),
     4  B(64,64), C(64,64), H(64,64),
     5  U1(5,101,2),  U2(5,101,2),  U3(5,101,2)
c
c
      COMMON /BASER/ A110,A120,A130,A210,A220,A230,A310,A320,A330,
     1                AR0,BR0,C00,CR0,DI0,DK0,
     2  DM220,DM230,DM240,DM250,DM260,DM270,DM280,DN0,E30,E60,EXPMAX0,
     3  FLX0,Q0,QA0,R0,RI0,S0,SCALE0,SIG0,STB50,T0,XNC0,XNEI0,XNM0
c
      COMMON /BASE1/ U0(1001), V0(1001), W0(1001),
     1  X0(1001), Y0(1001), Z0(1001), G0(1001),
     2  DU10(101), DU20(101), DU30(101), GRD0(1001), DEX0(1001),
     3  XI0(1001), EX0(1001), EX10(1001), DEX10(1001),
     4  VX0(1001), XX0(1001), RX0(1001), RH0(2048),
     5  VSP0(101), VSTP0(101), VXNE0(101), VXND0(101),
     6  VE30(101), VLR0(101), VLIN0(101), B50(101),
     7  PLAN0(300), D0(300), SA0(101), SB0(101)
c
      COMMON /BASE2/ P0(4,512), PX0(25,101), CX0(25,101),
     1  VY0(101,25), VH0(101,7), VF0(101,7), VG0(101,7), VS0(101,7),
     2  ZA0(101,7)  , ZP0(101,7), ZQ0(101,7), ZR0(101,7), ZM0(101,7),
     3  ZB0(101,7)  , ZU0(101,7), ZV0(101,7), ZZ0(101,7),
     4  B0(64,64), CC0(64,64), H0(64,64),
     5  U10(5,101,2),  U20(5,101,2),  U30(5,101,2)
c
      COMMON /TAU/   tclock, tsecov, testov, cumtim(4)
c
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), ticks,
     1                FR(9), TERR1(47), SUMW(7), START,
     2              SKALE(47), BIAS(47), WS(95), TOTAL(47), FLOPN(47),
     3                IQ(7), NPF, NPFS1(47)
c
c
c*******************************************************************************
c         Repeat execution of each Kernel(i) :     DO 1 L= 1,Loop   etc.
c*******************************************************************************
c
c    From the beginning in 1970 each sample kernel was executed just
c    once since supercomputers had high resolution, microsecond clocks.
c    In 1982 a repetition Loop was placed around each of the 24 LFK
c    kernels in order to run each kernel long enough for accurate
c    timing on mini-computer systems with poor cpu-clock resolution since
c    the majority of systems could only measure cpu-time to 0.01 seconds.
c    By 1990 however, several compilers' optimizers were factoring or
c    hoisting invariant computation outside some repetition Loops thus
c    distorting those Fortran samples.  The effect was usually absurd
c    Mflop rates which had to be corrected with compiler directives.
c    Therefore, in April 1990 these repetition Loops were removed from
c    subroutine KERNEL and submerged in subroutine TEST beyond the scope
c    of compiler optimizations.   Thus the 24 samples are now foolproof
c    and it will no longer be necessary to double check the machine code.
c
c    Very accurate, convergent methods have been developed to measure the
c    overhead time used for subroutines SECOND and TEST in subroutines
c    SECOVT and TICK respectively.  Thus, the LFK test may use substantially
c    more cpu time on systems with poor cpu-clock resolution.
c    The 24 C verison tests in CERNEL have also been revised to correspond with
c    the Fortran KERNEL. The 24 computation samples have NOT been changed.
c
c*******************************************************************************
c
cbug  IF( (LP.NE.Loop).OR.(L.LT.1).OR.(L.GT.Loop)) THEN
cbug      CALL TRACE('TEST    ')
cbug      CALL WHERE(0)
cbug  ENDIF
c                                    Repeat kernel test:   Loop times.
      IF( L .LT. Loop )  THEN
          L    = L + 1
          TEST = L
          RETURN
      ENDIF
c                                    Repeat kernel test:   Loop*Loops2
          ik   = i
      IF( mpy .LT. Loops2 )  THEN
          mpy  = mpy + 1
          nn   = n
c
           IF( i.EQ.0 ) GO TO 100
           IF( i.LT.0 .OR. i.GT.24 )  THEN
               CALL TRACE('TEST    ')
               CALL WHERE(0)
           ENDIF
c                   RE-INITIALIZE OVER-STORED INPUTS:
c
        GO TO( 100,   2, 100,   4,   5,   6, 100, 100,
     1         100,  10, 100, 100,  13,  14, 100,  16,
     2          17,  18,  19,  20,  21, 100,  23, 100, 100  ),  i
c
c     When MULTI.GE.100 each kernel is executed over a million times
c     and the time used to re-intialize overstored input variables
c     is negligible.  Thus each kernel may be run arbitrarily many times
c     (MULTI >> 100) without overflow and produce verifiable checksums.
c
c***********************************************************************
c
    2 DO 200 k= 1,nn
  200 X(k)= X0(k)
      GO TO 100
c***************************************
c
    4        m= (1001-7)/2
      DO 400 k= 7,1001,m
  400 XZ(k)= X0(k)
      GO TO 100
c***************************************
c
    5 DO 500 k= 1,nn
  500 X(k)= X0(k)
      GO TO 100
c***************************************
c
    6 DO 600 k= 1,nn
  600 W(k)= W0(k)
      GO TO 100
c***************************************
c
   10 DO 1000 k= 1,nn
      DO 1000 j= 5,13
 1000   PX(j,k)= PX0(j,k)
      GO TO 100
c***************************************
c
   13 DO 1300 k= 1,nn
         P(1,k)= P0(1,k)
         P(2,k)= P0(2,k)
         P(3,k)= P0(3,k)
 1300    P(4,k)= P0(4,k)
c
      DO 1301 k= 1,64
      DO 1301 j= 1,64
 1301    H(j,k)= H0(j,k)
      GO TO 100
c***************************************
c
   14 DO 1400   k= 1,nn
      RH(IR(k)  )= RH0(IR(k)  )
 1400 RH(IR(k)+1)= RH0(IR(k)+1)
      GO TO 100
c***************************************
c
   16 k2= 0
      k3= 0
      GO TO 100
c***************************************
c
   17 DO 1700 k= 1,nn
 1700     VXNE(k)= VXNE0(k)
      GO TO 100
c***************************************
c
   18 DO 1800 k= 2,6
      DO 1800 j= 2,nn
        ZU(j,k)= ZU0(j,k)
        ZV(j,k)= ZV0(j,k)
        ZR(j,k)= ZR0(j,k)
 1800   ZZ(j,k)= ZZ0(j,k)
      GO TO 100
c***************************************
c
   19 STB5= STB50
      GO TO 100
c***************************************
c
   20 XX(1)= XX0(1)
      GO TO 100
c***************************************
c
   21 DO 2100 k= 1,nn
      DO 2100 j= 1,25
 2100   PX(j,k)= PX0(j,k)
      GO TO 100
c***************************************
c
   23 DO 2300 k= 2,6
      DO 2300 j= 2,nn
 2300   ZA(j,k)= ZA0(j,k)
c***********************************************************************
c
  100 CONTINUE
c
          L    = 1
          TEST = 1
          RETURN
      ENDIF
c
          mpy  = 1
          L    = 1
          TEST = 0
c                                   switchback to TICK to measure testov
           IF( i.EQ. (-73))  RETURN
c
c***********************************************************************
c           t= second(0)  := cumulative cpu time for task in seconds.
c***********************************************************************
c
      cumtim(1)= 0.0d0
         TEMPUS= SECND2( cumtim(1)) - START
c
      CALL TRACE ('TEST    ')
cPFM      ikern= i
cPFM      call ENDPFM(ion)
c$C                           5 get number of page faults (optional)
c$      KSTAT= LIB$STAT_TIMER(5,KPF)
c$      NPF  = KPF - IPF
c
c
c                             Checksum results; re-initialize all inputs
      CALL TESTS ( i, TEMPUS )
c
c
c$C                           5 get number of page faults (optional) VAX
c$      NSTAT= LIB$STAT_TIMER(5,IPF)
c
cPFM       IF( INIPFM( ion, 0) .NE. 0 )  THEN
cPFM           CALL WHERE(20)
cPFM       ENDIF
      CALL TRACK ('TEST    ')
c
c      The following pause can be used for stop-watch timing of each kernel.
c      You may have to increase the iteration count MULTI in Subr. VERIFY.
c
c/           PAUSE
c
      mpy   = 1
      mpylim= Loops2
      L     = 1
      LP    = Loop
      ik    = i+1
      TEST  = 0
      cumtim(1)= 0.0d0
      START= SECND2( cumtim(1))
      RETURN
c
c$      DATA  IPF/0/, KPF/0/
      END
c
c***********************************************
      SUBROUTINE  TESTS( i, TEMPUS )
c***********************************************************************
c                                                                      *
c               CHECKSUM AND INITIALIZE THE EXECUTION OF KERNEL i      *
c                                                                      *
c                    i  - Input integer;  Test Kernel Serial Number    *
c               TEMPUS  - Input float  ;  Elapsed Cpu-time Test(i) used*
c                                                                      *
c***********************************************************************
c
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
c
c
c/      PARAMETER( l1= 1001, l2=  101, l1d= 2*1001 )
c/      PARAMETER( l13=  64, l13h= l13/2, l213= l13+l13h, l813= 8*l13 )
c/      PARAMETER( l14=2048, l16=  75, l416= 4*l16 , l21= 25 )
c
c/      PARAMETER( kn= 47, kn2= 95, np= 3, ls= 3*47, krs= 24)
c
      COMMON /ALPHA/ mk,ik,im,ml,il,Mruns,Nruns,jr,iovec,NPFS(8,3,47)
      COMMON /TAU/   tclock, tsecov, testov, cumtim(4)
c
      COMMON /BETA / tic, TIMES(8,3,47), SEE(5,3,8,3),
     1              TERRS(8,3,47), CSUMS(8,3,47),
     2              FOPN(8,3,47), DOS(8,3,47)
c
      COMMON /SPACES/ ion,j5,k2,k3,MULTI,laps,Loop,m,kr,LP,n13h,ibuf,nx,
     1 L,npass,nfail,n,n1,n2,n13,n213,n813,n14,n16,n416,n21,nt1,nt2,
     2 last,idebug,mpy,Loops2,mucho,mpylim, intbuf(16)
c
      COMMON /SPACER/ A11,A12,A13,A21,A22,A23,A31,A32,A33,
     1                AR,BR,C0,CR,DI,DK,
     2  DM22,DM23,DM24,DM25,DM26,DM27,DM28,DN,E3,E6,EXPMAX,FLX,
     3  Q,QA,R,RI,S,SCALE,SIG,STB5,T,XNC,XNEI,XNM
c
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), ticks,
     1                FR(9), TERR1(47), SUMW(7), START,
     2              SKALE(47), BIAS(47), WS(95), TOTAL(47), FLOPN(47),
     3                IQ(7), NPF, NPFS1(47)
c
cPFM  COMMON /KAPPA/ iflag1, ikern, statis(100,20), istats(100,20)
c
      COMMON /SPACEI/ WTP(3), MUL(3), ISPAN(47,3), IPASS(47,3)
c
      INTEGER    E,F,ZONE
      COMMON /ISPACE/ E(96), F(96),
     1  IX(1001), IR(1001), ZONE(300)
c
      COMMON /SPACE1/ U(1001), V(1001), W(1001),
     1  X(1001), Y(1001), Z(1001), G(1001),
     2  DU1(101), DU2(101), DU3(101), GRD(1001), DEX(1001),
     3  XI(1001), EX(1001), EX1(1001), DEX1(1001),
     4  VX(1001), XX(1001), RX(1001), RH(2048),
     5  VSP(101), VSTP(101), VXNE(101), VXND(101),
     6  VE3(101), VLR(101), VLIN(101), B5(101),
     7  PLAN(300), D(300), SA(101), SB(101)
c
c
      COMMON /SPACE2/ P(4,512), PX(25,101), CX(25,101),
     1  VY(101,25), VH(101,7), VF(101,7), VG(101,7), VS(101,7),
     2  ZA(101,7)  , ZP(101,7), ZQ(101,7), ZR(101,7), ZM(101,7),
     3  ZB(101,7)  , ZU(101,7), ZV(101,7), ZZ(101,7),
     4  B(64,64), C(64,64), H(64,64),
     5  U1(5,101,2),  U2(5,101,2),  U3(5,101,2)
c
          ik   = i
      CALL TRACE ('TESTS   ')
c
          NP   = Loop * Loops2
          Loop = 1
          LP   = Loop
          NN   = n
           IF( i.LT.0 .OR. i.GT.24 )  THEN
               CALL WHERE(0)
           ENDIF
c
           IF( i.EQ.0 )  GO TO 120
         CALL  SIZES(i)
c
c     Net Time=  Timing - Overhead Time
c
      TIME(i)= TEMPUS - REAL( NP) *testov - tsecov
c
c
        GO TO(  1,  2,  3,  4,  5,  6,  7,  8,  9, 10,
     1         11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
     2         21, 22, 23, 24, 25                      ), i
c
c
c
c***********************************************************************
c
    1 CSUM (1) =  SUMO ( X, n)
      TOTAL(1) =  NP*NN
      GO TO 100
c***********************************************************************
c
    2 CSUM (2) =  SUMO ( X, 2*n)
      TOTAL(2) =  NP*(NN-4)
      GO TO 100
c***********************************************************************
c
    3 CSUM (3) =  Q
      TOTAL(3) =  NP*NN
      GO TO 100
c***********************************************************************
c
    4        MM= (1001-7)/2
      DO 400 k = 7,1001,MM
  400      V(k)= X(k)
      CSUM (4) = SUMO ( V, 3)
      TOTAL(4) =  NP*(((NN-5)/5)+1)*3
      GO TO 100
c***********************************************************************
c
    5 CSUM (5) =  SUMO ( X(2), n-1)
      TOTAL(5) =  NP*(NN-1)
      GO TO 100
c***********************************************************************
c
    6 CSUM (6) =  SUMO ( W, n)
      TOTAL(6) =  NP*NN*((NN-1)/2)
      GO TO 100
c***********************************************************************
c
    7 CSUM (7) =  SUMO ( X, n)
      TOTAL(7) =  NP*NN
      GO TO 100
c***********************************************************************
c
    8 CSUM (8) = SUMO ( U1,5*n*2) + SUMO ( U2,5*n*2) + SUMO ( U3,5*n*2)
      TOTAL(8) =  NP*(NN-1)*2
      GO TO 100
c***********************************************************************
c
    9 CSUM (9) =  SUMO ( PX, 15*n)
      TOTAL(9) =  NP*NN
      GO TO 100
c***********************************************************************
c
   10 CSUM (10) =  SUMO ( PX, 15*n)
      TOTAL(10) =  NP*NN
      GO TO 100
c***********************************************************************
c
   11 CSUM (11) =  SUMO ( X(2), n-1)
      TOTAL(11) =  NP*(NN-1)
      GO TO 100
c***********************************************************************
c
   12 CSUM (12) =  SUMO ( X, n-1)
      TOTAL(12) =  NP*NN
      GO TO 100
c***********************************************************************
c
   13 CSUM (13) =  SUMO ( P, 8*n) + SUMO ( H, 8*n)
      TOTAL(13) =  NP*NN
      GO TO 100
c***********************************************************************
c
   14 CSUM (14) =  SUMO ( VX,n) + SUMO ( XX,n) + SUMO ( RH,67)
      TOTAL(14) =  NP*NN
      GO TO 100
c***********************************************************************
c
   15 CSUM (15) =  SUMO ( VY, n*7) + SUMO ( VS, n*7)
      TOTAL(15) =  NP*(NN-1)*5
      GO TO 100
c***********************************************************************
c
   16 CSUM (16) =  REAL( k3+k2+j5+m)
      FLOPN(16) =  ( k2+k2+10*k3 ) * Loops2
      TOTAL(16) =  1.0d0
      GO TO 100
c***********************************************************************
c
   17 CSUM (17) =  SUMO ( VXNE, n) + SUMO ( VXND, n) + XNM
      TOTAL(17) =  NP*NN
      GO TO 100
c***********************************************************************
c
   18 CSUM (18) =  SUMO ( ZR, n*7) + SUMO ( ZZ, n*7)
      TOTAL(18) =  NP*(NN-1)*5
      GO TO 100
c***********************************************************************
c
   19 CSUM (19) =  SUMO ( B5, n) + STB5
      TOTAL(19) =  NP*NN
      GO TO 100
c***********************************************************************
c
   20 CSUM (20) =  SUMO ( XX(2), n)
      TOTAL(20) =  NP*NN
      GO TO 100
c***********************************************************************
c
   21 CSUM (21) =  SUMO ( PX, 25*n)
      TOTAL(21) =  NP*25*25*NN
      GO TO 100
c***********************************************************************
c
   22 CSUM (22) =  SUMO ( W, n)
      TOTAL(22) =  NP*NN
      GO TO 100
c***********************************************************************
c
   23 CSUM (23) =  SUMO ( ZA, n*7)
      TOTAL(23) =  NP*(NN-1)*5
      GO TO 100
c***********************************************************************
c
   24 CSUM (24) =  REAL(m)
      TOTAL(24) =  NP*(NN-1)
      GO TO 100
c***********************************************************************
c
   25 CONTINUE
      GO TO 100
c***********************************************************************
c
  100 CONTINUE
c
c     delta( testov)= relerr * testov
            overr= SEE(2,1,jr,il)*REAL(NP)* testov
         TERR1(i)= 100.0
               IF( TIME(i).NE. 0.0d0) TERR1(i)= TERR1(i)*(overr/TIME(i))
         NPFS1(i)= NPF
               IF( ion .LE. 0 )  GO TO 120
c
c     If this clock resolution test fails, you must increase Loop (Subr. SIZES)
c
               IF( TERR1(i) .LT. 15.0)  GO TO 114
            WRITE( ion,113) I
  113 FORMAT(/,1X,I2,' TESTS:  POOR TIMING OR ERROR. NEED LONGER RUN')
c
  114      WRITE ( ion,115) i, TIME(i), TERR1(i), NPF
  115      FORMAT( 2X,i2,' Done  T=' ,E11.4,'  T err=' ,F8.2,'%' ,
     1             I8,'  Page-Faults' )
c
  120      IF( i.GE.0 .AND. i.LT.24 )  THEN
               CALL VALUES(i+1)
               CALL SIZES (i+1)
           ENDIF
c
      CALL TRACK ('TESTS   ')
      RETURN
      END
c
c***********************************************
      FUNCTION TICK( iou, ntimes)
c***********************************************
c
c            TICK       measures timing overhead of subroutine test
c
c     iou    -  Logical Output Device Number                           *
c
c***********************************************
c
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
c
c
c/      PARAMETER( l1= 1001, l2=  101, l1d= 2*1001 )
c/      PARAMETER( l13=  64, l13h= l13/2, l213= l13+l13h, l813= 8*l13 )
c/      PARAMETER( l14=2048, l16=  75, l416= 4*l16 , l21= 25 )
c
c/      PARAMETER( kn= 47, kn2= 95, np= 3, ls= 3*47, krs= 24)
c
c/      PARAMETER( nk= 47, nl= 3, nr= 8 )
      parameter( l4813= 4*512, l4813p= l4813 + 1 )
      INTEGER TEST
c
      COMMON /ALPHA/ mk,ik,im,ml,il,Mruns,Nruns,jr,iovec,NPFS(8,3,47)
      COMMON /TAU/   tclock, tsecov, testov, cumtim(4)
      COMMON /BETA / tic, TIMES(8,3,47), SEE(5,3,8,3),
     1              TERRS(8,3,47), CSUMS(8,3,47),
     2              FOPN(8,3,47), DOS(8,3,47)
c
c
      COMMON /SPACES/ ion,j5,k2,k3,MULTI,laps,Loop,m,kr,LP,n13h,ibuf,nx,
     1 L,npass,nfail,n,n1,n2,n13,n213,n813,n14,n16,n416,n21,nt1,nt2,
     2 last,idebug,mpy,Loops2,mucho,mpylim, intbuf(16)
c
      COMMON /SPACER/ A11,A12,A13,A21,A22,A23,A31,A32,A33,
     1                AR,BR,C0,CR,DI,DK,
     2  DM22,DM23,DM24,DM25,DM26,DM27,DM28,DN,E3,E6,EXPMAX,FLX,
     3  Q,QA,R,RI,S,SCALE,SIG,STB5,T,XNC,XNEI,XNM
c
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), ticks,
     1                FR(9), TERR1(47), SUMW(7), START,
     2              SKALE(47), BIAS(47), WS(95), TOTAL(47), FLOPN(47),
     3                IQ(7), NPF, NPFS1(47)
c
      COMMON /SPACEI/ WTP(3), MUL(3), ISPAN(47,3), IPASS(47,3)
c
      INTEGER    E,F,ZONE
      COMMON /ISPACE/ E(96), F(96),
     1  IX(1001), IR(1001), ZONE(300)
c
      COMMON /SPACE1/ U(1001), V(1001), W(1001),
     1  X(1001), Y(1001), Z(1001), G(1001),
     2  DU1(101), DU2(101), DU3(101), GRD(1001), DEX(1001),
     3  XI(1001), EX(1001), EX1(1001), DEX1(1001),
     4  VX(1001), XX(1001), RX(1001), RH(2048),
     5  VSP(101), VSTP(101), VXNE(101), VXND(101),
     6  VE3(101), VLR(101), VLIN(101), B5(101),
     7  PLAN(300), D(300), SA(101), SB(101)
c
      COMMON /SPACE2/ P(4,512), PX(25,101), CX(25,101),
     1  VY(101,25), VH(101,7), VF(101,7), VG(101,7), VS(101,7),
     2  ZA(101,7)  , ZP(101,7), ZQ(101,7), ZR(101,7), ZM(101,7),
     3  ZB(101,7)  , ZU(101,7), ZV(101,7), ZZ(101,7),
     4  B(64,64), C(64,64), H(64,64),
     5  U1(5,101,2),  U2(5,101,2),  U3(5,101,2)
c
      DIMENSION  TIM(20), TER(20), TMX(20), INX(20), P1(l4813p)
      EQUIVALENCE( P,P1)
      SAVE retest
c
c
      CALL TRACE ('TICK    ')
c
      ion= iou
      kr = mk
      n  = 0
      k2 = 0
      k3 = 0
      m  = 0
      neff= 0
      IF( il .EQ. 1 )  THEN
c
c***********************************************************************
c     Measure tsecov:  Overhead time for calling SECOND
c***********************************************************************
c
      tsecov = SECOVT( iou)
         tic = tsecov
c
c***********************************************************************
c     Measure testov:  Overhead time for calling TEST
c***********************************************************************
c
         testo= 0.00d0
           klm= 8000
            io= ABS(iou)
            jj= 0
            nt= ntimes - 6
             j= nt
            IF( nt.LT.8 .OR. nt.GT.30 )  GO TO 911
c
      DO 820 j= 1,nt
             L= 1
           mpy= 1
        Loops2= 1
        mpylim= Loops2
          Loop= klm
            LP= Loop
c                                  Measure overhead time for empty loop
      cumtim(1)= 0.0d0
             t0= SECND2( cumtim(1))
  801        IF( TEST(-73) .GT. 0 )  GO TO 801
  802        IF( TEST(-73) .GT. 0 )  GO TO 802
  803        IF( TEST(-73) .GT. 0 )  GO TO 803
  804        IF( TEST(-73) .GT. 0 )  GO TO 804
  805        IF( TEST(-73) .GT. 0 )  GO TO 805
  806        IF( TEST(-73) .GT. 0 )  GO TO 806
  807        IF( TEST(-73) .GT. 0 )  GO TO 807
  808        IF( TEST(-73) .GT. 0 )  GO TO 808
  809        IF( TEST(-73) .GT. 0 )  GO TO 809
  810        IF( TEST(-73) .GT. 0 )  GO TO 810
      cumtim(1)= 0.0d0
             t1= SECND2( cumtim(1)) - tsecov
         elapst= t1 - t0
          testo= elapst/( REAL(10*klm) + 1.0e-9)
          toler= 0.020d0
           rerr= 1.00d0
c
c                                  Convergence test:  Rel.error .LT. 1%
            IF( elapst.GT. 1.00d04 ) GO TO 911
            IF( elapst.LT. 1.00d-9 .AND. j.GT.8 ) GO TO 911
            IF( elapst.GT. 1.00d-9 ) THEN
                     jj= jj + 1
                TIM(jj)= testo
                     IF( jj.GT.1 ) THEN
                         rerr= RELERR( TIM(jj), TIM(jj-1))
                     ENDIF
                TER(jj)= rerr
            ENDIF
c
            IF( iou.GT.0 ) THEN
         WRITE( iou,64) 10*klm,  testo, rerr
            ENDIF
            IF( rerr  .LT. toler   ) GO TO 825
            IF( elapst.GT. 10.00d0 ) GO TO 822
           klm= klm + klm
  820 continue
c                                  Poor accuracy on exit from loop
  822     IF( j .LE. 1 )  GO TO 911
          IF( jj.LT. 1 )  GO TO 911
         CALL SORDID( INX,TMX,  TER,jj,1)
       testo= TIM( INX(1))
        rerr= TMX(1)
       WRITE( io,63)  100.00d0*rerr
c                                  Good convergence, satifies 1% error tolerence
  825 continue
      testov        = testo
      retest        = rerr * testov
      ENDIF
c
c***********************************************************************
c                                  Generate data sets
      SEE(1,1,jr,il)= testov
      SEE(2,1,jr,il)= retest
      ticks         = testov
      TICK          = testov
      L      = 1
      Loop   = 1
      LP     = Loop
      j      = TEST(0)
c
      DO 20 k= 1,47
      TIME(k)= 0.0d0
      CSUM(k)= 0.0d0
20    CONTINUE
c
      IF( il .EQ. 1 )  THEN
          CALL  STATS( SEE(1,2,jr,il), U, nt1)
c         CALL  STATS( SEE(1,3,jr,il), P, nt2)
          CALL  STATS( SEE(1,3,jr,il), P1(l4813+1), nt2-l4813)
      ELSE
          DO 45 k= 1,5
              SEE( k,2,jr,il)= SEE( k,2,jr,1)
              SEE( k,3,jr,il)= SEE( k,3,jr,1)
   45     continue
      ENDIF
c
      IF( iou.GT.0 ) THEN
      WRITE( iou, 99)
      WRITE( iou,100)
      WRITE( iou,102)  ( SEE(k,1,jr,il), k= 1,2)
      WRITE( iou,104)  ( SEE(k,2,jr,il), k= 1,4)
      WRITE( iou,104)  ( SEE(k,3,jr,il), k= 1,4)
      ENDIF
c
      CALL TRACK ('TICK    ')
      RETURN
c
  911         WRITE( io,61)
              WRITE( io,62) elapst, j
              CALL WHERE(0)
c
   61 FORMAT(1X,'FATAL(TICK): cant measure overhead time of subr. TEST')
   62 FORMAT(/,13X,'using SECOND:  elapst=',1E20.8,6X,'J=',I4)
   63 FORMAT(1X,'WARNING(TICK):  TEST overhead time relerr',f9.4,'%')
   64 FORMAT(1X,'testov(TICK)',I12,E12.4,F11.4)
   99 FORMAT(//,' CLOCK OVERHEAD: ' )
  100 FORMAT(/,14X,'AVERAGE',8X,'STANDEV',8X,'MINIMUM',8X,'MAXIMUM' )
  102 FORMAT(/,1X,' TICK',4E15.6)
  104 FORMAT(/,1X,' DATA',4E15.6)
      END
c
c***********************************************
      SUBROUTINE TILE( sm, si, OX,IX,W,ew,T,tiles,n)
c***********************************************
c
c     TILE       computes  m-tile value and corresponding index
c
c     sm      -  RESULT VALUE  IS m-TILE VALUE
c     si      -  RESULT VALUE  IS CORRESPONDING INDEX.r IN W
c
c     OX      -  INPUT  ARRAY  OF ORDERED (DECENDING) Xs.
c     IX      -  INPUT  ARRAY  OF INDEX LIST MAPS X TO OX.
c     W       -  INPUT  ARRAY  OF INPUT  WEIGHTS.
c     ew      -  INPUT  VALUE  FLAGS EQUAL WEIGHTS= 1.0; ELSE 0.0d0
c     T       -  INPUT  VALUE  IS SUM OF WEIGHTS
c     tiles   -  INPUT  VALUE  IS FRACTION OF RANGE, E.G. 0.25
c     n       -  INPUT  NUMBER OF INPUT  VALUES IN X.
c
c***********************************************
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
c
      DIMENSION  OX(n), IX(n), W(n)
c
      CALL TRACE ('TILE    ')
c
       thresh= tiles*T + 0.50d0*ew*W(1)
            R= 0.0d0
            S= R
      DO 70 k= 1,n
            S= R
            R= R + W( IX(k))
           IF( R .GT. thresh )  GO TO 7
   70 CONTINUE
            k= n
    7       z= 0.0d0
            y= 0.0d0
           IF( k.GT.1 )    y =   OX(k-1)
           IF( R.NE.S )    z = ( thresh - S)/( R - S)
           sm= y         + z * ( OX(k)  - y)
           si= REAL(k-1) + z
c
      CALL TRACK ('TILE    ')
      RETURN
      END
c
c***********************************************
      SUBROUTINE TRACE ( name )
c***********************************************
c
c      Records /DEBUG/ info: sequence of called subroutine names
c      Enters current subroutine name on top of /DEBUG/ stack
c      NOT NECESSARY FOR PERFORMANCE TEST, MAY BE DUMMIED.
c
c      name   -  Input;  Callers name
c      ISTACK -  Contains names of subroutines in active linkage chain.
c
c                Interupt shows active chain of subr. names in ISTACK:
c      bkp kernel
c      run
c      BREAKPOINT REACHED AT 00417457PB = KERNEL:KERNEL+201PB
c      bkp trap
c      run
c      BREAKPOINT REACHED AT 00450122PB = TRAP:TRAP+45PB
c      sub= tracks  bcd  istack,10  dec  nstack,10
c
c      ISTACK(1) = "IQRANF  VALUES  TEST    KERNEL   MAIN.  "
c      NSTACK(1) =  164  162  157  65  1  0  0  0  0  0
c
c                Setting TRACE  call 5317 causes CALL to STOPS:
c      sub= tracks
c      set match= 5317
c      rel all.
c      bkp trace
c      run
c      BREAKPOINT REACHED AT 00440575PB = STOPS:STOPS+6PB
c      sub= tracks  bcd  istack,10  dec  nstack,10
c
c      ISTACK(1) = "SORDID  STATW   SENSIT  REPORT   MAIN.  "
c      NSTACK(1) =  5317  5316  5308  5282  1  0  0  0  0  0
c
c***********************************************
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
c
      CHARACTER  name*8, ISTACK*8
      COMMON /DEBUG/     ISTACK(20)
      COMMON /ORDER/ inseq, match, NSTACK(20), isave, iret
c
c                              pushdown stack of subroutine names and call nrs.
          DO  1  k = 10,2,-1
          NSTACK(k)= NSTACK(k-1)
          ISTACK(k)= ISTACK(k-1)
    1     continue
c
          inseq= inseq + 1
      NSTACK(1)= inseq
      ISTACK(1)= name
          isave= inseq
c     WRITE( 6,111) inseq, ( ISTACK(k), k= 1,7 )
c 111 FORMAT(2X,I6,4X,10A8)
c
      IF( inseq.EQ.match ) THEN
          CALL STOPS
      ENDIF
c
          CALL WATCH(1)
c
      RETURN
      END
c
c***********************************************
      SUBROUTINE STOPS
c***********************************************
c
c     This routine is a convenient program break-point which is
c     selected by pre-setting:  match in COMMON /ORDER/  or by data
c     loading in BLOCK DATA  to equal the serial index of a
c     particular call to TRACE , as previously recorded in NSTACK.
c     The call to STOPS is selected in subroutine TRACE .
c
c     PAUSE 1
      RETURN
      END
c
c***********************************************
      SUBROUTINE TRACK( name )
c***********************************************
c
c      Releases current subroutine name from top of /DEBUG/ stack
c      NOT NECESSARY FOR PERFORMANCE TEST, MAY BE DUMMIED.
c
c***********************************************
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
c
      CHARACTER  name*8, ISTACK*8
      COMMON /DEBUG/     ISTACK(20)
      COMMON /ORDER/ inseq, match, NSTACK(20), isave, iret
c
           iret= iret + 1
           CALL  WATCH(2)
c                             pop stack of subroutine names
      IF( name.EQ. ISTACK(1))  THEN
           DO  1  k = 1,9
           NSTACK(k)= NSTACK(k+1)
           ISTACK(k)= ISTACK(k+1)
    1      continue
      ELSE
           ISTACK(20)= name
           CALL  WHERE(12)
      ENDIF
c
      RETURN
      END
c
c***********************************************
      SUBROUTINE TRAP( I, name, mini, maxi,meff)
c***********************************************
c
c      Checks that Index List values are in valid domain
c
c     I     - ARRAY  OF INPUT INDEX-LIST
c     name  -           INPUT CALLERS name
c     mini  - INPUT SMALLEST INDEX VALUE
c     maxi  - INPUT LARGEST  INDEX VALUE
c     meff  - NUMBER OF INPUT VALUES IN I.
c
c***********************************************
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
c
      COMMON /SPACES/ ion,j5,k2,k3,MULTI,laps,Loop,m,kr,LP,n13h,ibuf,nx,
     1 L,npass,nfail,n,n1,n2,n13,n213,n813,n14,n16,n416,n21,nt1,nt2,
     2 last,idebug,mpy,Loops2,mucho,mpylim, intbuf(16)
      DIMENSION  I(meff)
c
      CALL TRACE ('TRAP    ')
c
           LX= 0
      DO 1 k= 1,meff
          IF( I(k).LT.mini .OR. I(k).GT.maxi )  LX= k
    1 CONTINUE
c
          IF( LX.NE.0 )   THEN
              io= ABS( ion)
              IF( io.LE.0 .OR. io.GT.10 ) io=6
              WRITE( io,110)  LX, name
  110   FORMAT(////,' TRAP: ERROR IN INDEX-LIST(',i4,')  IN SUBR:  ',A )
              WRITE( io,113) I
  113         FORMAT(1X,10I6)
c
              CALL WHERE(0)
          ENDIF
c
      CALL TRACK ('TRAP    ')
      RETURN
      END
c
c***********************************************
      SUBROUTINE TRIAL( iou, i, t0, tj )
c***********************************************
c
c     TRIAL - validates checksums of current run for endurance trial
c
c***********************************************************************
c
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
cout  DOUBLE  PRECISION  SUMS                                           REDUNDNT
c
c/      PARAMETER( kn= 47, kn2= 95, np= 3, ls= 3*47, krs= 24)
c/      PARAMETER( nk= 47, nl= 3, nr= 8 )
      parameter( mall= 24 * 3 )
c
      COMMON /ALPHA/ mk,ik,im,ml,il,Mruns,Nruns,jr,iovec,NPFS(8,3,47)
c
      COMMON /BETA / tic, TIMES(8,3,47), SEE(5,3,8,3),
     1              TERRS(8,3,47), CSUMS(8,3,47),
     2              FOPN(8,3,47), DOS(8,3,47)
c
      COMMON /SPACES/ ion,j5,k2,k3,MULTI,laps,Loop,m,kr,LP,n13h,ibuf,nx,
     1 L,npass,nfail,n,n1,n2,n13,n213,n813,n14,n16,n416,n21,nt1,nt2,
     2 last,idebug,mpy,Loops2,mucho,mpylim, intbuf(16)
c
      COMMON /TAU/   tclock, tsecov, testov, cumtim(4)
c
      COMMON /PROOF/  SUMS(24,3,8)
      DIMENSION  ID(mall), LD(mall), CS1(mall), CS2(mall)
      SAVE isum
      MODI(ii,mm)= (MOD( ABS(ii)-1, mm) + 1)
      NPER(ii,mm)= ((ABS(ii)-1+mm)/(mm))
c
      CALL TRACE ('TRIAL   ')
c
      IF( i.EQ.1 ) THEN
           estime= (tj-t0) + REAL( Mruns) *( SECND2(0) - tj)
      WRITE( iou,70) estime, Nruns
      WRITE(   *,70) estime, Nruns
   70 FORMAT(/,' ESTIMATED TOTAL JOB CPU-TIME:=' ,F10.3,' sec.',
     1 '  ( Nruns=',I8,' Trials)',/)
      ENDIF
c
                           ijk= 4
      IF( MULTI.LE.   1 )  ijk= 1
      IF( MULTI.EQ.  10 )  ijk= 2
      IF( MULTI.EQ.  50 )  ijk= 3
      IF( MULTI.GE. 100 )  ijk= 4
c
           lx= 0
      DO  1 j= im,ml
      DO  1 k= 1,24
           lx= lx + 1
      CS1(lx)= CSUMS(jr,j,k)
      CS2(lx)= SUMS(k,j,ijk)
    1 continue
c
      CALL  SEQDIG( ID, isum, CS1, CS2, mall)
c
      IF( i.EQ.1 ) THEN
c
          DO 2 k= 1,mall
          LD(k)= ID(k)
    2     continue
      ELSE
          IF( isum.EQ.last .AND. isum.GT.200 ) THEN
              npass= npass + 1
          ELSE
              nfail= nfail + 1
c
              DO 4 k= 1,mall
              IF( ID(k) .NE. LD(k))  THEN
              WRITE( iou,333) i, MODI(k,24), NPER(k,24), ID(k), LD(k)
              ENDIF
    4         continue
          ENDIF
      ENDIF
c
c
      IF( i.LE.7 .OR. MODI(i,7).EQ.1 )  THEN
      WRITE( iou,111) i, isum, npass, nfail
      WRITE(   *,111) i, isum, npass, nfail
  111 FORMAT(' Trial=',I7,13X,'ChkSum=',I5,4X,'Pass=',I7,5X,'Fail=',I7)
c
c     cumtim(1)= 0.0d0
c          tjob= SECOND( cumtim(1)) - t0
c     WRITE( iou,123)  tjob
c     WRITE(   *,123)  tjob
c 123 FORMAT(2X,'Tcpu=',4X,F10.2,' sec')
c
c     WRITE( iou,222) ( MODI(k,24), ID(k), CS1(k), CS2(k), k= 1,mall )
c 222 FORMAT(2X,2I6,3X,2E24.16)
  333 FORMAT(1X,'TRIAL:',I7,6X,'Kernel=',I5,6X,'j= ',I7,6X,'ERROR',2I7)
      ENDIF
        last= isum
        ibuf= 0
c
      CALL TRACK ('TRIAL   ')
      RETURN
      END
c
c***********************************************
      SUBROUTINE VALID( VX,MAP,LX,  BL,X,BU,n )
c***********************************************
c
c      Compress valid data sets;  form compression list.
c
c
c     VX    - ARRAY  OF RESULT COMPRESSED Xs.
c     MAP   - ARRAY  OF RESULT COMPRESSION INDICES
c     LX     -           RESULT COMPRESSED LENGTH OF VX, MAP
c           -
c     BL    -           INPUT LOWER BOUND FOR VX
c     X     - ARRAY  OF INPUT VALUES.
c     BU    -           INPUT UPPER BOUND FOR VX
c     n     - NUMBER OF INPUT VALUES IN X.
c
c***********************************************
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
c
      DIMENSION  VX(n), MAP(n), X(n)
cLLL. OPTIMIZE LEVEL G
c
      CALL TRACE ('VALID   ')
c
           m= 0
           LX= 0
          IF( n.LE.0 )  GO TO 73
      DO 1 k= 1,n
              IF( X(k).LE. BL .OR. X(k).GE. BU )  GO TO 1
                     m= m + 1
                MAP(m)= k
                 VX(m)= X(k)
    1 CONTINUE
c
      LX= m
      IF( m.GT.0 )  THEN
          CALL TRAP( MAP, ' VALID  ' , 1, n,m)
      ENDIF
   73 CONTINUE
      CALL TRACK ('VALID   ')
      RETURN
      END
c
c***********************************************
      SUBROUTINE VALUES(i)
c***********************************************
c
c            VALUES     initializes special values
c
c     i    :=  kernel number
c
c****************************************************************************
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
cout  DOUBLE  PRECISION  DS, DW                                         REDUNDNT
c
c/      PARAMETER( l1=   1001, l2=   101, l1d= 2*1001 )
c/      PARAMETER( l13= 64, l13h= 64/2, l213= 64+32, l813= 8*64 )
c/      PARAMETER( l14= 2048, l16= 75, l416= 4*75 , l21= 25)
c
c/      PARAMETER( kn= 47, kn2= 95, np= 3, ls= 3*47, krs= 24)
c
      COMMON /SPACES/ ion,j5,k2,k3,MULTI,laps,Loop,m,kr,LP,n13h,ibuf,nx,
     1 L,npass,nfail,n,n1,n2,n13,n213,n813,n14,n16,n416,n21,nt1,nt2,
     2 last,idebug,mpy,Loops2,mucho,mpylim, intbuf(16)
c
      COMMON /SPACER/ A11,A12,A13,A21,A22,A23,A31,A32,A33,
     1                AR,BR,C0,CR,DI,DK,
     2  DM22,DM23,DM24,DM25,DM26,DM27,DM28,DN,E3,E6,EXPMAX,FLX,
     3  Q,QA,R,RI,S,SCALE,SIG,STB5,T,XNC,XNEI,XNM
c
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), ticks,
     1                FR(9), TERR1(47), SUMW(7), START,
     2              SKALE(47), BIAS(47), WS(95), TOTAL(47), FLOPN(47),
     3                IQ(7), NPF, NPFS1(47)
c
      COMMON /SPACEI/ WTP(3), MUL(3), ISPAN(47,3), IPASS(47,3)
c
      INTEGER    E,F,ZONE
      COMMON /ISPACE/ E(96), F(96),
     1  IX(1001), IR(1001), ZONE(300)
c
      COMMON /SPACE1/ U(1001), V(1001), W(1001),
     1  X(1001), Y(1001), Z(1001), G(1001),
     2  DU1(101), DU2(101), DU3(101), GRD(1001), DEX(1001),
     3  XI(1001), EX(1001), EX1(1001), DEX1(1001),
     4  VX(1001), XX(1001), RX(1001), RH(2048),
     5  VSP(101), VSTP(101), VXNE(101), VXND(101),
     6  VE3(101), VLR(101), VLIN(101), B5(101),
     7  PLAN(300), D(300), SA(101), SB(101)
c
      COMMON /SPACE2/ P(4,512), PX(25,101), CX(25,101),
     1  VY(101,25), VH(101,7), VF(101,7), VG(101,7), VS(101,7),
     2  ZA(101,7)  , ZP(101,7), ZQ(101,7), ZR(101,7), ZM(101,7),
     3  ZB(101,7)  , ZU(101,7), ZV(101,7), ZZ(101,7),
     4  B(64,64), C(64,64), H(64,64),
     5  U1(5,101,2),  U2(5,101,2),  U3(5,101,2)
c
      COMMON /BASE2/ P0(4,512), PX0(25,101), CX0(25,101),
     1  VY0(101,25), VH0(101,7), VF0(101,7), VG0(101,7), VS0(101,7),
     2  ZA0(101,7)  , ZP0(101,7), ZQ0(101,7), ZR0(101,7), ZM0(101,7),
     3  ZB0(101,7)  , ZU0(101,7), ZV0(101,7), ZZ0(101,7),
     4  B0(64,64), CC0(64,64), H0(64,64),
     5  U10(5,101,2),  U20(5,101,2),  U30(5,101,2)
c
      COMMON /SPACE3/ CACHE(8192)
c
c     ******************************************************************
      CALL TRACE ('VALUES  ')
c
      CALL SIZES (i)
      IP1= i
c              Initialize the dummy  Cache-memory with never used data-set.
      DO 666 k= 1,8192
      CACHE(k)= 0.10
  666 CONTINUE
c
      CALL  SUPPLY( i)
c
      IF( IP1.NE.13 ) GO TO 14
            DS= 1.000d0
            DW= 0.500d0
      DO 205 j= 1,4
      DO 205 k= 1,512
      P(j,k)  = DS
      P0(j,k) = DS
            DS= DS + DW
  205 CONTINUE
c
      DO 210 j= 1,96
      E(j) = 1
      F(j) = 1
  210 CONTINUE
c
   14 IF( IP1.NE.14) GO TO 16
c
      mmin= 1
      mmax= 1001
      CALL IQRANF( IX, mmin, mmax, 1001)
c
            DW= -100.000d0
      DO 215 J= 1,1001
      DEX(J) =  DW*DEX(J)
      GRD(J) = IX(J)
  215 CONTINUE
      FLX= 0.00100d0
c
   16 IF( IP1.NE.16 ) GO TO 50
cONDITIONS:
            MC= 2
            lr= n
            II= lr/3
            FW= 1.000d-4
          D(1)= 1.0198048642876400d0
      DO 400 k= 2,300
  400     D(k)= D(k-1) + FW/D(k-1)
             R= D(lr)
            FW= 1.000d0
      DO 403 LX= 1,MC
             m= (lr+lr)*(LX-1)
      DO 401 j= 1,2
      DO 401 k= 1,lr
             m= m+1
             S= REAL(k)
       PLAN(m)= R*((S + FW)/S)
  401  ZONE(m)= k+k
  403 CONTINUE
             k= lr+lr+1
       ZONE(k)= lr
             S= D(lr-1)
             T= D(lr-2)
c
   50 CONTINUE
c               Clear the scalar Cache-memory with never used data-set.
c     fw= 1.000d0
c     CALL SIGNEL( CACHE, fw, 0.0d0, 8192)
c
             j= 0
            sc= 0.0d0
      DO 777 k= 1,8192
            IF( CACHE(k).EQ. 0.0)  THEN
             j= j + k
            sc= sc + REAL(j*k)
            ENDIF
  777 CONTINUE
c
      CALL TRACK ('VALUES  ')
      RETURN
      END
c
c***********************************************
      SUBROUTINE VERIFY( iou )
c***********************************************************************
c                                                                      *
c      VERIFY     auxiliary test routine to check-out function SECOND  *
c                 and to verify that sufficiently long Loop sizes are  *
c                 defined in Subr. SIZES for accurate CPU timing.      *
c                                                                      *
c       iou    -  Logical Output Device Number                         *
c                                                                      *
c***********************************************************************
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
c
cLOX  REAL*8 SECOND
c
c/C/      PARAMETER( l1=   1001, l2=   101, l1d= 2*1001 )
c/C/      PARAMETER( l13= 64, l13h= 64/2, l213= 64+32, l813= 8*64 )
c/C/      PARAMETER( l14= 2048, l16= 75, l416= 4*75 , l21= 25)
c/C/      PARAMETER( kn= 47, kn2= 95, np= 3, ls= 3*47, krs= 24)
c
      parameter( ntmp= 100 )
c
      COMMON /SPACE1/ U(1001), V(1001), W(1001),
     1  X(1001), Y(1001), Z(1001), G(1001),
     2  DU1(101), DU2(101), DU3(101), GRD(1001), DEX(1001),
     3  XI(1001), EX(1001), EX1(1001), DEX1(1001),
     4  VX(1001), XX(1001), RX(1001), RH(2048),
     5  VSP(101), VSTP(101), VXNE(101), VXND(101),
     6  VE3(101), VLR(101), VLIN(101), B5(101),
     7  PLAN(300), D(300), SA(101), SB(101)
c
      COMMON /SPACE2/ P(4,512), PX(25,101), CX(25,101),
     1  VY(101,25), VH(101,7), VF(101,7), VG(101,7), VS(101,7),
     2  ZA(101,7)  , ZP(101,7), ZQ(101,7), ZR(101,7), ZM(101,7),
     3  ZB(101,7)  , ZU(101,7), ZV(101,7), ZZ(101,7),
     4  B(64,64), C(64,64), H(64,64),
     5  U1(5,101,2),  U2(5,101,2),  U3(5,101,2)
c
      COMMON /ALPHA/ mk,ik,im,ml,il,Mruns,Nruns,jr,iovec,NPFS(8,3,47)
      COMMON /TAU/   tclock, tsecov, testov, cumtim(4)
c
      COMMON /BETA / tic, TIMES(8,3,47), SEE(5,3,8,3),
     1              TERRS(8,3,47), CSUMS(8,3,47),
     2              FOPN(8,3,47), DOS(8,3,47)
c
      COMMON /SPACES/ ion,j5,k2,k3,MULTI,laps,Loop,m,kr,LP,n13h,ibuf,nx,
     1 L,npass,nfail,n,n1,n2,n13,n213,n813,n14,n16,n416,n21,nt1,nt2,
     2 last,idebug,mpy,Loops2,mucho,mpylim, intbuf(16)
c
      COMMON /SPACEI/ WTP(3), MUL(3), ISPAN(47,3), IPASS(47,3)
c
c
      DIMENSION  TIM(ntmp), TUM(ntmp), TAV(ntmp), TER(ntmp)
      DIMENSION  TMX(ntmp), SIG(ntmp), LEN(ntmp)
c
c
c     CALL TRACE ('VERIFY  ')
c
      DO  1 k = 1,101
          X(k)= 0.0d0
          Y(k)= 0.0d0
    1  CX(1,k)= 0.0d0
           nzd= 0
c
c***********************************************************************
c     Measure tsecov:  Overhead time for calling SECOND
c***********************************************************************
c
      tsecov = SECOVT( iou)
         tic = tsecov
c
c***********************************************************************
c     Measure time resolution of cpu-timer;  tclock= MIN t
c***********************************************************************
c
        fuzz= 1.00d-12
      nticks= INT( 1.00d2/( tsecov + fuzz ))
          IF( nticks.LT.10000 ) nticks= 10000
          dt= 0.00d0
          t1= SECND2( cum)
           m= 0
c
      DO 2 k= 1,nticks
          t2= SECND2( cum)
          IF( t2 .NE. t1 ) THEN
                  m= m + 1
                 dt= dt + ( t2 - t1 )
                 t1= t2
                 IF( m .GE. 200 ) GO TO 3
          ENDIF
    2 continue
c
    3     IF( m.LE.2 .OR. dt.LE.0.00d0 ) THEN
              tclock= 1.00d0
              WRITE(   *,163)
              WRITE( iou,163)
          ELSE
              tclock= dt/( REAL(m) + fuzz )
          ENDIF
c
       WRITE(   *,164) m, tclock
       WRITE( iou,164) m, tclock
  163 FORMAT(1X,'WARNING(VERIFY): POOR Cpu-timer resolution; REPLACE?')
  164 FORMAT('VERIFY:',I10,E12.4,' =  Time Resolution of Cpu-timer')
c
c****************************************************************************
c         VERIFY ADEQUATE Loop SIZE VERSUS CPU CLOCK ACCURACY
c****************************************************************************
c
c         VERIFY produced the following output on CRAY-XMP4 in a
c         fully loaded, multi-processing, multi-programming system:
c
c
c         VERIFY ADEQUATE Loop SIZE VERSUS CPU CLOCK ACCURACY
c         -----     -------     -------    -------   --------
c         EXTRA     MAXIMUM     DIGITAL    DYNAMIC   RELATIVE
c         Loop      CPUTIME     CLOCK      CLOCK     TIMING
c         SIZE      SECONDS     ERROR      ERROR     ERROR
c         -----     -------     -------    -------   --------
c             1  5.0000e-06      10.00%     17.63%     14.26%
c             2  7.0000e-06       7.14%      6.93%      4.79%
c             4  1.6000e-05       3.12%      6.56%      7.59%
c             8  2.8000e-05       1.79%      2.90%      2.35%
c            16  6.1000e-05       0.82%      6.72%      4.50%
c            32  1.1700e-04       0.43%      4.21%      4.62%
c            64  2.2700e-04       0.22%      3.13%      2.41%
c           128  4.4900e-04       0.11%      3.14%      0.96%
c           256  8.8900e-04       0.06%      2.06%      2.50%
c           512  1.7740e-03       0.03%      1.92%      1.59%
c          1024  3.4780e-03       0.01%      0.70%      1.63%
c          1360              Current Run:    MULTI=   10.000
c          2048  7.0050e-03       0.01%      0.74%      1.28%
c          4096  1.3823e-02       0.00%      1.35%      0.78%
c         -----     -------     -------    -------   --------
c
c          Approximate Serial Job Time=   2.5e+01 Sec.    ( Nruns= 7 RUNS)
c
c****************************************************************************
c
                WRITE( iou,45)
                WRITE( iou,49)
                WRITE( iou,46)
                WRITE( iou,47)
                WRITE( iou,48)
                WRITE( iou,49)
   45 FORMAT(/,8X,'VERIFY ADEQUATE Loop SIZE VERSUS CPU CLOCK ACCURACY')
   46 FORMAT(8X,'EXTRA     MAXIMUM     DIGITAL    DYNAMIC   RELATIVE')
   47 FORMAT(8X,'Loop      CPUTIME     CLOCK      CLOCK     TIMING  ')
   48 FORMAT(8X,'SIZE      SECONDS     ERROR      ERROR     ERROR   ')
   49 FORMAT(8X,'-----     -------     -------    -------   --------')
c
c
c****************************************************************************
c     Measure Cpu Clock Timing Errors As A Function Of Loop Size(lo)
c****************************************************************************
c
         ttest= 100.00d0 * tclock
        ilimit= 30
            nj= 5
            lo= 128
             i= 0
c
   10        i= i + 1
            lo= lo + lo
      DO 53  j= 1,nj
             n= 100
      cumtim(1)= 0.0d0
             t0= SECND2( cumtim(1))
c                                    Time Kernel 12
      DO 12 m = 1,lo
      DO 12 k = 1,n
   12     X(k)= X(k+1) - X(k)
c
      cumtim(1)= 0.0d0
         TIM(j)= SECND2( cumtim(1)) - t0 - tsecov
   53 continue
c                                    Compute Dynamic Clock Error
c
          CALL  STATS( TUM, TIM, nj)
         rterr= 100.0*( TUM(2)/( TUM(1) + fuzz ))
            IF( TUM(1).LE. 0.00d0)  rterr= 100.00d0
c
c                                    Compute Digital Clock Error
c
          CALL  TDIGIT( SIG(i), nzd, TUM(4))
c
        TAV(i)= TUM(1)
        TMX(i)= TUM(4)
        TER(i)= rterr
        LEN(i)= lo
      IF( i.GT.ilimit .AND. ( TUM(1).LT.fuzz )) THEN
      WRITE(  *,146)  lo, TUM(1)
  146 FORMAT('VERIFY:',I12,' Repetitions.  Bad Timer=',E14.5,' sec.')
      ENDIF
      IF( i.LE.8 .OR.  ( TUM(1).LT.ttest .AND. i.LT.ntmp )) GO TO 10
            nn= i
c
c****************************************************************************
c     Compute Multiple-Pass Loop Counters MULTI and Loops2
c     Such that:  each Kernel is run at least 100 ticks of Cpu-timer.
c****************************************************************************
c
          i2= 2
       MULTI= 1
       mucho= 1
        CALL  SIZES(12)
      loop12= IPASS(12,2) * MUL(2)
c
c
      multo= INT( (REAL(lo)/(REAL(loop12)+fuzz))*(ttest/(TUM(1)+fuzz)))
c
C     IF TIMING ERRORS ARE TOO LARGE, YOU MUST INCREASE MULTI...  here:
c
      MULTI= 2 * multo
      mucho= MULTI
c
c     When MULTI.GE.100 each kernel is executed over a million times
c     and the time used to re-intialize overstored input variables
c     is negligible.  Thus each kernel may be run arbitrarily many times
c     (MULTI >> 100) without overflow and produce verifiable checksums.
c
c     Each kernel's results are automatically checksummed for  MULTI :=
c
c     MULTI=   1      clock resolution << 0.01 SEC,  or Cpu << 1 Mflops
c     MULTI=  10      clock resolution << 0.01 SEC,  or Cpu <  2 Mflops
c     MULTI=  50      clock resolution <= 0.01 SEC,  or Cpu <  2 Mflops
c     MULTI= 100      clock resolution <= 0.01 SEC,  or Cpu <  5 Mflops
c     MULTI= 200      clock resolution <= 0.01 SEC,  or Cpu < 10 Mflops
c
c     MULTI=   1
c     MULTI=  10
c     MULTI=  50
c     MULTI= 100
c     MULTI= 200
c
                 mpy= 1
              Loops2= 1
              mpylim= Loops2
          IF( MULTI.LE.  1 ) THEN
              MULTI =    1
      ELSEIF( MULTI.LE. 10 ) THEN
              MULTI =   10
      ELSEIF( MULTI.LE. 50 ) THEN
              MULTI =   50
      ELSEIF( MULTI.LE.100 ) THEN
              MULTI =  100
      ELSE
              Loops2= (MULTI + 50)/100
              mpylim= Loops2
              MULTI =  100
      ENDIF
c
c
         mucho= MULTI
        loops0= loop12 * MULTI * Loops2
        repeat= REAL(    MULTI * Loops2 )
            IF( Loop.EQ.1 ) repeat= 1.00d0/( REAL( loop12) + fuzz)
c
c****************************************************************************
c     Estimate Timing Error By Comparing Time Of Each Run With Longest Run
c****************************************************************************
c
             m= 0
           tnn= ( TAV(nn) + 2.00d0* TAV(nn-1))* 0.500d0
          fuzz= 1.0d-12
            IF( tnn.LT.fuzz)  tnn= fuzz
      DO 69  i= 1,nn
         rterr= TER(i)
            lo= LEN(i)
c                                    Compute Relative Clock Error
c
            rt= 0.0d0
            IF( LEN(i).GE. 0)     rt= LEN(nn)/LEN(i)
         rperr= 100.00d0
            IF( tnn.GT.fuzz) rperr= 100.00d0*(ABS( tnn - rt*TAV(i))/tnn)
         WRITE( iou,64) lo, TMX(i), SIG(i),rterr, rperr
   64   FORMAT(6X,I7,E12.4,F11.2,'%',F10.2,'%',F10.2,'%')
c
c                                    Find loops0 Size Used
c
            IF( (loops0.GE.lo) .AND. (loops0.LE.2*lo))  THEN
                     m= lo
                WRITE( iou,66)  loops0, repeat
                WRITE(   *,66)  loops0, repeat
                IF( rterr .GT. 10.00d0)  THEN
                  WRITE( iou, 67)
                  WRITE( iou, 68)
                  WRITE(   *, 67)
                  WRITE(   *, 68)
                ENDIF
   66 FORMAT(7X,i6,7X,'Repetition Count = MULTI * Loops2 = ',F12.3)
   67 FORMAT(34X,'VERIFY: POOR TIMING OR ERROR. NEED LONGER RUN ' )
   68 FORMAT(34X,'INCREASE:   MULTI  IN SUBR. VERIFY'     )
            ENDIF
c
   69 continue
            IF( m.LE.0 )  THEN
                WRITE( iou,66)  loops0, repeat
                WRITE(   *,66)  loops0, repeat
            ENDIF
                WRITE( iou,49)
c
c****************************************************************************
c     Clock Calibration Test of Internal Cpu-timer SECOND;
c           Verify 10 Internal SECOND Intervals using External Stopwatch
c****************************************************************************
c
c
  106 FORMAT(//,' CLOCK CALIBRATION TEST OF INTERNAL CPU-TIMER: SECOND')
  107 FORMAT(' MONOPROCESS THIS TEST, STANDALONE, NO TIMESHARING.')
  108 FORMAT(' VERIFY TIMED INTERVALS SHOWN BELOW USING EXTERNAL CLOCK')
  109 FORMAT(' START YOUR STOPWATCH NOW !')
  113 FORMAT(/,'           Verify  T or DT  observe external clock:',/)
  114 FORMAT('           -------     -------      ------      -----')
  115 FORMAT('           Total T ?   Delta T ?    Mflops ?    Flops')
  119 FORMAT(4X,I2,3F12.2,2E15.5)
  120 FORMAT(' END CALIBRATION TEST.',/)
          WRITE( iou,106)
          WRITE( iou,107)
          WRITE( iou,108)
          WRITE( iou,109)
          WRITE( iou,113)
          WRITE( iou,114)
          WRITE( iou,115)
          WRITE( iou,114)
          WRITE(   *,106)
          WRITE(   *,107)
          WRITE(   *,108)
          WRITE(   *,109)
          WRITE(   *,113)
          WRITE(   *,114)
          WRITE(   *,115)
          WRITE(   *,114)
c
           task= 10.00d0
         passes= REAL(lo) * ( task/( tnn + fuzz))
         loiter= INT( passes )
          flops= 0.00d0
      cumtim(1)= 0.0d0
             t1= SECND2( cumtim(1))
             t2= 0.00d0
c
      DO 86   j= 1,4
              n= 100
             t0= t1
c                                    Time Kernel 12
      DO 82  m = 1,loiter
      DO 82  k = 1,n
   82      X(k)= X(k+1) - X(k)
c
      cumtim(1)= 0.0d0
             t1= SECND2( cumtim(1))
             td= t1 - t0 -tsecov
             t2= t2 + td
          flops= flops + passes*REAL(n)
         ratemf= ( 1.00d-6 * flops )/( t2 + fuzz )
          WRITE(   *,119)  j, t2, td, ratemf, flops
          WRITE( iou,119)  j, t2, td, ratemf, flops
   86 continue
          WRITE( iou,114)
          WRITE( iou,120)
          WRITE(   *,114)
          WRITE(   *,120)
c
c     CALL TRACK ('VERIFY  ')
      RETURN
      END
c
c***********************************************
      SUBROUTINE WATCH( mode)
c***********************************************
c
c  WATCH is called at every subroutine entry and exit point by TRACE .
c  COMMON variables may be tested continually during execution(watched)
c  for known error conditions so the occurance of the error is localized.
c  WATCH may be used for programmable data-breakpoints to aid debugging.
c
c***********************************************
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
c
c     parameter( ntests=  1, krs1= 24 + 1 )
      parameter( ntests= 14, krs1= 24 + 1 )
c
      CHARACTER  name*8, ISTACK*8
      COMMON /DEBUG/     ISTACK(20)
      COMMON /ORDER/ inseq, match, NSTACK(20), isave, iret
c
      COMMON /ALPHA/ mk,ik,im,ml,il,Mruns,Nruns,jr,iovec,NPFS(8,3,47)
      COMMON /TAU/   tclock, tsecov, testov, cumtim(4)
      COMMON /BETA / tic, TIMES(8,3,47), SEE(5,3,8,3),
     1              TERRS(8,3,47), CSUMS(8,3,47),
     2              FOPN(8,3,47), DOS(8,3,47)
c
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), ticks,
     1                FR(9), TERR1(47), SUMW(7), START,
     2              SKALE(47), BIAS(47), WS(95), TOTAL(47), FLOPN(47),
     3                IQ(7), NPF, NPFS1(47)
c
      COMMON /SPACES/ ion,j5,k2,k3,MULTI,laps,Loop,m,kr,LP,n13h,ibuf,nx,
     1 L,npass,nfail,n,n1,n2,n13,n213,n813,n14,n16,n416,n21,nt1,nt2,
     2 last,idebug,mpy,Loops2,mucho,mpylim, intbuf(16)
      DIMENSION  IE(20)
c     LOGICAL BOUNDS
c     BOUNDS(A,X,B,E)= ((((A)*(1.-E)).LE.(X)).AND.((X).LE.((B)*(1.+E))))
c
c                                       Debug Trace Info
                       name= 'watch'
c     IF( made.EQ.1 )  name= ' ENTRY  '
c     IF( made.EQ.2 )  name= ' RETURN '
c     WRITE(*,101) inseq, name, ISTACK(1)
c 101 FORMAT(1X,I6,5X,A ,1X,A )
c
c                                       Domain Tests of Critical Variables
      DO 1 k= 1,ntests
    1  IE(k)= 0
      IF(    testov  .NE. ticks      ) IE(1)= 1
      IF(    tsecov  .NE. tic        ) IE(2)= 2
      IF( inseq.LE.0 .OR. inseq.NE.isave .OR. inseq.GT.99999) IE(3)= 3
      IF( Nruns.LT.1 .OR. Nruns.GT.8 ) IE(4)= 4
      IF(    il.LT.1 .OR. il.GT.3    ) IE(5)= 5
      IF(    mk.LT.1 .OR. mk.GT.24   ) IE(6)= 6
      IF(    ik.LT.0 .OR. ik.GT.krs1  ) IE(7)= 7
      IF(    jr.LT.1 .OR. jr.GT.8    ) IE(8)= 8
      IF(    Loops2  .LT. 1          ) IE(9)= 9
      IF(    Loops2  .NE. mpylim     ) IE(10)= 10
      IF(    MULTI   .LT. 1          ) IE(11)= 11
      IF(    MULTI   .NE. mucho      ) IE(12)= 12
      IF(    Loop    .LT. 1          ) IE(13)= 13
      IF(    Loop    .NE. LP         ) IE(14)= 14
c
c                        Insert your debug data tests here
c     IF( BOUNDS( 1.7669e+5,CSUMS(jr,1,8),1.7669e+5,1.0e-3)) IE(15)= 15
c
      ierr= 0
      DO 2 k= 1,ntests
    2 ierr= ierr + IE(k)
          IF( ierr.NE.0 )   THEN
              io= ABS( ion)
              IF( io.LE.0 .OR. io.GT.10 ) io=6
                   k1=0
                   k2=0
              WRITE(  *,111)
              WRITE(  *,112) (    k , k= 1,ntests )
              WRITE(  *,112) ( IE(k), k= 1,ntests )
              WRITE(  *,112) k1,k2,inseq,Nruns,il,mk,ik,jr,
     1                       Loops2,mpylim,MULTI,mucho,Loop,LP
              WRITE( io,111)
              WRITE( io,112) (    k , k= 1,ntests )
              WRITE( io,112) ( IE(k), k= 1,ntests )
              WRITE( io,112) k1,k2,inseq,Nruns,il,mk,ik,jr,
     1                       Loops2,mpylim,MULTI,mucho,Loop,LP
  111         FORMAT(/,' WATCH: STORAGE FAULT DETECTED.  IE=')
  112         FORMAT(1X,15I5)
              CALL WHERE( mode)
          ENDIF
      RETURN
      END
c
c***********************************************
      SUBROUTINE WHERE( mode)
c***********************************************
c
c  Prints Subroutine names in the active linkage chain for debugging.
c
c***********************************************
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
c
      parameter( insert= 2 )
      COMMON /SPACES/ ion,j5,k2,k3,MULTI,laps,Loop,m,kr,LP,n13h,ibuf,nx,
     1 L,npass,nfail,n,n1,n2,n13,n213,n813,n14,n16,n416,n21,nt1,nt2,
     2 last,idebug,mpy,Loops2,mucho,mpylim, intbuf(16)
c
      CHARACTER  name*8, ISTACK*8
      COMMON /DEBUG/     ISTACK(20)
      COMMON /ORDER/ inseq, match, NSTACK(20), isave, iret
c
      made= MOD( mode,10)
                       name= 'internal'
      IF( made.EQ.1 )  name= ' ENTRY  '
      IF( made.EQ.2 )  name= ' RETURN '
      io= ABS( ion)
      IF( io.LE.0 .OR. io.GT.10 ) io=6
c
      IF( mode.EQ.12 ) THEN
           WRITE(  *,112)  ISTACK(20), ISTACK(1)
           WRITE( io,112)  ISTACK(20), ISTACK(1)
  112      FORMAT(2X,'WHERE: SEQ.ERROR.  RETURN ',A  ,'.NE. CALL ',A  )
      ENDIF
c
cPFM  IF( mode.EQ.20 ) THEN
cPFM       WRITE( io,9)
cPFM9      FORMAT(2X,'WHERE: INIPFM FAILED.' )
cPFM  ENDIF
      WRITE(  *,110)  name, ISTACK(1)
      WRITE( io,110)  name, ISTACK(1)
  110 FORMAT(/,' WHERE:  ERROR detected at ',A  ,' point in: ',A  )
c
      IF( made.EQ.1 .OR. made.EQ.2 )  THEN
c                    Pushdown stack of subroutine names and call nrs.
          DO  1  k = 12,insert+1,-1
          NSTACK(k)= NSTACK(k-insert)
          ISTACK(k)= ISTACK(k-insert)
    1     continue
c
          NSTACK(1)= inseq
          ISTACK(1)= 'WATCH   '
          NSTACK(2)= inseq
          ISTACK(2)= 'TRACE   '
          IF( made.EQ.2 )  ISTACK(2)= 'TRACK   '
      ENDIF
      WRITE(  *,111)
      WRITE(  *,114)
      WRITE(  *,113)
      WRITE(  *,114)
      WRITE(  *,118) ( ISTACK(k), NSTACK(k), k= 1,12 )
c
      WRITE( io,111)
      WRITE( io,114)
      WRITE( io,113)
      WRITE( io,114)
      WRITE( io,118) ( ISTACK(k), NSTACK(k), k= 1,12 )
  111 FORMAT(/,' ACTIVE SUBROUTINE LINKAGE CHAIN:')
  114 FORMAT('          ----           -----------')
  113 FORMAT('          name           call number')
  118 FORMAT(10X,A  ,4X,I8)
c
      DO 222 k= 1,200
      WRITE( io,221)
  221 FORMAT(/,' ********* TERMINAL ERROR; FLUSH I/O BUFFER **********')
  222 continue
c     PAUSE
      STOP
c     RETURN
      END

