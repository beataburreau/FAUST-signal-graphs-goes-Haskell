# Fixed-point Format Inference for the FAUST Signal Graph

[FAUST](https://faust.grame.fr) is a functional programming language for the digital audio processing domain. A Faust program describes an audio signal processor from which it is possible to generate the corresponding implementation in a number of programming languages. The [FAST](https://fast.grame.fr) project aims to extend the Faust compiler with code generation for FPGA-based platforms; an extension that would enable high-level programming - in Faust - of *ultra-low-latency* audio signal processors. Faust uses floating-point numbers, but programs for FPGA-based platforms should use fixed-point numbers when appropriate. So, extending the Faust compiler with code generation for FGPA requires a method for determining sensible fixed-point formats for the signal processor described by a Faust program. 

A fixed-point format is defined by two integers *m, l* and denoted *sfix(m, l)*. The integers are the positions of the format's Most and Least Significant Bits, abbreviated MSB and LSB. The focus of this project was to find a systematic and sensible way of determining the LSBs of the fixed-point formats for any Faust signal graph. This resulted in two algorithms formally presented in the [project report](ProjectReport.pdf) and whose implementation is provided in this repository. 

---

This project is built and run using [Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install-stack). 

With stack installed, run `stack install` to install dependencies and build the project, then `stack ghci` to launch the GHC interactive shell. 
From the shell, the LSBs can be inferred for any dot file (describing a FASUT signal graph) by running `Main.main` and supplying the relative path to the dot file. The output is printed in the shell with the inferred, and trimmed, LSBs in *sfix*. 

```bash
ghci> Main.main
Enter relative file path to a .dot file:
  test/test-files/+.dsp-sig.dot
Parse successful!

LSB inference successful!

1: +, Sample, Float
3: INPUT 1, Sample, Float
2: INPUT 0, Sample, Float
0: OUTPUT, Sample, Float

1 -> 0: Argument 0, [-2.0, 2.0], Float, sfix(3, -4)
3 -> 1: Argument 1, [-1.0, 1.0], Float, sfix(2, -4)
2 -> 1: Argument 0, [-1.0, 1.0], Float, sfix(2, -4)

LSB trimming successful!

1 -> 0: Argument 0, [-2.0, 2.0], Float, sfix(3, -2)
2 -> 1: Argument 0, [-1.0, 1.0], Float, sfix(2, -2)
3 -> 1: Argument 1, [-1.0, 1.0], Float, sfix(2, -2)

```

**Note:** 
The input and output precisions are specified, and can be altered, in Sfix.hs.

