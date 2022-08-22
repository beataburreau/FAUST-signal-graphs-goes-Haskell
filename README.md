# LSB inference using a Haskell representation of FAUST signal graphs


This project is built and run using [Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install-stack). 

With stack installed, run `stack install` to install dependencies and build the project, then `stack ghci` to launch the GHC interactive shell. 
From the shell, the LSBs can be inferred for any dot file (describing a FASUT signal graph) by running `Main.main` and supplying the relative path to the dot file. 

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

NOTE: The input and output precisions are specified, and can be altered, in Sfix.hs.

