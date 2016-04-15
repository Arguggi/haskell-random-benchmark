# Benchmark random libraries

The following libraries are included in the benchmark:

- [System.Random][System.Random]
- [System.Random.MWC][System.Random.MWC]
- [System.Random.Mersenne.Pure64][System.Random.Mersenne.Pure64]

to run it:

    $ stack bench --benchmark-arguments '--output=$benchmark.html'

this should create a file called `random-benchmark.html` in the directory. Then
simply view it with:

    $ sensible-browser random-benchmark.html

Otherwise a result of the benchmark on my machine is available in this [repo][benchmark-url].

[System.Random]: http://haddock.stackage.org/lts-5.12/random-1.1/System-Random.html
[System.Random.MWC]: http://haddock.stackage.org/lts-5.12/mwc-random-0.13.4.0/System-Random-MWC.html
[System.Random.Mersenne.Pure64]: http://haddock.stackage.org/lts-5.12/mersenne-random-pure64-0.2.0.5/System-Random-Mersenne-Pure64.html
[benchmark-url]: random-benchmark.html

