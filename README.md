# functional-pathtracers
##### Code by Joyce Lee

#### OCaml and Haskell implementations of a basic Monte Carlo pathtracer
#### Inspired by CIS 460, 120, and 194 at UPenn

### What is it?
> Path tracing is a computer graphics Monte Carlo method of rendering images of three-dimensional scenes such that the global illumination is faithful to reality. Fundamentally, the algorithm is integrating over all the illuminance arriving to a single point on the surface of an object.

The above definition is courtesy of Wikipedia, and does a very good job of summing up what it is. It implies that the algorithm terminates when every pixel value converges, but in practice this is intractable. Instead, a maximum recursion depth is specified instead.

### Why functional?
The concept of pathtracing is mostly recursive, and simply iterated over a two dimensional array of pixels, which lends itself nicely to functional programs. They usually are not written in languages such as OCaml or Haskell because these run many times slower than languages like C or C++, even with compiler optimizations. However, the functional implementation is arguably more elegant and easier to understand.

### How To Use
Both the Haskell and OCaml versions provide a hardcoded scene  which will be rendered if no arguments are given. The two of them have slightly different command line arguments though.

 * OCaml: compile with **ocamlopt pathtracer.ml** and run with
   1. ./a.out *width height samples*
   2. ./a.out *width height output-file*
   3. ./a.out *width height output-file config-file*

   The default output_file is **ocaml.ppm**.
 * Haskell: compile with **ghc -O2 pathtracer.hs** and run with
   1. ./pathtracer
   2. ./pathtracer *width height samples*
   3. ./pathtracer *width height samples config-file*

   The default width, height, and samples are 100, 100, and 60 respectively. The output is always written to **stdout**.

To make your own configuration, each line represents a sphere. The lines should be formatted as such:
##### radius centerX centerY centerZ emittedR emittedG emittedB colorR colorG colorB [Diff/Spec/Refr]

### Check it out!
Take a look at the samples:

 1. [animation.gif](https://github.com/leejcw/functional-pathtracers/blob/master/output/animation.gif) (OCaml) *post-processed in C to denoise with a small Gaussian kernel and compress*
 2. [example.png](https://github.com/leejcw/functional-pathtracers/blob/master/output/example.png) (OCaml) and [example-hs.png](https://github.com/leejcw/functional-pathtracers/blob/master/output/example-hs.png) (Haskell) *note the OCaml division by zero artifact*
 3. [green.gif](https://github.com/leejcw/functional-pathtracers/blob/master/output/green.gif) (OCaml)
 4. [pathtr.png](https://github.com/leejcw/functional-pathtracers/blob/master/output/pathtr.png) (Haskell)
 5. [specular.png](https://github.com/leejcw/functional-pathtracers/blob/master/output/specular.png) (OCaml)

### To Do
 * Artifacts in Haskell implementation due to non-uniform sampling
 * Make it faster?
