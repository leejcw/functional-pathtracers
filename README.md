# functional-pathtracers
##### Code by Joyce Lee

#### OCaml and Haskell implementations of a basic Monte Carlo pathtracer
#### Inspired by CIS 460, 120, and 194 at UPenn

### What is it?
> Path tracing is a computer graphics Monte Carlo method of rendering images of three-dimensional scenes such that the global illumination is faithful to reality. Fundamentally, the algorithm is integrating over all the illuminance arriving to a single point on the surface of an object.

The above definition is courtesy of Wikipedia, and does a very good job of summing up what it is. It implies that the algorithm terminates when every pixel value converges, but in practice this is intractable. Instead, a maximum recursion depth is specified instead.

### Why functional?
The concept of pathtracing is mostly recursive, and simply iterated over a two dimensional array of pixels, which lends itself nicely to functional programs. They usually are not written in languages such as OCaml or Haskell because these run many times slower than languages like C or C++, even with compiler optimizations. However, the functional implementation is arguably more elegant and easier to understand.

### Check it out!
Take a look at the samples:

 1. [animation.gif](https://github.com/leejcw/functional-pathtracers/blob/master/output/animation.gif) (OCaml) *post-processed in C to denoise with a small Gaussian kernel and compress*
 2. [example.png](https://github.com/leejcw/functional-pathtracers/blob/master/output/example.png) (OCaml)
 3. [green.gif](https://github.com/leejcw/functional-pathtracers/blob/master/output/green.gif) (OCaml) *note the OCaml division by zero artifact*
 4. [pathtr.ppm](https://github.com/leejcw/functional-pathtracers/blob/master/output/pathtr.ppm) (Haskell)
 5. [specular.png](https://github.com/leejcw/functional-pathtracers/blob/master/output/specular.png) (OCaml)

### To Do
 * Artifacts in Haskell implementation due to non-uniform sampling
 * Allow a configuration file to be provided instead of a hard coded scene
 * Command line arguements for size and samples
 * Make it faster?