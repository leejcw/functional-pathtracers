# functional-pathtracers
##### Code by Joyce Lee

### What is it?
> Path tracing is a computer graphics Monte Carlo method of rendering images of three-dimensional scenes such that the global illumination is faithful to reality. Fundamentally, the algorithm is integrating over all the illuminance arriving to a single point on the surface of an object.

The above definition is courtesy of Wikipedia, and does a very good job of summing up what it is. It implies that the algorithm terminates when every pixel value converges, but in practice this is intractable. Instead, a maximum recursion depth is specified instead.

### Why functional?
The concept of pathtracing is mostly recursive, and simply iterated over a two dimensional array of pixels, which lends itself nicely to functional programs. They usually are not written in languages such as OCaml or Haskell because these run many times slower than languages like C or C++, even with compiler optimizations. However, the functional implementation is arguably more elegant and easier to understand.

### Check it out!
Take a look at the samples, which are all rendered by the OCaml implementation (though the animated one was post-processed in C to denoise with a small Gaussian kernel and compress).

### Bugs
The Haskell implementation is mostly correct, though there is a bug with "overexposure" and non-uniform randomness.