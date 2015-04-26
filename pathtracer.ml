type vec = float * float * float
type ray = vec * vec
type refl =
    Diff
  | Spec
  | Refr

let ( ++ ) ((x1, y1, z1): vec) ((x2, y2, z2): vec) : vec =
  (x1 +. x2, y1 +. y2, z1 +. z2)

let ( -- ) ((x1, y1, z1): vec) ((x2, y2, z2): vec) : vec =
  (x1 -. x2, y1 -. y2, z1 -. z2)

let ( *** ) ((x1, y1, z1): vec) ((x2, y2, z2): vec) : vec =
  (x1 *. x2, y1 *. y2, z1 *. z2)

let ( |* ) (a: float) ((x, y, z): vec) : vec =
  (a *. x, a *. y, a *. z)

let norm (v: vec) : vec =
  let (x, y, z) = v in
  (1. /. (sqrt (x *. x +. y *. y +. z *. z))) |* v

let dot ((x1, y1, z1): vec) ((x2, y2, z2): vec) : float =
  (x1 *. x2 +. y1 *. y2 +. z1 *. z2)

let cross ((x1, y1, z1): vec) ((x2, y2, z2): vec) : vec =
  (y1 *. z2 -. z1 *. y2, z1 *. x2 -. x1 *. z2, x1 *. y2 -. y1 *. x2)

let clamp (x: float) : float =
  if x < 0. then 0. else if x > 1. then 1. else x

let rgb (x: float) : int =
  int_of_float ((clamp x ** (1. /. 2.2)) *. 255. +. 0.5)

class sphere (rad: float) (pos: vec) (ems: vec) (col:vec) (rfl:refl) =
object (self)
  val r:float = rad
  val p:vec = pos
  val e:vec = ems
  val c:vec = col
  val rf:refl = rfl
  method ps = p
  method em = e
  method intersect ((ro, rd): ray) =
    let eps = 1.e-4 in
    let op = p -- ro in
    let b = dot op rd in
    let det = b *. b -. dot op op +. r *. r in
    if det < 0. then 0. else
      begin
	let d = sqrt det in
	let t = b -. d in
	if t > eps then t else
	  begin
	    let t2 = b +. d in
	    if t2 > eps then t2 else 0.
	  end
      end
  method color = (c, rf)
end

(* Currently hard-coded scene
   TODO: Change to parsing from config file. *)
let spheres = [|
  new sphere 1e5 ((1e5 +. 1.), 40.8, 81.6)
    (0., 0., 0.) (0.75, 0.25, 0.25) Diff;
  new sphere 1e5 ((-1e5 +. 99.), 40.8, 81.6)
    (0., 0., 0.) (0.25, 0.25, 0.75) Diff;
  new sphere 1e5 (50., 40.8, 1e5)
    (0., 0., 0.) (0.75, 0.75, 0.75) Diff;
  new sphere 1e5 (50., 40.8, (-1e5 +. 170.))
    (0., 0., 0.) (0., 0., 0.) Diff;
  new sphere 1e5 (50., 1e5, 81.6)
    (0., 0., 0.) (0.75, 0.75, 0.75) Diff;
  new sphere 1e5 (50., (-1e5 +. 81.6), 81.6)
    (0., 0., 0.) (0.75, 0.75, 0.75) Diff;
  new sphere 16.5 (27., 16.5, 47.)
    (0., 0., 0.) (0.6, 0.6, 0.6) Spec;
  new sphere 16.5 (73., 16.5, 78.)
    (0., 0., 0.) (0.65, 0.5, 0.999) Refr;
  new sphere 16.5 (55., 60., 30.)
    (0., 0., 0.) (1., 1., 0.3) Diff;
  new sphere 600. (50., 681.33, 81.6)
    (12., 12., 12.) (0., 0., 0.) Diff
	      |];;

let intersect (r: ray) : (float * int) =
  let n = Array.length spheres in
  let t = ref 1.e20 in
  let id = ref (- 1) in
  for i = 0 to n - 1 do
    let d = spheres.(i)#intersect r in
    if d > 0. && d < !t then (t := d; id := i);
  done;
  !t, !id

let rec radiance (r: ray) (d: int) : vec =
  let (t, i) = intersect r in
  if i < 0 then (0., 0., 0.) else
    begin
      let sph = spheres.(i) in
      let (ro, rd) = r in
      let v = ro ++ (t |* rd) in
      let n = norm (v -- sph#ps) in
      let n' = if dot n rd < 0. then n else - 1. |* n in
      let ((x, y, z), rf) = sph#color in
      let p = max x (max y z) in
      let f = fst sph#color in
      if (d > 5 && Random.float 1. > p) then sph#em else
	begin
	  let f = if d > 5 then (1. /. p |* f) else f in
	  begin match rf with
	      Diff ->
		let pi = 4. *. atan 1. in
		let o1 = 2. *. pi *. Random.float 1. in
		let o2 = Random.float 1. in
		let o2' = sqrt o2 in
		let (nx, ny, nz) = n' in
		let u = norm (cross (if abs_float nx > 0.1 then (0., 1., 0.)
		  else (1., 0., 0.)) n') in
		let u' = cross n' u in
		let u'' = norm
		  ((cos o1 *. o2' |* u) ++ 
		      (sin o1 *. o2' |* u') ++
		      (sqrt (1. -. o2) |* n')) in
		sph#em ++ (f *** radiance (v, u'') (d + 1))
	    | Spec ->
	      sph#em ++ (f ***
			   radiance (v, (rd -- (2. *. dot n rd |* n))) (d + 1))
	    | Refr ->
	      let rfr = (v, rd -- (2. *. dot n rd |* n)) in
	      let i = dot n n' > 0. in
	      let ior = 1.33 in (* TODO: read from config file *)
	      let ir = if i then 1. /. ior else ior in
	      let idd = dot rd n' in
	      let c2t = 1. -. ir *. ir *. (1. -. idd *. idd) in
	      if c2t < 0. then sph#em ++ (f *** radiance rfr (d + 1)) else
		begin
		  let td = norm (ir |* rd -- ((if i then 1. else - 1.) *.
						 idd *. ir *. sqrt c2t |* n))
		  in
		  let a = ior -. 1. in
		  let b = ior +. 1. in
		  let r0 = a *. a /. (b *. b) in
		  let c = 1. -. (if i then -. idd else dot td n) in
		  let r1 = r0 +. (1. -. r0) *. (c ** 5.) in
		  (* TODO: config the 5 *)
		  let r2 = 1. -. r1 in
		  let p0 = 0.25 +. 0.5 *. r1 in
		  let p1 = r1 /. p0 in
		  let p2 = r2 /. (1. -. p0) in
		  sph#em ++ (f *** (
		    if d > 2 then
		      begin
			if Random.float 1. < p0 then p1 |* radiance rfr (d + 1)
			else p2 |* radiance (v, td) (d + 1)
		      end
		    else
		      r1 |* radiance rfr (d + 1) ++
			  (r2 |* radiance (v, td) (d + 1))
		  ))
		end
	  end
	end
    end
      
let draw (w: int) (h: int) (ss: int) (file_name: string) : unit =
  let w' = float_of_int w in
  let h' = float_of_int h in
  let (co, cd) = ((50., 52., 295.6), norm (0., - 0.042612, - 1.)) in
  let ct = w' *. 0.5135 /. h' in
  let cx = (ct, 0., 0.) in
  let cy = 0.5135 |* (norm (cross cx cd)) in
  let c = Array.make (w * h) (0., 0., 0.) in
  for y = 0 to h - 1 do
    let y' = float_of_int y in
    for x = 0 to w - 1 do
      let x' = float_of_int x in
      Printf.printf "\r%5.2f%% complete" (100. *. y' /. (h' -. 1.));
      for sy = 0 to 1 do
	let i = (h - y - 1) * w + x in
	for sx = 0 to 1 do
	  let r = ref (0., 0., 0.) in
	  for s = 1 to ss do
	    let r1 = Random.float 2. in
	    let r2 = Random.float 2. in
	    let dx = if r1 < 1. then sqrt r1 -. 1. else 1. -. sqrt (2. -. r1)
	    in
	    let dy = if r2 < 1. then sqrt r2 -. 1. else 1. -. sqrt (2. -. r2)
	    in
	    let d = 
	     ((((float_of_int sx +. 0.5 +. dx) /. 2. +. x') /. w' -. 0.5) |*
		 cx) ++
		  ((((float_of_int sy +. 0.5 +. dy) /.
			2. +. y') /. h' -. 0.5) |* cy) ++ cd in
	    r :=  !r ++ ((1. /. float_of_int ss) |*
		radiance (co ++ (140. |* d), norm d) 0);
	  done;
	  let (rx, ry, rz) = !r in
	  Array.set c i (c.(i) ++ (0.25 |* (clamp rx, clamp ry, clamp rz)));
	done;
      done;
    done;
  done;
  let print_vec (o: out_channel) ((r, g, b): vec) : unit =
    Printf.fprintf o "%d %d %d " (rgb r) (rgb g) (rgb b) in
  let oc = open_out file_name in
  Printf.fprintf oc "P3\n%d %d\n255\n" w h;
  for i = 0 to (Array.length c - 1) do
    print_vec oc c.(i);
  done;
  close_out oc

;; if Array.length Sys.argv < 4 then
    Printf.printf "Please run with ./a.out <width> <height> <samples> [file]\n"
  else
    try
      let w = int_of_string Sys.argv.(1) in
      let h = int_of_string Sys.argv.(2) in
      let s = int_of_string Sys.argv.(3) in
      let f = 
	try (if Array.length Sys.argv > 4 then Sys.argv.(4) else "ocaml.ppm")
	with _ -> "ocaml.ppm" in
      draw w h s f
    with _ -> Printf.printf "Invalid arguments given.
              Please run with ./a.out <width> <height> <samples> [file] \n"
